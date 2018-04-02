#'
#' Automate Laina's R codes step 1 to 2 
#' 
#' @author Richard Li
#' 
#' @param filename file location and name from STATA
#' @param years a character vector of the names for each five year period
#' @param idVar typically "v002"
#' @param regionVar typically "v024", for older surveys might ve "v101"
#' @param timeVar typically "per5"
#' @param ageVar typically "ageGrpD"
#' @param weightsVar typically "v005"
#' # @param maxItr max number of iterations to run in GLM, increase if not convergent warning is shown. GLM default is 
#' @param geo.recode the recode matrix to be used
#'
#' @return a matrix of period-region summary
#' 
#' @examples 
#'  # Phil 2013 example for Yuan
#'  filename <- "~/Dropbox/Mortality Estimation Project_Clark et al./Countries/Philippines/Analysis/Stata/Data/Phil2013mod.dta"
#'  years<-c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14")
#'  phil2013 <- countrySummary(filename = filename, years = years)
#'  

countrySummary <- function(filename, years = NULL, idVar = "v002", regionVar = "region", timeVar = "per5", ageVar = "ageGrpD", weightsVar = "v005", geo.recode = NULL){
  # check all elements are provided
  if(is.null(filename)){
    stop("No filename specified!")
  }
  if(is.null(years)){
    stop("Names of the 5-year intervals not provided!")
  }
  
  # load dependencies
  library(survey)
  library(foreign)
  # help function
  expit<-function(x){
    exp(x)/(1+exp(x))
  }
  
  logit<-function(x){
    log(x/(1-x))
  }
  
  # read data
  births<-read.dta(filename)
  cat("finish loading data\n")

  # Fix for Senegal
  if(sum(births$region == "NA") > 0){
    births <- births[births$region != "NA", ]
  }
  
  if(!regionVar %in% colnames(births)){
    if("v101" %in% colnames(births)){
      colnames(births)[which(colnames(births) == "v101")] <- regionVar
      warning("region variable not defined: using v101", immediate. = TRUE)
    }else if("v024" %in% colnames(births)){
      colnames(births)[which(colnames(births) == "v024")] <- regionVar
      warning("region variable not defined: using v024", immediate. = TRUE)
    }else{
      stop("region variable not defined, and no v101 or v024!")
    }
  }

  # recode geo information
  if(!is.null(geo.recode)){
    births <- ChangeRegion(births, Bmat = geo.recode, regionVar = regionVar)
  }

  # create new variables
  births$region0 <- births[, regionVar]
  births$id0 <- births[, idVar]
  births$weights0 <- births[, weightsVar]
  births$time0 <- births[, timeVar]
  births$age0 <- births[, ageVar]

  # fix for input data issue
  if(sum(c(0, 1, 12, 24, 36, 48) %in% births$age0 ) == 6){
    births$age0[births$age0 == 0] <- "0"
    births$age0[births$age0 == 1] <- "1-11"
    births$age0[births$age0 == 12] <- "12-23"
    births$age0[births$age0 == 24] <- "24-35"
    births$age0[births$age0 == 36] <- "36-47"
    births$age0[births$age0 == 48] <- "48-59"
  }
  
  # create warnings if there are different levels in time periods
  # hardcode fix for some Countries
  if("00-03" %in% births$time0){
    warning("change 00-03 to 00-04!", immediate. = TRUE)
    births$time0 <- as.character(births$time0)
    births$time0[which(births$time0 == "00-03")] <- "00-04"
  }
  if("00-06" %in% births$time0){
    warning("change 00-06 to 00-04!", immediate. = TRUE)
    births$time0 <- as.character(births$time0)
    births$time0[which(births$time0 == "00-06")] <- "00-04"
  }
  if("85-99" %in% births$time0){
    warning("change 85-99 to 85-89!", immediate. = TRUE)
    births$time0 <- as.character(births$time0)
    births$time0[which(births$time0 == "85-99")] <- "85-89"
  }
   if("95=99" %in% births$time0){
    warning("change 95=99 to 95-99!", immediate. = TRUE)
    births$time0 <- as.character(births$time0)
    births$time0[which(births$time0 == "95=99")] <- "95-99"
  }
  time_inconsistent <- which(!(births$time0 %in% years))
  if(length(time_inconsistent) > 0){
    warning(paste("Name for time periods are inconsistent. Found the following levels in data:", 
                  unique(births$time0[time_inconsistent])), immediate. = TRUE)
  }
  
  
  # setting up survey design object
  ## @todo remove this hard-coded ~v001 + v002 by evaluating a string
  if(is.null(births$cluster)){
    if("v002" %in% colnames(births)){
      births$cluster <- "~v001 + v002"
      warning("Cluster not specified. Using v001 + v002", immediate. = TRUE)
    }else{
      births$cluster <- "~v001"
      warning("Cluster not specified. v002 not specified. Using v001", immediate. = TRUE)
    }
  }else{
      print(births$cluster[1])
  }

  if(is.null(births$strata)){
    stop("Strata not defined.")
  }
  options( survey.lonely.psu = "adjust")
  my.svydesign <- svydesign(id= formula(births$cluster[1]), 
                            strata = ~strata, 
                            nest=T, 
                            weights= ~weights0, data=births)

  # get region list, sorted alphabetically
  regions_list <- as.character(sort(names(table(births$region0))[as.vector(table(births$region0)!=0)]))
  regions_num <- 1:length(regions_list)
  
  # add "All" to all regions
  regions_list <- c("All", regions_list)
  regions_num <- c(0, regions_num)
    
  # create result data frame 
  results<-data.frame(region=rep(regions_list,each=length(years)))
  results$region_num <-rep(regions_num,each=length(years))
  results$years<-rep(years,length(regions_list))
  # add empty variables
  results$var.est <- results$logit.est <- results$upper <- results$lower <- results$u5m <- NA
  
  # updated helper function: region.time.HT
  # notes: the original function only works when the selected area-time combination has data. Add a new line of codes so that when there is no data for selected combination, return a line of NA values
  # updated Aug 17, 2015: Enable area = "All"
  
  region.time.HT.withNA<-function(area, time){
    cat(".")
    if(area == "All"){
      tmp <- subset(my.svydesign,(time0==time))      
    }else{
      tmp<-subset(my.svydesign,(time0==time & region0==as.character(area)))
    }
    if(dim(tmp)[1] == 0){
      return(rep(NA, 5))
    }else if(sum(tmp$variables$died) == 0){
      warning(paste0(area, " ", time, " has no death, set to NA\n"), immediate. = TRUE)
      return(rep(NA, 5))
    }else{
      glm.ob<-svyglm(died~(-1)+factor(age0),design=tmp,family=quasibinomial, maxit = 50)
      return(get.est.withNA(glm.ob))
    }
  }
  
  # updated helper function: get.est
  # notes: the original function only works when all age group exist, if some age groups are missing, the dimension from covariance matrix will not match the ns vector
  # note: not vary satisfying modification yet since the "factor(ageGrpD)"
  
  get.est.withNA<-function(glm.ob){
    ## initialize with the full covariance matrix
    V <- matrix(0, 6, 6)
    betas <- rep(NA, 6)
    labels <- c("0", "1-11", "12-23", "24-35", "36-47", "48-59")
    labels <- paste("factor(age0)", labels, sep = "")
    colnames(V) <- rownames(V) <- labels
    names(betas) <- labels
    # now get the regression covariance matrix 
    V2<-vcov(glm.ob)
    # check if the columns match
    if(length(which(colnames(V2) %in% colnames(V) == FALSE)) > 0){
      stop("Error for input age group names!")
    }
    # fill in the full V
    V[rownames(V2), colnames(V2)] <- V2
    
    ## same fill for betas
    ## under 5 child mortality for males in 00-02 ##
    betas2<-summary(glm.ob)$coef[,1]
    betas[names(betas2)] <- betas2 
    
    # (removed) avoid NA by replacing with 0
    # This is wrong way to handle NA! messed up the mean estimates
    # betas[which(is.na(betas))] <- 0
    
    ns<-c(1,11,12,12,12,12)
    probs<-expit(betas)
  
    u5m.est<-(1-prod((1-probs)^ns, na.rm = TRUE))#*1000  
    
    ## partial derivatives ##
    gamma<-prod((1+exp(betas))^ns, na.rm = TRUE)
    derivatives<-(gamma)/(gamma-1)*ns*expit(betas)
    
    ## now handle the NA in derivatives, it won't affect mean estimation
    derivatives[which(is.na(derivatives))] <- 0
    ## Items to return ##
    var.est<-t(derivatives)%*%V%*%derivatives
    lims<-logit(u5m.est)+qnorm(c(0.025,0.975))*sqrt(var.est)
    return(c(u5m.est,expit(lims),logit(u5m.est),var.est))
  }

  # run for all combinations
  x<-mapply(region.time.HT.withNA,
            area = results$region,
            time = results$years)
  results[,4:8]<-t(x)

  # some test scripts...    
  #   for(i in 1:30){
  #     test <- region.time.HT.withNA(results$region[i], results$years[i])
  #     if(length(which(is.na(test)))>0) cat(i)
  #   }
  
  return(results)
}


#'
#' Making region names consistent
#'
#' @param data default the region variable is coded as "region" column
#' @param Bmat matrix of changes
#' 
#' @return data after changing region names
#' 

ChangeRegion <- function(data, Bmat, regionVar = "region"){
  final_names <- colnames(Bmat)
  current_names <- rownames(Bmat)
  nf <- length(final_names)
  nc <- length(current_names)
  current_region <- as.character(data[, regionVar])
  
  # check if there are regions not contained in the names given
  missing <- which(current_region %in% current_names == FALSE)
  if(length(missing) > 0){
      missingregion <- unique(current_region[missing])
       warning(paste("Name for regions are inconsistent. Found the following regions in data:", missingregion), immediate. = TRUE)    
  }

  # count changes
  nchange <- ncount <- 0
  
  for(i in 1:nc){
    tmp <- Bmat[i, ]
    if(sum(tmp) > 1){
      stop(paste("more than one region to map to: ", current_names[i]))
    }
    else if(sum(tmp) == 1){
      which <- which(current_region == current_names[i])
      to <- final_names[which(tmp == 1)]
      if(current_names[i] != to && length(which) > 0){
        nchange <- nchange + 1
        ncount <- ncount + length(which)
      }
      current_region[which] <- to
    }
    else if(sum(tmp) == 0){
      # nchange <- nchange + 1
      # ncount <- ncount + length(which(current_region == current_names[i]))
    }
  }
  cat(paste(nchange, "names changed, in total", ncount, "rows in data changed"))
  data[, regionVar] = current_region
  return(data)
}