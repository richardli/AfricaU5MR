#'
#' Function to fit INLA to the combined dataset
#'
#' @param R (R, 1/R) is set to be residual odds ratio
#' @param nsamp sample to simulate for scaling factor
#' @param nsamp.check sample to simulate for checking range
#' @param Amat
#'
simhyper <- function(R = 2, nsamp = 100000, nsamp.check = 5000, Amat, nperiod = 6){
    ####################################################################
    # (R,1/R) is the range of the residual odds ratios
    # gives a=d/2 where d = degrees of freedom of marginal Student’s t
    d <- 1; a <- d/2; p <- 0.025; b <-(log(R))^2*d/(2*qt(p,df=d)^2)
    a.iid = a
    b.iid = b
    #####################################################################
    # Check range using simulation #
    tausamp <- rgamma(nsamp,a,b)
    Usamp <- rnorm(nsamp,mean=0,sd=1/sqrt(tausamp))
    # The adjacency matrix for 6 years
    m2 = c(1,rep(2, nperiod - 2),1)
    # create the adjacency #
    before <- 1:(nperiod - 2)
    after <- 3:(nperiod)
    alternate <- c(rbind(before, after)) 
    adj2<-c(2, alternate, nperiod - 1)

    make.Q <- function(num.neighbors, neighbors, omega.sq = 1){
      n <- length(num.neighbors)
      mat <- matrix(0, ncol = n, nrow = n)
      diag(mat) <- num.neighbors
      mat[cbind(rep(1:n, num.neighbors), neighbors)] <- -1
      mat/omega.sq
    }
    vars.Q <- function(eigenvalues,eigenvectors){
      margsum <- 0
      nloop <- length(eigenvalues)-1
      for (i in 1:nloop){
        ev <- eigenvectors[,i]
        margsum <- margsum + ev %*% t(ev)/ eigenvalues[i]
      }
      margvars <- diag(margsum)
      margvars
    }
    #
    sim.Q <- function(Q){
      eigenQ <- eigen(Q)
      rankQ <- qr(Q)$rank
      sim <- as.vector(eigenQ$vectors[,1:rankQ] %*%
                         matrix(
                           rnorm(rep(1, rankQ), rep(0, rankQ),
                           1/sqrt(eigenQ$values[1:rankQ])),
                           ncol = 1))
      sim
    }
    #
    Q <- make.Q(m2, adj2, 1)
    eigentemp <- eigen(Q)
    eigenvaluesQ <- eigentemp$values
    eigenvectorsQ <- eigentemp$vectors
    rankQ <- qr(Q)$rank # 5
    margy <- mean(vars.Q(eigenvaluesQ,eigenvectorsQ))
    a.rw1 = a
    b.rw1 = b/margy
    taustarsamp <- rgamma(nsamp.check,a.rw1,b.rw1)
    Ustarsamp <- matrix(nrow=nsamp.check,ncol=nperiod)
    for (i in 1:nsamp.check){
      Qstar <- Q*taustarsamp[i]
      Ustarsamp[i,] <- sim.Q(Qstar)
    }
    check.rw1 = quantile(exp(Ustarsamp),p=c(0.025,0.5,0.975)) 
    
   
    # tausamp <- rgamma(nsamp,a,b)
    # Usamp <- rnorm(nsamp,mean=0,sd=1/sqrt(tausamp))
    # quantile(exp(Usamp),p=c(0.025,0.5,0.975))
    # m2 <- c(2, 3, rep(4, nperiod - 4), 3, 2)
    # # create the adjacency #
    # before2 <- 1:(nperiod - 4)
    # before1 <- 2:(nperiod - 3)
    # after1 <- 4:(nperiod - 1)
    # after2 <- 5:(nperiod)
    # alternate <- c(rbind(before1, before2, after1, after2)) 

    # # adj2<-c(2, 3, 
    # #         1, 3, 4,
    # #         alternate, 
    # #         nperiod - 3, nperiod - 3, nperiod, 
    # #         nperiod - 2, nperiod - 1)
    # # Q <- make.Q(m2, adj2, 1)
    # The adjacency matrix for 6 years

    if(nperiod > 4){
      Q<-matrix(0,nrow=nperiod,ncol=nperiod)
      Q[1,1:3]<-c(1,-2,1)
      Q[2,1:4]<-c(-2,5,-4,1)
      for(j in 3:(nperiod-2)){
          Q[j,(j-2):(j+2)]<-c(1,-4,6,-4,1)
      }
      Q[nperiod,(nperiod-2):nperiod]<-c(1,-2,1)
      Q[nperiod-1,(nperiod-3):nperiod]<-c(1,-4,5,-2)
    }else{
      stop("RW2 prior not specified for n < 5")
    }
    vars.Q <- function(eigenvalues,eigenvectors, rankQ){
      margsum <- 0
      nloop <- rankQ
      for (i in 1:nloop){
        ev <- eigenvectors[,i]
        margsum <- margsum + ev %*% t(ev)/ eigenvalues[i]
      }
      margvars <- diag(margsum)
    margvars 
    }
    sim.Q <- function(Q){
      eigenQ <- eigen(Q)
      rankQ <- qr(Q)$rank
      sim <- as.vector(eigenQ$vectors[,1:rankQ] %*%
                         matrix(
                           rnorm(rep(1, rankQ), rep(0, rankQ),
                           1/sqrt(eigenQ$values[1:rankQ])),
                           ncol = 1))
    sim
    }
    eigentemp <- eigen(Q)
    eigenvaluesQ <- eigentemp$values
    eigenvectorsQ <- eigentemp$vectors
    rankQ <- qr(Q)$rank # 4
    margy <- mean(vars.Q(eigenvaluesQ,eigenvectorsQ, rankQ))
    a.rw2 = a
    b.rw2 = b/margy
    taustarsamp <- rgamma(nsamp.check,a.rw2,b.rw2)
    Ustarsamp <- matrix(nrow=nsamp.check,ncol=nperiod)
    for (i in 1:nsamp.check){
      Qstar <- Q*taustarsamp[i]
      Ustarsamp[i,] <- sim.Q(Qstar)
    }
    check.rw2 = quantile(exp(Ustarsamp),p=c(0.025,0.5,0.975)) 
    
   
    ####################################################################
    tausamp <- rgamma(nsamp,a,b)
    Usamp <- rnorm(nsamp,mean=0,sd=1/sqrt(tausamp))
    m2 <- apply(Amat,1,sum)
    # create the adjacency list #
    nums<-c(1:dim(Amat)[1])
    adj2<-NULL
    for(i in 1:dim(Amat)[1]){
      adj2<-c(adj2,nums[as.numeric(Amat[i,])==1])
    }
    make.Q <- function(num.neighbors, neighbors, omega.sq = 1){
      n <- length(num.neighbors)
      mat <- matrix(0, ncol = n, nrow = n)
      diag(mat) <- num.neighbors
      mat[cbind(rep(1:n, num.neighbors), neighbors)] <- -1
      mat/omega.sq
    }
    vars.Q <- function(eigenvalues,eigenvectors, rankQ){
      margsum <- 0
      nloop <- rankQ
      for (i in 1:nloop){
        ev <- eigenvectors[,i]
        margsum <- margsum + ev %*% t(ev)/ eigenvalues[i]
      }
      margvars <- diag(margsum)
    margvars 
    }
    #
    sim.Q <- function(Q){
      eigenQ <- eigen(Q)
      rankQ <- qr(Q)$rank
      sim <- as.vector(eigenQ$vectors[,1:rankQ] %*%
                         matrix(
                           rnorm(rep(1, rankQ), rep(0, rankQ),
                           1/sqrt(eigenQ$values[1:rankQ])),
                           ncol = 1))
    sim
    }
    #
    Q <- make.Q(m2, adj2, 1)
    eigentemp <- eigen(Q)
    eigenvaluesQ <- eigentemp$values
    eigenvectorsQ <- eigentemp$vectors
    rankQ <- qr(Q)$rank # 20
    margy <- mean(vars.Q(eigenvaluesQ,eigenvectorsQ, rankQ))
    a.icar = a
    b.icar = b/margy
    taustarsamp <- rgamma(nsamp.check,a.icar, b.icar)
    Ustarsamp <- matrix(nrow=nsamp.check,ncol=dim(Amat)[1])
    for (i in 1:nsamp.check){
      Qstar <- Q*taustarsamp[i]
      Ustarsamp[i,] <- sim.Q(Qstar)
    }
    check.icar = quantile(exp(Ustarsamp),p=c(0.025,0.5,0.975))
  return(list(a.iid = a.iid, 
              b.iid = b.iid, 
              a.rw1 = a.rw1, 
              b.rw1 = b.rw1, 
              check.rw1 = check.rw1, 
              a.rw2 = a.rw2, 
              b.rw2 = b.rw2, 
              check.rw2 = check.rw2, 
              a.icar = a.icar, 
              b.icar = b.icar, 
              check.icar = check.icar))
}
#'
#' Function to fit INLA to the combined dataset
#'
#' @param data combiend dataset
#' @param Amat adj matrix
#' @param na.rm (not currently used)
#' @param case 
#' @param rest parameters to be updated 
#'
fitINLA <- function(data, Amat, year_names,
                    na.rm = TRUE,
                    mu0 = log(0.1/0.9),
                    sig2.0 = 10000,
                    redo.prior = FALSE,
                    use.prior = NULL,
                    case = 11,
                    a.iid = NULL,
                    b.iid = NULL,
                    a.rw1 = NULL,
                    b.rw1 = NULL,    
                    a.icar = NULL,
                    b.icar = NULL){
  require(INLA)
  
  ####################################################################
  # Re-calculate hyper-priors
  if(redo.prior){
      priors <- simhyper(R = 2, nsamp = 100000, nsamp.check = 5000, Amat = Amat, nperiod = length(year_names))
  }  
  if(!is.null(use.prior)){
    a.iid <- priors$a.iid
    b.iid <- priors$b.iid
    a.rw1 <- priors$a.rw1
    b.rw1 <- priors$b.rw1
    a.rw2 <- priors$a.rw2
    b.rw2 <- priors$b.rw2
    a.icar <- priors$a.icar
    b.icar <- priors$b.icar
  }

  ####################################################################
#   # remove NA rows? e.g. if no 10-14 available
  if(na.rm){
    na.count <- apply(data, 1, function(x){length(which(is.na(x)))})
    to_remove <- which(na.count == 6)
    if(length(to_remove) > 0) data <- data[-to_remove, ]
  }
  ####################################################################
  # get the list of region and numeric index in one data frame
  region_names <- colnames(Amat)#names(table(data$region))
  # reorder as in Amat
  # region_names <- region_names[match(colnames(Amat), region_names)]
  region_count <- length(region_names)                      
  regions <- data.frame(region = region_names,
                      region_number = seq(1, region_count))
  
  # -- merging in the alphabetical region number 1:21 -- #
  dat <- merge(data, regions, by="region")
  
  # -- creating IDs for the spatial REs -- #
  dat$region.struct <- dat$region.unstruct <- dat$region_number
  
  ###################################################################
  # get the lsit of region and numeric index in one data frame
  year_count <- length(year_names)                      
  years <- data.frame(year = year_names,
                      year_number = seq(1, year_count))
  
  # -- creating IDs for the temporal REs -- # 
  dat$time.unstruct <- dat$time.struct <- years[match(dat$years, years[,1]), 2]
  
  ##################################################################
  # get the number of surveys
  survey_count <- length(table(data$survey))
  ##################################################################
  # --  these are the time X survey options -- #
  x <- expand.grid(1:year_count, 1:survey_count)
  survey.time <- data.frame(time.unstruct = x[,1],
                            survey = x[,2],
                            survey.time = c(1:nrow(x)))
  
  # -- these are the area X survey options -- #
  x<-expand.grid(1:region_count, 1:survey_count)
  survey.area<-data.frame(region_number=x[,1], survey=x[,2],
                          survey.area=c(1:nrow(x)))
  
  # -- these are the area X time options -- #
  x<-expand.grid(1:region_count,1:year_count)
  time.area<-data.frame(region_number = x[,1], time.unstruct=x[,2],
                        time.area=c(1:nrow(x)))
  
  # -- these are the area X time X survey options -- #
  x<-expand.grid(1:region_count, 1:year_count, 1:survey_count)
  survey.time.area<-data.frame(region_number=x[,1], time.unstruct=x[,2],
                               survey=x[,3], survey.time.area=c(1:nrow(x)))
  
  # -- merge these all into the data sets -- #
  newdata <- dat
  if(sum(!is.na(dat$survey)) > 0){
    newdata <-merge(newdata, survey.time,by=c("time.unstruct","survey"))
    newdata <-merge(newdata, survey.area,by=c("region_number","survey"))
    newdata <-merge(newdata, survey.time.area,by=c("region_number","time.unstruct","survey"))
  }
  newdata <-merge(newdata, time.area,by=c("region_number","time.unstruct"))
  
  # -- interactions for structured space-time effects -- #
  newdata$idII<-newdata$time.unstruct
  newdata$groupII<-newdata$region.unstruct
  
  newdata$idIII<-newdata$region.unstruct
  newdata$groupIII<-newdata$time.unstruct
  
  newdata$idIV<-newdata$region.unstruct
  newdata$groupIV<-newdata$time.unstruct

  #-------------------------------------------------------#
  # ---------------- Beginning of Step 6 -----------------#
  #-------------------------------------------------------#
  # Fit space-time models and compare using LCPO.
  # this section includes one example all of the other models
  # you might want to consider are included at the end of the doc
  
  ##########################
  ### Model Selection ######
  ##########################
  
  # -- subset of not missing and not direct estimate of 0 -- #
  exdat<-newdata
  exdat<-exdat[!is.na(exdat$logit.est) & exdat$logit.est > (-20), ]
  
  
  # -- prior distributions -- #
  
  # -- fitting the model in INLA -- #
  if(case == 0){
    # test case
     mod<-logit.est~f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.struct,model="rw1",param=c(a.rw1,b.rw1))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))
     # +f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 1){
    mod<-logit.est~f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw1",param=c(a.rw1,b.rw1))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 2){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw1",param=c(a.rw1,b.rw1))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 3){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(survey.area,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw1",param=c(a.rw1,b.rw1))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 4){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(survey.time,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw1",param=c(a.rw1,b.rw1))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 5){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(survey.area,model="iid", param=c(a.iid,b.iid))+f(survey.time,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw1",param=c(a.rw1,b.rw1))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 6){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(survey.time.area,model="iid", param=c(a.iid,b.iid))+f(survey.area,model="iid", param=c(a.iid,b.iid))+f(survey.time,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw1",param=c(a.rw1,b.rw1))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  # -- type I Space-Time Interactions, w/ RW2 -- #
  if(case == 7){
    mod<-logit.est~f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw2",param=c(a.rw2,b.rw2))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 8){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw2",param=c(a.rw2,b.rw2))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 9){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(survey.area,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw2",param=c(a.rw2,b.rw2))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 10){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(survey.time,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw2",param=c(a.rw2,b.rw2))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 11){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(survey.area,model="iid", param=c(a.iid,b.iid))+f(survey.time,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw2",param=c(a.rw2,b.rw2))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 12){
    mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(survey.time.area,model="iid", param=c(a.iid,b.iid))+f(survey.area,model="iid", param=c(a.iid,b.iid))+f(survey.time,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw2",param=c(a.rw2,b.rw2))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 13){
    mod <- logit.est ~ f(region.unstruct,model="iid",param=c(a.iid,b.iid)) +
            f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar)) +
            f(time.struct,model="rw1",param=c(a.rw1,b.rw1))  +
            f(time.unstruct,model="iid",param=c(a.iid,b.iid)) +
            f(time.area,model="iid", param=c(a.iid,b.iid))
  }
  if(case == 14){
    mod <- logit.est ~ f(region.unstruct,model="iid",param=c(a.iid,b.iid)) +
            f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar)) +
            f(time.struct,model="rw2",param=c(a.rw2,b.rw2),extraconstr = list(A = matrix(c(rep(1, 7)), 1, 7), e = 0))  +
            f(time.unstruct,model="iid",param=c(a.iid,b.iid)) +
            f(time.area,model="iid", param=c(a.iid,b.iid))
  }

  # mod<-logit.est~f(survey,model="iid", param=c(a.iid,b.iid))+f(survey.area,model="iid", param=c(a.iid,b.iid))+f(survey.time,model="iid", param=c(a.iid,b.iid))+f(region.unstruct,model="iid",param=c(a.iid,b.iid))+f(region.struct, graph=Amat,model="besag",param=c(a.icar,b.icar))+f(time.struct,model="rw2",param=c(a.rw2,b.rw2))+f(time.unstruct,model="iid",param=c(a.iid,b.iid))+f(time.area,model="iid", param=c(a.iid,b.iid))
  
  inla11 <- inla(mod, 
                 family = "gaussian", 
                 control.compute=list(dic=T,mlik=T,cpo=T),
                 data =exdat,
                 control.predictor=list(compute=TRUE),
                 control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
                 scale=exdat$logit.prec)
  
  return(list(model = mod,
              fit = inla11, 
              Amat = Amat, 
              newdata = exdat, 
              time = seq(0, year_count - 1), 
              area = seq(0, region_count - 1),
              survey.time = survey.time, 
              survey.area = survey.area, 
              time.area = time.area,
              survey.time.area = survey.time.area,
              a.iid = a.iid, 
              b.iid = b.iid, 
              a.rw1 = a.rw1, 
              b.rw1 = b.rw1, 
              a.rw2 = a.rw2, 
              b.rw2 = b.rw2, 
              a.icar = a.icar, 
              b.icar = b.icar))
  
}

projINLA <- function(fitted, proj.time, which.area, get_sample = FALSE, nsample=1000){
  
  expit<-function(x){
      exp(x)/(1+exp(x))
  }
  model <- fitted$model
  data <- fitted$newdata
  fit <- fitted$fit
  Amat <- fitted$Amat
  n.region <- dim(Amat)[1]
  a.iid <- fitted$a.iid
  b.iid <- fitted$b.iid 
  a.rw1 <- fitted$a.rw1 
  b.rw1 <- fitted$b.rw1 
  a.rw2 <- fitted$a.rw2
  b.rw2 <- fitted$b.rw2 
  a.icar <- fitted$a.icar 
  b.icar <- fitted$b.icar 
  # need to create a missing 7th time point if 10-14 has no data, 
  # otherwise, project one further
  if(proj.time == "05-09"){
    n.time <- 6
  }else if(proj.time == "10-14"){
    n.time <- 7
  }else if(proj.time == "15-19"){
    n.time <- 8
  }

  exist <- which(data$time.unstruct == n.time)
  if(sum(exist) > 0){
    data <- data[-exist, ]
  }

  # tmp<-data[data$survey==max(data$survey) & data$time.struct==1,]
  # refine this line if the last survey does not cover all regions...
  tmp<-data[match(unique(data$region), data$region), ]
  tmp$time.unstruct<-tmp$time.struct<-tmp$idII<-tmp$groupIII<-tmp$groupIV<-n.time
  tmp$logit.est<-tmp$logit.prec<-tmp$survey<-tmp$survey.time<-tmp$survey.area<-tmp$survey.time.area<-NA
  tmp$time.area<-(n.region * (n.time - 1) + 1) : (n.region * n.time)
  tmp$years<-proj.time
  tmp$u5m <- tmp$lower <- tmp$upper <- tmp$var.est <- NA
  exdatproj<-rbind(data,tmp)


  smoothed<-rep(NA,n.time)
  upper<-rep(NA,n.time)
  lower<-rep(NA,n.time)
  sample <- matrix(NA, n.time, nsample)

  # - which space-time interaction IDs do we need - #
  spacetimenum<-unique(exdatproj[exdatproj$region==which.area,c("time.area")])
  # - which numeric value this area is coded - #
  spacenum <- unique(exdatproj[exdatproj$region==which.area,c("region_num")])

  for(i in 1:n.time){       
        time<-rep(NA,n.time)
        time[i]<-1
        area<-rep(NA,n.region)
        area[spacenum]<-1
        space.time<-rep(NA, n.region * n.time)
        space.time[spacetimenum[i]]<-1
        
        
        lc1<-inla.make.lincomb("(Intercept)" = 1,
                               region.unstruct=area,
                               region.struct=area, 
                               time.struct=time,
                               time.unstruct=time,
                               time.area=space.time)
        
        mod <- inla(model, 
                    family = "gaussian", 
                    data =exdatproj, lincomb=lc1,
                    control.predictor=list(compute=TRUE),
                    control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
                    scale=logit.prec)
        
        smoothed[i]<-expit(mod$summary.lincomb.derived[,"0.5quant"])
        upper[i]<-expit(mod$summary.lincomb.derived[,"0.975quant"])
        lower[i]<-expit(mod$summary.lincomb.derived[,"0.025quant"])
        sample[i, ] <- inla.rmarginal(nsample, mod$marginals.lincomb.derived[[1]])
        cat(".")
  }
  cat("\n")
  if(get_sample){
    return(sample)
  }else{
    return(data.frame(cbind(smoothed,lower,upper)))
  }
}


# multiple projection wrapper, note proj.time needs to be ordered
projINLA_multi <- function(fitted, proj.time, which.area, quantiles, return_raw = FALSE){
  quantlabel <- paste0(quantiles, "quant")
  expit<-function(x){
      exp(x)/(1+exp(x))
  }
  model <- fitted$model
  data <- fitted$newdata
  fit <- fitted$fit
  Amat <- fitted$Amat
  n.region <- dim(Amat)[1]
  a.iid <- fitted$a.iid
  b.iid <- fitted$b.iid 
  a.rw1 <- fitted$a.rw1 
  b.rw1 <- fitted$b.rw1 
  a.rw2 <- fitted$a.rw2
  b.rw2 <- fitted$b.rw2 
  a.icar <- fitted$a.icar 
  b.icar <- fitted$b.icar 
  # need to create a missing 7th time point if 10-14 has no data, 
  # otherwise, project one further
  if(length(proj.time) == 1){
      if(proj.time == "05-09"){
        n.time <- 6
      }else if(proj.time == "10-14"){
        n.time <- 7
      }else if(proj.time == "15-19"){
        n.time <- 8
      }    
  }else if(length(proj.time) == 2){
      if(proj.time[2] == "10-14"){
        n.time <- c(6, 7)
      }else if(proj.time[2] == "15-19"){
        n.time <- c(7, 8)
      }    
  }else if(length(proj.time) == 3){
      n.time <- c(6, 7, 8)
  }

  exist <- which(data$time.unstruct %in% n.time)
  if(sum(exist) > 0){
    data <- data[-exist, ]
  }

  # tmp<-data[data$survey==max(data$survey) & data$time.struct==1,]
  # refine this line if the last survey does not cover all regions...
  exdatproj <- data
  for(nextTime in n.time){
    tmp<-data[match(unique(data$region), data$region), ]
    tmp$time.unstruct<-tmp$time.struct<-tmp$idII<-tmp$groupIII<-tmp$groupIV<-nextTime
    tmp$logit.est<-tmp$logit.prec<-tmp$survey<-tmp$survey.time<-tmp$survey.area<-tmp$survey.time.area<-NA
    tmp$time.area<-(n.region * (nextTime - 1) + 1) : (n.region * nextTime)
    tmp$years<-proj.time[nextTime - 5]
    tmp$u5m <- tmp$lower <- tmp$upper <- tmp$var.est <- NA
    exdatproj<-rbind(exdatproj,tmp[, colnames(exdatproj)])
  }


  smoothed<-rep(NA,max(n.time))
  upper<-rep(NA,max(n.time))
  lower<-rep(NA,max(n.time))
  raw <- matrix(0, max(n.time), 1000)

  # - which space-time interaction IDs do we need - #
  spacetimenum<-unique(exdatproj[exdatproj$region==which.area,c("time.area")])
  # - which numeric value this area is coded - #
  spacenum <- unique(exdatproj[exdatproj$region==which.area,c("region_num")])

  for(i in 1:max(n.time)){       
        time<-rep(NA,max(n.time))
        time[i]<-1
        area<-rep(NA,n.region)
        area[spacenum]<-1
        space.time<-rep(NA, n.region * max(n.time))
        space.time[spacetimenum[i]]<-1
        
        
        lc1<-inla.make.lincomb("(Intercept)" = 1,
                               region.unstruct=area,
                               region.struct=area, 
                               time.struct=time,
                               time.unstruct=time,
                               time.area=space.time)
        
        mod <- inla(model, 
                    family = "gaussian", 
                    data =exdatproj, lincomb=lc1,
                    control.predictor=list(compute=TRUE),
                    control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
                    scale=logit.prec, 
                    quantiles = quantiles)
        
        smoothed[i]<-expit(mod$summary.lincomb.derived[,quantlabel[2]])
        upper[i]<-expit(mod$summary.lincomb.derived[,quantlabel[3]])
        lower[i]<-expit(mod$summary.lincomb.derived[,quantlabel[1]])
        raw[i, ] <- inla.rmarginal(1000, mod$marginals.lincomb.derived[[1]])
        cat(".")
  }
  cat("\n")
  if(return_raw){
    return(raw)
  }else{
    return(data.frame(cbind(smoothed,lower,upper)))
  }
}

#' Function to post-process sd of random effects from INLA fit
#'
#' @param inla.fit INLA object
#' @param prior matrix of two columns, specifying Gamma hyper prior for precision
#' @nsample number of samples to draw
#'
#' @return Mean, 2.5%, 50% and 97.5% percentile of the draws of sd.

SDcompareINLA <- function(inla.fit = NULL, prior = NULL, nsample = 1e6){
 

  if(!(is.null(inla.fit))){
      precs <- inla_model[[1]]$fit$summary.hyperpar
      npar <- dim(precs)[1]
      out <- matrix(NA, npar, 4)
      names <- rep(NA, npar)

      for(i in 1:npar){
            # get summary stats
            out[i, ] <- c(1/precs[i, 1]^0.5, 
                          1/precs[i, 3]^0.5, 
                          1/precs[i, 4]^0.5, 
                          1/precs[i, 5]^0.5)   
            # get random effect name
            names[i] <- gsub(x = rownames(precs)[i], "Precision for ", "") 
      }
      rownames(out) <- names
      colnames(out) <- c("Mean", "Lower",  "Median","Upper")
      return(out)

  }else if(!(is.null(prior))){
      
      if(is.null(dim(prior))){
        prior <- matrix(prior, ncol = 2)
      }
      
      npar <- dim(prior)[1]
      names <- rownames(prior)
      out <- matrix(NA, npar, 4)
      for(i in 1:npar){
          prior.sample <- 1/sqrt(rgamma(nsample, prior[i, 1], prior[i, 2]))
          out[i, ] <- c(mean(prior.sample, na.rm = T), quantile(prior.sample, c(0.025, 0.5, 0.975), na.rm = T))  
      }
      rownames(out) <- names
      colnames(out) <- c("Mean", "Lower", "Median", "Upper")
      return(out)
  }

}