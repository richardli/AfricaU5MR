## Script for reproducing each single countries
##
##
##
if(FALSE){
	## Script to run only Cross validation
	start_time <- Sys.time()
	countries <- list.files("../Data/map/")
	countries <- countries[countries %in% c("AfricanCountries", "World", "Bangladesh", "Cambodia", "Indonesia", "Philippines") == FALSE]
	for(countryname in countries){
		print(countryname)
		source("CrossValidation.R")
	}
	end_time <- Sys.time()
	print(end_time - start_time)  
}
print("======= Start Cross Validation =======")
library(gdata)
library(foreign)
library(spdep)
library(maptools)
library(gpclib)
gpclibPermit()
library(plyr)
library(INLA)
inla.setOption(mkl=TRUE) 
library(ggplot2)
library(RColorBrewer)
library(lattice)
library(maptools)
library(SUMMER)
# library(batch)
load(paste0("Fitted-new/", countryname, "-yearly.rda"))
countryname2 <- gsub(" ", "", countryname)

set.seed(12345)

periods.5 <- c("Morocco")
periods.6 <- c("Benin", "Burkina Faso", "Egypt", "Niger",  "Cambodia", "Ghana", "Zimbabwe", "Mali", "Madagascar", "Burundi")
periods.7 <- c("Cameroon", "DRC", "Nigeria", "Uganda", "Bangladesh", "Indonesia", "Philippines","Bangladesh", "Rwanda", "Malawi", "Zambia", "Lesotho", "Liberia", "Namibia", "Sierra Leone", "DRC", "Tanzania", "Gabon", "Kenya", "Togo", "Mozambique", "Chad", "Congo", "Guinea", "Ethiopia", "Kenya", "Comoros", "Cote_dIvoire", "Gambia", "Senegal", "Angola", "Gambia")
if(countryname %in% periods.5){
	years <- c("80-84", "85-89", "90-94", "95-99", "00-04")	
}else if (countryname %in% periods.6){
	years <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09")	
}else if (countryname %in% periods.7){
	years <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14")	
}else{
	stop("How many years NOT determined")
}
year_range <- c(1980, 2019)
years.all <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14", "15-19")	
years.med <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017)	

quantiles = c(0.025, 0.5, 0.975)
##---------------------------------------------------##
## Extract objects
data <- out$data.HT.combined
geo <- out$geo
nb.r <- poly2nb(geo, queen=F, row.names = geo$NAME_final)
if(countryname == "Comoros"){
	final_name_ordered <- out$geo$NAME_final
	mat <- matrix(1, length(final_name_ordered), length(final_name_ordered))
	diag(mat) <- 0
	rownames(mat) <- final_name_ordered
}else{
	mat <- nb2mat(nb.r, style="B",zero.policy=TRUE)
}
colnames(mat) <- rownames(mat)
mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
timelabel <- years.all
countrylabel <- countryname
final_name_ordered <- geo$NAME_final
expit<-function(x){ exp(x)/(1+exp(x))}
logit<-function(x){ log(x/(1-x))}

#########################################################
# source("../HelperFunctions/inla_function.R")
# source("../HelperFunctions/inla_function_yearly.R")
priors <- list(a.iid = 0.5, a.rw1=0.5, a.rw2 = 0.5, a.icar=0.5, b.iid = 0.001487953, b.rw1 = 0.001487953, b.rw2 = 0.001487953, b.icar = 0.001487953)

##########################################
## Cross Validation by Year
##########################################
results <- data.frame(matrix(NA, length(final_name_ordered)*length(years), 7))
results$region <- rep(final_name_ordered, each=length(years))
results$year <- rep(years, length(final_name_ordered))
colnames(results)[1:7] <- c("med", "med.logit", "lower", "upper", "compare", "compare.lower", "compare.upper")
results$sd <- NA
results$compare.sd <- NA
allfits <- NULL

counter <- 1
for(yy in 1:length(years)){
	remove <- 
		# intersect(
		# which(data.adj$region == final_name_ordered[rr]),
		which(data$years == years[yy])
		# )
	data.rm <- data[-remove, ]
	# data.adj.rm[remove, c(3:7, 10:16)] <- NA
	inla_model <- fitINLA(data.rm, geo = geo, priors = priors, useHyper = TRUE, Amat = mat, year_names = years.all, rw = 2, year_range = year_range, m = 5, is.yearly = TRUE, type.st = 4)
	allfits[[counter]] <- inla_model
	names(allfits)[counter] <- years[yy]
	counter <- counter + 1
	for(rr in 1:length(final_name_ordered)){
		
		lincombs.info <- inla_model$lincombs.info
		index <- lincombs.info$Index[lincombs.info$District == rr & lincombs.info$Year == 5 * length(years.all) + yy]
	    tmp.logit <- inla.rmarginal(100000, inla_model$fit$marginals.lincomb.derived[[index]])
	    tmp <- expit(tmp.logit)

	    which <- intersect(which(results$region == final_name_ordered[rr]), which(results$year == years[yy]))
	    which2 <- intersect(which(data$region == final_name_ordered[rr]), which(data$year == years[yy]))
 		results$med[which] <- mean(tmp)
	    results$med.logit[which] <- mean(tmp.logit)	  
	    results$upper[which] <- quantile(tmp, .975)
	    results$lower[which] <- quantile(tmp, .025)
	    results$compare[which]<- data[which2, "u5m"]
	    results$compare.lower[which]<- (data[which2, "lower"])
	    results$compare.upper[which]<-  (data[which2, "upper"])
	    results$sd[which] <- sd(tmp.logit)
	    results$compare.sd[which] <- NA
	    cat(".")
	}
}


results1 <- results[, c(1:4, 8, 9, 10)]
results1$type = "CV"
results2 <- results[, c(5, 5:7, 8, 9, 11)]
results2[,2] <- logit(results2[,1])
results2$type = "Direct"
colnames(results2) <- colnames(results1)
resultsYear <- rbind(results1, results2)
resultsYear$year <- factor(resultsYear$year, levels = years)

cover1 <- sum((results2$med < results1$upper) * (results2$med > results1$lower), na.rm=T) / sum(!is.na(results2$med))
my.dodge <- position_dodge(width = 0.5)
g <- ggplot(data = resultsYear, aes(x = year, y = med, color = type))
g <- g + geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = .2, position = my.dodge)
g <- g + geom_point(position = my.dodge)
g <- g + facet_wrap(~region, scales = "free")
g <- g + ggtitle(paste0("Cross Validation by deleting one time period at a time (coverage: ", round(cover1, 3), ")"))
g1 <- g
pdf(paste0("Figures/yearly/CV_byYear_", countryname2, ".pdf"), width = 12, height = 7)
print(g1) 
dev.off()

##########################################
## add back in measurement error variance
##########################################
results <- data.frame(matrix(NA, length(final_name_ordered)*length(years), 7))
results$region <- rep(final_name_ordered, each=length(years))
results$year <- rep(years, length(final_name_ordered))
colnames(results)[1:7] <- c("med", "med.logit", "lower", "upper", "compare", "compare.lower", "compare.upper")
results$sd <- NA
results$compare.sd <- NA
counter <- 1
for(yy in 1:length(years)){
	name <- years[yy]
	inla_model <- allfits[[name]]

	for(rr in 1:length(final_name_ordered)){

		lincombs.info <- inla_model$lincombs.info
		index <- lincombs.info$Index[lincombs.info$District == rr & lincombs.info$Year == 5 * length(years.all) + yy]
	   
	    which <- intersect(which(results$region == final_name_ordered[rr]), which(results$year == years[yy]))
	    which2 <- intersect(which(data$region == final_name_ordered[rr]), which(data$year == years[yy]))

		tmp.logit <- inla.rmarginal(100000, inla_model$fit$marginals.lincomb.derived[[index]])
		if(is.na(data[which2,"var.est"])){
			sd <- 0
		}else{
			sd <- data[which2,"var.est"]^.5
		}
	    tmp <- expit(tmp.logit + rnorm(100000, mean=0, sd = sd))


	     results$med[which] <- mean(tmp)
	    results$med.logit[which] <- mean(tmp.logit)
	    results$upper[which] <- quantile(tmp, .975)
	    results$lower[which] <- quantile(tmp, .025)
	    results$compare[which]<- data[which2, "u5m"]
	    results$compare.lower[which]<- (data[which2, "lower"])
	    results$compare.upper[which]<-  (data[which2, "upper"])
	    results$sd[which] <- sd(tmp.logit)
	    results$compare.sd[which] <- sd 
	    cat(".")
	}
}


results1 <- results[, c(1:4, 8, 9, 10)]
results1$type = "CV"
results2 <- results[, c(5, 5:7, 8, 9, 11)]
results2[,2] <- logit(results2[,1])
results2$type = "Direct"
colnames(results2) <- colnames(results1)
resultsYear2 <- rbind(results1, results2)
resultsYear2$year <- factor(resultsYear2$year, levels = years) 

cover1a <- sum((results2$med < results1$upper) * (results2$med > results1$lower), na.rm=T) / sum(!is.na(results2$med))
my.dodge <- position_dodge(width = 0.5)
g <- ggplot(data = resultsYear2, aes(x = year, y = med, color = type))
g <- g + geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = .2, position = my.dodge)
g <- g + geom_point(position = my.dodge)
g <- g + facet_wrap(~region, scales = "free")
g <- g + ggtitle(paste0("Cross Validation by deleting one time x area combination at a time (coverage: ", round(cover1a, 3), ")"))
g2 <- g
pdf(paste0("Figures/yearly/CV_byYear_withError_", countryname2, ".pdf"), width = 12, height = 7)
print(g2) 
dev.off()
########################################
## Cross validation by area X time
########################################
results <- data.frame(matrix(NA, length(final_name_ordered)*length(years), 7))
results$region <- rep(final_name_ordered, each=length(years))
results$year <- rep(years, length(final_name_ordered))
colnames(results)[1:7] <- c("med", "med.logit", "lower", "upper", "compare", "compare.lower", "compare.upper")
results$sd <- NA
results$compare.sd <- NA
counter <- 1
allfits2 <- NULL
for(rr in 1:length(final_name_ordered)){
	for(yy in 1:length(years)){
		remove <- 
		intersect(
		which(data$region == final_name_ordered[rr]),
		which(data$years == years[yy])
		)
		data.rm <- data[-remove, ]
		# data.adj.rm[remove, c(3:7, 10:16)] <- NA
		inla_model <- fitINLA(data.rm, geo = geo, priors = priors, useHyper = TRUE, Amat = mat, year_names = years.all, rw = 2, year_range = year_range, m = 5, is.yearly = TRUE, type.st = 4)
		allfits2[[counter]] <- inla_model
		names(allfits2)[counter] <- paste0(final_name_ordered[rr], years[yy])
		counter <- counter + 1
	
		lincombs.info <- inla_model$lincombs.info
		index <- lincombs.info$Index[lincombs.info$District == rr & lincombs.info$Year == 5 * length(years.all) + yy]
	    tmp.logit <- inla.rmarginal(100000, inla_model$fit$marginals.lincomb.derived[[index]])
	    tmp <- expit(tmp.logit)

	    which <- intersect(which(results$region == final_name_ordered[rr]), which(results$year == years[yy]))
	    which2 <- intersect(which(data$region == final_name_ordered[rr]), which(data$year == years[yy]))
	     results$med[which] <- mean(tmp)
	    results$med.logit[which] <- mean(tmp.logit)
	    results$upper[which] <- quantile(tmp, .975)
	    results$lower[which] <- quantile(tmp, .025)
	    results$compare[which]<- data[which2, "u5m"]
	    results$compare.lower[which]<- (data[which2, "lower"])
	    results$compare.upper[which]<-  (data[which2, "upper"])
	    results$sd[which] <- sd(tmp.logit)
	    results$compare.sd[which] <- NA
	    cat(".")
	}
}

results1 <- results[, c(1:4, 8, 9, 10)]
results1$type = "CV"
results2 <- results[, c(5, 5:7, 8, 9, 11)]
results2[,2] <- logit(results2[,1])
results2$type = "Direct"
colnames(results2) <- colnames(results1)
resultsComb <- rbind(results1, results2)
resultsComb$year <- factor(resultsComb$year, levels = years) 

cover2 <- sum((results2$med < results1$upper) * (results2$med > results1$lower), na.rm=T) / sum(!is.na(results2$med))
my.dodge <- position_dodge(width = 0.5)
g <- ggplot(data = resultsComb, aes(x = year, y = med, color = type))
g <- g + geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = .2, position = my.dodge)
g <- g + geom_point(position = my.dodge)
g <- g + facet_wrap(~region, scales = "free")
g <- g + ggtitle(paste0("Cross Validation by deleting one time x area combination at a time (coverage: ", round(cover2, 3), ")"))
g3 <- g
pdf(paste0("Figures/yearly/CV_byYearRegion_", countryname2, ".pdf"), width = 12, height = 7)
print(g3) 
dev.off()

##########################################
## add back in measurement error variance
##########################################
results <- data.frame(matrix(NA, length(final_name_ordered)*length(years), 7))
results$region <- rep(final_name_ordered, each=length(years))
results$year <- rep(years, length(final_name_ordered))
colnames(results)[1:7] <- c("med", "med.logit", "lower", "upper", "compare", "compare.lower", "compare.upper")
results$sd <- NA
results$compare.sd <- NA
counter <- 1
for(rr in 1:length(final_name_ordered)){
	for(yy in 1:length(years)){
		name <- paste0(final_name_ordered[rr], years[yy])
		inla_model <- allfits2[[name]]

		lincombs.info <- inla_model$lincombs.info
		index <- lincombs.info$Index[lincombs.info$District == rr & lincombs.info$Year == 5 * length(years.all) + yy]
	   
	    which <- intersect(which(results$region == final_name_ordered[rr]), which(results$year == years[yy]))
	    which2 <- intersect(which(data$region == final_name_ordered[rr]), which(data$year == years[yy]))

		tmp.logit <- inla.rmarginal(100000, inla_model$fit$marginals.lincomb.derived[[index]])
		if(is.na(data[which2,"var.est"])){
			sd <- 0
		}else{
			sd <- data[which2,"var.est"]^.5
		}
	    tmp <- expit(tmp.logit + rnorm(100000, mean=0, sd = sd))


	     results$med[which] <- mean(tmp)
	    results$med.logit[which] <- mean(tmp.logit)
	    results$upper[which] <- quantile(tmp, .975)
	    results$lower[which] <- quantile(tmp, .025)
	    results$compare[which]<- data[which2, "u5m"]
	    results$compare.lower[which]<- (data[which2, "lower"])
	    results$compare.upper[which]<-  (data[which2, "upper"])
	    results$sd[which] <- sd(tmp.logit)
	    results$compare.sd[which] <- sd 
	    cat(".")
	}
}

results1 <- results[, c(1:4, 8, 9, 10)]
results1$type = "CV"
results2 <- results[, c(5, 5:7, 8, 9, 11)]
results2[,2] <- logit(results2[,1])
results2$type = "Direct"
colnames(results2) <- colnames(results1)
resultsComb2 <- rbind(results1, results2)
resultsComb2$year <- factor(resultsComb2$year, levels = years) 

cover2a <- sum((results2$med < results1$upper) * (results2$med > results1$lower), na.rm=T) / sum(!is.na(results2$med))
my.dodge <- position_dodge(width = 0.5)
g <- ggplot(data = resultsComb2, aes(x = year, y = med, color = type))
g <- g + geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = .2, position = my.dodge)
g <- g + geom_point(position = my.dodge)
g <- g + facet_wrap(~region, scales = "free")
g <- g + ggtitle(paste0("Cross Validation by deleting one time x area combination at a time (coverage: ", round(cover2a, 3), ")"))
g4 <- g
pdf(paste0("Figures/yearly/CV_byYearRegion_withError_", countryname2, ".pdf"), width = 12, height = 7)
print(g4) 
dev.off()

out <- list(resultsYear = resultsYear, 
			resultsYear2 = resultsYear2,
			resultsComb = resultsComb,
			resultsComb2 = resultsComb2,
			g1 = g1, 
			g2 = g2, 
			g3 = g3,
			g4 = g4,
			cover1 = cover1, 
			cover2 = cover1a,
			cover3 = cover2,
			cover4 = cover2a)
save(out, file = paste0("Fitted-new/", countryname, "-yearlyCV.rda"))

out$allfits = allfits
out$allfits2 = allfits2
save(out, file = paste0("Fitted-new/", countryname, "-yearlyCVfull.rda"))



