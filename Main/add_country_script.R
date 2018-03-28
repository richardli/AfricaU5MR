library(spdep)
library(maptools)
library(gpclib)
gpclibPermit()
library(plyr)
library(INLA)
library(ggplot2)
library(RColorBrewer)
library(lattice)
library(maptools)
# library(batch)
for(countryname in c("Indonesia", "Nigeria", "Zimbabwe", "Bangladesh", "Burkina Faso", "Ghana", "Zambia")){
	dir <- "~/Dropbox/Godwin-Wakefield/u5m/Kenya/MultipleCountries/"
	load(paste0(dir, "data_for_plotting/",countryname, "/", countryname, ".rda"))	

	info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
	filenames <- as.character(info[, 1])[which(as.character(info[, 1]) != "")]
	surveylabels <- as.character(info[, 2])[which(as.character(info[, 2]) != "")]
	final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]

	periods.5 <- c("Ethiopia", "Morocco")
	periods.6 <- c("Burkina Faso", "Egypt", "Kenya", "Niger",  "Cambodia")
	periods.7 <- c("Cameroon", "DRC", "Mali", "Nigeria", "Uganda", "Bangladesh", "Indonesia", "Philippines","Bangladesh", "Rwanda", "Ghana", "Malawi", "Zambia", "Zimbabwe")

	if(countryname %in% periods.5){
		years <- c("80-84", "85-89", "90-94", "95-99", "00-04")	
	}else if (countryname %in% periods.6){
		years <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09")	
	}else if (countryname %in% periods.7){
		years <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14")	
	}else{
		stop("How many years NOT determined")
	}

	##---------------------------------------------------##
	## read data
	data <- NULL
	for(i in 1:length(filenames)){
		tmp <- paste0("../Data/5q0_estimates_new/", countryname,
			"/", filenames[i], ".csv")
		if(file.exists(tmp)){
			d <- read.csv(paste0("../Data/5q0_estimates_new/", countryname,
				"/", filenames[i], ".csv"))
			d$survey <- i
			data <- rbind(data, d)		
		}
	}

	data <- data[,c("region","years","u5m","lower","upper",
	             "logit.est","var.est","region_num","survey")]
	data$logit.prec <- 1/data$var.est

	# country specific fix
	if(countryname == "Zambia"){
		data$region[which(data$region == "northwestern")] <- "north-western"
	}
	if(countryname == "Bangladesh"){
		data$region <- revalue(data$region, c("barishal"="barisal", "rajashahi"="rajshahi", "rajshani"="rajshahi"))
	}
	if(countryname == "Zimbabwe"){
		data$region <- revalue(data$region, c("matebeleland north"="matabeleland north", "matebeleland south"="matabeleland south", "harare/chitungwiza"="harare"))
	}

	# add NA rows for incomplete data
	regions_in_data <- as.character(unique(data$region))
	add.region <- add.year <- add.survey <- NULL
	for(year in years){
		data.sub <- data[which(data$years == year), ]
		for(survey in 1:length(filenames)){
				data.sub.sub <- data.sub[which(data.sub$survey == survey), ]
				for(region in regions_in_data){
					if(region %in% data.sub.sub$region == FALSE){
						add.region <- c(add.region, region)
						add.year <- c(add.year, year)
						add.survey <- c(add.survey, survey)
						cat(paste("Impute NA for missing region:", region, year, "survey", survey, "\n"))
					}
				}	
		}
	}

	if(length(add.region) > 0){
		add <- data.frame(region = add.region, 
						  years = add.year,
						  u5m = NA, 
						  lower = NA, 
						  upper = NA,
						  logit.est = NA, 
						  var.est = NA, 
						  region_num = NA, 
						  survey = add.survey,
						  logit.prec = NA)
		data <- rbind(data, add)
	}

	# remove extra years in data if any
	data <- data[which(data$year %in% years), ]


	data0 <- data
	data0 <- data0[data0$region == "All", ]
	time_region <- unique(data0[, c(1, 2)])
	data <- data.frame(region = time_region$region, years = time_region$years, u5m = NA, lower=NA, upper=NA, logit.est=NA, var.est=NA, region_num = NA, survey = NA, logit.prec = NA)
	  expit<-function(x){
	    exp(x)/(1+exp(x))
	  }
	for(i in 1:dim(data)[1]){
		tmp <- intersect(which(data0$region == data$region[i]), 
						 which(data0$years == data$years[i]))
		data[i, "logit.prec"] <- sum(data0[tmp, "logit.prec"], na.rm = TRUE)
		if(data[i, "logit.prec"] == 0){
			data[i, "var.est"] <- NA
		}else{
			data[i, "var.est"] <- 1 / data[i, "logit.prec"]
		}

		weights <- data0[tmp, "logit.prec"] / data[i, "logit.prec"]
		data[i, "logit.est"] <- sum(weights * data0[tmp, "logit.est"], na.rm = TRUE)
		data[i, "u5m"] <- expit(data[i, "logit.est"])

		data[i, "lower"] <- expit(data[i, "logit.est"] + qnorm(0.975)*sqrt(data[i, "var.est"]))
		data[i, "upper"] <- expit(data[i, "logit.est"] + qnorm(0.025)*sqrt(data[i, "var.est"]))
		data[i, "region_num"] <- data0[tmp, "region_num"][1]
	}
	
	out$data.HT.national <- data0
	out$data.HT.combined.national <- data
	save(out, file = paste0(dir, "data_for_plotting/",countryname, "/", countryname, ".rda"))
}

# for(countryname in c("Indonesia", "Nigeria", "Zimbabwe", "Bangladesh", "Burkina Faso", "Ghana", "Zambia")){
# 	dir <- "~/Dropbox/Godwin-Wakefield/u5m/Kenya/MultipleCountries/"
# 	load(paste0(dir, "data_for_plotting/",countryname, "/", countryname, ".rda"))		
# 	print(countryname)
# 	print(unique(out$data.HT.combined$region))
# }