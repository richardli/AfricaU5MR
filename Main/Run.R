## Script for reproducing each single countries
##
##
##
if(FALSE){
	# Script to run the whole analysis
	start_time <- Sys.time()
	countries <- list.files("../Data/map/")
	countries <- countries[countries %in% c("AfricanCountries", "World", "Bangladesh", "Cambodia", "Indonesia", "Philippines") == FALSE]
	for(countryname in countries){
		print(countryname)
		source("Run.R")
	}
	end_time <- Sys.time()
	print(end_time - start_time) # ~2+hr without CV
}

library(gdata)
library(foreign)
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


set.seed(12345)
# countryname <- commandArgs(trailingOnly = TRUE)
# countryname <- "Malawi"  
updateplot <- FALSE

# parseCommandArgs()

# fix for INLA to run on stats cluster...
# INLA:::inla.dynload.workaround() 

# Read in country specific information
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
filenames <- as.character(info[, 1])[which(as.character(info[, 1]) != "")]
surveylabels <- as.character(info[, 2])[which(as.character(info[, 2]) != "")]
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]

# todo: Ethiopia needs double check
# todo: Malawi needs to be put into periods.7
# todo: Zimbabwe needs to be put into periods.7
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
if(countryname == "Morocco"){
	data$region[which(data$region == "tensifit")] <- "tensift"
}
if(countryname == "Tanzania"){
	data <- data[data$region != "pemba", ]
}
if(countryname == "Togo"){
	data$region <- revalue(data$region, c("maritime"="maritime (sans agglomération de lomé)", "lome"="grande agglomération de lomé"))
}
if(countryname == "Mozambique"){
	data$region <- tolower(data$region)
	data$region[data$region == "all"] <- "All"
}
if(countryname == "Guinea"){
	data <- data[!is.na(data$region), ]
}
if(countryname == "Nigeria"){
	data <- data[!is.na(data$region), ]
}
if(countryname == "Ethiopia"){
	data$region <- revalue(data$region, c("snnpr"="snnp", "addis abeba" = "addis ababa", "addis"="addis ababa", "addis adaba"="addis ababa", "afar" = "affar", "oromia" = "oromiya", "ben-gumz" = "benishangul-gumuz", "benishangul" = "benishangul-gumuz"))
}
if(countryname == "Rwanda"){
	data$region <- revalue(data$region, c("east"="East", "kigali city"="Kigali", "north"="North", "south"="South", "west"="West"))
	data <- data[!is.na(data$region), ]
}
if(countryname == "Senegal"){
	data$region <- tolower(data$region)
	data$region <- revalue(data$region, c("ziguinchor"="zuguinchor"))
	data$region[data$region == "all"] <- "All"

	# remove the early estimates for 2015, 2016 surveys
	remove <- intersect(which(data$years == "80-84"), which(data$survey %in% c(7, 8)))
	data[remove, c("u5m", "lower", "upper", "logit.est", "var.est", "logit.prec")] <- NA

}
if(countryname == "Comoros"){
	data$region <- revalue(data$region, c("ndzuwani"="ndzouani", "mwali"="moheli"))
}
if(countryname == "Angola"){
	data$region <- revalue(data$region, c("bi\xe9"="bié", "hu\xedla" = "huíla", "u\xedge"="uíge"))
}
if(countryname == "Comoros"){
	data$region <- revalue(data$region, c("Mwali"="moheli", "Ndzuwani"="ndzouani", "Ngazidja"="ngazidja"))
}
#######################################
#### HIV adjustment
# input: 5q0, variance of logit, and adjustment factor c
# output: 5q0, logit, variance of logit, and prec (adjusted)
adjust <- function(p, v, c){
	f.prime <- 1 - (c - 1) * exp(p) / (c + (c-1) * exp(p))
	p <- p / c
	v <- v * f.prime^2
	return(c(p, log(p/(1-p)), v, 1/v))
}
# input: current year, per5 label, adjustment file
# output: adjustment factor c
adj_lookup <- function(y, per, adj){
	per <- as.character(per)
	labels <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14")
	if(!per %in% labels) stop("----- File contains unhandled per5! ----\n")
	start <- seq(1980, 2010, by = 5)[which(labels == per)]
	diff <- as.numeric(y) - start
	# diff the survey-year - start of per5
	# e.g., estimator of 80-84 in a 1998 survey: diff = 18 
	#		estimator of 95-99 in a 1998 survey: diff = 3
	# In the later case, we need HIV adjustment factor for 0-3, 
	#      instead, for now we use just 0-4 for diff < 4
	#      todo: can/should we be more precise for this?
	if(diff > 4){
		period <- paste0(diff - 4, "-", diff)
	}else if(diff >= 0){
		period <- "0-4"
	}else{
		stop("Something wrong with the survey year\n")
	}
	out <- adj[which(adj[, 1] == period), 2]
	if(length(out) == 0) out <- NA
	return(out)
}
# input: data, adjustment matrix, survey year
# output: new data
adjust.meta <- function(d, adj, year){
	for(i in 1:dim(d)[1]){
		if(!is.na(d$u5m[i])){
			cc <- adj_lookup(year, d[i, "years"], adj)
			# if adjustment factor is NA, don't do anything
			if(!is.na(cc)){
				new <- adjust(d$u5m[i], d$var.est[i], cc)
				d[i, "u5m"] <- new[1]
				d[i, "logit.est"] <- new[2]
				d[i, "var.est"] <- new[3]
				d[i, "logit.prec"] <- new[4]
				d[i, "lower"] <- new[2] + qnorm(0.025)*sqrt(new[3])
				d[i, "upper"] <- new[2] + qnorm(0.975)*sqrt(new[3])				
			}
		}
	}
	return(d)
}

country.hiv <- c("Malawi", "Mozambique", "Namibia", "Rwanda", "Tanzania", "Zambia", "Lesotho")
region.hiv <- c("Zimbabwe", "Kenya")
if(countryname %in% country.hiv){
	data.nohiv <- data
	hivfiles <- list.files("HIV_Adjustments")
	hivfiles <- hivfiles[grep(countryname, hivfiles)]
	if(length(hivfiles) != 0){
		post <- gsub("[^0-9]", "", surveylabels)
		filepost <- gsub("[^0-9]", "", hivfiles)
		## for every survey year we have
		for(year in post){
			## if there's HIV adjustment files
			if(year %in% filepost){
				file <- paste0("HIV_Adjustments/", hivfiles[which(filepost == year)])
				if(grepl(".xlsx", file)){
					adj <- read.xls(file, 1)
				}else{
					adj <- read.csv(file)	
				}
				# get original data of this year
				which.sub <- which(data$survey == grep(year, surveylabels))
				sub <- data[which.sub, ]
				sub.new <- adjust.meta(sub, adj, year)
				data[which.sub, ] <- sub.new
			}
		}
	}
}else if(countryname %in% region.hiv){
	## to-be-updated
	data.nohiv <- data
	hivfiles <- list.files(paste0("HIV_Adjustments/", countryname))
	if(length(hivfiles) > 0){
		filepost <-  gsub(paste0(countryname, "_"), "",  hivfiles)
		yearpost <-  gsub("[^0-9]", "",  filepost)
		regionpost <-  gsub("[0-9]", "",  filepost)
		regionpost <-  tolower(gsub("_.csv", "",  regionpost))
		# actual year and region
		post1 <- gsub("[^0-9]", "", surveylabels)
		post1 <- substring(post1, 1, 4) # remove multi-year labels
		post2 <- tolower(gsub(" ", "", unique(data$region[data$region != "All"])))
		# create a new reformatted region names in data file to be consistent
		regions_tolower <- tolower(data$region)
		regions_tolower <- gsub(" ", "", regions_tolower)

		for(year in post1){
			for(region in post2){
				select <- intersect(which(yearpost == year), which(regionpost == region))
				if(length(select) > 0){
					file <- paste0("HIV_Adjustments/", countryname, "/", hivfiles[select])
					if(grepl(".xlsx", file)){
						adj <- read.xlsx(file, 1)
					}else{
						adj <- read.csv(file)	
					}
					# todo: finish here
					which.sub <- intersect(which(data$survey == grep(year, surveylabels)), which(regions_tolower == region))
					sub <- data[which.sub, ]
					sub.new <- adjust.meta(sub, adj, year)
					data[which.sub, ] <- sub.new
	
				} 
			}
		}
	}

}else{
	data.nohiv <- data
}

# Zimbabwe has both national and regional HIV adjustments
if(countryname %in% c("Zimbabwe")){
	hivfiles <- list.files("HIV_Adjustments")
	hivfiles <- hivfiles[grep(countryname, hivfiles)]
	if(length(hivfiles) != 0){
		post <- gsub("[^0-9]", "", surveylabels)
		filepost <- gsub("[^0-9]", "", hivfiles)
		## for every survey year we have
		for(year in post){
			## if there's HIV adjustment files
			if(year %in% filepost){
				file <- paste0("HIV_Adjustments/", hivfiles[which(filepost == year)])
				if(grepl(".xlsx", file)){
					adj <- read.xls(file, 1)
				}else{
					adj <- read.csv(file)	
				}
				# get original data of this year
				which.sub <- intersect(which(data$survey == grep(year, surveylabels)), which(data$region == "All"))
				sub <- data[which.sub, ]
				sub.new <- adjust.meta(sub, adj, year)
				data[which.sub, ] <- sub.new
			}
		}
	}
}

data$u5m.nohiv <-  data.nohiv$u5m
data$lower.nohiv <-  data.nohiv$lower
data$upper.nohiv <-  data.nohiv$upper
data$logit.est.nohiv <-  data.nohiv$logit.est
data$var.est.nohiv <-  data.nohiv$var.est
data$logit.prec.nohiv <-  data.nohiv$logit.prec


# add NA rows for incomplete data
regions_in_data <- as.character(unique(data$region))
add.region <- add.year <- add.survey <- NULL
for(year in years.all){
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
					  logit.prec = NA,
					  u5m.nohiv = NA, 
					  lower.nohiv = NA, 
					  upper.nohiv = NA, 
					  logit.est.nohiv = NA, 
					  var.est.nohiv = NA, 
					  logit.prec.nohiv = NA)
	data <- rbind(data, add)
}

# remove extra years in data if any
fulldata <- data[which(data$year %in% years.all), ]

##---------------------------------------------------##
# Get aggregated version of estimates
data0 <- fulldata
data0.national <- fulldata
data0 <- data0[data0$region != "All", ]
time_region <- unique(data0[, c(1, 2)])
data <- data.frame(region = time_region$region, years = time_region$years, u5m = NA, lower=NA, upper=NA, logit.est=NA, var.est=NA, region_num = NA, survey = NA, logit.prec = NA,  u5m.nohiv = NA, lower.nohiv = NA, upper.nohiv = NA, logit.est.nohiv = NA, var.est.nohiv = NA,logit.prec.nohiv = NA)
expit<-function(x){
    exp(x)/(1+exp(x))
}
for(i in 1:dim(data)[1]){
	tmp <- intersect(which(data0$region == data$region[i]), 
					 which(data0$years == data$years[i]))
	# Version adjusting for HIV
	data[i, "logit.prec"] <- sum(data0[tmp, "logit.prec"], na.rm = TRUE)
	if(data[i, "logit.prec"] == 0){
		data[i, "var.est"] <- NA
		data[i, "logit.prec"] <- NA
	}else{
		data[i, "var.est"] <- 1 / data[i, "logit.prec"]
		weights <- data0[tmp, "logit.prec"] / data[i, "logit.prec"]
		data[i, "logit.est"] <- sum(weights * data0[tmp, "logit.est"], na.rm = TRUE)
		data[i, "u5m"] <- expit(data[i, "logit.est"])

		data[i, "lower"] <- expit(data[i, "logit.est"] + qnorm(0.975)*sqrt(data[i, "var.est"]))
		data[i, "upper"] <- expit(data[i, "logit.est"] + qnorm(0.025)*sqrt(data[i, "var.est"]))
	}
	data[i, "region_num"] <- data0[tmp, "region_num"][1]
	

	# Version not adjusting for HIV
	data[i, "logit.prec.nohiv"] <- sum(data0[tmp, "logit.prec.nohiv"], na.rm = TRUE)
	if(data[i, "logit.prec.nohiv"] == 0){
		data[i, "var.est.nohiv"] <- NA
		data[i, "logit.prec.nohiv"] <- NA
	}else{
		data[i, "var.est.nohiv"] <- 1 / data[i, "logit.prec.nohiv"]
		weights <- data0[tmp, "logit.prec.nohiv"] / data[i, "logit.prec.nohiv"]
		data[i, "logit.est.nohiv"] <- sum(weights * data0[tmp, "logit.est.nohiv"], na.rm = TRUE)
		data[i, "u5m.nohiv"] <- expit(data[i, "logit.est.nohiv"])

		data[i, "lower.nohiv"] <- expit(data[i, "logit.est.nohiv"] + qnorm(0.975)*sqrt(data[i, "var.est.nohiv"]))
		data[i, "upper.nohiv"] <- expit(data[i, "logit.est.nohiv"] + qnorm(0.025)*sqrt(data[i, "var.est.nohiv"]))
	}
}

data0.national <- data0.national[data0.national$region == "All", ]
time_region <- unique(data0.national[, c(1, 2)])
data.national <- data.frame(region = time_region$region, years = time_region$years, u5m = NA, lower=NA, upper=NA, logit.est=NA, var.est=NA, region_num = NA, survey = NA, logit.prec = NA)

for(i in 1:dim(data.national)[1]){
	tmp <- intersect(which(data0.national$region == fulldata$region[i]), 
					 which(data0.national$years == fulldata$years[i]))
	# version adjusting for HIV
	data.national[i, "logit.prec"] <- sum(data0.national[tmp, "logit.prec"], na.rm = TRUE)
	if(data.national[i, "logit.prec"] == 0){
		data.national[i, "var.est"] <- NA
		data.national[i, "logit.prec"] <- NA
	}else{
		data.national[i, "var.est"] <- 1 / data.national[i, "logit.prec"]
		weights <- data0.national[tmp, "logit.prec"] / data.national[i, "logit.prec"]
		data.national[i, "logit.est"] <- sum(weights * data0.national[tmp, "logit.est"], na.rm = TRUE)
		data.national[i, "u5m"] <- expit(data.national[i, "logit.est"])

		data.national[i, "lower"] <- expit(data.national[i, "logit.est"] + qnorm(0.975)*sqrt(data.national[i, "var.est"]))
		data.national[i, "upper"] <- expit(data.national[i, "logit.est"] + qnorm(0.025)*sqrt(data.national[i, "var.est"]))
	}
	data.national[i, "region_num"] <- data0.national[tmp, "region_num"][1]

	# version not adjusting for HIV
	data.national[i, "logit.prec.nohiv"] <- sum(data0.national[tmp, "logit.prec.nohiv"], na.rm = TRUE)
	if(data.national[i, "logit.prec.nohiv"] == 0){
		data.national[i, "var.est.nohiv"] <- NA
		data.national[i, "logit.prec.nohiv"] <- NA
	}else{
		data.national[i, "var.est.nohiv"] <- 1 / data.national[i, "logit.prec.nohiv"]
		weights <- data0.national[tmp, "logit.prec.nohiv"] / data.national[i, "logit.prec.nohiv"]
		data.national[i, "logit.est.nohiv"] <- sum(weights * data0.national[tmp, "logit.est.nohiv"], na.rm = TRUE)
		data.national[i, "u5m.nohiv"] <- expit(data.national[i, "logit.est.nohiv"])

		data.national[i, "lower.nohiv"] <- expit(data.national[i, "logit.est.nohiv"] + qnorm(0.975)*sqrt(data.national[i, "var.est.nohiv"]))
		data.national[i, "upper.nohiv"] <- expit(data.national[i, "logit.est.nohiv"] + qnorm(0.025)*sqrt(data.national[i, "var.est.nohiv"]))
	}	
}

##---------------------------------------------------##
## read map
# install.packages("gpclib", type="source")
library(spdep)
library(maptools)
library(gpclib)
if(file.exists(paste0("../Data/map/", countryname, 
	"/sdr_subnational_boundaries.shp"))){
	geo <- readShapePoly(paste0("../Data/map/", countryname, 
	"/sdr_subnational_boundaries.shp"))
	geo$NAME_1 <- geo$REGNAME


	# special fix for maps
	if(countryname == "Tanzania"){
	  geo <- geo[geo$NAME_1 != "rest zanzibar",]
	  geo <- geo[geo$NAME_1 != "pemba",]
	  geo$NAME_1 <- revalue(geo$NAME_1, c("coast"="pwani"))
	  geo$NAME_final <- final_name_ordered

	}else if(countryname == "Indonesia"){
	  geo <- geo[geo$NAME_1 != "east timor",]
	  geo$NAME_final <- final_name_ordered

	}else if(countryname == "Burkina Faso"){
	   gpclibPermit()
	   # Merge two areas
	   merge.id <- c("Central/South", "North", "East", "West", "Central/South")
	   NAME_final <- data.frame(NAME_final = final_name_ordered)
	   NAME_final$NAME_final <- as.character(NAME_final$NAME_final)
	   rownames(NAME_final) <- final_name_ordered
	   geo <- unionSpatialPolygons(geo, merge.id )
	   geo <- SpatialPolygonsDataFrame(geo, NAME_final)
	}else{
		geo$NAME_final <- final_name_ordered
	}

	nb.r <- poly2nb(geo, queen=F, row.names = geo$NAME_final)
	# Construct adj matrix
	if(countryname == "Comoros"){
		mat <- matrix(1, length(final_name_ordered), length(final_name_ordered))
		diag(mat) <- 0
		rownames(mat) <- final_name_ordered
	}else{
		mat <- nb2mat(nb.r, style="B",zero.policy=TRUE)
	}
	colnames(mat) <- rownames(mat)
	mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])

	# check map and data contains the same region names
	regions_in_data <- as.character(unique(data$region))
	regions_in_map <- final_name_ordered
	if(sum(1 - regions_in_map %in% regions_in_data) > 0){
		stop("Exist regions in Map file, but not in Data")
	}else if(sum(1 - regions_in_data %in% c("All", regions_in_map)) > 0){
		stop("Exist regions in Data, but not in Map file")
	}else{
		if(updateplot){
			# Do one more eyeball check of the map
			pdf(paste0("CountryInfo/Regions_", countryname, ".pdf"), 
			height = 20, width = 20)
			plot(geo)
			text(coordinates(geo), labels = geo$NAME_final)
			dev.off()		
		}
	}		
# different shapefile source for Gambia	
}else if(file.exists(paste0("../Data/map/", countryname, 
	"/GMB_adm1.shp")) && countryname == "Gambia"){
	geo <- readShapePoly(paste0("../Data/map/", countryname, "/GMB_adm1.shp"))
	geo$NAME_final <- final_name_ordered

	nb.r <- poly2nb(geo, queen=F, row.names = geo$NAME_final)
	mat <- nb2mat(nb.r, style="B",zero.policy=TRUE)
	colnames(mat) <- rownames(mat)
	mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])

	# check map and data contains the same region names
	regions_in_data <- as.character(unique(data$region))
	regions_in_map <- final_name_ordered
	if(sum(1 - regions_in_map %in% regions_in_data) > 0){
		stop("Exist regions in Map file, but not in Data")
	}else if(sum(1 - regions_in_data %in% c("All", regions_in_map)) > 0){
		stop("Exist regions in Data, but not in Map file")
	}else{
		if(updateplot){
			# Do one more eyeball check of the map
			pdf(paste0("CountryInfo/Regions_", countryname, ".pdf"), 
			height = 20, width = 20)
			plot(geo)
			text(coordinates(geo), labels = geo$NAME_1)
			dev.off()		
		}
	}	

}else{
	stop("Map file do not exist!")
}

quantiles = c(0.025, 0.5, 0.975)
##---------------------------------------------------##
## Fit INLA
data$region_num <- match(data$region, c("All", geo$NAME_final )) - 1
surveylabel <- surveylabels
timelabel <- years.all
countrylabel <- countryname

######################################################################
## First fit national models
source("../HelperFunctions/inla_function.R")
source("../HelperFunctions/inla_function_yearly.R")

# priors <- simhyper(R = 2, nsamp = 100000, nsamp.check = 5000, Amat = mat)
# cat("RW1 prior: \n")
# print(priors$check.rw1)
# cat("RW2 prior: \n")
# print(priors$check.rw2)
# cat("ICAR prior: \n")
# print(priors$check.icar)

priors <- list(a.iid = 0.5, a.rw1=0.5, a.rw2 = 0.5, a.icar=0.5, b.iid = 0.001487953, b.rw1 = 0.001487953, b.rw2 = 0.001487953, b.icar = 0.001487953)
# inla_model <- NULL
# if(length(years) == 7) year_range <- c(1980, 2014)
# if(length(years) == 6) year_range <- c(1980, 2009)

# inla_model <- NULL
inla_model.national <- NULL
inla_model.national[[1]] <- fitINLA_yearly(data.national, geo = NULL, priors = priors, useHyper = TRUE, Amat = NULL, year_names = years.all, rw = 1, year_range = year_range, m = 5, is.yearly = TRUE)
inla_model.national[[2]] <- fitINLA_yearly(data.national, geo = NULL, priors = priors, useHyper = TRUE, Amat = NULL, year_names = years.all, rw = 2, year_range = year_range, m = 5, is.yearly = TRUE)

######################################################################
## Perform benchmarking to UN B3 and refit national models
timelabel.yearly <- c(year_range[1] : year_range[2], years.all)
results <- expand.grid(District = 0, Year = timelabel.yearly)
results$med <- results$q025 <- results$q975 <- results$logit.med <- results$logit.q025 <- results$logit.q975 <- NA
mod <- inla_model.national[[2]]$fit
lincombs.info <- inla_model.national[[2]]$lincombs.info
for(i in 1:length(timelabel.yearly)){
index <- lincombs.info$Index[lincombs.info$District == 0 & lincombs.info$Year == i]
tmp.logit <- inla.rmarginal(100000, mod$marginals.lincomb.derived[[index]])
tmp <- expit(tmp.logit)


results$med[results$District == 0 & results$Year == timelabel.yearly[i]] <- median(tmp)
results$q975[results$District == 0 & results$Year == timelabel.yearly[i]] <- quantile(tmp, .975)
results$q025[results$District == 0 & results$Year == timelabel.yearly[i]] <- quantile(tmp, .025)
results$logit.med[results$District == 0 & results$Year == timelabel.yearly[i]] <- median(tmp.logit)
results$logit.q975[results$District == 0 & results$Year == timelabel.yearly[i]] <- quantile(tmp.logit, .975)
results$logit.q025[results$District == 0 & results$Year == timelabel.yearly[i]] <- quantile(tmp.logit, .025)
}
results$is.yearly <- !(results$Year %in% years.all)
results$Year.num <- as.numeric(as.character(results$Year))
######################################################################
## Read UN data
UN <- read.csv('../Data/comparison/UNestimates_2017_08_08.csv', header = T)
UN$country <- as.character(UN$country)
UN$country[UN$country == "Democratic Republic of the Congo"] <- "DRC"
UN$country[UN$country == "Cote d'Ivoire"] <- "Cote_dIvoire"
UN <- UN[UN$country == countryname, ]
sub1 <- data.frame(Country = UN$country, 
				   Region = "All", 
				   Method = "UN", 
				   Year = UN$period, 
				   Median = UN$Median/1000, 
				   Lower = UN$Lower/1000, 
				   Upper = UN$Upper/1000)

# IHME
IHME <- read.csv('../Data/comparison/IHMEestimates_2017_08_08.csv', header = T)
IHME$country <- as.character(IHME$country)
IHME$country[IHME$country == "Democratic Republic of the Congo"] <- "DRC"
IHME$country[IHME$country == "Cote d'Ivoire"] <- "Cote_dIvoire"

IHME <- IHME[IHME$country == countryname, ]
sub2 <- data.frame(Country = IHME$country, 
				   Region = "All", 
				   Method = "IHME", 
				   Year = IHME$period, 
				   Median = IHME$Median/1000, 
				   Lower = IHME$Lower/1000, 
				   Upper = IHME$Upper/1000) 


sub0 <- data.frame(Country = countryname, 
				   Region = "All", 
				   Method = "RW2", 
				   Year = years.all, 
				   Median = results[match(years.all, results$Year), "med"], 
				   Lower = results[match(years.all, results$Year), "q025"], 
				   Upper =results[match(years.all, results$Year), "q975"])

adj <- sub0$Median/sub1$Median[match(sub0$Year, sub1$Year)]

adj.all <- function(d, adj){
	for(i in 1:dim(d)[1]){
		if(!is.na(d$u5m[i])){
				new <- adjust(d$u5m[i], d$var.est[i], adj)
				d[i, "u5m"] <- new[1]
				d[i, "logit.est"] <- new[2]
				d[i, "var.est"] <- new[3]
				d[i, "logit.prec"] <- new[4]
				d[i, "lower"] <- new[2] + qnorm(0.025)*sqrt(new[3])
				d[i, "upper"] <- new[2] + qnorm(0.975)*sqrt(new[3])			
		}
	}
	return(d)
}

data.adj <- data
data.national.adj <- data.national
for(i in 1:length(years.all)){
	which.sub <- which(data.adj$year == years.all[i])
	sub <- data[which.sub, ]
	sub.new <- adj.all(d = sub, adj = adj[i])
	data.adj[which.sub, ] <- sub.new

	which.sub <- which(data.national.adj$year == years.all[i])
	sub <- data.national.adj[which.sub, ]
	sub.new <- adj.all(d = sub, adj = adj[i])
	data.national.adj[which.sub, ] <- sub.new
}

inla_model.national[[3]] <- fitINLA_yearly(data.national.adj, geo = NULL, priors = priors, useHyper = TRUE, Amat = NULL, year_names = years.all, rw = 1, year_range = year_range, m = 5, is.yearly = TRUE)
inla_model.national[[4]] <- fitINLA_yearly(data.national.adj, geo = NULL, priors = priors, useHyper = TRUE, Amat = NULL, year_names = years.all, rw = 2, year_range = year_range, m = 5, is.yearly = TRUE)

results.original <- results
mod <- inla_model.national[[4]]$fit
lincombs.info <- inla_model.national[[4]]$lincombs.info
for(i in 1:length(timelabel.yearly)){
	index <- lincombs.info$Index[lincombs.info$District == 0 & lincombs.info$Year == i]
	tmp.logit <- inla.rmarginal(100000, mod$marginals.lincomb.derived[[index]])
	tmp <- expit(tmp.logit)


	results$med[results$District == 0 & results$Year == timelabel.yearly[i]] <- median(tmp)
	results$q975[results$District == 0 & results$Year == timelabel.yearly[i]] <- quantile(tmp, .975)
	results$q025[results$District == 0 & results$Year == timelabel.yearly[i]] <- quantile(tmp, .025)
	results$logit.med[results$District == 0 & results$Year == timelabel.yearly[i]] <- median(tmp.logit)
	results$logit.q975[results$District == 0 & results$Year == timelabel.yearly[i]] <- quantile(tmp.logit, .975)
	results$logit.q025[results$District == 0 & results$Year == timelabel.yearly[i]] <- quantile(tmp.logit, .025)
}
results$is.yearly <- !(results$Year %in% years.all)
results$Year.num <- as.numeric(as.character(results$Year))	
sub3 <- data.frame(Country = countryname, 
				   Region = "All", 
				   Method = "RW2-adj", 
				   Year = years.all, 
				   Median = results[match(years.all, results$Year), "med"], 
				   Lower = results[match(years.all, results$Year), "q025"], 
				   Upper =results[match(years.all, results$Year), "q975"])
sub4 <- data.frame(Country = countryname, 
				   Region = "All", 
				   Method = "RW2", 
				   Year = timelabel.yearly, 
				   Median = results[match(timelabel.yearly, results$Year), "med"], 
				   Lower = results[match(timelabel.yearly, results$Year), "q025"], 
				   Upper =results[match(timelabel.yearly, results$Year), "q975"])

estimates.national <- rbind(sub1, sub2, sub0, sub3)
estimates.national.final <- rbind(sub1, sub2, sub4)

######################################################################
## Fit subnational models with both adjusted and unadjusted data
source("../HelperFunctions/inla_function_yearly.R")

inla_model.yearly <- NULL
inla_model.yearly[[1]] <- fitINLA_yearly(data, geo = geo,  priors = priors, useHyper=TRUE, Amat = mat, year_names = years.all,  rw = 1, year_range = year_range, m=5, is.yearly=TRUE)
inla_model.yearly[[2]] <- fitINLA_yearly(data, geo = geo,  priors = priors, useHyper=TRUE, Amat = mat, year_names = years.all,  rw = 2, year_range = year_range, m=5, is.yearly=TRUE)
inla_model.yearly[[3]] <- fitINLA_yearly(data.adj, geo = geo,  priors = priors, useHyper=TRUE, Amat = mat, year_names = years.all,  rw = 1, year_range = year_range, m=5, is.yearly=TRUE, type.st = 4)
inla_model.yearly[[4]] <- fitINLA_yearly(data.adj, geo = geo,  priors = priors, useHyper=TRUE, Amat = mat, year_names = years.all,  rw = 2, year_range = year_range, m=5, is.yearly=TRUE, type.st = 4)


######################################################################
## New projection (adjusted version)
for(rw in 1:2){
	timelabel.yearly <- c(year_range[1] : year_range[2], years.all)
	results <- expand.grid(District = 1:length(final_name_ordered), Year = timelabel.yearly)
	results$med <- results$q025 <- results$q975 <- NA
	results$logit.med <- results$logit.q025 <- results$logit.q975 <- NA
	mod <- inla_model.yearly[[rw+2]]$fit
	lincombs.info <- inla_model.yearly[[rw + 2]]$lincombs.info

	for(j in 1:length(final_name_ordered)){
	  for(i in 1:length(timelabel.yearly)){
	    index <- lincombs.info$Index[lincombs.info$District == j & lincombs.info$Year == i]
	    tmp.logit <- inla.rmarginal(100000, mod$marginals.lincomb.derived[[index]])
	    tmp <- expit(tmp.logit)
	    
	   
	    results$med[results$District == j & results$Year == timelabel.yearly[i]] <- median(tmp)
	    results$q975[results$District == j & results$Year == timelabel.yearly[i]] <- quantile(tmp, .975)
	    results$q025[results$District == j & results$Year == timelabel.yearly[i]] <- quantile(tmp, .025)
	    results$logit.med[results$District == j & results$Year == timelabel.yearly[i]] <- median(tmp.logit)
	    results$logit.q975[results$District == j & results$Year == timelabel.yearly[i]] <- quantile(tmp.logit, .975)
	    results$logit.q025[results$District == j & results$Year == timelabel.yearly[i]] <- quantile(tmp.logit, .025)
	  }
	  cat(".")
	}

	results$is.yearly <- !(results$Year %in% years.all)
	results$Year.num <- as.numeric(as.character(results$Year))	
	for(i in 1:length(years.med)){
		results$Year.num[which(results$Year == years.all[i])] <- years.med[i]
	}
	results$District <- final_name_ordered[results$District]

	if(rw == 1) results.rw1 <- results
	if(rw == 2) results.rw2 <- results
}

model.rw1 <- inla_model.yearly[[3]]
model.rw2 <- inla_model.yearly[[4]]

out <- list(countryname = countryname, 
			results.rw1 = results.rw1, 
			results.rw2 = results.rw2, 			
			geo = geo,
			regions = final_name_ordered,
			years = year_range[1] : year_range[2],
			periods = years,
			data.full = fulldata,  
			data.HT.combined = data,
			data.HT = data0, 
			data.HT.national = data0.national,
			data.HT.combined.national.unadj = data.national, 
			model.rw1 = inla_model.yearly[[3]], 
			model.rw2 = inla_model.yearly[[4]], 
			model.rw1.unadj = inla_model.yearly[[1]], 
			model.rw2.unadj = inla_model.yearly[[2]], 
			estimates.national = estimates.national, 
			estimates.national.final = estimates.national.final, 
			national.model.rw1 = inla_model.national[[3]],
			national.model.rw2 = inla_model.national[[4]],
			national.model.rw1.unadj = inla_model.national[[1]],
			national.model.rw2.unadj = inla_model.national[[2]],
			data.HT.combined.national = data.national.adj
)
out$data.HT$survey.label <- surveylabels[out$data.HT$survey]
save(out, file = paste0("Fitted/", countryname, "-yearly.rda"))



