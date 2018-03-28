source("../HelperFunctions/5q0glm2.R")
source("CombinedRegionChange.R")
years<-c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14")
countrylist <- c("Bangladesh", "Burkina Faso", "Cambodia", "Egypt", "Ethiopia", "Ghana", "Indonesia", "Malawi", "Nigeria", "Philippines", "Uganda", "Zambia", "Zimbabwe", "Cameroon", "Mali", "Morocco", "Niger","Benin", "DRC", "Lesotho", "Liberia", "Namibia", "Sierra Leone", " Rwanda", "Tanzania", "Guinea", "Gabon", "Kenya",  "Chad", "Madagascar", "Mozambique", "Togo")
countrylist <- c("Ethiopia", "Benin", "Congo", "Guinea")
countrylist <- c("Angola", "Gambia", "Senegal", "Comoros", "Cote_dIvoire", "Burundi")

for(k in 1:length(countrylist)){
	countryname <- countrylist[k]

	# Create new directory
	dir.create(paste0("../Data/5q0_estimates_new/", countryname), showWarnings = FALSE)
	filenames <- sort(list.files(paste0("../Data/CountryFix/", countryname, "/")))
	filenames <- filenames[grepl(".dta", tolower(filenames))]
	Bmat <- getBmat(countryname)

	# extract information from previously constructed spreadsheet
	# just a hack to make output filenames consistent
	if(file.exists(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))){
		info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
		filenames2 <- as.character(info[, 1])[which(as.character(info[, 1]) != "")]
		svy.years <- unlist(regmatches(filenames, gregexpr("[0-9]+", filenames)))
		output.years <- unlist(regmatches(filenames2, gregexpr("[0-9]+", filenames2)))
	}else{
		filenames2 <- NULL
		svy.years <- unlist(regmatches(filenames, gregexpr("[0-9]+", filenames)))
		svy.years <- substr(svy.years, 1, 4)
		output.years <- svy.years
	}

	
	# country-specific fixes
	if(countryname == "Zimbabwe"){
		output.years <- c("1994", "1999", "2006", "2011", "2015")
	}
	if(countryname == "Zambia"){
		output.years <- c("1992", "1996", "2001", "2007", "2014")
	}
	
	# need to rerun Cameroon 1991 later remove it for now
	if(countryname == "Cameroon"){
		output.years = output.years[which(output.years != "1991")]
		svy.years = svy.years[which(svy.years != "1991")]
		filenames = filenames[which(filenames != "cameroon.births.1991.dta")]
	}
	# need to fix Kenya 1989
	if(countryname == "Kenya"){
		output.years = output.years[which(output.years != "1989")]
		svy.years = svy.years[which(svy.years != "1989")]
		filenames = filenames[which(filenames != "kenya.births.1989.dta")]
		filenames2 = filenames2[which(filenames2 != "kenya1989")]
	}


	print(countryname)
	out <- vector("list", length(filenames))
	for(i in 1:length(filenames)){
		out[[i]] <- countrySummary(filename = paste0("../Data/CountryFix/", countryname, "/", filenames[i]), years = years, regionVar = "region", geo.recode = Bmat)
		if(is.null(filenames2)){
			write.csv(out[[i]], file = paste0("../Data/5q0_estimates_new/", countryname, "/", countryname, svy.years[i], ".csv"), row.names = F) 
		}else{
			write.csv(out[[i]], file = paste0("../Data/5q0_estimates_new/", countryname, "/", filenames2[which(output.years == svy.years[i])], ".csv"), row.names = F) 
		}
	}

	# write the info files
	# After this step, go to CSV file to correct region order as in the map files
	if(is.null(filenames2)){
		library(foreign)
		Filenames <- paste0(countryname, svy.years)
		SurveyNames <- paste0("DHS", svy.years)
		RegionNames <- NULL
		for(i in 1:length(filenames)){
			tmp <- read.dta(paste0("../Data/CountryFix/", countryname, "/", filenames[i]))
			if("region" %in% colnames(tmp)){
				RegionNames <- unique(c(RegionNames, as.character(tmp[, "region"])))
			}else if("v024" %in% colnames(tmp)){
				RegionNames <- unique(c(RegionNames, as.character(tmp[, "v024"])))
			}else if("v101" %in% colnames(tmp)){
				RegionNames <- unique(c(RegionNames, as.character(tmp[, "v101"])))
			}else{
				stop("Region variable missing")
			}
		}

		maxline <- max(length(Filenames), length(RegionNames))
		Filenames <- c(Filenames, rep("", maxline - length(Filenames)))
		SurveyNames <- c(SurveyNames, rep("", maxline - length(SurveyNames)))
		RegionNames <- c(RegionNames, rep("", maxline - length(RegionNames)))
		out <- cbind(Filenames, SurveyNames, RegionNames)
		write.csv(out, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)
	}
	
	# print(svy.years)
	# print(output.years)
	# print("")
}
