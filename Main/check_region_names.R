countrylist <- c("Bangladesh", "Burkina Faso", "Cambodia", "Egypt", "Ethiopia", "Ghana", "Indonesia", "Malawi", "Nigeria", "Philippines", "Uganda", "Zambia", "Zimbabwe", "Cameroon", "Mali", "Morocco", "Niger", "Benin", "DRC", "Lesotho", "Liberia", "Namibia", "Sierra Leone", "Rwanda", "Tanzania")
countrylist <- c(countrylist, c("Gambia", "Senegal", "Comoros", "Cote_dIvoire", "Burundi"))


library(plyr)
library(maptools)

cat("Examine naming difference:\n")
for(countryname in countrylist){
	info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
	final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]


	if(!file.exists(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))){
		warning(paste("Map file not exist:", countryname))

	}else{
		geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
		geo$NAME_1 <- geo$REGNAME

		# special fix for maps
		if(countryname == "Tanzania"){
		  geo <- geo[geo$NAME_1 != "rest zanzibar",]
		  geo <- geo[geo$NAME_1 != "pemba",]
		  geo$NAME_1 <- revalue(geo$NAME_1, c("coast"="pwani"))
		  name_in_map <- geo$NAME_1
		}else if(countryname == "Indonesia"){
		  geo <- geo[geo$NAME_1 != "east timor",]
  		  name_in_map <- geo$NAME_1
		}else if(countryname == "Burkina Faso"){
		   gpclibPermit()
		   # Merge two areas
		   merge.id <- c("Central/South", "North", "East", "West", "Central/South")
		   NAME_final <- data.frame(NAME_final = final_name_ordered)
		   NAME_final$NAME_final <- as.character(NAME_final$NAME_final)
		   rownames(NAME_final) <- final_name_ordered
		   geo <- unionSpatialPolygons(geo, merge.id )
		   geo <- SpatialPolygonsDataFrame(geo, NAME_final)
		   name_in_map <- rownames(NAME_final)
		}else{
			name_in_map <- geo$NAME_1
		}

		if(length(name_in_map) != length(final_name_ordered)){
			warning(paste("region number inconsistent:", countryname))
		}
		for(i in 1:length(name_in_map)){
			if(tolower(as.character(name_in_map[i])) != tolower(as.character(final_name_ordered[i]))){
				cat(countryname, " map: ", as.character(name_in_map[i]), 
					" info file: ", as.character(final_name_ordered[i]), "\n")
			}
		}
		cat("\n")
	}
}

######################################################################
# Fix the info files
countryname <- "Ethiopia"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
new_ordered <- tolower(as.character(geo$REGNAME))
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)

countryname <- "DRC"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
new_ordered <- tolower(as.character(geo$REGNAME))
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)


countryname <- "Lesotho"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
new_ordered <- tolower(as.character(geo$REGNAME))
new_ordered[1] <- "butha-buthe"
new_ordered[5] <- "thaba-tseka"
new_ordered[9] <- "qasha's nek"
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)


countryname <- "Liberia"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
new_ordered <- tolower(as.character(geo$REGNAME))
new_ordered[4] <- "south eastern a"
new_ordered[5] <- "south eastern b"
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)

countryname <- "Namibia"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
new_ordered <- tolower(as.character(geo$REGNAME))
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)

countryname <- "Sierra Leone"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
new_ordered <- tolower(as.character(geo$REGNAME))
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)


countryname <- "Togo"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
new_ordered <- tolower(as.character(geo$REGNAME))
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)



countryname <- "Mozambique"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- tolower(as.character(info[, 3])[which(as.character(info[, 3]) != "")])
new_ordered <- tolower(as.character(geo$REGNAME))
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)


countryname <- "Madagascar"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- tolower(as.character(info[, 3])[which(as.character(info[, 3]) != "")])
new_ordered <- tolower(as.character(geo$REGNAME))
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)


countryname <- "Chad"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- tolower(as.character(info[, 3])[which(as.character(info[, 3]) != "")])
new_ordered <- tolower(as.character(geo$REGNAME))
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)



countryname <- "Senegal"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
final_name_ordered <- tolower(final_name_ordered )
new_ordered <- as.character(geo$REGNAME)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)

countryname <- "Comoros"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
final_name_ordered <- tolower(final_name_ordered )
new_ordered <- as.character(geo$REGNAME)
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)


countryname <- "Cote_dIvoire"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
final_name_ordered <- tolower(final_name_ordered )
new_ordered <- as.character(geo$REGNAME)
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)

countryname <- "Burundi"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
final_name_ordered <- tolower(final_name_ordered )
new_ordered <- as.character(geo$REGNAME)
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)

countryname <- "Angola"
info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
# final_name_ordered <- tolower(final_name_ordered)
new_ordered <- tolower(as.character(geo$REGNAME))
sum(new_ordered %in% final_name_ordered) == length(final_name_ordered)
sum(final_name_ordered%in% new_ordered) == length(final_name_ordered)
info[, 3] <- ""
info[1:length(new_ordered), 3] <- new_ordered
info <- info[1:length(new_ordered), ]
write.csv(info, paste0("../CountryMain/CountryInfo/", countryname, ".csv"), row.names = FALSE)

######################################################
######################################################
library(spdep)
library(maptools)
library(gpclib)
countrylist <- c("Bangladesh", "Burkina Faso", "Cambodia", "Egypt", "Ethiopia", "Ghana", "Indonesia", "Malawi", "Nigeria", "Philippines", "Uganda", "Zambia", "Zimbabwe", "Cameroon", "Mali", "Morocco", "Niger", "Benin", "DRC", "Lesotho", "Liberia", "Namibia", "Sierra Leone", "Rwanda", "Tanzania", "Gambia", "Senegal", "Comoros", "Cote_dIvoire", "Burundi")
for(countryname in countrylist){
	print(countryname)
	if(file.exists(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))){
		geo <- readShapePoly(paste0("../Data/map/", countryname, 
		"/sdr_subnational_boundaries.shp"))
		geo$NAME_1 <- geo$REGNAME
		info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
		final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]

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
		if(countryname %in% c("Burkina Faso", "Gabon") == FALSE){
			name_in_map <- geo$REGNAME
			for(i in 1:length(name_in_map)){
				if(tolower(as.character(name_in_map[i])) != tolower(as.character(final_name_ordered[i]))){
					cat(countryname, " map: ", as.character(name_in_map[i]), 
						" info file: ", as.character(final_name_ordered[i]), "\n")
				}
			}
		}
		# Do one more eyeball check of the map
		pdf(paste0("../CountryMain/CountryInfo/Regions_", countryname, ".pdf"), 
		height = 20, width = 20)
		plot(geo)
		if(countryname == "Gabon"){
			geo$NAME_final <- iconv(geo$NAME_final, "latin1", "ASCII", sub="")
		}
		text(coordinates(geo), labels = geo$NAME_final)
		dev.off()		
	
	}else{
		warning(paste("Map file do not exist!", countryname))
	}
}
