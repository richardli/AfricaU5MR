library(xtable)
remove(list = ls())
countries <- list.files("../Data/map/")
countries <- countries[countries %in% c("AfricanCountries", "World", "Bangladesh", "Cambodia", "Indonesia", "Philippines") == F]
count <- 0
counter <- 1
tab <- matrix(NA, length(countries), 2)
for(CIndex in 1:length(countries)){
	countryname <- countries[CIndex]
	if(file.exists(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))){
		info <- read.csv(paste0("../CountryMain/CountryInfo/", countryname, ".csv"))
		years.out <- info[info[,2] != "", 2]
		count <- count + length(years.out)	
		years.out <- gsub("DHS ", "", years.out)
		years.out <- gsub("DHS", "", years.out)
		years.out <- paste(years.out, collapse = ", ")
		tab[counter, 1] <- countryname
		tab[counter, 2] <- years.out
		counter <- counter + 1
	}	
}
tab <- tab[1:(counter-1), ]
colnames(tab) <- c("Country", "DHS")
write.csv(tab, file = "Tables/surveylist.csv", row.names = F)