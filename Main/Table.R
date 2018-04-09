#######################################################
## Get final tables
#######################################################
library(spdep)
library(maptools)
library(gpclib)
library(plyr)
library(raster)
library(reshape2)
library(INLA)
library(ggplot2)
library(RColorBrewer)
library(lattice)
library(xlsx)
library(maptools)
library(RColorBrewer)
library(classInt)
library(xtable)
gpclibPermit()

countries <- list.files("../Data/map/")
countries <- countries[countries %in% c("Bangladesh", "Indonesia", "Philippines", "Cambodia",  "AfricanCountries", "World") == FALSE]
countries[3] <- "Angola"
countries[1] <- "Burkina Faso"
years <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14") 

expit<-function(x){
    exp(x)/(1+exp(x))
}
geo.rest <- readShapePoly("../Data/map/AfricanCountries/AfricanCountires.shp")
load("../Data/Africa_all.rda")

plot.res.all <- NULL
for(CIndex in 1:length(countries)){
    countryname <- countries[CIndex]
    if(!file.exists(paste0("Fitted/", countryname, "-yearly.rda"))){
      print(countries[CIndex])
      next
    }
    load(paste0("Fitted/", countryname, "-yearly.rda"))
    
    plot.res <- out$results.rw2
    plot.res$District <- plot.res$District

	plot.res$drop <- NA
	for(i in 1:dim(plot.res)[1]){
		base <- plot.res$med[intersect(which(plot.res$District == plot.res$District[i]), which(plot.res$Year == "1990"))]
		plot.res$drop[i] <- (base - plot.res$med[i] )/ base
    }
    plot.res$country <- countryname
    # plot.res <- merge(plot.res, data, by = "Year")
    plot.res.all <- rbind(plot.res.all, plot.res) 
    cat(".")
}
plot.res.all$achieve <- plot.res.all$drop > 2/3
n.region <- length(unique(plot.res.all$District))
n.country <- length(unique(plot.res.all$country))
print(paste0( 
"Number of regions: ", n.region, "   ",
"Number of countries: ", n.country
))


# add national estimates
plot.res.national <- NULL
for(CIndex in 1:length(countries)){
    countryname <- countries[CIndex]
    if(!file.exists(paste0("Fitted/", countryname, "-yearly.rda"))){
      print(countries[CIndex])
      next
    }
    load(paste0("Fitted/", countryname, "-yearly.rda"))
    tmp <- out$estimates.national.final
    # nrow <- dim(tmp)[1]
    # nrep <- length(out$regions)
    # tmp <- do.call("rbind", replicate(nrep, tmp, simplify = FALSE))
    # tmp$Region <- rep(out$regions, each=nrow)
    tmp$District <- tmp$Region
    colnames(tmp)[colnames(tmp)=="Median"] <- "med"
    colnames(tmp)[colnames(tmp)=="Lower"] <- "q025"
    colnames(tmp)[colnames(tmp)=="Upper"] <- "q975"
    tmp$country <- countryname
    # plot.res <- merge(plot.res, data, by = "Year")
    plot.res.national <- rbind(plot.res.national, tmp) 
    cat(".")
}
plot.res.national$is.yearly <- !(plot.res.national$Year %in% years)



# add national estimates
HT.all <- NULL
HT.national <- NULL
adjust.ratio <- matrix(NA, length(countries), length(years))
colnames(adjust.ratio) <- years
for(CIndex in 1:length(countries)){
    countryname <- countries[CIndex]
    if(!file.exists(paste0("Fitted/", countryname, "-yearly.rda"))){
      print(countries[CIndex])
      next
    }
    load(paste0("Fitted/", countryname, "-yearly.rda"))
    tmp1 <- out$data.HT.combined.national.unadj
    tmp2 <- out$data.HT.combined

    # ratio of benchmarking adjustment (unadj / adj)
    tmp3 <- out$data.HT.combined.national
    ratio <- merge(tmp1[, c("years", "u5m")],
                   tmp3[, c("years", "u5m")], by="years")
    tmp4 <- ratio[,2]/ratio[,3]

    adjust.ratio[CIndex, ] <- tmp4[match(years, as.character(ratio$years))]

    tmp1 <- tmp1[, c("region", "years", "u5m", "lower", "upper")]
    tmp2 <- tmp2[, c("region", "years", "u5m", "lower", "upper")]

    tmp1$country <- countryname
    tmp2$country <- countryname

    # plot.res <- merge(plot.res, data, by = "Year")
    HT.all <- rbind(HT.all, tmp1) 
    HT.national <- rbind(HT.national, tmp2) 
    cat(".")
}

# save the table of adjustment
tab0 <- data.frame(country = countries)
tab0 <- cbind(tab0, adjust.ratio)
tab0$mean <- apply(tab0[,-1], 1, mean, na.rm=TRUE)
tab0[,1] <- as.character(tab0[,1])
ave <- apply(tab0[,-1], 2, mean, na.rm = TRUE)
tab0 <- rbind(tab0, rep(NA, dim(tab0)[2]))
tab0[dim(tab0)[1], -1] <- ave
tab0[dim(tab0)[1], 1] <- "Average"
print(xtable(tab0), include.rownames = F, file = "Tables/final_table_benchmark.tex")

# add in UN and IHME yearly estimates
UN <- read.csv("../Data/comparison/UNestimates_yearly_2018_03_14.csv")
colnames(UN)[c(1:5)] <- c("Country", "Year", "q025", "med", "q975")
UN$Country <- revalue(UN$Country, c("Cote d Ivoire"="Cote_dIvoire", "Gambia The"="Gambia", "Congo DR"="DRC"))
UN$Region <- "All"
UN$Method <- "UN"
UN$District <- paste0(UN$Country, "All")
UN$country <- UN$Country
UN$med <- UN$med/1000
UN$q025 <- UN$q025/1000
UN$q975 <- UN$q975/1000

IHME <- read.csv("../Data/comparison/IHMEestimates_yearly_2018_03_14.csv")
colnames(IHME)[c(1:5)] <- c("Country", "Year", "med", "q975", "q025")
IHME$Country <- revalue(IHME$Country, c("Cote d'Ivoire"="Cote_dIvoire", "The Gambia"="Gambia", "Democratic Republic of the Congo"="DRC"))
IHME$Region <- "All"
IHME$Method <- "IHME"
IHME$District <- paste0(IHME$Country, "All")
IHME$country <- IHME$Country
IHME$med <- IHME$med/1000
IHME$q025 <- IHME$q025/1000
IHME$q975 <- IHME$q975/1000


# add in UN and IHME period estimates
UN2 <- read.csv("../Data/comparison/UNestimates_2017_08_08.csv")
colnames(UN2)[c(1:5)] <- c("Country", "Year", "med", "q025", "q975")
UN2$Country <- revalue(UN2$Country, c("Cote d'Ivoire"="Cote_dIvoire", "Democratic Republic of the Congo"="DRC"))
UN2$Region <- "All"
UN2$Method <- "UN"
UN2$District <- paste0(UN2$Country, "All")
UN2$country <- UN2$Country
UN2$med <- UN2$med/1000
UN2$q025 <- UN2$q025/1000
UN2$q975 <- UN2$q975/1000

IHME2 <- read.csv("../Data/comparison/IHMEestimates_2017_08_08.csv")
colnames(IHME2)[c(1:5)] <- c("Country", "Year", "med", "q025", "q975")
IHME2$Country <- revalue(IHME2$Country, c("Cote d'Ivoire"="Cote_dIvoire", "Democratic Republic of the Congo"="DRC"))
IHME2$Region <- "All"
IHME2$Method <- "IHME"
IHME2$District <- paste0(IHME2$Country, "All")
IHME2$country <- IHME2$Country
IHME2$med <- IHME2$med/1000
IHME2$q025 <- IHME2$q025/1000
IHME2$q975 <- IHME2$q975/1000

#####################################################
## Organize the final output
#####################################################
colnames <- c("Country", "Region", "Year", "Median", "Lower", "Upper")


# Yearly subnational RW2
tab1 <- plot.res.all[plot.res.all$is.yearly==TRUE, c("country", "District", "Year", "med", "q025", "q975")]
colnames(tab1) <- colnames
tab1$Method <- "RW2"
print(dim(tab1))
print(head(tab1))

# Period subnational RW2
tab2 <- plot.res.all[plot.res.all$is.yearly==FALSE, c("country", "District", "Year", "med", "q025", "q975")]
colnames(tab2) <- colnames
tab2$Method <- "RW2"
print(dim(tab2))
print(head(tab2))

# Yearly national RW2
tab3 <- plot.res.national[plot.res.national$is.yearly==TRUE, c("Country", "Region", "Year", "med", "q025", "q975", "Method")]
tab3 <- tab3[tab3$Method == "RW2", ]
colnames(tab3) <-  c(colnames, "Method")
tab3$Method <- "RW2"
print(dim(tab3))
print(head(tab3))

# Period national RW2
tab4 <- plot.res.national[plot.res.national$is.yearly==FALSE, c("Country", "Region", "Year", "med", "q025", "q975", "Method")]
tab4 <- tab4[tab4$Method == "RW2", ]
colnames(tab4) <- c(colnames, "Method")
tab4$Method <- "RW2"
print(dim(tab4))
print(head(tab4))

# Yearly national UN
UN <- UN[UN$Country %in% countries, ]
tab5 <- UN[, c("Country", "Region", "Year", "med", "q025", "q975")]
colnames(tab5) <- colnames
tab5$Method <- "UN"
print(dim(tab5))
print(head(tab5))

# Period national UN
UN2<- UN2[UN2$Country %in% countries, ]
tab6 <- UN2[, c("Country", "Region", "Year", "med", "q025", "q975")]
colnames(tab6) <- colnames
tab6$Method <- "UN"
print(dim(tab6))
print(head(tab6))

# Yearly national IHME
IHME <- IHME[IHME$Country %in% countries, ]
tab7 <- IHME[, c("Country", "Region", "Year", "med", "q025", "q975")]
colnames(tab7) <- colnames
tab7$Method <- "IHME"
print(dim(tab7))
print(head(tab7))

# Period national IHME
IHME2 <- IHME2[IHME2$Country %in% countries, ]
tab8 <- IHME2[, c("Country", "Region", "Year", "med", "q025", "q975")]
colnames(tab8) <- colnames
tab8$Method <- "IHME"
print(dim(tab8))
print(head(tab8))

# Period subnational HT-Direct
tab9 <- HT.all[, c("country", "region", "years", "u5m", "lower", "upper")]
colnames(tab9) <- colnames
tab9$Method <- "HT-Direct"
print(dim(tab9))
print(head(tab9))

# Period national HT-Direct
tab10 <- HT.national[, c("country", "region", "years", "u5m", "lower", "upper")]
colnames(tab10) <- colnames
tab10$Method <- "HT-Direct"
print(dim(tab10))
print(head(tab10))

# all yearly estimates
out1 <- rbind(tab1, tab3, tab5, tab7)
out1 <- out1[with(out1, order(Country, Region, Year, Method)), ]

# all period estimates
out2 <- rbind(tab2, tab4, tab6, tab8, tab9, tab10)
out2 <- out2[with(out2, order(Country, Region, Year, Method)), ]

write.csv(out1, file = "Tables/YearlyEstimatesAll.csv", row.names = FALSE)
write.csv(out2, file = "Tables/PeriodEstimatesAll.csv", row.names = FALSE)


####################
# library(Hmisc)
tab <- out1
tab <- tab[!is.na(tab$Median), ]
tab$Region <- gsub(".*:","",tab$Region)
# resolve encoding for Gabon
tab$Region[tab$Region == "east (haut-ogoou\xe9 & ogoou\xe9-lolo)"] <- "East"
tab$Region[tab$Region == "north (ogoou\xe9-ivindo & woleu-ntem)"] <- "North"
tab$Region[tab$Region == "south (ngouni\xe9, nyanga)"] <- "South"
tab$Region[tab$Region == "west (estuaire, moyen-ogoou\xe9 & ogoou\xe9-maritime)"] <- "West"
tab$Region <- toupper(tab$Region)
tab$Median <- tab$Median * 1000
tab$Lower <- tab$Lower * 1000
tab$Upper <- tab$Upper * 1000
tab$Country[tab$Country == "Cote_dIvoire"] <- "Cote d'Ivoire"


tab <- xtable(tab, label = "fulltable", caption = "Complete results.")
addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste("\\hline \n",
                             "\\endhead \n",
                             "\\hline \n",
                             "{\\footnotesize Continued on next page} \n",
                             "\\endfoot \n",
                             "\\endlastfoot \n",sep=""))
print(tab, tabular.environment = "longtable", 
      floating = FALSE,
      include.rownames = FALSE, 
      add.to.row = addtorow,
      hline.after=c(-1),
      file = "Tables/final_xtable_yearly.tex") 



tab <- out2
tab <- tab[!is.na(tab$Median), ]
tab$Region <- gsub(".*:","",tab$Region)
# resolve encoding for Gabon
tab$Region[tab$Region == "east (haut-ogoou\xe9 & ogoou\xe9-lolo)"] <- "East"
tab$Region[tab$Region == "north (ogoou\xe9-ivindo & woleu-ntem)"] <- "North"
tab$Region[tab$Region == "south (ngouni\xe9, nyanga)"] <- "South"
tab$Region[tab$Region == "west (estuaire, moyen-ogoou\xe9 & ogoou\xe9-maritime)"] <- "West"
tab$Region <- toupper(tab$Region)
tab$Median <- tab$Median * 1000
tab$Lower <- tab$Lower * 1000
tab$Upper <- tab$Upper * 1000
tab$Country[tab$Country == "Cote_dIvoire"] <- "Cote d'Ivoire"


tab <- xtable(tab, label = "fulltable", caption = "Complete results.")
addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste("\\hline \n",
                             "\\endhead \n",
                             "\\hline \n",
                             "{\\footnotesize Continued on next page} \n",
                             "\\endfoot \n",
                             "\\endlastfoot \n",sep=""))
print(tab, tabular.environment = "longtable", 
      floating = FALSE,
      include.rownames = FALSE, 
      add.to.row = addtorow,
      hline.after=c(-1),
      file = "Tables/final_xtable_period.tex") 

