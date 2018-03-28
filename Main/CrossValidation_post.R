library(ggplot2)
library(spdep)
library(maptools)
library(gpclib)
library(plyr)
library(raster)
library(reshape2)
library(xtable)
gpclibPermit()

countries <- list.files("../Data/map/")
countries <- countries[countries %in% c("Bangladesh", "Indonesia", "Philippines", "Cambodia",  "AfricanCountries", "World") == FALSE]

coverage <- matrix(NA, length(countries), 2+4)
coverage.lastperiod <- matrix(NA, length(countries))
cover.count <- rep(0, 4)

logit <- function(x){log(x / (1 - x))}
expit <- function(x){exp(x) / (1 + exp(x))}
d1Yall <- d1Call <- NULL
for(CIndex in 1:length(countries)){
	countryname <- countries[CIndex]
	countryname2 <- gsub(" ", "", countryname)

	if(!file.exists(paste0("Fitted/", countryname, "-yearlyCV.rda"))){
		print(countryname)
	}else{
		load(paste0("Fitted/", countryname, "-yearlyCV.rda"))

		# bias plot of the yearly CV
		d1 <- out$resultsYear2[out$resultsYear2$type == "CV", ]
		d2 <- out$resultsYear2[out$resultsYear2$type == "Direct", ]  
		colnames(d2)[c(1,2,3,4,7)] <- paste0(colnames(d2)[c(1,2,3,4,7)], ".compare")
		d2 <- d2[, -8]
		d1 <- merge(d1, d2)
		d1$bias <- d1$med.logit - d1$med.logit.compare
		d1$bias.scaled <- d1$bias / sqrt(d1$sd^2 + d1$sd.compare^2)
		d1Y <- d1

		# look at last period only for reference
		last <- which(d1Y$year == "10-14")
		if(length(last) == 0 || sum(is.na(d1Y[last,"med"])) == dim(d1Y)[1]){
			last <- which(d1Y$year == "05-09")
		}
		coverage.lastperiod[CIndex] <- length(intersect(which(d1Y[last, "lower"] < d1Y[last, "med.compare"]), 
			which(d1Y[last, "upper"] > d1Y[last, "med.compare"]))) / sum(!is.na(d1Y[last,"med.compare"]))

		# bias plot of the combined CV
		d1 <- out$resultsComb2[out$resultsComb2$type == "CV", ]
		d2 <- out$resultsComb2[out$resultsComb2$type == "Direct", ]  
		colnames(d2)[c(1,2,3,4,7)] <- paste0(colnames(d2)[c(1,2,3,4,7)], ".compare")
		d2 <- d2[, -8]
		d1 <- merge(d1, d2)
		d1$bias <- d1$med.logit - d1$med.logit.compare
		d1$bias.scaled <- d1$bias / sqrt(d1$sd^2 + d1$sd.compare^2)
		d1C <- d1

		cover.count[1] <- cover.count[1] + length(intersect(
			which(d1Y[, "lower"] < d1Y[, "med.compare"]),
			which(d1Y[, "upper"] > d1Y[, "med.compare"]))) 
		cover.count[2] <- cover.count[2] +  sum(!is.na(d1Y[,"med.compare"]))
		cover.count[3] <- cover.count[3] +  length(intersect(
			which(d1C[, "lower"] < d1C[, "med.compare"]),
			which(d1C[, "upper"] > d1C[, "med.compare"]))) 
		cover.count[4] <- cover.count[4] +  sum(!is.na(d1C[,"med.compare"]))

		# check 
		# par(mfrow= c(1,2))
		# plot(logit(d1C$upper), logit(d1C$med) + 1.95*sqrt(d1C$sd^2+d1C$sd.compare^2))
		# abline(c(0,1))
		# plot(logit(d1C$lower), logit(d1C$med) - 1.95*sqrt(d1C$sd^2+d1C$sd.compare^2))
		# abline(c(0,1))

		m1 <- mean(d1Y$bias.scaled, na.rm=T) 
		m2 <- mean(d1C$bias.scaled, na.rm=T) 
		s1 <- sd(d1Y$bias.scaled, na.rm=T) 
		s2 <- sd(d1C$bias.scaled, na.rm=T) 
		coverage[CIndex, ] <- c(out$cover2, out$cover4, m1, s1, m2, s2)

		pdf(paste0("Figures/yearly/CVbias", countryname2, ".pdf"), width = 7, height = 3.7)
		m1 <- round(m1, 3)
		m2 <- round(m2, 3)
		s1 <- round(s1, 3)
		s2 <- round(s2, 3)

		par(mfrow = c(1, 2))
		# hist(d1Y$bias.scaled, main = paste0("CV by time\nmean=",m1," sd=", s1), xlab = "Rescaled bias", cex.main=1)
		# abline(v=0, col='red', lty = 2, lwd = 2.5)
		# qqnorm(d1Y$bias.scaled, main = paste0("CV by time"), cex.main=1)
		# abline(c(0,1), col='red', lwd = 1.5)
		hist(d1C$bias.scaled, main = paste0("CV by time-space\nmean=",m2," sd=", s2), xlab = "Rescaled bias", cex.main=1)
		abline(v=0, col='red', lty = 2, lwd = 2.5)
			qqnorm(d1C$bias.scaled, main = paste0("CV by time-space"), cex.main=1)
		abline(c(0,1), col='red', lwd = 1.5)
		dev.off()

		pdf(paste0("Figures/yearly/CVbiasbyRegion", countryname2, ".pdf"), width = 7, height = 6)
		d1C$year <- factor(d1C$year, levels =  c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14") )
		if(countryname == "Gabon"){
			d1C$region[d1C$region == "east (haut-ogoou\xe9 & ogoou\xe9-lolo)"] <- "East"
			d1C$region[d1C$region == "north (ogoou\xe9-ivindo & woleu-ntem)"] <- "North"
			d1C$region[d1C$region == "south (ngouni\xe9, nyanga)"] <- "South"
			d1C$region[d1C$region == "west (estuaire, moyen-ogoou\xe9 & ogoou\xe9-maritime)"] <- "West"
		}
		my.dodge <- position_dodge(width = 0.2)
		g <- ggplot(aes(x = year, y = bias.scaled, color = region, group = region), data = d1C)
		g <- g + geom_hline(yintercept = 0, color="red", linetype="longdash")
		g <- g + geom_point(position = my.dodge)
		g <- g + geom_line( position = my.dodge,alpha=0.3)
		g <- g + theme_bw() + xlab("Year") + ylab("Rescaled bias")
		g <- g + theme(legend.position="bottom", legend.text=element_text(size=11))
		print(g)
		dev.off()


		cat(".")
		d1Yall <- rbind(d1Yall, d1Y)
		d1Call <- rbind(d1Call, d1C)		
	}
}

# look at last year
mean(coverage.lastperiod, na.rm=T)

# look at all combined (coverage)
print(paste("1st CV cover probability:", cover.count[1] / cover.count[2]))
print(paste("2nd CV cover probability:", cover.count[3] / cover.count[4]))

# look at all combined
m1 <- mean(d1Yall$bias.scaled, na.rm=T) 
m2 <- mean(d1Call$bias.scaled, na.rm=T) 
s1 <- sd(d1Yall$bias.scaled, na.rm=T) 
s2 <- sd(d1Call$bias.scaled, na.rm=T) 

pdf(paste0("Figures/yearly/CVbias.pdf"), width = 7, height = 3.7)
m1 <- round(m1, 3)
m2 <- round(m2, 3)
s1 <- round(s1, 3)
s2 <- round(s2, 3)

par(mfrow = c(1, 2))
# hist(d1Yall$bias.scaled, main = paste0("CV by time\nmean=",m1," sd=", s1), xlab = "Rescaled bias", cex.main=1)
# abline(v=0, col='red', lty = 2, lwd = 2.5)
# qqnorm(d1Yall$bias.scaled, main = paste0("CV by time"), cex.main=1)
# abline(c(0,1), col='red', lwd = 1.5)
hist(d1Call$bias.scaled, main = paste0("CV by time-space\nmean=",m2," sd=", s2), xlab = "Rescaled bias", cex.main=1)
abline(v=0, col='red', lty = 2, lwd = 2.5)
	qqnorm(d1Call$bias.scaled, main = paste0("CV by time-space"), cex.main=1)
abline(c(0,1), col='red', lwd = 1.5)
dev.off()

cat(".")
tab <- data.frame(country = countries,
					# byYear = coverage[,1],
					# byYearMeanBias = coverage[,3],
					# byYearSDBias = coverage[,4],
					byYearRegion = coverage[,2], 
					byYearRegionMeanBias = coverage[,5],
					byYearRegionSDBias = coverage[,6]
					)

tab <- rbind(tab, c(NA, apply(tab[, -1], 2, mean, na.rm = T)))
tab[,1] <- as.character(tab[,1])
tab[dim(tab)[1], 1] <- "Average"
print(xtable(tab), include.rownames = F, file = "Tables/final_table_cv.tex")



## Update the plot to have consistent themes
for(CIndex in 1:length(countries)){
	countryname <- countries[CIndex]
	countryname2 <- gsub(" ", "", countryname)

	if(!file.exists(paste0("Fitted/", countryname, "-yearlyCV.rda"))){
		print(countryname)
	}else{
		load(paste0("Fitted/", countryname, "-yearlyCV.rda"))
	}
	pdf(paste0("Figures/yearly/CV_byYear_withError_", countryname2, ".pdf"), width = 12, height = 7)
	print(out$g2 + theme_bw() +ggtitle("Cross validation by time periods")+ theme(legend.position="bottom", legend.text=element_text(size=11))) 
	dev.off()
		pdf(paste0("Figures/yearly/CV_byYearRegion_withError_", countryname2, ".pdf"), width = 12, height = 7)
	print(out$g4 + theme_bw() +ggtitle("Cross validation by time periods and regions")+ theme(legend.position="bottom", legend.text=element_text(size=11))) 
	dev.off()
	cat(".")
}