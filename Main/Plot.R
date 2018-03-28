if(FALSE){
  start_time <- Sys.time()
  countries <- list.files("../Data/map/")
  countries <- countries[countries %in% c("AfricanCountries", "World", "Bangladesh", "Cambodia", "Indonesia", "Philippines") == F]
  for(countryname in countries){
  	print(countryname)
  	source("Plot.R")
  }
  end_time <- Sys.time()
  print(end_time - start_time) # ~1hr 
}


library(ggplot2)
library(maptools)
library(RColorBrewer)
library(classInt)
library(reshape2)
library(plyr)
source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")

load(paste0("Fitted/", countryname, "-yearly.rda"))

# todo: Ethiopia needs double check
# todo: Malawi needs to be put into periods.7
# todo: Zimbabwe needs to be put into periods.7
periods.5 <- c("Morocco")
periods.6 <- c("Benin", "Burkina Faso", "Egypt", "Niger",  "Cambodia", "Ghana", "Zimbabwe", "Mali", "Madagascar", "Burundi")
periods.7 <- c("Cameroon", "DRC", "Nigeria", "Uganda", "Bangladesh", "Indonesia", "Philippines","Bangladesh", "Rwanda", "Malawi", "Zambia", "Lesotho", "Liberia", "Namibia", "Sierra Leone", "DRC", "Tanzania", "Gabon", "Kenya", "Togo", "Mozambique", "Chad", "Congo", "Guinea", "Ethiopia", "Kenya", "Comoros", "Cote_dIvoire", "Gambia", "Senegal", "Angola")
if(countryname %in% periods.5){
  years <- c("80-84", "85-89", "90-94", "95-99", "00-04") 
}else if (countryname %in% periods.6){
  years <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09")  
}else if (countryname %in% periods.7){
  years <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14") 
}else{
  stop("How many years NOT determined")
}


countryname2 <- gsub(" ", "", countryname)
years.all <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14", "15-19")
years.med <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017)
expit<-function(x){
    exp(x)/(1+exp(x))
}
# merge data
temp <- out$data.HT.combined[, c("region", "years", "logit.est.nohiv", "u5m")]
colnames(temp) = c("District", "Year", "logit.est.nohiv", "u5m")
plot.res <- merge(out$results.rw2, temp, by = c("District" , "Year"), all=TRUE)
plot.res.more <- out$data.HT[, c("region", "years", "logit.est.nohiv", "u5m", "survey", "survey.label")]
colnames(plot.res.more)[1:2] = c("District", "Year")
plot.res.more <- merge(plot.res.more, out$results.rw2, by = c("District" , "Year"), all=TRUE)

n.region <- length(unique(out$results.rw2$District))
regions <- unique(out$results.rw2$District)
n.survey <- max(out$data.HT$survey)
surveys <- unique(out$data.HT$survey.label)
exist <- length(years)


###############################################
## Fix encoding of Gabon
###############################################
if(countryname=="Gabon"){
  out$results.rw2$District <- gsub(".*:","",out$results.rw2$District)
  # resolve encoding for Gabon
  out$results.rw2$District[out$results.rw2$District == "east (haut-ogoou\xe9 & ogoou\xe9-lolo)"] <- "East"
  out$results.rw2$District[out$results.rw2$District == "north (ogoou\xe9-ivindo & woleu-ntem)"] <- "North"
  out$results.rw2$District[out$results.rw2$District == "south (ngouni\xe9, nyanga)"] <- "South"
  out$results.rw2$District[out$results.rw2$District == "west (estuaire, moyen-ogoou\xe9 & ogoou\xe9-maritime)"] <- "West"
  out$results.rw2$District <- toupper(out$results.rw2$District)

  out$data.HT$region <- gsub(".*:","",out$data.HT$region)
  # resolve encoding for Gabon
  out$data.HT$region[out$data.HT$region == "east (haut-ogoou\xe9 & ogoou\xe9-lolo)"] <- "East"
  out$data.HT$region[out$data.HT$region == "north (ogoou\xe9-ivindo & woleu-ntem)"] <- "North"
  out$data.HT$region[out$data.HT$region == "south (ngouni\xe9, nyanga)"] <- "South"
  out$data.HT$region[out$data.HT$region == "west (estuaire, moyen-ogoou\xe9 & ogoou\xe9-maritime)"] <- "West"
  out$data.HT$region <- toupper(out$data.HT$region)
}




# ###############################################
# ## Plot: Benchmark
# ###############################################
pdf(paste0("Figures/yearly/Yearly_national_", countryname2, ".pdf"), width = 9, height = 5)

estimates.national <- out$estimates.national
estimates.national$Periods <- match(estimates.national$Year, years.all)
estimates.national$Method <- revalue(estimates.national$Method, c("RW2-adj"="RW2 (benchmarked)"))
Project <- rep(TRUE, dim(estimates.national)[1])
my.dodge <- position_dodge(width = 0.2)
g <- ggplot(aes(x = Periods, y = Median, ymin = Lower, ymax = Upper, color = Method, linetype=Method), data = estimates.national)
g <- g + geom_vline(xintercept = exist+0.5, color="gray50", linetype="longdash")
g <- g + geom_point(position = my.dodge)
g <- g + geom_line( position = my.dodge)
g <- g + geom_errorbar(size = .7, width = .05, position = my.dodge)
g <- g + theme_bw() + xlab("Year") + ylab("U5MR")
g <- g + scale_x_continuous(breaks=1:length(years.all), labels=years.all)
g <- g + scale_linetype_manual(labels=c("UN", "IHME", "RW2", "RW2 (benchmarked)"), values = c(2,3,1,1))
g <- g + theme(legend.position="bottom", legend.text=element_text(size=11))
# g <- g + ggtitle("Comparing national U5MR estimates using original and benchmarked data")
print(g)
dev.off()

###############################################
## Plot: Yearly vs Period
###############################################
ht <- 6
if(n.region > 6) ht <- 7
if(n.region > 10) ht <- 8
pdf(paste0("Figures/yearly/Yearly_v_Periods_", countryname2, ".pdf"), width = 12, height = ht)
my.dodge <- position_dodge(width = 1)
my.dodge.small <- position_dodge(width = .3)
g <- ggplot(aes(x = Year.num, y = med, ymin = q025, ymax = q975, color = District), data = out$results.rw2)
# g <- g + geom_rect(aes(xmin=years.med[exist]+2.5,xmax=Inf,ymin=-Inf,ymax=Inf),alpha=0.01,fill="#fee8c8", color="gray90")
g <- g + geom_vline(xintercept = years.med[exist]+2.5, color="gray50", linetype="longdash")
g <- g + geom_point(data = subset( out$results.rw2, is.yearly), position = my.dodge.small, alpha = 0.3)
g <- g + geom_line(data = subset( out$results.rw2, is.yearly), position = my.dodge.small, alpha = 0.3)
# g <- g + geom_errorbar(size = .2, width = .05, data = subset(results, is.yearly), position = my.dodge, alpha = 0.2)
g <- g + geom_errorbar(size = .7, width = .05, data = subset( out$results.rw2, !is.yearly), position = my.dodge)
g <- g + geom_point(shape = 17, size = 2.5, data = subset( out$results.rw2, !is.yearly), position = my.dodge)
# g <- g + geom_line(data = subset(results, !is.yearly), position = my.dodge, alpha = 1)
g <- g + ylim(range(out$results.rw2[!out$results.rw2$is.yearly, c(3:5)]))
g <- g + theme_bw() + scale_x_continuous(breaks=years.med,
        labels=years.all) + xlab("Year") + ylab("U5MR")
g <- g + theme(legend.position="bottom")
# g <- g + ggtitle("Comparing subnational yearly and period U5MR estimates using benchmarked data")
print(g)
dev.off()


###############################################
## Plot: Smoothed vs Direct
###############################################
##smooth v direct
pdf(paste0("Figures/yearly/SmoothvDirect", countryname2, "_meta.pdf"), width = 12, height = 6)
par(mfrow = c(1, 2))
n.years <- max(match(plot.res$Year, years.all), na.rm=T)
cols <- colorRampPalette(brewer.pal(min(8, n.years), "Dark2"))(n.years)
min <- 0
max <- 0.20
max2 <- max(c(expit(out$data.HT.combined$logit.est.nohiv), plot.res$med), na.rm = TRUE)*1.05
if(max2 > max) max <- max2

plot(NA, xlab = "Direct Estimates",
     ylab = "Smooth Estimats", main = "Combined", cex.main=1.4,
     xlim = c(min,max), ylim = c(min,max))
abline(0,1,lty = 2)
year.num <- match(plot.res$Year, years.all)
points(expit(plot.res$logit.est.nohiv), plot.res$med, col = cols[year.num], lwd = 3)
# for(i in 1:length(surv.years)){
#   whichrows <- which(out$data.HT.combined$years == as.character(surv.years[i]))
#   whichrows <- whichrows[match(areas.smooth, 
#             as.character(out$data.HT.combined$region[whichrows]))]
#   points(expit(out$data.HT.combined$logit.est.nohiv[whichrows]),
#          plot.res$med[plot.res$Year == as.character(surv.years[i])],
#          col = cols[i])
# }
legend('topleft', pch = 1, col = cols[1:n.years],
       legend = years.all[1:n.years], bty = 'n', pt.lwd=3)


plot(NA, xlab = "Direct Estimates",
     ylab = "Smooth Estimates",
     main = "Original", cex.main = 1.4,
     xlim = c(min, max), ylim = c(min, max))
abline(0,1,lty = 2)
year.num <- match(plot.res.more$Year, years.all)
points(expit(plot.res.more$logit.est.nohiv), plot.res.more$med, col = cols[year.num], pch = plot.res.more$survey, lwd = 1.5)
# for(i in 1:length(surv.years)){
#   for(j in 1:n.survey){
#     tmp <- out$data.HT$logit.est.nohiv[out$data.HT$years == as.character(surv.years[i])
#                                  & out$data.HT$survey == j]
#   points(expit(tmp),
#          rep(plot.res$med[plot.res$Year == as.character(surv.years[i])], 1),
#          col = cols[i], pch = j)
#   }
# }
legend('topleft', pch = c(rep(NA, n.years), 
                              1:n.survey), 
       col = c(cols[1:n.years], rep('black', n.survey)),
       lty = c(rep(1, n.years), rep(NA, n.survey)),
       legend = c(as.character(years.all[1:n.years]), surveys) , bty = 'n')
dev.off()



###############################################
## Plot: Map projection
###############################################
pdf(paste0("Figures/yearly/SmoothMedian", countryname2, ".pdf"), width = 8, height = 8)
med.palette <- brewer.pal(n = 9, name = "Purples")
med.int <- classIntervals(round(plot.res$med, 3),
                          n = 9, style = 'jenks')
med.col <- findColours(med.int, med.palette)
plot.res$med.col <- med.col
par(mfrow = c(3, 3), mai = c(.25, 0.1,0.3,0.1), oma = c(0.5, 0.1, 0.1, 0.1))
for(i in 1:n.years){  
  tmp <- plot.res[plot.res$Year == years.all[i],]
  tmp.col <- rep(NA, n.region)
  for(j in 1:n.region){
    tmp.col[j] <- tmp$med.col[tmp$District == regions[j]]
  }
  plot(out$geo, col = tmp.col, border = FALSE,
       main  = years.all[i])
 
}
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "center",inset = 0,
       legend = names(attr(med.col, 'table')), 
       fill = med.palette, cex= 1.25, horiz = FALSE, bty = 'n')
dev.off()



###############################################
## Plot: Map reduction
###############################################
pdf(paste0("Figures/yearly/ReductionMedian", countryname2, ".pdf"), width = 8, height = 8)
maxratio <- 1
minratio <- 0
base <- plot.res[plot.res$Year == 1990,]
plot.res$ratio <- 0
for(i in 1:(length(years.all))){ 
  tmp <- plot.res[plot.res$Year == years.all[i], ] 
  for(j in 1:n.region){
    tmp$ratio[tmp$District == regions[j]] <- 1 - tmp$med[tmp$District == regions[j]] / base$med[base$District == regions[j]]
  }
  plot.res[plot.res$Year == years.all[i], ] <- tmp
}
minratio <- round(min(plot.res$ratio), 4)
med.palette <- brewer.pal(n = 9, name = "Oranges")
fixed <- round(seq(minratio, maxratio, len = 9), 3)
med.int <- classIntervals(round(plot.res$ratio, 3),
                          n = 10, style = 'fixed', fixedBreaks = fixed)
med.col <- findColours(med.int, rev(med.palette))
plot.res$med.col.ratio <- med.col

# pdf(paste(paste("SmoothMedianRW2-reduction", country, sep = ''), "_meta.pdf", sep = ''))
par(mfrow = c(3, 3), mai = c(.25, 0.1,0.3,0.1), oma = c(0.5, 0.1, 0.1, 0.1))
for(i in 1:(length(years.all))){ 
  tmp <- plot.res[plot.res$Year == years.all[i],]
  tmp.col <- rep(NA, n.region)
  for(j in 1:n.region){
    tmp.col[j] <- tmp$med.col.ratio[tmp$District == regions[j]] 
  }
  plot(out$geo, col = tmp.col, border = FALSE,
       main  = years.all[i])
}
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "center",inset = 0,
       legend = names(attr(med.col, 'table')), 
       fill = rev(med.palette), cex= 1.25, horiz = FALSE, bty = 'n')
dev.off()



###############################################
## Plot: Line plot
###############################################
pdf(paste0("Figures/yearly/LineMedian", countryname2, ".pdf"), width = 9, height = 6)
my.dodge <- position_dodge(width = 1)
g <- ggplot(aes(x = Year.num, y = med, ymin = q025, ymax = q975, color = District), data = subset(out$results.rw2, !is.yearly))
# g <- g + geom_rect(aes(xmin=years.med[exist]+2.5,xmax=Inf,ymin=-Inf,ymax=Inf),alpha=0.01,fill="#fee8c8", color="gray90")
g <- g + geom_vline(xintercept = years.med[exist]+2.5, color="gray50", linetype="longdash")
g <- g + geom_line(position = my.dodge)
g <- g + geom_errorbar(size = .7, width = .05, data = subset( out$results.rw2, !is.yearly), position = my.dodge)
g <- g + geom_point(shape = 17, size = 2.5, data = subset( out$results.rw2, !is.yearly), position = my.dodge)
# g <- g + geom_line(data = subset(results, !is.yearly), position = my.dodge, alpha = 1)
# g <- g + ylim(range(out$results.rw2[!out$results.rw2$is.yearly, c(3:5)]))
g <- g + theme_bw() + scale_x_continuous(breaks=years.med,
        labels=years.all) + xlab("Year") + ylab("U5MR")
# g <- g + ggtitle("Subnational period U5MR estimates using benchmarked data")
print(g)
dev.off()



# ###############################################
# ## Plot: Line plot subnational
# ###############################################
if(n.region <= 6){
  par(mfrow=c(2,3))
  hh <- 6
  ww <- 9
}else if(n.region <= 9){
  par(mfrow=c(3,3))
  hh <- 9
  ww <- 9
}else if(n.region <= 12){
  par(mfrow=c(3,4))
  hh <- 9
  ww <- 12
}else if(n.region <= 16){
  par(mfrow=c(4,4))
  hh <- 12
  ww <- 12
}else if(n.region <= 20){
  par(mfrow=c(5,4))
  hh <- 15
  ww <- 12
}else{
  par(mfrow=c(6,5))
  hh <- 18    
  ww <- 15
}
myP <- colorRampPalette(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"))(n.survey)
pdf(paste0("Figures/yearly/LineSubMedian", countryname2, ".pdf"), width = ww, height = hh)
toplot <- out$results.rw2
toplot$is.HT <- FALSE
toplot$survey <- NA
HT <- out$data.HT
HT <- HT[!is.na(HT$u5m), ]
HT <- data.frame(District = HT$region, 
                 Year = HT$year, 
                 q975 = NA, 
                 q025 = NA,
                 med = HT$u5m, 
                 logit.q975 = NA,
                 logit.q025 = NA,
                 logit.med = NA,
                 is.yearly = FALSE,
                 Year.num = toplot$Year.num[match(HT$year, toplot$Year)], 
                 is.HT = TRUE, 
                 survey = HT$survey.label)
toplot <- rbind(toplot, HT)

my.dodge <- position_dodge(width = 1)
g <- ggplot(aes(x = Year.num, y = med, ymin = q025, ymax = q975, color = survey), data = subset(toplot, (!is.yearly) & (!is.HT) ))
g <- g + geom_vline(xintercept = years.med[exist]+2.5, color="gray50", linetype="longdash")
g <- g + geom_point(data = subset(toplot, is.HT), position = my.dodge)
g <- g + geom_line(data = subset(toplot, (is.yearly) & (!is.HT) ), position = my.dodge, color = "red3", alpha = 0.5)
g <- g + geom_errorbar(size = .7, width = .05, data = subset(toplot, (!is.yearly) & (!is.HT)), color = "red3", alpha = 0.5)
g <- g + geom_point(shape = 17, size = 2, data = subset(toplot, (!is.yearly) & (!is.HT)), color = "red3")

g <- g + facet_wrap(~District)
g <- g + scale_color_manual(values=myP)
g <- g + theme_bw() + scale_x_continuous(breaks=years.med,
        labels=years.all) + xlab("Year") + ylab("U5MR")
g <- g + theme(legend.position="bottom", legend.text=element_text(size=11), axis.text.x = element_text(angle = 45, hjust = 1))
print(g)
dev.off()


