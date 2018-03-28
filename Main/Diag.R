##################################################
## Code to perform diagnostics on the RW2 model
##################################################
library(ggplot2)
library(gridExtra)
mod <- inla_model.yearly[[4]] 
years.med <- c(1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017)
years.single <- year_range[1] : year_range[2]
countryname2 <- gsub(" ", "", countryname)

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


print("Start diagnostics")
gout <- NULL

# Fixed effect
# beta <- mod$fit$summary.fixed[2, 1]
# print(round(mod$fit$summary.fixed[2, ], 4))

# AR2
tmp <- mod$fit$marginals.random$time.struct
quants <- data.frame(matrix(NA, length(tmp), 3))
for(i in 1:length(tmp)){
	quants[i, ] <- inla.qmarginal(c(0.025, 0.5, 0.975), tmp[[i]])
}
# add in the fixed effect
# fixed <- mod$newdata[match(1:length(tmp), mod$newdata$time.unstruct), "time.fix"]
# quants <- quants + beta * fixed

quants$Year <- c(years.single, years.med+0.2)
quants$Period <- quants$Year %in% (years.med+0.2)
quants$Project <- quants$Year > (years.med[length(years)]+2)
g <- ggplot(aes(x=Year, y=X2, ymin=X1, ymax=X3, color=Period, linetype=Project), data = quants)
g <- g + geom_point(data=subset(quants, Period==F)) + geom_errorbar(data=subset(quants, Period==F))
g <- g + geom_point(data=subset(quants, Period==T), size=2) + geom_errorbar(data=subset(quants, Period==T),width=1.5,size=1.1)
g <- g + ylab("Trend + Random effect")+ ggtitle("Trend + RW2 aggregate")
gout[[1]] <- g

# Time IID
tmp <- mod$fit$marginals.random$time.unstruct
quants <- data.frame(matrix(NA, length(tmp), 3))
for(i in 1:length(tmp)){
	quants[i, ] <- inla.qmarginal(c(0.025, 0.5, 0.975), tmp[[i]])
}
quants$Year <- c(years.single, years.med+0.2)
quants$Period <- quants$Year %in% (years.med+0.2)
quants$Project <- quants$Year > (years.med[length(years)]+2)
g <- ggplot(aes(x=Year, y=X2, ymin=X1, ymax=X3, color=Period, linetype=Project), data = quants)
g <- g + geom_point(data=subset(quants, Period==F)) + geom_errorbar(data=subset(quants, Period==F))
g <- g + geom_point(data=subset(quants, Period==T), size=2) + geom_errorbar(data=subset(quants, Period==T),width=1.5,size=1.1)
g <- g + ylab("Random effect")+ ggtitle("Time IID aggregate")
gout[[2]] <- g


# ICAR
tmp <- mod$fit$marginals.random$region.struct
quants <- data.frame(matrix(NA, length(tmp), 1))
for(i in 1:length(tmp)){
  quants[i, ] <- inla.qmarginal(c(0.5), tmp[[i]])
}
colnames(quants) <- "median"
quants$id <- final_name_ordered
geof <- fortify(geo, region="NAME_final")
geof <- join(geof, quants, by="id")
g <- ggplot() + geom_polygon(data = geof, aes(x=long, y = lat, fill = median, group = group)) 
g <- g + ggtitle("Space ICAR")
gout[[3]] <- g

# Space IID
tmp <- mod$fit$marginals.random$region.unstruct
quants <- data.frame(matrix(NA, length(tmp), 1))
for(i in 1:length(tmp)){
  quants[i, ] <- inla.qmarginal(c(0.5), tmp[[i]])
}
colnames(quants) <- "median"
quants$id <- final_name_ordered
geof <- fortify(geo, region="NAME_final")
geof <- join(geof, quants, by="id")
g <- ggplot() + geom_polygon(data = geof, aes(x=long, y = lat, fill = median, group = group)) 
g <- g + ggtitle("Space IID")
gout[[4]] <- g

# Time-Space interaction
tmp <- mod$fit$marginals.random$time.area
is.yearly <- which(mod$time.area$time.unstruct %in% (1:length(years.single)))
is.period <- which(mod$time.area$time.unstruct %in% (1:length(years.single)) == F)
quants1 <- data.frame(matrix(NA, nrow = length(is.yearly), ncol = 3))
for(i in 1:length(is.yearly)){
  quants1[i,] <- inla.qmarginal(c(0.025, 0.5, 0.975), tmp[[is.yearly[i]]])  
}
quants2 <- data.frame(matrix(NA, nrow = length(is.period), ncol = 3))
for(i in 1:length(is.period)){
  quants2[i,] <- inla.qmarginal(c(0.025, 0.5, 0.975), tmp[[is.period[i]]])  
}

quants1$Year <- rep(years.single, length(final_name_ordered))
quants2$Year <- rep(years.med+0.2, length(final_name_ordered))
quants1$Region <- rep(final_name_ordered, each=length(years.single))
quants2$Region <- rep(final_name_ordered, each=length(years.med))
quants <- rbind(quants1, quants2)

quants$Yearlabel <- c(years.single, years.all)[match(quants$Year, c(years.single, years.med+0.2))]
quants$Period <- quants$Year %in% (years.med+0.2)
quants$Project <- quants$Year > (years.med[length(years)]+2)


if(dim(mod$fit$summary.fixed)[1] > 2){
	fixed.area <- mod$fit$summary.fixed[-c(1,2), 1]
	quants3 <- data.frame(region.num = 1:length(fixed.area), 
		fixed.area = fixed.area)
	quants3$Region <- mod$newdata$region[match(quants3$region.num, mod$newdata$region_number)]
	quants <- merge(quants, quants3)
	quants$year.num <- mod$newdata$time.fix[match(quants$Yearlabel, mod$newdata$years)]
	quants$X1 <- quants$X1 + quants$fixed.area * quants$year.num
	quants$X2 <- quants$X2 + quants$fixed.area * quants$year.num
	quants$X3 <- quants$X3 + quants$fixed.area * quants$year.num
}


# for(i in 1:length(final_name_ordered)){
g <- ggplot(aes(x=Year, y=X2, ymin=X1, ymax=X3, color=Period, linetype=Project), data = quants)
g <- g + geom_point(data=subset(quants, Period==F)) + geom_errorbar(data=subset(quants, Period==F))
g <- g + geom_point(data=subset(quants, Period==T), size=2) + geom_errorbar(data=subset(quants, Period==T),width=1.5,size=1.1)
g <- g + ylab("Trend + Random effect")+ ggtitle("Space-Time Type IV Interaction aggregate")
g <- g + facet_wrap(~Region)
gout[[5]] <- g

quants$id <- quants$Region
quants$median <- quants$X2
geof <- fortify(geo, region="NAME_final")
geof <- join(geof, quants, by="id")
geof$Yearlabel <- factor(geof$Yearlabel, levels=c(years.all, years.single))

g <- ggplot() + geom_polygon(data = subset(geof, Yearlabel %in% years.all), aes(x=long, y = lat, fill = median, group = group)) 
g <- g + facet_wrap(~Yearlabel)
g <- g + ggtitle("Period Space-Time Type IV Interaction aggregate + Regional Trends")
gout[[6]] <- g

g <- ggplot() + geom_polygon(data = subset(geof, Yearlabel %in% years.all==FALSE), aes(x=long, y = lat, fill = median, group = group)) 
g <- g + facet_wrap(~Yearlabel)
g <- g + ggtitle("Yearly Space-Time Type IV Interaction aggregate + Regional Trends")
gout[[7]] <- g



###############################################
## Table: Variance decomposition
###############################################
periods <- years
n.periods <- length(years)
n.area <- length(final_name_ordered)

# beta <- mod$fit$summary.fixed[2, 1]
# beta_s <- mod$fit$summary.fixed[3:(n.area+2), 1]
# tvec <- (mod$newdata[!is.na(mod$newdata[, "logit.est"]), "time.fix"])
# svec <- (mod$newdata[!is.na(mod$newdata[, "logit.est"]), "region_number"])
# svec <- beta + beta_s[svec]
# var.fx <-  var(svec * tvec)

marg.icar <- matrix(NA, nrow = n.area, ncol = 100000)
icars <- mod$fit$marginals.random$region.struct
for(i in 1:n.area){
  marg.icar[i, ] <- inla.rmarginal(100000, icars[[i]])
}
vars.icar <- apply(marg.icar, 2, var)
var.icar <- median(vars.icar)

rws <- mod$fit$marginals.random$time.struct
yearlocation <- (length(years.all) * 5 + 1) : (length(years.all) * 5 + length(years))
# yearlocation <- 1 : (n.periods * 5 )  
marg.rw <- matrix(NA, nrow = length(yearlocation), ncol = 100000)
index <- 1
for(i in yearlocation){
  marg.rw[index,] <- inla.rmarginal(100000, rws[[i]])  
  index <- index + 1
}
vars.rw <- apply(marg.rw, 2, var)
var.rw <- median(vars.rw)

utimes <- mod$fit$marginals.random$time.unstruct
marg <- matrix(NA, nrow = length(yearlocation), ncol = 100000)
index <- 1
for(i in yearlocation){
  marg[index,] <- inla.rmarginal(100000, utimes[[i]])  
  index <- index + 1
}
var.utimes <- median(apply(marg, 2, var))

uregions <- mod$fit$marginals.random$region.unstruct
marg <- matrix(NA, nrow = length(uregions), ncol = 100000)
for(i in 1:length(uregions)){
  marg[i,] <- inla.rmarginal(100000, uregions[[i]])  
}
var.uregions<- median(apply(marg, 2, var))

utimeregions <- mod$fit$marginals.random$time.area
yearlocation2 <- which(mod$time.area$time.unstruct %in% yearlocation)
marg <- matrix(NA, nrow = length(yearlocation2), ncol = 100000)
for(i in 1:length(yearlocation2)){
  marg[i,] <- inla.rmarginal(100000, utimeregions[[yearlocation2[i]]])  
}
var.utimeregions <- median(apply(marg, 2, var))

# others <- 1/summary(mod$fit)$hyperpar$`0.5quant`[-c(2,3)]
# names <- summary(mod$fit)$random.names[-c(2,3)]
all.vars <- c(var.rw, var.icar, var.utimes, var.uregions, var.utimeregions)
names <- c("RW2", "ICAR", "time.unstruct", "region.unstruct", "time.area")

prop <- all.vars/sum(all.vars)
variance <- data.frame(
  Median = all.vars,
  Proportion = prop, 
  Name = factor(names, levels=rev(names)))
rownames(variance) <- names
g <- ggplot(data = variance, aes(x = Name, y = Median))
g <- g + geom_bar(stat="identity") + coord_flip() 
g <- g + ggtitle("Total variance explained by each component")
gout[[8]] <- g

pdf(paste0("Figures/yearly/Diagnostics-", countryname2, ".pdf"), width = 8, height = 28)
grid.arrange(grobs=gout, ncol = 2, layout_matrix = matrix(c(1,2,3,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8), byrow=T, ncol=2))
dev.off()


# update variance
load(paste0("Fitted/", countryname, "-yearly.rda"))
out$variance.rw2 <- variance
save(out, file = paste0("Fitted/", countryname, "-yearly.rda"))



