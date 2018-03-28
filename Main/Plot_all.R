library(spdep)
library(maptools)
library(gpclib)
library(plyr)
library(raster)
library(reshape2)
gpclibPermit()

countries <- list.files("../Data/map/")
countries <- countries[countries %in% c("Bangladesh", "Indonesia", "Philippines", "Cambodia",  "AfricanCountries", "World") == FALSE]
countries[3] <- "Angola"
countries[1] <- "Burkina Faso"

# if(countries[1] != "Burkina Faso"){
# 	stop("First country not Burkina Faso")
# }else{
# 	# Read first country, Burkina Faso
# 	info <- read.csv(paste0("../CountryMain/CountryInfo/", countries[1], ".csv"))
# 	final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
# 	geo <- readShapePoly(paste0("../Data/map/", countries[1], "/sdr_subnational_boundaries.shp"))
# 	merge.id <- c("Central/South", "North", "East", "West", "Central/South")
# 	newdata <- data.frame(NAME_final = final_name_ordered)
# 	newdata$NAME_final <- as.character(newdata$NAME_final)
# 	newdata <- cbind(data.frame(geo)[-1, ], newdata)

# 	rownames(newdata) <- final_name_ordered
# 	geo.all <- unionSpatialPolygons(geo, merge.id )
# 	geo.all <- SpatialPolygonsDataFrame(geo.all, newdata)
# 	geo.all$NAME_1 <- geo.all$NAME_final <- paste0(countries[1], ":", geo.all$NAME_final)
# 	geo.all$Country_name <- countries[1]
	

# 	# add all other countries

# 	for(i in 2:length(countries)){
# 		if(!file.exists(paste0("../CountryMain/CountryInfo/", countries[i], ".csv"))){
# 			stop(paste("Missing country information", countries[i]))
# 		}
# 		info <- read.csv(paste0("../CountryMain/CountryInfo/", countries[i], ".csv"))
# 		final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
#     if(countries[i] != "Gambia"){
#   		geo <- readShapePoly(paste0("../Data/map/", countries[i], "/sdr_subnational_boundaries.shp"))
#   		geo$NAME_1 <- geo$REGNAME
#     }else{
#        next
#     }
# 		# country-specific fixes
# 		if(countries[i] == "Tanzania"){
# 		  geo <- geo[geo$NAME_1 != "rest zanzibar",]
# 		  geo <- geo[geo$NAME_1 != "pemba",]
# 		  geo$NAME_1 <- revalue(geo$NAME_1, c("coast"="pwani"))
# 		  geo$NAME_final <- final_name_ordered
# 		}else if(countries[i] == "Indonesia"){
# 		  geo <- geo[geo$NAME_1 != "east timor",]
# 		  geo$NAME_final <- final_name_ordered
# 		}
# 		geo$NAME_final <- final_name_ordered
# 		geo$NAME_final <- paste0(countries[i], ":", geo$NAME_final)
# 		geo$Country_name <- countries[i]

#     geo.all <- rbind(geo.all, geo, makeUniqueIDs = TRUE)
#     cat(".")  
# 	}
# }
# ## add Gambia
# info <- read.csv(paste0("../CountryMain/CountryInfo/Gambia.csv"))
# final_name_ordered <- as.character(info[, 3])[which(as.character(info[, 3]) != "")]
# geo <- readShapePoly(paste0("../Data/map/Gambia/GMB_adm1.shp"))
# geo$NAME_final <- final_name_ordered
# geo$NAME_final <- paste0("Gambia:", geo$NAME_final)
# geo$Country_name <- "Gambia"
# geo.all <- bind(geo.all, geo)
# save(geo.all, file = "../data/Africa_all.rda")

# region_name_only <- unlist(strsplit(geo.all$NAME_final, ":"))
# region_name_only <- region_name_only[2 * (1:(length(region_name_only)/2))]
# country_color <- as.numeric(as.factor(geo.all$Country_name))
# country_color <- rainbow(length(unique(country_color)))[country_color]

# pdf("Figures/Africa_all.pdf", height = 20, width = 20)
# plot(geo.all, 
# 	col = country_color)
# text(coordinates(geo.all), 
# 	labels = region_name_only)
# dev.off()


# ##################################################################
# ##################################################################
library(spdep)
library(maptools)
library(gpclib)
library(plyr)
library(INLA)
library(ggplot2)
library(RColorBrewer)
library(lattice)
library(xlsx)
library(maptools)
library(RColorBrewer)
library(classInt)
library(xtable)
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
    plot.res$District <- paste0(countryname, ":", plot.res$District)

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
    tmp$District <- paste0(countryname, ":", tmp$Region)
    colnames(tmp)[colnames(tmp)=="Median"] <- "med"
    colnames(tmp)[colnames(tmp)=="Lower"] <- "q025"
    colnames(tmp)[colnames(tmp)=="Upper"] <- "q975"
    tmp$country <- countryname
    # plot.res <- merge(plot.res, data, by = "Year")
    plot.res.national <- rbind(plot.res.national, tmp) 
    cat(".")
}
n.region <- length(unique(plot.res.national$District))
n.country <- length(unique(plot.res.national$country))
print(paste0( 
"Number of regions: ", n.region, "   ",
"Number of countries: ", n.country
))

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

plot.res.national <- rbind(plot.res.national, UN)
plot.res.national <- rbind(plot.res.national, IHME)
plot.res.national <- rbind(plot.res.national, UN2)
plot.res.national <- rbind(plot.res.national, IHME2)
plot.res.national$drop <- NULL
for(i in 1:dim(plot.res.national)[1]){
  which <- intersect(which(plot.res.national$Country == plot.res.national$Country[i]), which(plot.res.national$Method == plot.res.national$Method[i]))
  which <- intersect(which, which(plot.res.national$Year == "1990"))
  base <- plot.res.national$med[which]
  plot.res.national$drop[i] <- (base - plot.res.national$med[i])/ base
}



###################################################
# Variance plot
###################################################
# add variance estimates
plot.res.var <- data.frame(matrix(NA, length(countries), 5))
for(CIndex in 1:length(countries)){
    countryname <- countries[CIndex]
    if(!file.exists(paste0("Fitted/", countryname, "-yearly.rda"))){
      print(countries[CIndex])
      next
    }
    load(paste0("Fitted/", countryname, "-yearly.rda"))
    plot.res.var[CIndex, ] <- out$variance.rw2$Proportion
    cat(".")
}
colnames(plot.res.var) <- out$variance.rw2$Name
plot.res.var$country <- countries
plot.res.var$spatial_var <- plot.res.var$ICAR + plot.res.var$region.unstruct

tab <- plot.res.var[, c(6, 1, 2, 4, 3, 5)]
for(j in 2:6){
  tab[, j] <- paste0(round(tab[, j]*100, 1), "%")
}
print(xtable(tab), include.rownames = F, file = "Tables/final_table_var.tex")

geo.all3 <- merge(geo.all, plot.res.var, by.x = "Country_name", by.y = "country")
geo.all3Points <- fortify(geo.all3, region = "NAME_final")
geo.all3Points <- merge(geo.all3Points, geo.all3@data, by = "id", by.y = "NAME_final")

g<- ggplot() 
g <- g + geom_polygon(data = geo.rest, aes(x=long, y=lat, group = group), fill="gray90", color = "gray40")
g0 <- g + geom_polygon(data = geo.all3Points, aes(x=long, y=lat, group = group, fill = spatial_var), color = "gray80") 
g1 <- g + geom_polygon(data = geo.all3Points, aes(x=long, y=lat, group = group, fill = RW2), color = "gray80") 
g2 <- g + geom_polygon(data = geo.all3Points, aes(x=long, y=lat, group = group, fill = ICAR), color = "gray80") 
g3 <- g + geom_polygon(data = geo.all3Points, aes(x=long, y=lat, group = group, fill = time.unstruct), color = "gray80") 
g4 <- g + geom_polygon(data = geo.all3Points, aes(x=long, y=lat, group = group, fill = region.unstruct), color = "gray80") 
g5 <- g + geom_polygon(data = geo.all3Points, aes(x=long, y=lat, group = group, fill = time.area), color = "gray80") 


sc2 <- geom_polygon(data = geo.rest, aes(x=long, y=lat, group = group), fill=NA, color = "gray40")
myPalette <- colorRampPalette((brewer.pal(9, "BuPu")))
sc3 <- theme(legend.position = c(0.1, 0.2), 
          legend.title = element_text(size = 20, face = "bold"), 
          legend.text = element_text(size = 15), 
          legend.key.size = unit(1.5, "cm"),
          axis.text = element_blank(), 
          axis.title = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())  

jpeg(paste0("Figures/Africa_random_space", ".jpeg"), width=1000, height=1100)
g0 <- g0 + sc2 + scale_fill_gradientn(colours = myPalette(20), limits=c(0, 1), labels = scales::percent_format(), name = "Space")  + coord_map() + ylim(-37,37) + theme_bw() + sc3
print(g0)
dev.off() 

jpeg(paste0("Figures/Africa_random_RW2", ".jpeg"), width=1000, height=1100)
g1 <- g1 + sc2 + scale_fill_gradientn(colours = myPalette(20), limits=c(0, 1), labels = scales::percent_format(), name = "RW2")  + coord_map() + ylim(-37,37) + theme_bw() + sc3
print(g1)
dev.off()  

jpeg(paste0("Figures/Africa_random_ICAR", ".jpeg"), width=1000, height=1100)
g2 <- g2 + sc2 + scale_fill_gradientn(colours = myPalette(20), limits=c(0, 1), labels = scales::percent_format(), name = "ICAR")  + coord_map() + ylim(-37,37) + theme_bw() + sc3
print(g2)
dev.off()  

jpeg(paste0("Figures/Africa_random_timeIID", ".jpeg"), width=1000, height=1100)
g3 <- g3 + sc2 + scale_fill_gradientn(colours = myPalette(20), limits=c(0, 1), labels = scales::percent_format(), name = "TimeIID")  + coord_map() + ylim(-37,37) + theme_bw() + sc3
print(g3)
dev.off() 

jpeg(paste0("Figures/Africa_random_spaceIID", ".jpeg"), width=1000, height=1100)
g4 <- g4 + sc2 + scale_fill_gradientn(colours = myPalette(20), limits=c(0, 1), labels = scales::percent_format(), name = "SpaceIID")  + coord_map() + ylim(-37,37) + theme_bw() + sc3
print(g4)
dev.off() 

jpeg(paste0("Figures/Africa_random_spacetime", ".jpeg"), width=1000, height=1100)
g5 <- g5 + sc2 + scale_fill_gradientn(colours = myPalette(20), limits=c(0, 1), labels = scales::percent_format(), name = "Space-time")  + coord_map() + ylim(-37,37) + theme_bw() + sc3
print(g5)
dev.off() 


# Repeat this for 2015 and 2015-2019 

whichYear <- "2015"
whichYear <- "15-19"
gpclibPermit()

########################################################
# MDG drops
########################################################
sub <- plot.res.all[plot.res.all$Year == whichYear, ]
tab <- aggregate(achieve ~ country, sub, sum)
total <- aggregate(District ~ country, plot.res.all, function(x){length(unique(x))})[,2]
tab$ratio <- round(tab$achieve / total * 100, 2)
tab$achieve <- paste0(tab$achieve, "/", total)
tab$ratio <- paste0(tab$ratio, "%")
tab$median <- aggregate(drop ~ country, sub, median)[,2] 
max <- aggregate(drop ~ country, sub, function(x){max(x)})[,2]
min <- aggregate(drop ~ country, sub, function(x){min(x)})[,2]
tab$Range <- paste0("[", format(min, digits=1), ",", format(max, digits=2), "]")
colnames(tab) <- c("Country", "MDG4 achieved", "Percent achieved", "Median deduction", "[Min, Max]")
print(xtable(tab, digits=c(0,0,0,2,3,0)), include.rownames = F, file = paste0("Tables/final_table_mdg", whichYear, ".tex"))

########################################################
# Map plot of MDG drops, subnational and national model
########################################################
sub <- plot.res.all[plot.res.all$Year == whichYear, ]
sub0 <- plot.res.national[plot.res.national$Year == whichYear, ]
sub0 <- sub0[sub0$Method == "RW2", c("Country", "drop")]
colnames(sub0) <- c("country", "drop.national")
sub <- merge(sub, sub0)
sub$District <- as.character(sub$District)

if(whichYear == "2015"){
  types <- 1:4
  sub0 <- plot.res.national[plot.res.national$Year == whichYear, ]
  sub0 <- sub0[sub0$Method %in% c("UN"), c("Country", "drop")]
  colnames(sub0) <- c("country", "drop.UN")
  sub0 <- sub0[sub0$country %in% countries, ]
  sub1 <- plot.res.national[plot.res.national$Year == whichYear, ]
  sub1 <- sub1[sub1$Method %in% c("IHME"), c("Country", "drop")]
  colnames(sub1) <- c("country", "drop.IHME")
  sub1 <- sub1[sub1$country %in% countries, ]
  sub <- merge(sub, sub0)
  sub <- merge(sub, sub1)
}else{
  types <- 1:2
}


ymin <- min(c(sub$drop, sub$drop.national, sub$drop.UN, sub$drop.IHME)) 
ymax <- max(c(sub$drop, sub$drop.national, sub$drop.UN, sub$drop.IHME)) 
ymax <- max(round(ymax, 2)+0.01, 0.9)
ymin <- round(ymin, 2) - 0.01

# check names again
sub$District[sub$District == "Bangladesh:barisal"] <- "Bangladesh:barishal"
sub$District[sub$District == "Bangladesh:rajshahi"] <- "Bangladesh:rajshani"
sub$NAME_final <- sub$District
print(paste0( 
"Number of regions not matching the map: ",
length(sub$District[which(sub$District %in% geo.all$NAME_final == F)])
))

# merge with map
geo.all2 <- merge(geo.all, sub, by.x = "NAME_final", by.y = "NAME_final")
geo.all2Points <- fortify(geo.all2, region = "NAME_final")
geo.all2Points <- merge(geo.all2Points, geo.all2@data, by = "id", by.y = "NAME_final")

# add central location for country name annotation
tmp <- by(geo.all2Points, geo.all2Points$Country_name, function(x) {Polygon(x[c('long', 'lat')])@labpt})
centroids <- setNames(do.call("rbind.data.frame", tmp), c('long', 'lat')) 
rownames(centroids) <- names(tmp)
centroids$label <- geo.all2Points$Country_name[match(rownames(centroids), geo.all2Points$Country_name)]

for(type in types){
  if(type == 1){
    geo.all2Points$toplot <- geo.all2Points$drop
  }else if(type == 2){
    geo.all2Points$toplot <- geo.all2Points$drop.national
  }else if(type == 3){
    geo.all2Points$toplot <- geo.all2Points$drop.UN
  }else if(type == 4){
    geo.all2Points$toplot <- geo.all2Points$drop.IHME
  }
  g<- ggplot() 
  g <- g + geom_polygon(data = geo.rest, aes(x=long, y=lat, group = group), fill="gray90", color = "gray40")
  g <- g + geom_polygon(data = geo.all2Points, aes(x=long, y=lat, group = group, fill = toplot), color = "gray80")
  g <- g + geom_polygon(data = geo.rest, aes(x=long, y=lat, group = group), fill=NA, color = "gray40")
  g <- g + coord_map() + ylim(-37,37) + theme_bw() + theme(legend.position = c(0.1, 0.2), 
            legend.title = element_text(size = 20, face = "bold"), 
            legend.text = element_text(size = 15), 
            legend.key.size = unit(1.5, "cm"),
            axis.text = element_blank(), 
            axis.title = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) 
  g <- g #+ggtitle(title) + theme(plot.title = element_text(size=33))
  min <- ymin 
  max <- ymax
  breaks <- c(min, 0, 0.33, 0.67, 0.9)
  
  ratio <- ((2/3 - min) / (max - min))
  mult <- (max - min) * 300
  base_l <- colorRampPalette(brewer.pal(9,"RdYlBu"))(ratio*mult*2) # blue2red(ratio * mult * 2)
  base_u <- colorRampPalette(brewer.pal(9,"RdYlBu"))((1-ratio)*mult*2) # blue2red((1-ratio) * mult * 2)
  basecolor2 <- c(base_l[1:(ratio * mult)], 
                base_u[-(1:((1 - ratio) * mult))])
  # image(matrix(1:length(basecolor2)), col = basecolor2)
  sc <- scale_fill_gradientn(colors = basecolor2, limit = c(min, max), breaks = breaks, name = "Deduction")
  
  
  if(type == 1){
    pre <- "Africa" 
  }else if(type == 2){
    pre = "Africa_national"
  }else if(type == 3){
    pre = "Africa_UN"
  }else{
    pre = "Africa_IHME"
  }
  jpeg(paste0("Figures/", pre, "_reduction_", whichYear, ".jpeg"), width=1000, height=1100)
  g <- g + sc
  print(g)
  dev.off() 
  # jpeg(paste0("Figures/", pre, "_reduction_", post, whichYear, "_label.jpeg"), width=1000, height=1100)
  # g <- g + with(centroids, annotate(geom="text", x = long, y=lat, label = label, size = 4))    
  # print(g)
  # dev.off() 
}
 
###################################################
#  Projection
###################################################
sub <- plot.res.all[plot.res.all$Year == whichYear, ]
sub0 <- plot.res.national[plot.res.national$Year == whichYear, ]
sub0a <- sub0[sub0$Method == "RW2", c("Country", "med")]
colnames(sub0a) <- c("country", "med.national")
sub <- merge(sub, sub0a)

if(whichYear == "2015"){
  sub0 <- plot.res.national[plot.res.national$Year == "2015", ]
  sub0b <- sub0[sub0$Method == "UN", c("Country", "med")]
  colnames(sub0b) <- c("country", "med.UN")
  sub0c <- sub0[sub0$Method == "IHME", c("Country", "med")]
  colnames(sub0c) <- c("country", "med.IHME")
  sub <- merge(sub, sub0b)
  sub <- merge(sub, sub0c)
  types <- 1:4
}else{
  types <- 1:2
}

ymin <- min(c(sub$med, sub$med.national, sub$med.UN, sub$med.IHME)) 
ymax <- max(c(sub$med, sub$med.national, sub$med.UN, sub$med.IHME))  
ymax <- round(ymax, 2) + 0.01 
ymin <- round(ymin, 2) - 0.01
sub$District <- as.character(sub$District)

# check names again
sub$District[sub$District == "Bangladesh:barisal"] <- "Bangladesh:barishal"
sub$District[sub$District == "Bangladesh:rajshahi"] <- "Bangladesh:rajshani"
sub$NAME_final <- sub$District
print(paste0( 
"Number of regions not matching the map: ",
length(sub$District[which(sub$District %in% geo.all$NAME_final == F)])
))

# merge with map
geo.all2 <- merge(geo.all, sub, by.x = "NAME_final", by.y = "NAME_final")
geo.all2Points <- fortify(geo.all2, region = "NAME_final")
geo.all2Points <- merge(geo.all2Points, geo.all2@data, by = "id", by.y = "NAME_final")

# add central location for country name annotation
tmp <- by(geo.all2Points, geo.all2Points$Country_name, function(x) {Polygon(x[c('long', 'lat')])@labpt})
centroids <- setNames(do.call("rbind.data.frame", tmp), c('long', 'lat')) 
rownames(centroids) <- names(tmp)
centroids$label <- geo.all2Points$Country_name[match(rownames(centroids), geo.all2Points$Country_name)]

for(type in types){
  if(type == 1){
    geo.all2Points$toplot <- geo.all2Points$med
  }else if(type == 2){
    geo.all2Points$toplot <- geo.all2Points$med.national
  }else if(type == 3){
    geo.all2Points$toplot <- geo.all2Points$med.UN
  }else if(type == 4){
    geo.all2Points$toplot <- geo.all2Points$med.IHME
  }
  g<- ggplot() 
  g <- g + geom_polygon(data = geo.rest, aes(x=long, y=lat, group = group), fill="gray90", color = "gray40")
  g <- g + geom_polygon(data = geo.all2Points, aes(x=long, y=lat, group = group, fill = toplot), color = "gray80")
  g <- g + geom_polygon(data = geo.rest, aes(x=long, y=lat, group = group), fill=NA, color = "gray40")
  g <- g + coord_map() + ylim(-37,37) + theme_bw() +
      theme(legend.position = c(0.1, 0.2), 
            legend.title = element_text(size = 20, face = "bold"), 
            legend.text = element_text(size = 15), 
            legend.key.size = unit(1.5, "cm"),
            axis.text = element_blank(), 
            axis.title = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  g <- g #+ggtitle(title) + theme(plot.title = element_text(size=33))    
  # g <- g +  with(centroids, annotate(geom="text", x = long, y=lat, label = label, size = 4))    
  min <- ymin
  max <- ymax 
  myPalette <- colorRampPalette((brewer.pal(9, "YlGn")))
    if(type == 1){
    pre <- "Africa" 
  }else if(type == 2){
    pre = "Africa_national"
  }else if(type == 3){
    pre = "Africa_UN"
  }else{
    pre = "Africa_IHME"
  }
  jpeg(paste0("Figures/", pre, "_u5mr_", whichYear, ".jpeg"), width=1000, height=1100)
   g <- g + scale_fill_gradientn(colours = myPalette(20), limits=c(min, max), labels = scales::percent_format(), name = "U5MR")
  print(g)
  dev.off() 
}





 


# ###################################################
# # Relative risk plot
# ###################################################

# source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")
# med.palette <- brewer.pal(n = 9, name = "Purples")
# med.int <- classIntervals(round(plot.res.all$rr, 3),
#                           n = 9, style = 'jenks')
# # med.int.HT <- classIntervals(round(plot.res.all$rr, 3),
# #                           n = 9, style = 'jenks')
# med.col <- findColours(med.int, med.palette)
# # med.col.HT <- findColours(med.int.HT, med.palette)
# plot.res.all$med.col <- med.col
# # plot.res.all$med.col.HT <- med.col.HT
# n.area <- dim(geo.all)[1]
# areas.smooth <- geo.all$NAME_final

# years <- c("80-84", "85-89", "90-94", "95-99", "00-04", "05-09", "10-14", "15-19")
# pre <- "Asia"
# if(is.Africa) pre <- "Africa" 
# jpeg(paste0("../Figures/rr_", pre, ".jpeg"), width=9000, height=6200)
# m <- matrix(c(1:8, 9,9,9,9),nrow = 3,ncol = 4,byrow = TRUE)
# layout(mat = m, heights = c(1.1,1.1,0.1))
# for(i in 1:length(years)){
#   year <- years[i]
#     # par(mai = c(1, 0.1,0.3,0.1), oma = c(0.5, 0.1, 0.1, 0.1))
#     tmp <- plot.res.all[plot.res.all$Year == years[i],]
#     tmp.col <- rep(NA, n.area)
#     # tmp2.col <- rep(NA, n.area)
#     for(j in 1:n.area){
#       if(areas.smooth[j] %in% tmp$District){
#         tmp.col[j] <- tmp$med.col[tmp$District == areas.smooth[j]]
#         # tmp2.col[j] <- tmp$med.col.HT[tmp$District == areas.smooth[j]]
#       }else{
#         tmp.col[j] <- NA
#         # tmp2.col[j] <- NA
#       }
#     }
    
#     plot(geo.all, col = tmp.col, border = FALSE,
#          main  = paste("", years[i]), cex.main=15, line = -20)
#     plot(geo.rest,add=T, border = "gray60", lwd=8)
#     # plot(geo.all, col = tmp2.col, border = FALSE,
#     #      main  = paste("Direct estimator:", years[i]), cex.main=3.5)
#     # plot(geo.rest,add=T, border = "gray60")
# }
# plot(1, type = "n", axes=FALSE, xlab="", ylab="")
# legend(x = "center",inset = 0,
#    legend = names(attr(med.col, 'table')), 
#    fill = med.palette, cex= 12, horiz = TRUE, bty = 'n')
# dev.off()

