## srun --pty --partition=build --time=1:00:00 --mem-per-cpu=2500 /bin/bash
## module load R
## R
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("classInt")
install.packages("gdata")
install.packages("foreign")
install.packages("spdep")
install.packages("maptools")
install.packages("gpclib")
install.packages("plyr")
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
install.packages("lattice")
install.packages("gridExtra")


library(ggplot2)
library(RColorBrewer)
library(classInt)
library(gdata)
library(foreign)
library(spdep)
library(maptools)
library(gpclib)
library(plyr)
library(lattice)
library(gridExtra)
library(INLA)
