## srun --pty --partition=short --time=1:00:00 --mem-per-cpu=2500 /bin/bash

setwd("../")
countries <- list.files("../Data/map/")
countries <- countries[countries %in% c("AfricanCountries", "World", "Bangladesh", "Cambodia", "Indonesia", "Philippines") == FALSE]
countryname <- countries[as.numeric(commandArgs(trailingOnly = TRUE)[1])]
print(countryname)


# start_time <- Sys.time()
# source("Run.R")
# end_time <- Sys.time()
# t1 <- end_time - start_time
# print(end_time - start_time)

# start_time <- Sys.time()
# source("Diag.R")
# end_time <- Sys.time()
# t2 <- end_time - start_time
# print(end_time - start_time)

# start_time <- Sys.time()
# source("Plot.R")
# end_time <- Sys.time()
# t3 <- end_time - start_time
# print(end_time - start_time)

start_time <- Sys.time()
source("CrossValidation.R")
end_time <- Sys.time()
t4 <- end_time - start_time
print(end_time - start_time)

# time <- c(t1, t2, t3, t4)
# names(time) <- c("Fit", "Diag", "Plot", "CV")
# write.csv(time, file = paste0("runscripts/time/", countryname, ".csv"))


# Angola: 
# fit=17.45585min Diag=4.8961min, Plot=35.91sec, CV=?
# Tanzania: 
# fit=16.7186min Diag=1.1794min, Plot=19.4654sec, CV=7.601982hr

