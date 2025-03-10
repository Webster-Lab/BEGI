#### read me ####
# The purpose of this script is to convert the rate of change of dissolved oxygen for each event to the rate of change of ecosystem respiration using Odum's equation
# ER = D - dDO/dt in g O2/m^2/15 min

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)

#### Import rate of change RDS file ####
roc_all = readRDS("EXO_compiled/roc_all.rds")

#### Test conversion DO to ER ####
#for one event, D is assumed to be fixed
#IMPORTANT: I am testing this with a D of 0 but I need to look further into what would be a reasonable D
D<-0
#roc_all[["SLOC_rates"]][["SLOC_DO1"]]$rate_of_change
#convert rate of change from mg to g
roc_all[["SLOC_rates"]][["SLOC_DO1"]]$rate_of_change_g <- roc_all[["SLOC_rates"]][["SLOC_DO1"]]$rate_of_change/1000

#create function to convert DO to ER
do2er <- function(DO_roc){
  D - DO_roc
}
#add new column using function
roc_all[["SLOC_rates"]][["SLOC_DO1"]]$ER <- do2er(roc_all[["SLOC_rates"]][["SLOC_DO1"]]$rate_of_change_g)

#plot ER v time to check
plot(roc_all[["SLOC_rates"]][["SLOC_DO1"]]$datetimeMT,roc_all[["SLOC_rates"]][["SLOC_DO1"]]$ER,
     xlab = "date time",
     ylab = "ER (g O2/m^2/15 min)",
     type="l")
abline(h=0,col="blue")

#plot DO rate v time to compare
plot(roc_all[["SLOC_rates"]][["SLOC_DO1"]]$datetimeMT,roc_all[["SLOC_rates"]][["SLOC_DO1"]]$rate_of_change_g,
     xlab = "date time",
     ylab = "DO (g O2/m^2/15 min)",
     type="l")
abline(h=0,col="blue")

#### Convert DO to ER for all events ####
#Define D
D<-0

#Define function to convert DO to ER
do2er <- function(DO_roc){
  D - DO_roc
}

#create for loop for each well
#SLOC#
for (name in names(roc_all[["SLOC_rates"]])){
  #convert rate of change from mg to g
  roc_all[["SLOC_rates"]][[name]]$rate_of_change_g <- 
    roc_all[["SLOC_rates"]][[name]]$rate_of_change / 1000
  
  #convert DO roc to ER roc
  roc_all[["SLOC_rates"]][[name]]$ER <- 
    do2er(roc_all[["SLOC_rates"]][[name]]$rate_of_change_g)
}

#SLOW#
for (name in names(roc_all[["SLOW_rates"]])){
  #convert rate of change from mg to g
  roc_all[["SLOW_rates"]][[name]]$rate_of_change_g <- 
    roc_all[["SLOW_rates"]][[name]]$rate_of_change / 1000
  
  #convert DO roc to ER roc
  roc_all[["SLOW_rates"]][[name]]$ER <- 
    do2er(roc_all[["SLOW_rates"]][[name]]$rate_of_change_g)
}

#VDOW#
for (name in names(roc_all[["VDOW_rates"]])){
  #convert rate of change from mg to g
  roc_all[["VDOW_rates"]][[name]]$rate_of_change_g <- 
    roc_all[["VDOW_rates"]][[name]]$rate_of_change / 1000
  
  #convert DO roc to ER roc
  roc_all[["VDOW_rates"]][[name]]$ER <- 
    do2er(roc_all[["VDOW_rates"]][[name]]$rate_of_change_g)
}

#VDOS#
for (name in names(roc_all[["VDOS_rates"]])){
  #convert rate of change from mg to g
  roc_all[["VDOS_rates"]][[name]]$rate_of_change_g <- 
    roc_all[["VDOS_rates"]][[name]]$rate_of_change / 1000
  
  #convert DO roc to ER roc
  roc_all[["VDOS_rates"]][[name]]$ER <- 
    do2er(roc_all[["VDOS_rates"]][[name]]$rate_of_change_g)
}

#### Plot to check ####
#plot ER v time to check
#plot(roc_all[["SLOC_rates"]][["SLOC_DO2"]]$datetimeMT,roc_all[["SLOC_rates"]][["SLOC_DO2"]]$ER,
#     xlab = "date time",
#     ylab = "ER (g O2/m^2/15 min)",
#     type="l")
#abline(h=0,col="blue")

#### Integrate under ER curve ####
#AUC(x = (as.numeric(roc_all[["SLOC_rates"]][["SLOC_DO1"]]$datetimeMT)[-1]),
#    y = roc_all[["SLOC_rates"]][["SLOC_DO1"]]$ER[-1],
#    method = "trapezoid",
#    na.rm = FALSE)
# 0.05809091

#SLOC#

SLOC_ER_results <- numeric(length(roc_all[["SLOC_rates"]])) 
names(SLOC_ER_results) <- names(roc_all[["SLOC_rates"]])

for (i in seq_along(roc_all[["SLOC_rates"]])) {
  SLOC_ER_results[i] <- AUC(
    x = as.numeric(roc_all[["SLOC_rates"]][[i]]$datetimeMT)[-1],
    y = roc_all[["SLOC_rates"]][[i]]$ER[-1],
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(SLOC_ER_results)
#shouldn't be getting negative values with AUC? 




