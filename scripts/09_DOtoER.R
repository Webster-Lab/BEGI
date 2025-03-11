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


#SLOW#

SLOW_ER_results <- numeric(length(roc_all[["SLOW_rates"]])) 
names(SLOW_ER_results) <- names(roc_all[["SLOW_rates"]])

for (i in seq_along(roc_all[["SLOW_rates"]])) {
  SLOW_ER_results[i] <- AUC(
    x = as.numeric(roc_all[["SLOW_rates"]][[i]]$datetimeMT)[-1],
    y = roc_all[["SLOW_rates"]][[i]]$ER[-1],
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(SLOW_ER_results)
#Need to fix code to account for NA values that are not in the first position of each dataframe

#VDOW#

VDOW_ER_results <- numeric(length(roc_all[["VDOW_rates"]])) 
names(VDOW_ER_results) <- names(roc_all[["VDOW_rates"]])

for (i in seq_along(roc_all[["VDOW_rates"]])) {
  VDOW_ER_results[i] <- AUC(
    x = as.numeric(roc_all[["VDOW_rates"]][[i]]$datetimeMT)[-1],
    y = roc_all[["VDOW_rates"]][[i]]$ER[-1],
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(VDOW_ER_results)


#VDOS#

VDOS_ER_results <- numeric(length(roc_all[["VDOS_rates"]])) 
names(VDOS_ER_results) <- names(roc_all[["VDOS_rates"]])

for (i in seq_along(roc_all[["VDOS_rates"]])) {
  VDOS_ER_results[i] <- AUC(
    x = as.numeric(roc_all[["VDOS_rates"]][[i]]$datetimeMT)[-1],
    y = roc_all[["VDOS_rates"]][[i]]$ER[-1],
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(VDOS_ER_results)


#### Boxplot of ER ####

ER_AUC<-data.frame(ER=c(SLOC_ER_results,SLOW_ER_results,VDOW_ER_results,VDOS_ER_results),
                   Well=rep(c("SLOC","SLOW","VDOW","VDOS"),
                            times=c(length(SLOC_ER_results),length(SLOW_ER_results),length(VDOW_ER_results),length(VDOS_ER_results))))
ER_AUC$Event<-c('SLOC_DO1','SLOC_DO2','SLOC_DO3','SLOC_DO4','SLOC_DO5','SLOC_DO6','SLOC_DO7','SLOC_DO8','SLOC_DO9','SLOC_DO10',
                'SLOC_DO11','SLOC_DO12','SLOC_DO13','SLOC_DO14','SLOC_DO15','SLOC_DO16','SLOC_DO17','SLOC_DO18','SLOC_DO19','SLOC_DO20',
                'SLOC_DO21','SLOW_DO1','SLOW_DO2','SLOW_DO3','SLOW_DO4','SLOW_DO5','SLOW_DO6','SLOW_DO7','SLOW_DO8','SLOW_DO9','SLOW_DO10',
                'VDOW_DO1','VDOW_DO2','VDOW_DO3','VDOW_DO4','VDOW_DO5','VDOW_DO6','VDOW_DO7','VDOW_DO8','VDOW_DO9','VDOW_DO10',
                'VDOW_DO11','VDOW_DO12','VDOW_DO13','VDOW_DO14','VDOW_DO15','VDOW_DO16','VDOW_DO17','VDOW_DO18',
                'VDOS_DO1','VDOS_DO2','VDOS_DO3','VDOS_DO4','VDOS_DO5','VDOS_DO6','VDOS_DO7','VDOS_DO8','VDOS_DO9','VDOS_DO10')
#ER_AUC$Eventdate <-c(SLOC_dates,SLOW_dates,VDOW_dates,VDOS_dates) 

ER_AUC_bp<-ggplot(data=ER_AUC,mapping=aes(x=Well, y=ER))+geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+labs(y = "Net Ecosystem Respiration (g O2/m^2)")
print(ER_AUC_bp)




