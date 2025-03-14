#### read me ####
# The purpose of this script is to convert the rate of change of dissolved oxygen for each event to the rate of change of ecosystem respiration using Odum's equation
# The first conversion method used was a first attempt and is not accurate.
# Q = D - R where Q is the integral of the rate of change of DO, D is the POSITIVE integral of the rate of change of DO, and R is ER

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

#### (IGNORE) Convert DO to ER and plot with DO = -ER ####
#IMPORTANT This method was my first attempt at estimating ER and is no longer accurate.
#See next section for new method
#Define D
#D<-0

#Define function to convert DO to ER
#do2er <- function(DO_roc){
 # D - DO_roc
#}

#create for loop for each well
#SLOC#
#for (name in names(roc_all[["SLOC_rates"]])){
  #convert rate of change from mg to g
 # roc_all[["SLOC_rates"]][[name]]$rate_of_change_g <- 
  #  roc_all[["SLOC_rates"]][[name]]$rate_of_change / 1000
  
  #convert DO roc to ER roc
  #roc_all[["SLOC_rates"]][[name]]$ER <- 
   # do2er(roc_all[["SLOC_rates"]][[name]]$rate_of_change_g)
#}

#SLOW#
#for (name in names(roc_all[["SLOW_rates"]])){
  #convert rate of change from mg to g
 # roc_all[["SLOW_rates"]][[name]]$rate_of_change_g <- 
  #  roc_all[["SLOW_rates"]][[name]]$rate_of_change / 1000
  
  #convert DO roc to ER roc
#  roc_all[["SLOW_rates"]][[name]]$ER <- 
 #   do2er(roc_all[["SLOW_rates"]][[name]]$rate_of_change_g)
#}

#VDOW#
#for (name in names(roc_all[["VDOW_rates"]])){
  #convert rate of change from mg to g
 # roc_all[["VDOW_rates"]][[name]]$rate_of_change_g <- 
  #  roc_all[["VDOW_rates"]][[name]]$rate_of_change / 1000
  
  #convert DO roc to ER roc
#  roc_all[["VDOW_rates"]][[name]]$ER <- 
 #   do2er(roc_all[["VDOW_rates"]][[name]]$rate_of_change_g)
#}

#VDOS#
#for (name in names(roc_all[["VDOS_rates"]])){
  #convert rate of change from mg to g
 # roc_all[["VDOS_rates"]][[name]]$rate_of_change_g <- 
  #  roc_all[["VDOS_rates"]][[name]]$rate_of_change / 1000
  
  #convert DO roc to ER roc
#  roc_all[["VDOS_rates"]][[name]]$ER <- 
 #   do2er(roc_all[["VDOS_rates"]][[name]]$rate_of_change_g)
#}

### Plot to check ###
#plot ER v time to check (SLOC_DO4: 10/16)
#plot(roc_all[["SLOC_rates"]][["SLOC_DO4"]]$datetimeMT,roc_all[["SLOC_rates"]][["SLOC_DO4"]]$ER,
  #   xlab = "date time",
   #  ylab = "ER (g O2/m^2/15 min)",
    # type="l")
#abline(h=0,col="blue")
#plot DO rate v time to compare
#plot(roc_all[["SLOC_rates"]][["SLOC_DO4"]]$datetimeMT,roc_all[["SLOC_rates"]][["SLOC_DO4"]]$rate_of_change_g,
 #    xlab = "date time",
  #   ylab = "DO (g O2/m^2/15 min)",
   #  type="l")
#abline(h=0,col="blue")

#plot ER v time to check (SLOC_DO5: 10/17-18)
#plot(roc_all[["SLOC_rates"]][["SLOC_DO5"]]$datetimeMT,roc_all[["SLOC_rates"]][["SLOC_DO5"]]$ER,
 #    xlab = "date time",
  #   ylab = "ER (g O2/m^2/15 min)",
   #  type="l")
#abline(h=0,col="blue")
#plot DO rate v time to compare
#plot(roc_all[["SLOC_rates"]][["SLOC_DO5"]]$datetimeMT,roc_all[["SLOC_rates"]][["SLOC_DO5"]]$rate_of_change_g,
 #    xlab = "date time",
  #   ylab = "DO (g O2/m^2/15 min)",
   #  type="l")
#abline(h=0,col="blue")


### Integrate under ER curve ###
#AUC(x = (as.numeric(roc_all[["SLOC_rates"]][["SLOC_DO1"]]$datetimeMT)[-1]),
#    y = roc_all[["SLOC_rates"]][["SLOC_DO1"]]$ER[-1],
#    method = "trapezoid",
#    na.rm = FALSE)
# 0.05809091

#SLOC#

#SLOC_ER_results <- numeric(length(roc_all[["SLOC_rates"]])) 
#names(SLOC_ER_results) <- names(roc_all[["SLOC_rates"]])

#for (i in seq_along(roc_all[["SLOC_rates"]])) {
 # x_vals <- as.numeric(roc_all[["SLOC_rates"]][[i]]$datetimeMT)
  #y_vals <- roc_all[["SLOC_rates"]][[i]]$ER
  
# Remove NA values
#  valid_indices <- !is.na(y_vals)
 # x_vals <- x_vals[valid_indices]
  #y_vals <- y_vals[valid_indices]

#  SLOC_ER_results[i] <- AUC(
 #   x = x_vals,
  #  y = y_vals,
   # method = "trapezoid",
    #na.rm = FALSE
#  )
#}

#View(SLOC_ER_results)


#SLOW#

#SLOW_ER_results <- numeric(length(roc_all[["SLOW_rates"]])) 
#names(SLOW_ER_results) <- names(roc_all[["SLOW_rates"]])

#for (i in seq_along(roc_all[["SLOW_rates"]])) {
 # x_vals <- as.numeric(roc_all[["SLOW_rates"]][[i]]$datetimeMT)
  #y_vals <- roc_all[["SLOW_rates"]][[i]]$ER
  
  # Remove NA values
  #valid_indices <- !is.na(y_vals)
  #x_vals <- x_vals[valid_indices]
  #y_vals <- y_vals[valid_indices]
  
  #SLOW_ER_results[i] <- AUC(
   # x = x_vals,
    #y = y_vals,
    #method = "trapezoid",
    #na.rm = FALSE
  #)
#}

#View(SLOW_ER_results)



#VDOW#

#VDOW_ER_results <- numeric(length(roc_all[["VDOW_rates"]])) 
#names(VDOW_ER_results) <- names(roc_all[["VDOW_rates"]])

#for (i in seq_along(roc_all[["VDOW_rates"]])) {
 # x_vals <- as.numeric(roc_all[["VDOW_rates"]][[i]]$datetimeMT)
  #y_vals <- roc_all[["VDOW_rates"]][[i]]$ER
  
  # Remove NA values
  #valid_indices <- !is.na(y_vals)
  #x_vals <- x_vals[valid_indices]
  #y_vals <- y_vals[valid_indices]
  
  #VDOW_ER_results[i] <- AUC(
   # x = x_vals,
    #y = y_vals,
    #method = "trapezoid",
    #na.rm = FALSE
  #)
#}

#View(VDOW_ER_results)



#VDOS#

#VDOS_ER_results <- numeric(length(roc_all[["VDOS_rates"]])) 
#names(VDOS_ER_results) <- names(roc_all[["VDOS_rates"]])

#for (i in seq_along(roc_all[["VDOS_rates"]])) {
 # x_vals <- as.numeric(roc_all[["VDOS_rates"]][[i]]$datetimeMT)
  #y_vals <- roc_all[["VDOS_rates"]][[i]]$ER
  
  # Remove NA values
  #valid_indices <- !is.na(y_vals)
  #x_vals <- x_vals[valid_indices]
  #y_vals <- y_vals[valid_indices]
  
#  VDOS_ER_results[i] <- AUC(
 #   x = x_vals,
  #  y = y_vals,
   # method = "trapezoid",
    #na.rm = FALSE
  #)
#}

#View(VDOS_ER_results)


### Boxplot of ER ###

#ER_AUC<-data.frame(ER=c(SLOC_ER_results,SLOW_ER_results,VDOW_ER_results,VDOS_ER_results),
 #                  Well=rep(c("SLOC","SLOW","VDOW","VDOS"),
  #                          times=c(length(SLOC_ER_results),length(SLOW_ER_results),length(VDOW_ER_results),length(VDOS_ER_results))))
#ER_AUC$Event<-c('SLOC_DO1','SLOC_DO2','SLOC_DO3','SLOC_DO4','SLOC_DO5','SLOC_DO6','SLOC_DO7','SLOC_DO8','SLOC_DO9','SLOC_DO10',
 #               'SLOC_DO11','SLOC_DO12','SLOC_DO13','SLOC_DO14','SLOC_DO15','SLOC_DO16','SLOC_DO17','SLOC_DO18','SLOC_DO19','SLOC_DO20',
  #              'SLOC_DO21','SLOW_DO1','SLOW_DO2','SLOW_DO3','SLOW_DO4','SLOW_DO5','SLOW_DO6','SLOW_DO7','SLOW_DO8','SLOW_DO9','SLOW_DO10',
   #             'VDOW_DO1','VDOW_DO2','VDOW_DO3','VDOW_DO4','VDOW_DO5','VDOW_DO6','VDOW_DO7','VDOW_DO8','VDOW_DO9','VDOW_DO10',
    #            'VDOW_DO11','VDOW_DO12','VDOW_DO13','VDOW_DO14','VDOW_DO15','VDOW_DO16','VDOW_DO17','VDOW_DO18',
     #           'VDOS_DO1','VDOS_DO2','VDOS_DO3','VDOS_DO4','VDOS_DO5','VDOS_DO6','VDOS_DO7','VDOS_DO8','VDOS_DO9','VDOS_DO10')
#ER_AUC$Eventdate <-c(SLOC_dates,SLOW_dates,VDOW_dates,VDOS_dates) 

#ER_AUC_bp<-ggplot(data=ER_AUC,mapping=aes(x=Well, y=ER))+geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+labs(y = "Net Ecosystem Respiration (g O2/m^2)")
#print(ER_AUC_bp)


### Positive ER integral ###

#SLOC#
#SLOC_posER_results <- numeric(length(roc_all[["SLOC_rates"]])) 
#names(SLOC_posER_results) <- names(roc_all[["SLOC_rates"]])

#for (i in seq_along(roc_all[["SLOC_rates"]])) {
 # x_vals <- as.numeric(roc_all[["SLOC_rates"]][[i]]$datetimeMT)
  #y_vals <- roc_all[["SLOC_rates"]][[i]]$ER
  
  # Remove NA values
#  valid_indices <- !is.na(y_vals)
 # x_vals <- x_vals[valid_indices]
  #y_vals <- y_vals[valid_indices]
  
  # Set negative values to zero to exclude them from integration
#  y_vals[y_vals < 0] <- 0


 # SLOC_posER_results[i] <- AUC(
  #  x = x_vals,
   # y = y_vals,
    #method = "trapezoid",
    #na.rm = FALSE
#  )
#}

#View(SLOC_posER_results)


#SLOW#
#SLOW_posER_results <- numeric(length(roc_all[["SLOW_rates"]])) 
#names(SLOW_posER_results) <- names(roc_all[["SLOW_rates"]])

#for (i in seq_along(roc_all[["SLOW_rates"]])) {
 # x_vals <- as.numeric(roc_all[["SLOW_rates"]][[i]]$datetimeMT)
  #y_vals <- roc_all[["SLOW_rates"]][[i]]$ER
  
  # Remove NA values
#  valid_indices <- !is.na(y_vals)
 # x_vals <- x_vals[valid_indices]
  #y_vals <- y_vals[valid_indices]
  
  # Set negative values to zero to exclude them from integration
#  y_vals[y_vals < 0] <- 0
  
  
 # SLOW_posER_results[i] <- AUC(
  #  x = x_vals,
   # y = y_vals,
    #method = "trapezoid",
    #na.rm = FALSE
  #)
#}

#View(SLOW_posER_results)


#VDOW#
#VDOW_posER_results <- numeric(length(roc_all[["VDOW_rates"]])) 
#names(VDOW_posER_results) <- names(roc_all[["VDOW_rates"]])

#for (i in seq_along(roc_all[["VDOW_rates"]])) {
#  x_vals <- as.numeric(roc_all[["VDOW_rates"]][[i]]$datetimeMT)
#  y_vals <- roc_all[["VDOW_rates"]][[i]]$ER
  
  # Remove NA values
#  valid_indices <- !is.na(y_vals)
#  x_vals <- x_vals[valid_indices]
#  y_vals <- y_vals[valid_indices]
  
  # Set negative values to zero to exclude them from integration
 # y_vals[y_vals < 0] <- 0
  
  
  #VDOW_posER_results[i] <- AUC(
   # x = x_vals,
    #y = y_vals,
  #  method = "trapezoid",
   # na.rm = FALSE
  #)
#}

#View(VDOW_posER_results)


#VDOS#
#VDOS_posER_results <- numeric(length(roc_all[["VDOS_rates"]])) 
#names(VDOS_posER_results) <- names(roc_all[["VDOS_rates"]])

#for (i in seq_along(roc_all[["VDOS_rates"]])) {
#  x_vals <- as.numeric(roc_all[["VDOS_rates"]][[i]]$datetimeMT)
 # y_vals <- roc_all[["VDOS_rates"]][[i]]$ER
  
  # Remove NA values
  #valid_indices <- !is.na(y_vals)
  #x_vals <- x_vals[valid_indices]
  #y_vals <- y_vals[valid_indices]
  
  # Set negative values to zero to exclude them from integration
  #y_vals[y_vals < 0] <- 0
  
  
  #VDOS_posER_results[i] <- AUC(
   # x = x_vals,
    #y = y_vals,
    #method = "trapezoid",
    #na.rm = FALSE
  #)
#}

#View(VDOS_posER_results)

### plot positive ER boxplot ###

#posER_AUC<-data.frame(posER=c(SLOC_posER_results,SLOW_posER_results,VDOW_posER_results,VDOS_posER_results),
 #                  Well=rep(c("SLOC","SLOW","VDOW","VDOS"),
  #                          times=c(length(SLOC_posER_results),length(SLOW_posER_results),length(VDOW_posER_results),length(VDOS_posER_results))))
#posER_AUC$Event<-c('SLOC_DO1','SLOC_DO2','SLOC_DO3','SLOC_DO4','SLOC_DO5','SLOC_DO6','SLOC_DO7','SLOC_DO8','SLOC_DO9','SLOC_DO10',
 #               'SLOC_DO11','SLOC_DO12','SLOC_DO13','SLOC_DO14','SLOC_DO15','SLOC_DO16','SLOC_DO17','SLOC_DO18','SLOC_DO19','SLOC_DO20',
  #              'SLOC_DO21','SLOW_DO1','SLOW_DO2','SLOW_DO3','SLOW_DO4','SLOW_DO5','SLOW_DO6','SLOW_DO7','SLOW_DO8','SLOW_DO9','SLOW_DO10',
   #             'VDOW_DO1','VDOW_DO2','VDOW_DO3','VDOW_DO4','VDOW_DO5','VDOW_DO6','VDOW_DO7','VDOW_DO8','VDOW_DO9','VDOW_DO10',
    #            'VDOW_DO11','VDOW_DO12','VDOW_DO13','VDOW_DO14','VDOW_DO15','VDOW_DO16','VDOW_DO17','VDOW_DO18',
     #           'VDOS_DO1','VDOS_DO2','VDOS_DO3','VDOS_DO4','VDOS_DO5','VDOS_DO6','VDOS_DO7','VDOS_DO8','VDOS_DO9','VDOS_DO10')
#ER_AUC$Eventdate <-c(SLOC_dates,SLOW_dates,VDOW_dates,VDOS_dates) 

#ER_AUC_bp<-ggplot(data=posER_AUC,mapping=aes(x=Well, y=posER))+geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+labs(y = "Net Positive Ecosystem Respiration (g O2/m^2)")
#print(ER_AUC_bp)


#### Convert DO to ER with Q = D -R ####
#1. rate of change of DO
#2. integral of rate of change of DO
#3. POSITIVE integral of rate of change of DO
#4. 2. - 3. = -ER

#### Convert DO rate of change mg to g ####

#SLOC#
for (name in names(roc_all[["SLOC_rates"]])){
 roc_all[["SLOC_rates"]][[name]]$rate_of_change_g <- 
  roc_all[["SLOC_rates"]][[name]]$rate_of_change / 1000
}

#SLOW#
for (name in names(roc_all[["SLOW_rates"]])){
  roc_all[["SLOW_rates"]][[name]]$rate_of_change_g <- 
    roc_all[["SLOW_rates"]][[name]]$rate_of_change / 1000
}

#VDOW#
for (name in names(roc_all[["VDOW_rates"]])){
  roc_all[["VDOW_rates"]][[name]]$rate_of_change_g <- 
    roc_all[["VDOW_rates"]][[name]]$rate_of_change / 1000
}

#VDOS#
for (name in names(roc_all[["VDOS_rates"]])){
  roc_all[["VDOS_rates"]][[name]]$rate_of_change_g <- 
    roc_all[["VDOS_rates"]][[name]]$rate_of_change / 1000
}

#### Integral of DO rate of change ####

#SLOC#

SLOC_DO_results <- numeric(length(roc_all[["SLOC_rates"]])) 
names(SLOC_DO_results) <- names(roc_all[["SLOC_rates"]])

for (i in seq_along(roc_all[["SLOC_rates"]])) {
 x_vals <- as.numeric(roc_all[["SLOC_rates"]][[i]]$datetimeMT)
 y_vals <- roc_all[["SLOC_rates"]][[i]]$rate_of_change_g

# Remove NA values
  valid_indices <- !is.na(y_vals)
  x_vals <- x_vals[valid_indices]
  y_vals <- y_vals[valid_indices]

  SLOC_DO_results[i] <- AUC(
   x = x_vals,
   y = y_vals,
   method = "trapezoid",
   na.rm = FALSE
   )
}

View(SLOC_DO_results)

#SLOW#

SLOW_DO_results <- numeric(length(roc_all[["SLOW_rates"]])) 
names(SLOW_DO_results) <- names(roc_all[["SLOW_rates"]])

for (i in seq_along(roc_all[["SLOW_rates"]])) {
  x_vals <- as.numeric(roc_all[["SLOW_rates"]][[i]]$datetimeMT)
  y_vals <- roc_all[["SLOW_rates"]][[i]]$rate_of_change_g
  
  # Remove NA values
  valid_indices <- !is.na(y_vals)
  x_vals <- x_vals[valid_indices]
  y_vals <- y_vals[valid_indices]
  
  SLOW_DO_results[i] <- AUC(
    x = x_vals,
    y = y_vals,
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(SLOW_DO_results)

#VDOW#

VDOW_DO_results <- numeric(length(roc_all[["VDOW_rates"]])) 
names(VDOW_DO_results) <- names(roc_all[["VDOW_rates"]])

for (i in seq_along(roc_all[["VDOW_rates"]])) {
  x_vals <- as.numeric(roc_all[["VDOW_rates"]][[i]]$datetimeMT)
  y_vals <- roc_all[["VDOW_rates"]][[i]]$rate_of_change_g
  
  # Remove NA values
  valid_indices <- !is.na(y_vals)
  x_vals <- x_vals[valid_indices]
  y_vals <- y_vals[valid_indices]
  
  VDOW_DO_results[i] <- AUC(
    x = x_vals,
    y = y_vals,
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(VDOW_DO_results)

#VDOS#

VDOS_DO_results <- numeric(length(roc_all[["VDOS_rates"]])) 
names(VDOS_DO_results) <- names(roc_all[["VDOS_rates"]])

for (i in seq_along(roc_all[["VDOS_rates"]])) {
  x_vals <- as.numeric(roc_all[["VDOS_rates"]][[i]]$datetimeMT)
  y_vals <- roc_all[["VDOS_rates"]][[i]]$rate_of_change_g
  
  # Remove NA values
  valid_indices <- !is.na(y_vals)
  x_vals <- x_vals[valid_indices]
  y_vals <- y_vals[valid_indices]
  
  VDOS_DO_results[i] <- AUC(
    x = x_vals,
    y = y_vals,
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(VDOS_DO_results)


#### POSITIVE of DO rate of change (D) ####

#SLOC#
SLOC_D <- numeric(length(roc_all[["SLOC_rates"]])) 
names(SLOC_D) <- names(roc_all[["SLOC_rates"]])

for (i in seq_along(roc_all[["SLOC_rates"]])) {
 x_vals <- as.numeric(roc_all[["SLOC_rates"]][[i]]$datetimeMT)
 y_vals <- roc_all[["SLOC_rates"]][[i]]$rate_of_change_g

# Remove NA values
  valid_indices <- !is.na(y_vals)
  x_vals <- x_vals[valid_indices]
  y_vals <- y_vals[valid_indices]

# Set negative values to zero to exclude them from integration
  y_vals[y_vals < 0] <- 0


 SLOC_D[i] <- AUC(
  x = x_vals,
  y = y_vals,
  method = "trapezoid",
  na.rm = FALSE
  )
}

View(SLOC_D)

#SLOW#
SLOW_D <- numeric(length(roc_all[["SLOW_rates"]])) 
names(SLOW_D) <- names(roc_all[["SLOW_rates"]])

for (i in seq_along(roc_all[["SLOW_rates"]])) {
  x_vals <- as.numeric(roc_all[["SLOW_rates"]][[i]]$datetimeMT)
  y_vals <- roc_all[["SLOW_rates"]][[i]]$rate_of_change_g
  
  # Remove NA values
  valid_indices <- !is.na(y_vals)
  x_vals <- x_vals[valid_indices]
  y_vals <- y_vals[valid_indices]
  
  # Set negative values to zero to exclude them from integration
  y_vals[y_vals < 0] <- 0
  
  
  SLOW_D[i] <- AUC(
    x = x_vals,
    y = y_vals,
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(SLOW_D)

#VDOW#
VDOW_D <- numeric(length(roc_all[["VDOW_rates"]])) 
names(VDOW_D) <- names(roc_all[["VDOW_rates"]])

for (i in seq_along(roc_all[["VDOW_rates"]])) {
  x_vals <- as.numeric(roc_all[["VDOW_rates"]][[i]]$datetimeMT)
  y_vals <- roc_all[["VDOW_rates"]][[i]]$rate_of_change_g
  
  # Remove NA values
  valid_indices <- !is.na(y_vals)
  x_vals <- x_vals[valid_indices]
  y_vals <- y_vals[valid_indices]
  
  # Set negative values to zero to exclude them from integration
  y_vals[y_vals < 0] <- 0
  
  
  VDOW_D[i] <- AUC(
    x = x_vals,
    y = y_vals,
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(VDOW_D)

#VDOS#
VDOS_D <- numeric(length(roc_all[["VDOS_rates"]])) 
names(VDOS_D) <- names(roc_all[["VDOS_rates"]])

for (i in seq_along(roc_all[["VDOS_rates"]])) {
  x_vals <- as.numeric(roc_all[["VDOS_rates"]][[i]]$datetimeMT)
  y_vals <- roc_all[["VDOS_rates"]][[i]]$rate_of_change_g
  
  # Remove NA values
  valid_indices <- !is.na(y_vals)
  x_vals <- x_vals[valid_indices]
  y_vals <- y_vals[valid_indices]
  
  # Set negative values to zero to exclude them from integration
  y_vals[y_vals < 0] <- 0
  
  
  VDOS_D[i] <- AUC(
    x = x_vals,
    y = y_vals,
    method = "trapezoid",
    na.rm = FALSE
  )
}

View(VDOS_D)

#### Q - D = -ER ####

#SLOC#
names(SLOC_DO_results)<-NULL
names(SLOC_D)<-NULL

SLOC_ER <- SLOC_DO_results - SLOC_D
names(SLOC_ER) <- names(roc_all[["SLOC_rates"]]) 

View(SLOC_ER)


#SLOW#
names(SLOW_DO_results)<-NULL
names(SLOW_D)<-NULL

SLOW_ER <- SLOW_DO_results - SLOW_D
names(SLOW_ER) <- names(roc_all[["SLOW_rates"]]) 

View(SLOW_ER)


#VDOW#
names(VDOW_DO_results)<-NULL
names(VDOW_D)<-NULL

VDOW_ER <- VDOW_DO_results - VDOW_D
names(VDOW_ER) <- names(roc_all[["VDOW_rates"]]) 

View(VDOW_ER)


#VDOS#
names(VDOS_DO_results)<-NULL
names(VDOS_D)<-NULL

VDOS_ER <- VDOS_DO_results - VDOS_D
names(VDOS_ER) <- names(roc_all[["VDOS_rates"]]) 

View(VDOS_ER)

#### ER boxplot ####

odumER<-data.frame(ER=c(SLOC_ER,SLOW_ER,VDOW_ER,VDOS_ER),
                  Well=rep(c("SLOC","SLOW","VDOW","VDOS"),
                          times=c(length(SLOC_ER),length(SLOW_ER),length(VDOW_ER),length(VDOS_ER))))
odumER$Event<-c('SLOC_DO1','SLOC_DO2','SLOC_DO3','SLOC_DO4','SLOC_DO5','SLOC_DO6','SLOC_DO7','SLOC_DO8','SLOC_DO9','SLOC_DO10',
               'SLOC_DO11','SLOC_DO12','SLOC_DO13','SLOC_DO14','SLOC_DO15','SLOC_DO16','SLOC_DO17','SLOC_DO18','SLOC_DO19','SLOC_DO20',
              'SLOC_DO21','SLOW_DO1','SLOW_DO2','SLOW_DO3','SLOW_DO4','SLOW_DO5','SLOW_DO6','SLOW_DO7','SLOW_DO8','SLOW_DO9','SLOW_DO10',
             'VDOW_DO1','VDOW_DO2','VDOW_DO3','VDOW_DO4','VDOW_DO5','VDOW_DO6','VDOW_DO7','VDOW_DO8','VDOW_DO9','VDOW_DO10',
            'VDOW_DO11','VDOW_DO12','VDOW_DO13','VDOW_DO14','VDOW_DO15','VDOW_DO16','VDOW_DO17','VDOW_DO18',
           'VDOS_DO1','VDOS_DO2','VDOS_DO3','VDOS_DO4','VDOS_DO5','VDOS_DO6','VDOS_DO7','VDOS_DO8','VDOS_DO9','VDOS_DO10')
#odumER$Eventdate <-c(SLOC_dates,SLOW_dates,VDOW_dates,VDOS_dates) 

odumER_bp<-ggplot(data=odumER,mapping=aes(x=Well, y=ER))+geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+labs(y = "Ecosystem Respiration (g O2/m^2)")
print(odumER_bp)


