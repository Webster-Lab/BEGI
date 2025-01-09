#### read me ####
# The purpose of this script is to trim the water level dataset, calculate the mean and CV of the whole dataset for each well, and then calculate the mean and CV for each event (x days leading up to each event)

#### Libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)

#### Import finalized water level data ####
#as list
BEGI_PTz_DTW = readRDS("DTW_compiled/BEGI_PTz_DTW.rds")

#as dataframe
BEGI_PT_DTW_all = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")

#### Trim data frame to match sonde length ####

BEGI_PT_DTW_trim <- BEGI_PT_DTW_all[BEGI_PT_DTW_all$datetimeMT >= "2023-09-15 00:00:00" 
                                    & BEGI_PT_DTW_all$datetimeMT <= "2024-09-04 00:00:00",]

#### Whole Well Mean/Var ####

BEGI_PT_DTW_trim <- BEGI_PT_DTW_trim %>%
  spread (wellID, DTW_m) %>%
  mutate_at(c("SLOC","SLOW","VDOS","VDOW"))

wells<-c("SLOC","SLOW","VDOS","VDOW")

gwmean_well<-c(mean(BEGI_PT_DTW_trim$SLOC, na.rm = TRUE),
                  mean(BEGI_PT_DTW_trim$SLOW, na.rm = TRUE),
                  mean(BEGI_PT_DTW_trim$VDOS, na.rm = TRUE),
                  mean(BEGI_PT_DTW_trim$VDOW, na.rm = TRUE))

cv <- function (x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}
gwvar_well<-c(cv(BEGI_PT_DTW_trim$SLOC),
              cv(BEGI_PT_DTW_trim$SLOW),
              cv(BEGI_PT_DTW_trim$VDOS),
              cv(BEGI_PT_DTW_trim$VDOW))

gwmv_well<-data.frame(wells,gwmean_well,gwvar_well)
gwmv_well #df of wellIDs and their average depth to water (m) and CV for the entire timeseries

####import list of event dates per well ####
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")


#### DO event mean ####

#SLOC
#mean calculated 2 days before each event
SLOC_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOC_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_mean <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_mean = mean(SLOC, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOC_event_mean
  SLOC_event_mean <- c(SLOC_event_mean,SLOC_mean)
}


#SLOW
#mean calculated 2 days before each event
SLOW_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_mean <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_mean = mean(SLOW, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOW_event_mean
  SLOW_event_mean <- c(SLOW_event_mean,SLOW_mean)
}


#VDOS
#mean calculated 2 days before each event
VDOS_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_mean <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_mean = mean(VDOS, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOS_event_mean
  VDOS_event_mean <- c(VDOS_event_mean,VDOS_mean)
}


#VDOW
#mean calculated 2 days before each event
VDOW_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_mean <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_mean = mean(VDOW, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOW_event_mean
  VDOW_event_mean <- c(VDOW_event_mean,VDOW_mean)
}


#### DO event CV ####

#SLOC
#CV calculated 2 days before each event
SLOC_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOC_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_cv <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_cv = cv(SLOC))
  #mean of DTW_m over period of temptimes
  #add mean to SLOC_event_mean
  SLOC_event_cv <- c(SLOC_event_cv,SLOC_cv)
}


#SLOW
#CV calculated 2 days before each event
SLOW_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_cv <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_cv = cv(SLOW))
  #mean of DTW_m over period of temptimes
  #add mean to SLOW_event_mean
  SLOW_event_cv <- c(SLOW_event_cv,SLOW_cv)
}


#VDOS
#CV calculated 2 days before each event
VDOS_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_cv <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_cv = cv(VDOS))
  #mean of DTW_m over period of temptimes
  #add mean to VDOS_event_mean
  VDOS_event_cv <- c(VDOS_event_cv,VDOS_cv)
}


#VDOW
#CV calculated 2 days before each event
VDOW_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_cv <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_cv = cv(VDOW))
  #mean of DTW_m over period of temptimes
  #add mean to VDOW_event_mean
  VDOW_event_cv <- c(VDOW_event_cv,VDOW_cv)
}


