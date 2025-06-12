#### read me ####
# The purpose of this script is to trim the water level dataset, calculate the mean and CV of the whole dataset for each well, and then calculate the mean and CV for each event (x days leading up to each event)

#### Libraries and functions####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)

cv <- function (x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}

#### Import finalized water level data ####

#as dataframe
BEGI_PT_DTW_all = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")

#### Trim data frame to match sonde length and add constant ####

BEGI_PT_DTW_trim <- BEGI_PT_DTW_all[BEGI_PT_DTW_all$datetimeMT >= "2023-09-15 00:00:00" 
                                    & BEGI_PT_DTW_all$datetimeMT <= "2024-09-04 00:00:00",]

BEGI_PT_DTW_trim$DTW_m_con = BEGI_PT_DTW_trim$DTW_m + 1

#### Whole Well Mean/Var ####

BEGI_PT_DTW_trim <- BEGI_PT_DTW_trim %>%
  spread (wellID, DTW_m_con) #%>%
  #mutate_at(c("SLOC","SLOW","VDOS","VDOW")) # this line of code isn't working for AJW. I'm not sure what the intention is.

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
gwmv_well 

#### export for use in other scripts ####
write.csv(gwmv_well, "DTW_compiled/gwmv_well.csv")
saveRDS(BEGI_PT_DTW_trim, "DTW_compiled/BEGI_PT_DTW_trim.rds")

####import list of event dates per well ####
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")
BEGI_PT_DTW_trim = readRDS("DTW_compiled/BEGI_PT_DTW_trim.rds")

#Turns lists into vectors
SLOC_dates <- c(BEGI_events[["Eventdate"]][["SLOC_dates"]])
SLOW_dates <- c(BEGI_events[["Eventdate"]][["SLOW_dates"]])
VDOS_dates <- c(BEGI_events[["Eventdate"]][["VDOS_dates"]])
VDOW_dates <- c(BEGI_events[["Eventdate"]][["VDOW_dates"]])

#### DO event dtw mean (2 days) ####
# note from AJW: I need the events labeled by date (or something more sophisticated if they span multiple dates) so that I can match them to the AUC etc. results for modeling!

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

SLOC_event_mean = unlist(SLOC_event_mean,use.names = F)

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

SLOW_event_mean = unlist(SLOW_event_mean,use.names = F)


#VDOS
#mean calculated 2 days before each event
VDOS_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_mean <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_mean = mean(VDOS, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOS_event_mean
  VDOS_event_mean <- c(VDOS_event_mean,VDOS_mean)
}

VDOS_event_mean = unlist(VDOS_event_mean,use.names = F)


#VDOW
#mean calculated 2 days before each event
VDOW_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_mean <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_mean = mean(VDOW, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOW_event_mean
  VDOW_event_mean <- c(VDOW_event_mean,VDOW_mean)
}

VDOW_event_mean = unlist(VDOW_event_mean,use.names = F)


#### DO event dtw CV (2 days) ####
# note from AJW: I need the events labeled by date (or something more sophisticated if they span multiple dates) so that I can match them to the AUC etc. results for modeling!

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
  #cv of DTW_m over period of temptimes
  #add cv to SLOC_event_cv
  SLOC_event_cv <- c(SLOC_event_cv,SLOC_cv)
}

SLOC_event_cv = unlist(SLOC_event_cv,use.names = F)


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
  #cv of DTW_m over period of temptimes
  #add cv to SLOW_event_mean
  SLOW_event_cv <- c(SLOW_event_cv,SLOW_cv)
}

SLOW_event_cv = unlist(SLOW_event_cv,use.names = F)


#VDOS
#CV calculated 2 days before each event
VDOS_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_cv <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_cv = cv(VDOS))
  #cv of DTW_m over period of temptimes
  #add cv to VDOS_event_mean
  VDOS_event_cv <- c(VDOS_event_cv,VDOS_cv)
}

VDOS_event_cv = unlist(VDOS_event_cv,use.names = F)


#VDOW
#CV calculated 2 days before each event
VDOW_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_cv <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_cv = cv(VDOW))
  #cv of DTW_m over period of temptimes
  #add cv to VDOW_event_mean
  VDOW_event_cv <- c(VDOW_event_cv,VDOW_cv)
}

VDOW_event_cv = unlist(VDOW_event_cv,use.names = F)


#### Dataframe of event mean/var for 2 days ####
#BEGI_events[["Eventdate"]][["SLOC_DO"]]
DO_event_mean <- c(SLOC_event_mean,SLOW_event_mean,VDOS_event_mean,VDOW_event_mean)
DO_event_cv <- c(SLOC_event_cv,SLOW_event_cv,VDOS_event_cv,VDOW_event_cv)
Eventdates<-c(SLOC_dates,SLOW_dates,VDOS_dates,VDOW_dates)
WellID<-c(rep(c("SLOC","SLOW","VDOS","VDOW"),
             times=c(length(SLOC_event_mean),length(SLOW_event_mean),length(VDOS_event_mean),length(VDOW_event_mean))))
DO_event_mv <- data.frame(WellID,Eventdates,DO_event_mean,DO_event_cv)
View(DO_event_mv)

write_csv(DO_event_mv,"DTW_compiled/DO_mv_2days.csv")


#### Event dtw mean (1 day) ####

#SLOC
#mean calculated 1 day before each event
SLOC_event_mean1<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOC_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]-(60*60*24),
                  to=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_mean1 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_mean1 = mean(SLOC, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOC_event_mean
  SLOC_event_mean1 <- c(SLOC_event_mean1,SLOC_mean1)
}

SLOC_event_mean1 = unlist(SLOC_event_mean1,use.names = F)

#SLOW
#mean calculated 1 day before each event
SLOW_event_mean1<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]-(60*60*24),
                  to=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_mean1 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_mean1 = mean(SLOW, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOW_event_mean
  SLOW_event_mean1 <- c(SLOW_event_mean1,SLOW_mean1)
}

SLOW_event_mean1 = unlist(SLOW_event_mean1,use.names = F)

#VDOW
#mean calculated 1 day before each event
VDOW_event_mean1<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*24),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_mean1 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_mean1 = mean(VDOW, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOW_event_mean
  VDOW_event_mean1 <- c(VDOW_event_mean1,VDOW_mean1)
}

VDOW_event_mean1 = unlist(VDOW_event_mean1,use.names = F)

#VDOS
#mean calculated 1 day before each event
VDOS_event_mean1<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*24),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_mean1 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_mean1 = mean(VDOS, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOS_event_mean
  VDOS_event_mean1 <- c(VDOS_event_mean1,VDOS_mean1)
}

VDOS_event_mean1 = unlist(VDOS_event_mean1,use.names = F)


#### Event dtw CV (1 day) ####
# note from AJW: I need the events labeled by date (or something more sophisticated if they span multiple dates) so that I can match them to the AUC etc. results for modeling!

#SLOC
#CV calculated 1 day before each event
SLOC_event_cv1<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOC_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]-(60*60*24),
                  to=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_cv1 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_cv1 = cv(SLOC))
  #cv of DTW_m over period of temptimes
  #add cv to SLOC_event_cv
  SLOC_event_cv1 <- c(SLOC_event_cv1,SLOC_cv1)
}

SLOC_event_cv1 = unlist(SLOC_event_cv1,use.names = F)

#SLOW
#CV calculated 1 day before each event
SLOW_event_cv1<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]-(60*60*24),
                  to=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_cv1 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_cv1 = cv(SLOW))
  #cv of DTW_m over period of temptimes
  #add cv to SLOW_event_cv
  SLOW_event_cv1 <- c(SLOW_event_cv1,SLOW_cv1)
}

SLOW_event_cv1 = unlist(SLOW_event_cv1,use.names = F)

#VDOW
#CV calculated 1 day before each event
VDOW_event_cv1<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*24),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_cv1 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_cv1 = cv(VDOW))
  #cv of DTW_m over period of temptimes
  #add cv to VDOW_event_cv
  VDOW_event_cv1 <- c(VDOW_event_cv1,VDOW_cv1)
}

VDOW_event_cv1 = unlist(VDOW_event_cv1,use.names = F)

#VDOS
#CV calculated 1 day before each event
VDOS_event_cv1<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*24),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_cv1 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_cv1 = cv(VDOS))
  #cv of DTW_m over period of temptimes
  #add cv to VDOS_event_cv
  VDOS_event_cv1 <- c(VDOS_event_cv1,VDOS_cv1)
}

VDOS_event_cv1 = unlist(VDOS_event_cv1,use.names = F)

#### dataframe for event mean/var for 1 day ####
DO_event_mean1 <- c(SLOC_event_mean1,SLOW_event_mean1,VDOS_event_mean1,VDOW_event_mean1)
DO_event_cv1 <- c(SLOC_event_cv1,SLOW_event_cv1,VDOS_event_cv1,VDOW_event_cv1)
Eventdates<-c(SLOC_dates,SLOW_dates,VDOS_dates,VDOW_dates)
WellID<-c(rep(c("SLOC","SLOW","VDOS","VDOW"),
              times=c(length(SLOC_event_mean1),length(SLOW_event_mean1),length(VDOS_event_mean1),length(VDOW_event_mean1))))
DO_event_mv1 <- data.frame(WellID,Eventdates,DO_event_mean1,DO_event_cv1)
View(DO_event_mv1)

write_csv(DO_event_mv1,"DTW_compiled/DO_mv_1day.csv")

#### Event dtw mean (5 days) ####

#SLOC
#mean calculated 5 day before each event
SLOC_event_mean5<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOC_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]-(60*60*120),
                  to=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_mean5 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_mean5 = mean(SLOC, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOC_event_mean
  SLOC_event_mean5 <- c(SLOC_event_mean5,SLOC_mean5)
}

SLOC_event_mean5 = unlist(SLOC_event_mean5,use.names = F)


#SLOW
#mean calculated 5 day before each event
SLOW_event_mean5<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]-(60*60*120),
                  to=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_mean5 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_mean5 = mean(SLOW, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOW_event_mean
  SLOW_event_mean5 <- c(SLOW_event_mean5,SLOW_mean5)
}

SLOW_event_mean5 = unlist(SLOW_event_mean5,use.names = F)


#VDOW
#mean calculated 5 day before each event
VDOW_event_mean5<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*120),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_mean5 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_mean5 = mean(VDOW, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOW_event_mean
  VDOW_event_mean5 <- c(VDOW_event_mean5,VDOW_mean5)
}

VDOW_event_mean5 = unlist(VDOW_event_mean5,use.names = F)


#VDOS
#mean calculated 5 day before each event
VDOS_event_mean5<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*120),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_mean5 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_mean5 = mean(VDOS, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOS_event_mean
  VDOS_event_mean5 <- c(VDOS_event_mean5,VDOS_mean5)
}

VDOS_event_mean5 = unlist(VDOS_event_mean5,use.names = F)



#### Event dtw cv (5 days) ####

#SLOC#
#calculating the dtw cv for 5 days preceeding an event

SLOC_event_cv5<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOC_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]-(60*60*120),
                  to=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_cv5 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_cv5 = cv(SLOC))
  #cv of DTW_m over period of temptimes
  #add cv to SLOC_event_cv
  SLOC_event_cv5 <- c(SLOC_event_cv5,SLOC_cv5)
}

SLOC_event_cv5 = unlist(SLOC_event_cv5,use.names = F)


#SLOW#
#calculating the dtw cv for 5 days preceeding an event

SLOW_event_cv5<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]-(60*60*120),
                  to=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_cv5 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "SLO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_cv5 = cv(SLOW))
  #cv of DTW_m over period of temptimes
  #add cv to SLOW_event_cv
  SLOW_event_cv5 <- c(SLOW_event_cv5,SLOW_cv5)
}

SLOW_event_cv5 = unlist(SLOW_event_cv5,use.names = F)

#VDOW#
#calculating the dtw cv for 5 days preceeding an event

VDOW_event_cv5<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*120),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_cv5 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_cv5 = cv(VDOW))
  #cv of DTW_m over period of temptimes
  #add cv to VDOW_event_cv
  VDOW_event_cv5 <- c(VDOW_event_cv5,VDOW_cv5)
}

VDOW_event_cv5 = unlist(VDOW_event_cv5,use.names = F)

#VDOS#
#calculating the dtw cv for 5 days preceeding an event

VDOS_event_cv5<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*120),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_cv5 <- BEGI_PT_DTW_trim %>%
    ungroup() %>%
    filter(siteID == "VDO") %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_cv5 = cv(VDOS))
  #cv of DTW_m over period of temptimes
  #add cv to VDOS_event_cv
  VDOS_event_cv5 <- c(VDOS_event_cv5,VDOS_cv5)
}

VDOS_event_cv5 = unlist(VDOS_event_cv5,use.names = F)


#### Dataframe for event mean/var for 5 days ####
DO_event_mean5 <- c(SLOC_event_mean5,SLOW_event_mean5,VDOS_event_mean5,VDOW_event_mean5)
DO_event_cv5 <- c(SLOC_event_cv5,SLOW_event_cv5,VDOS_event_cv5,VDOW_event_cv5)
Eventdates<-c(SLOC_dates,SLOW_dates,VDOS_dates,VDOW_dates)
WellID<-c(rep(c("SLOC","SLOW","VDOS","VDOW"),
              times=c(length(SLOC_event_mean5),length(SLOW_event_mean5),length(VDOS_event_mean5),length(VDOW_event_mean5))))
DO_event_mv5 <- data.frame(WellID,Eventdates,DO_event_mean5,DO_event_cv5)
View(DO_event_mv5)


#### Stitched dataframe of all event mean/var + export ####

DO_event_mvall <- merge(DO_event_mv1, DO_event_mv, by = c("WellID","Eventdates"), all = TRUE)
DO_event_mvall <- merge(DO_event_mvall, DO_event_mv5, by = c("WellID","Eventdates"), all = TRUE)
View(DO_event_mvall)

write_csv(DO_event_mvall,"DTW_compiled/DO_mv.csv")

#### Event dtw timeseries ####
#Import DTW data
DTW_df = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")

### spread DTW_m in DTW_df to 
DTW_df <- DTW_df %>%
  spread (wellID, DTW_m)

#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")


#SLOC#
dtw_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["SLOC_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["SLOC_dates"]][[i]]
  start_time <- event_time - 60*60*48
  
  tempdat <- DTW_df[
    DTW_df$datetimeMT >= start_time & DTW_df$datetimeMT < event_time,
  ]
#remove NAs from dataset  
  valid_SLOC <- tempdat$SLOC[!is.na(tempdat$SLOC)]
#extract 192 values  
  if (length(valid_SLOC) >= 192) {
    ts_values <- valid_SLOC[1:192]
  } else {
    ts_values <- c(valid_SLOC, rep(NA, 192 - length(valid_SLOC)))
  }
  
  dtw_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOC_dtw <- as.data.frame(do.call(rbind, dtw_timeseries))
colnames(SLOC_dtw) <- paste0("t", seq_len(192))
SLOC_dtw$event_time <- BEGI_events[["Eventdate"]][["SLOC_dates"]]

#SLOW#
dtw_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["SLOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]][[i]]
  start_time <- event_time - 60*60*48
  
  tempdat <- DTW_df[
    DTW_df$datetimeMT >= start_time & DTW_df$datetimeMT < event_time,
  ]
  #remove NAs from dataset  
  valid_SLOW <- tempdat$SLOW[!is.na(tempdat$SLOW)]
  #extract 192 values  
  if (length(valid_SLOW) >= 192) {
    ts_values <- valid_SLOW[1:192]
  } else {
    ts_values <- c(valid_SLOW, rep(NA, 192 - length(valid_SLOW)))
  }
  
  dtw_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOW_dtw <- as.data.frame(do.call(rbind, dtw_timeseries))
colnames(SLOW_dtw) <- paste0("t", seq_len(192))
SLOW_dtw$event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]]


#VDOW#
dtw_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]][[i]]
  start_time <- event_time - 60*60*48
  
  tempdat <- DTW_df[
    DTW_df$datetimeMT >= start_time & DTW_df$datetimeMT < event_time,
  ]
  #remove NAs from dataset  
  valid_VDOW <- tempdat$VDOW[!is.na(tempdat$VDOW)]
  #extract 192 values  
  if (length(valid_VDOW) >= 192) {
    ts_values <- valid_VDOW[1:192]
  } else {
    ts_values <- c(valid_VDOW, rep(NA, 192 - length(valid_VDOW)))
  }
  
  dtw_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOW_dtw <- as.data.frame(do.call(rbind, dtw_timeseries))
colnames(VDOW_dtw) <- paste0("t", seq_len(192))
VDOW_dtw$event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]]


#VDOS#
dtw_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOS_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]][[i]]
  start_time <- event_time - 60*60*48
  
  tempdat <- DTW_df[
    DTW_df$datetimeMT >= start_time & DTW_df$datetimeMT < event_time,
  ]
  #remove NAs from dataset  
  valid_VDOS <- tempdat$VDOS[!is.na(tempdat$VDOS)]
  #extract 192 values  
  if (length(valid_VDOS) >= 192) {
    ts_values <- valid_VDOS[1:192]
  } else {
    ts_values <- c(valid_VDOS, rep(NA, 192 - length(valid_VDOS)))
  }
  
  dtw_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOS_dtw <- as.data.frame(do.call(rbind, dtw_timeseries))
colnames(VDOS_dtw) <- paste0("t", seq_len(192))
VDOS_dtw$event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]]

#combine all dtw dataframes
event_dtw <- rbind(SLOC_dtw,SLOW_dtw,VDOW_dtw,VDOS_dtw)
#save
saveRDS(event_dtw, "DTW_compiled/event_dtw.rds")

