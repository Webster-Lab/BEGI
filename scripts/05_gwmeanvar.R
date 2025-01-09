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

#### AJW check ####
#need to use trimmed dataset
mean(BEGI_PT_DTW_all$DTW_m[BEGI_PT_DTW_all$wellID=="SLOC"], na.rm = T) #-0.0155472
cv(BEGI_PT_DTW_all$DTW_m[BEGI_PT_DTW_all$wellID=="SLOC"]) # -602.7168
# I am not sure why these values are different from Eve's method above
# the cv of SLOC also seems suspiciously high... how does cv handle values that move between neg and pos?
cv(BEGI_PT_DTW_all$DTW_m[BEGI_PT_DTW_all$wellID=="SLOC" &
                           BEGI_PT_DTW_all$DTW_m<0]) # -96.20933
sd(BEGI_PT_DTW_all$DTW_m[BEGI_PT_DTW_all$wellID=="SLOC"], na.rm = T) # 0.09370556
# ah... can't use cv for data with neg. values: https://stats.stackexchange.com/questions/56399/why-is-the-coefficient-of-variation-not-valid-when-using-data-with-positive-and
# one option is to add a constant to all data so that it is all positive, though this is problematic for reasons I don't quite understand (see https://stats.stackexchange.com/questions/252714/alternative-for-coefficient-of-variation-due-to-negative-observations). I'll do it for now and address in more depth later.

BEGI_PT_DTW_all$DTW_m_con = BEGI_PT_DTW_all$DTW_m + 1
# this adds a constant to all DTW values so that none are negative

# AJW version:
gwmv_well = data.frame(wells = c("SLOC","SLOW","VDOS","VDOW"),
                       gwmean_well = c(mean(BEGI_PT_DTW_all$DTW_m[BEGI_PT_DTW_all$wellID=="SLOC"], na.rm = T),
                                       mean(BEGI_PT_DTW_all$DTW_m[BEGI_PT_DTW_all$wellID=="SLOW"], na.rm = T),
                                       mean(BEGI_PT_DTW_all$DTW_m[BEGI_PT_DTW_all$wellID=="VDOS"], na.rm = T),
                                       mean(BEGI_PT_DTW_all$DTW_m[BEGI_PT_DTW_all$wellID=="VDOW"], na.rm = T)),
                       gwvar_well = c(cv(BEGI_PT_DTW_all$DTW_m_con[BEGI_PT_DTW_all$wellID=="SLOC"]),
                                      cv(BEGI_PT_DTW_all$DTW_m_con[BEGI_PT_DTW_all$wellID=="SLOW"]),
                                      cv(BEGI_PT_DTW_all$DTW_m_con[BEGI_PT_DTW_all$wellID=="VDOS"]),
                                      cv(BEGI_PT_DTW_all$DTW_m_con[BEGI_PT_DTW_all$wellID=="VDOW"]))
                       )

#   wells gwmean_well gwvar_well
# 1  SLOC  -0.0155472   9.518543
# 2  SLOW   0.4561454   8.699865
# 3  VDOS   1.6773447   5.918672
# 4  VDOW   1.6059399   5.507157
# these seem more reasonable and informative!

#### export for use in other scripts ####
write.csv(gwmv_well, "DTW_compiled/gwmv_well.csv")

####import list of event dates per well ####
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")


#### DO event mean ####
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
# note from AJW: I need the events labeled by date (or something more sophisticated if they span multiple dates) so that I can match them to the AUC etc. results for modeling!
# I also need this to be re-calculated using the DTW_m_con value of DTW (DTW + a constant so that there are no neg. values) since I discovered that CV doesn't work with data that has negatives and positives (see notes in whole well cacluation above)

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


