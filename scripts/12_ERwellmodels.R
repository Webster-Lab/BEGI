#### read me ####

# the purpose of this script is to test for differences in magnitudes and rates of respiration events between wells for the Webster Lab BEGI project
#Using Odum's method for estimating respiration

# There are 2 sites (SLO and VOD).
# There are 2 wells within each site (SLOC, SLOW, VDOS, and VDOW).
# There are multiple respiration events in each well: 
# SLOC SLOW VDOS VDOW 
# 21   10   10   18 

#### libraries & fxns ####

library(tidyverse)
library(lubridate)
library(forecast)
library(zoo)
library(xts)
library(nlme)
library(visreg)
library(psych)
library(ggeffects)
library(margins)
library(psych)
library(car)
library(tsibble)
library(lme4)
library(lmerTest)
library(MuMIn)


# replace NaNs with NA
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

#### load and wrangle data ####

# Odum's method of quantifying the size of respiration events from %DO timeseries
odumER = readRDS("EXO_compiled/odumER.rds")
odumER$siteID = substr(odumER$Well, start = 1, stop = 3)
names(odumER)[names(odumER) == 'Well'] <- 'wellID'
odumER[sapply(odumER, is.character)] <- lapply(odumER[sapply(odumER, is.character)],  as.factor)
names(odumER)[names(odumER) == 'Event'] <- 'eventID'

# mean and variance summary of depth to water (DTW) for each well for whole period of %DO observation
gwmv_well = read.csv("DTW_compiled/gwmv_well.csv", row.names = 1)
names(gwmv_well)[names(gwmv_well) == 'wells'] <- 'wellID'
gwmv_well$siteID = substr(gwmv_well$wellID, start = 1, stop = 3)
gwmv_well[sapply(gwmv_well, is.character)] <- lapply(gwmv_well[sapply(gwmv_well, is.character)],  as.factor)

# join data
ER_events = left_join(odumER, gwmv_well, by=c("siteID","wellID"))

# replace NaNs with NA
ER_events[is.nan(ER_events)] <- NA

# clean up environment
rm(odumER)

#Multiply by -1 and remove zeroes to handle log transformations
ER_events$posER <- ER_events$ER * -1
ER_events_filtered <- ER_events[ER_events$posER > 0, ]


#### explore data ####

# explore structure
with(ER_events, table(wellID))

ER_events = 
  ER_events %>% 
  group_by(siteID, wellID) %>% 
  arrange(Eventdate)
summary(ER_events)

# explore correlations
psych::pairs.panels(ER_events[c("ER", "gwmean_well", "gwvar_well")], stars = TRUE)
# gw mean and gw var are highly correlated, so we can't include them in the same model or test for an interaction. This is good motivation to simply test for group mean differences by well and site and interpret effects of gw mean and var qualitatively. 

