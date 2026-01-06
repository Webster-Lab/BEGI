#### read me ####
#The purpose of this script is to align delineated DO and fDOM events to test their correlation
#DO events and fDOM consumption were independently delineated but share some overlap.
#This script will index each DO event to align each one with its corresponding fDOM consumption
# The break down of DO events is   SLOC (18), SLOW (9), VDOW (15), VDOS (10)
# The break down of fDOM events is SLOC (28), SLWOW (14), VDOW (28), VDOS (21)


#### Libraries ####
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)
library(dplyr)

#### Import all events ####
#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#### Evaluating start of events ####
# SLOC DO
for (i in seq_along(BEGI_events[["DO_events"]][["SLOC_DO"]])) {
  start <- BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]
  end <- BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[length(
           BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT)]
  
  x <- paste(start, end, collapse = ", ")
  print(x)
}

# SLOC fDOM
for (i in seq_along(BEGI_events[["fDOM_events"]][["SLOC_fDOM"]])) {
  start <- BEGI_events[["fDOM_events"]][["SLOC_fDOM"]][[i]]$datetimeMT[1]
  end <- BEGI_events[["fDOM_events"]][["SLOC_fDOM"]][[i]]$datetimeMT[length(
    BEGI_events[["fDOM_events"]][["SLOC_fDOM"]][[i]]$datetimeMT)]
  
  x <- paste(start, end, collapse = ", ")
  print(x)
}

