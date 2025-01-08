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

