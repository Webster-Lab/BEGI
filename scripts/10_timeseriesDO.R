#### read me ####

# the purpose of this script is to clean each well's time series of DO data to later convert to ER

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)

#### Import compiled EXO1 RDS file ####
BEGI_EXO.or2 = readRDS("EXO_compiled/BEGI_EXO.or2.rds")

#### Correct negative DO values ####
BEGI_EXO.or2[["VDOW"]]$ODO.mg.L.mn <- BEGI_EXO.or2[["VDOW"]]$ODO.mg.L.mn + 0.36
BEGI_EXO.or2[["VDOS"]]$ODO.mg.L.mn <- BEGI_EXO.or2[["VDOS"]]$ODO.mg.L.mn + 0.42
BEGI_EXO.or2[["SLOW"]]$ODO.mg.L.mn <- BEGI_EXO.or2[["SLOW"]]$ODO.mg.L.mn + 0.32
BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn <- BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn + 2.2


#### Read in .csv files of service dates and times ####
service.VDOW = read.csv("EXO_compiled/service.VDOW.csv", row.names = 1)
names(service.VDOW) = "datetimeMT"
service.VDOW$datetimeMT = as.POSIXct(service.VDOW$datetimeMT, tz="US/Mountain")

service.VDOS = read.csv("EXO_compiled/service.VDOS.csv", row.names = 1)
names(service.VDOS) = "datetimeMT"
service.VDOS$datetimeMT = as.POSIXct(service.VDOS$datetimeMT, tz="US/Mountain")

service.SLOC = read.csv("EXO_compiled/service.SLOC.csv", row.names = 1)
names(service.SLOC) = "datetimeMT"
service.SLOC$datetimeMT = as.POSIXct(service.SLOC$datetimeMT, tz="US/Mountain")

service.SLOW = read.csv("EXO_compiled/service.SLOW.csv", row.names = 1)
names(service.SLOW) = "datetimeMT"
service.SLOW$datetimeMT = as.POSIXct(service.SLOW$datetimeMT, tz="US/Mountain")

# sunrise/sunset

suntimes = 
  getSunlightTimes(date = seq.Date(from = as.Date("2023-09-14"), to = as.Date("2024-09-5"), by = 1),
                   keep = c("sunrise", "sunset"),
                   lat = 34.9, lon = -106.7, tz = "US/Mountain")

pm.pts = suntimes$sunset[-(nrow(suntimes))]
am.pts = suntimes$sunrise[-1]

#### Vector of dates ####

date <- (
  datetimeMT = seq.POSIXt(
    from = ISOdatetime(2023,09,15,0,0,0, tz = "US/Mountain"),
    to = ISOdatetime(2024,09,04,0,0,0, tz= "US/Mountain"),
    by = "24 h" ))

date = as.Date(date)


