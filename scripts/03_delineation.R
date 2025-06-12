#### read me ####

# the purpose of this script is to delineate respiration events from the compiled EXO1 RDS files in the Webster BEGI project
# DO events are delineated and their ROC and AUC are calculated
# fdom is converted to DOC and delineated

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)
library(readxl)
library(ggplot2)
library(patchwork)

#### Import compiled EXO1 RDS file ####
BEGI_EXO.or2 = readRDS("EXO_compiled/BEGI_EXO.or2.rds")

#Changing BEGI_EXO.or2 to EXOz.tc to include temp corrected fDOM data in each dataframe
EXOz.tc = readRDS("EXO_compiled/BEGI_EXOz.tc.rds")

#correct negative DO values
EXOz.tc[["VDOW"]]$ODO.mg.L.mn <- EXOz.tc[["VDOW"]]$ODO.mg.L.mn + 0.36
EXOz.tc[["VDOS"]]$ODO.mg.L.mn <- EXOz.tc[["VDOS"]]$ODO.mg.L.mn + 0.42
EXOz.tc[["SLOW"]]$ODO.mg.L.mn <- EXOz.tc[["SLOW"]]$ODO.mg.L.mn + 0.32
EXOz.tc[["SLOC"]]$ODO.mg.L.mn <- EXOz.tc[["SLOC"]]$ODO.mg.L.mn + 2.2

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

#### plot to check ####

#test data
#i<-date[1]

## SLOC 24 h ##
for (i in c(1:length(date))) {
  dz = date[i]
  tempdat = BEGI_EXO.or2[["SLOC"]][BEGI_EXO.or2[["SLOC"]]$datetimeMT < (as.POSIXct(dz, "00:00:01 MDT") +(60*60*24))&
                                   BEGI_EXO.or2[["SLOC"]]$datetimeMT > as.POSIXct(dz,"00:00:01 MDT"),]

#save plot 
file_name = paste("plots/delineations/SLOC/SLOC_", dz, ".pdf", sep="")
pdf(file_name)

par(mfrow=c(2,1))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,80))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

dev.off()
}

## SLOW 24 h ##
for (i in c(1:length(date))) {
  dz = date[i]
  tempdat = BEGI_EXO.or2[["SLOW"]][BEGI_EXO.or2[["SLOW"]]$datetimeMT < (as.POSIXct(dz, "00:00:01 MDT") +(60*60*24))&
                                     BEGI_EXO.or2[["SLOW"]]$datetimeMT > as.POSIXct(dz,"00:00:01 MDT"),]
  
  #save plot 
  file_name = paste("plots/delineations/SLOW/SLOW_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v=as.POSIXct(service.SLOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,80))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v=as.POSIXct(service.SLOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

## VDOW 24 h ##
for (i in c(1:length(date))) {
  dz = date[i]
  tempdat = BEGI_EXO.or2[["VDOW"]][BEGI_EXO.or2[["VDOW"]]$datetimeMT < (as.POSIXct(dz, "00:00:01 MDT") +(60*60*24))&
                                     BEGI_EXO.or2[["VDOW"]]$datetimeMT > as.POSIXct(dz,"00:00:01 MDT"),]
  
  #save plot 
  file_name = paste("plots/delineations/VDOW/VDOW_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,100))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

## VDOS 24 h ##
for (i in c(1:length(date))) {
  dz = date[i]
  tempdat = BEGI_EXO.or2[["VDOS"]][BEGI_EXO.or2[["VDOS"]]$datetimeMT < (as.POSIXct(dz, "00:00:01 MDT") +(60*60*24))&
                                     BEGI_EXO.or2[["VDOS"]]$datetimeMT > as.POSIXct(dz,"00:00:01 MDT"),]
  
  #save plot 
  file_name = paste("plots/delineations/VDOS/VDOS_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v=as.POSIXct(service.VDOS$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v=as.POSIXct(service.VDOS$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

### event dates ####

SLOC_events <- c("2023-10-09","2023-10-10","2023-10-11","2023-10-16","2023-10-17","2023-10-18","2023-11-18","2023-11-21","2023-11-26","2023-12-06","2023-12-18","2023-12-23","2023-12-24","2023-12-25","2023-12-29","2024-01-11","2024-01-12","2024-01-19","2024-01-21","2024-01-22","2024-01-23","2024-01-24","2024-02-10","2024-02-21","2024-03-09","2024-03-17","2024-03-20","2024-04-01","2024-04-02","2024-04-14","2024-04-15","2024-05-23","2024-07-04","2024-07-05","2024-07-13","2024-07-17","2024-08-19","2024-08-26","2024-08-27","2024-08-29","2024-08-30")
SLOC_events <- as.Date(SLOC_events)
#metabolism
SLOC_mx <- c("2023-10-09","2023-10-10","2023-10-11","2023-11-26","2023-12-06","2023-12-18","2023-12-23","2023-12-24","2023-12-25","2024-01-19","2024-01-21","2024-01-22","2024-01-23","2024-01-24","2024-02-10","2024-02-21","2024-03-20","2024-04-14","2024-04-15","2024-05-23","2024-07-17")
SLOC_mx <- as.Date(SLOC_mx)
#lateral transfer
SLOC_lt <- c("2024-01-11","2024-01-12","2024-03-09","2024-03-17","2024-04-01","2024-04-02","2024-07-04","2024-07-05","2024-07-13","2024-08-26","2024-08-27","2024-08-29","2024-08-30")
SLOC_lt <- as.Date(SLOC_lt)
#other
SLOC_other <- c("2023-10-16","2023-10-17","2023-10-18","2023-11-18","2023-11-21","2023-12-29","2024-08-19")
SLOC_other <- as.Date(SLOC_other)

SLOW_events <- c("2023-09-20","2023-10-16","2023-11-21","2023-12-18","2023-12-25","2023-12-26","2023-12-27","2023-12-28","2023-12-29","2024-01-19","2024-02-10","2024-02-11","2024-02-21","2024-03-20","2024-04-24","2024-04-25","2024-04-26","2024-04-27","2024-04-28","2024-05-14","2024-06-30","2024-07-13","2024-07-17","2024-07-22","2024-08-19")
SLOW_events <- as.Date(SLOW_events)
SLOW_mx <- c("2023-10-16","2023-11-21","2023-12-18","2023-12-25","2023-12-26","2023-12-27","2023-12-28","2023-12-29","2024-01-19","2024-02-21","2024-04-24","2024-04-25","2024-04-26","2024-04-27","2024-04-28","2024-07-17","2024-08-19")
SLOW_mx <- as.Date(SLOW_mx)
SLOW_lt <- c("2024-02-10","2024-02-11","2024-06-30","2024-07-13","2024-07-22")
SLOW_lt <- as.Date(SLOW_lt)
SLOW_other <- c("2023-09-20","2024-03-20","2024-05-14")
SLOW_other <- as.Date(SLOW_other)

VDOS_events <- c("2023-09-21","2023-09-22","2023-10-10","2023-10-16","2023-10-25","2023-10-26","2023-10-28","2023-10-29","2023-11-03","2023-11-04","2023-11-04","2023-11-11","2023-11-12","2023-11-13","2023-11-14","2023-11-15","2023-11-16","2023-11-17","2023-11-25","2023-11-27","2023-11-29","2023-11-30","2024-02-21","2024-04-17","2024-05-23","2024-06-12","2024-07-10","2024-07-11","2024-07-12","2024-07-13","2024-07-14","2024-07-16","2024-07-17","2024-07-18","2024-08-07","2024-08-08","2024-08-09","2024-08-10","2024-08-11","2024-08-12","2024-08-19")
VDOS_events <- as.Date(VDOS_events)
VDOS_mx <- c("2023-10-16","2023-10-25","2023-10-26","2023-10-28","2023-10-29","2023-11-03","2023-11-04","2023-11-11","2023-11-12","2023-11-13","2023-11-14","2023-11-15","2023-11-16","2023-11-17","2023-11-25","2023-11-30","2024-02-21","2024-04-17","2024-05-23","2024-06-12","2024-07-10","2024-07-11","2024-07-12","2024-07-13","2024-07-14","2024-07-16","2024-07-17","2024-07-18","2024-08-07","2024-08-08","2024-08-09","2024-08-10","2024-08-11","2024-08-12","2024-08-19")
VDOS_mx <- as.Date(VDOS_mx)
VDOS_lt <- c()
VDOS_lt <- as.Date(VDOS_lt)
VDOS_other <- c("2023-09-21","2023-09-22","2023-11-27","2023-11-29")
VDOS_other <- as.Date(VDOS_other)

VDOW_events <- c("2023-09-17","2023-09-18","2023-09-19","2023-09-30","2023-10-01","2023-10-02","2023-10-03","2023-10-04","2023-10-05","2023-10-06","2023-10-07","2023-10-11","2023-10-16","2023-10-17","2023-10-17","2023-11-02","2023-11-03","2023-11-04","2023-11-05","2023-11-06","2023-11-18","2023-11-19","2023-11-20","2023-11-21","2023-11-22","2023-11-27","2023-11-28","2023-11-29","2023-12-04","2023-12-05","2023-12-18","2023-12-19","2024-02-21","2024-02-22","2024-02-23","2024-02-24","2024-02-25","2024-02-26","2024-02-27","2024-02-28","2024-02-29","2024-02-21","2024-03-06","2024-03-07","2024-03-08","2024-03-09","2024-03-10","2024-03-20","2024-03-21","2024-03-22","2024-04-17","2024-05-14","2024-05-15","2024-05-23","2024-05-24","2024-06-17","2024-06-18","2024-06-19","2024-07-17","2024-07-18","2024-08-19","2024-08-20")
VDOW_events <- as.Date(VDOW_events)
VDOW_mx <- c("2023-09-17","2023-09-18","2023-09-19","2023-10-16","2023-10-17","2023-11-02","2023-11-03","2023-11-04","2023-11-05","2023-11-06","2023-11-20","2023-11-21","2023-11-22","2023-11-27","2023-11-28","2023-11-29","2023-12-04","2023-12-05","2023-12-18","2023-12-19","2024-02-21","2024-02-22","2024-02-23","2024-02-24","2024-02-25","2024-02-26","2024-02-27","2024-02-28","2024-02-29","2024-03-06","2024-03-07","2024-03-08","2024-03-09","2024-03-10","2024-03-20","2024-03-21","2024-03-22","2024-06-17","2024-06-18","2024-06-19","2024-07-17","2024-07-18","2024-08-19","2024-08-20")
VDOW_mx <- as.Date(VDOW_mx)
VDOW_lt <- c()
VDOW_lt <- as.Date(VDOW_lt)
VDOW_other <- c("2023-09-30","2023-10-01","2023-10-02","2023-10-03","2023-10-04","2023-10-05","2023-10-06","2023-10-07","2023-10-11","2023-11-18","2023-11-19","2024-04-17","2024-05-14","2024-05-15","2024-05-23","2024-05-24")
VDOW_other <- as.Date(VDOW_other)
#pulse of fDOM and turbidity in VDOW 11/2023
#It seems like fDOM starts to decrease significantly over several days after a sonde event. It makes me think that a sonde event introduces enough oxygen into the wells (which wouldn't show up as DO) to spur aerobic respiration/DOC consumption


#### Plotting just the events ####

#SLOC#
for (i in c(1:length(SLOC_events))) {
  dz = SLOC_events[i]
  tempdat = BEGI_EXO.or2[["SLOC"]][BEGI_EXO.or2[["SLOC"]]$datetimeMT < (as.POSIXct(dz, "00:00:01 MDT") +(60*60*24))&
                                     BEGI_EXO.or2[["SLOC"]]$datetimeMT > as.POSIXct(dz,"00:00:01 MDT"),]
  
  #save plot 
  file_name = paste("plots/delineations/SLOC/events/SLOC_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  # Define the range of datetime values
  start_time <- min(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  end_time <- max(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  
  # Create a sequence of 15-minute intervals
  intervals_15min <- seq(from = start_time, to = end_time, by = "15 min")
  
  # Create a sequence of hourly intervals
  hour_intervals <- seq(from = start_time, to = end_time, by = "1 hour")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

#SLOW#
for (i in c(1:length(SLOW_events))) {
  dz = SLOW_events[i]
  tempdat = BEGI_EXO.or2[["SLOW"]][BEGI_EXO.or2[["SLOW"]]$datetimeMT < (as.POSIXct(dz, "00:00:01 MDT") +(60*60*24))&
                                     BEGI_EXO.or2[["SLOW"]]$datetimeMT > as.POSIXct(dz,"00:00:01 MDT"),]
  
  #save plot 
  file_name = paste("plots/delineations/SLOW/events/SLOW_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  # Define the range of datetime values
  start_time <- min(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  end_time <- max(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  
  # Create a sequence of 15-minute intervals
  intervals_15min <- seq(from = start_time, to = end_time, by = "15 min")
  
  # Create a sequence of hourly intervals
  hour_intervals <- seq(from = start_time, to = end_time, by = "1 hour")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.SLOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.SLOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

#VDOW#
for (i in c(1:length(VDOW_events))) {
  dz = VDOW_events[i]
  tempdat = BEGI_EXO.or2[["VDOW"]][BEGI_EXO.or2[["VDOW"]]$datetimeMT < (as.POSIXct(dz, "00:00:01 MDT") +(60*60*24))&
                                     BEGI_EXO.or2[["VDOW"]]$datetimeMT > as.POSIXct(dz,"00:00:01 MDT"),]
  
  #save plot 
  file_name = paste("plots/delineations/VDOW/events/VDOW_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  # Define the range of datetime values
  start_time <- min(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  end_time <- max(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  
  # Create a sequence of 15-minute intervals
  intervals_15min <- seq(from = start_time, to = end_time, by = "15 min")
  
  # Create a sequence of hourly intervals
  hour_intervals <- seq(from = start_time, to = end_time, by = "1 hour")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

# VDOS #
for (i in c(1:length(VDOS_events))) {
  dz = VDOS_events[i]
  tempdat = BEGI_EXO.or2[["VDOS"]][BEGI_EXO.or2[["VDOS"]]$datetimeMT < (as.POSIXct(dz, "00:00:01 MDT") +(60*60*24))&
                                     BEGI_EXO.or2[["VDOS"]]$datetimeMT > as.POSIXct(dz,"00:00:01 MDT"),]
  
  #save plot 
  file_name = paste("plots/delineations/VDOS/events/VDOS_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(7,1))
  
  # Define the range of datetime values
  start_time <- min(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  end_time <- max(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  
  # Create a sequence of 15-minute intervals
  intervals_15min <- seq(from = start_time, to = end_time, by = "15 min")
  
  # Create a sequence of hourly intervals
  hour_intervals <- seq(from = start_time, to = end_time, by = "1 hour")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.VDOS$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.VDOS$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="fDOM (QSU)")
  
  dev.off()
}




#### Adding event column to dataframe ####
#for every row where event is happening, and column of event code to categorize
#add column event_type to each well dataframe based on conditions (the date) %in%
#add column value to r based on condition


### SLOC ### 
EXOz.tc[["SLOC"]]$event <- with (EXOz.tc[["SLOC"]], ifelse(as.Date(EXOz.tc[["SLOC"]]$datetimeMT) %in% SLOC_mx, 'metabolism',
                                                                     ifelse(as.Date(EXOz.tc[["SLOC"]]$datetimeMT) %in% SLOC_lt, 'lateral transfer',
                                                                            ifelse(as.Date(EXOz.tc[["SLOC"]]$datetimeMT) %in% SLOC_other, 'other', 'NA'))))

### SLOW ###
EXOz.tc[["SLOW"]]$event <- with (EXOz.tc[["SLOW"]], ifelse(as.Date(EXOz.tc[["SLOW"]]$datetimeMT) %in% SLOW_mx, 'metabolism',
                                                                     ifelse(as.Date(EXOz.tc[["SLOW"]]$datetimeMT) %in% SLOW_lt, 'lateral transfer',
                                                                            ifelse(as.Date(EXOz.tc[["SLOW"]]$datetimeMT) %in% SLOW_other, 'other', 'NA'))))

### VDOS ###
EXOz.tc[["VDOS"]]$event <- with (EXOz.tc[["VDOS"]], ifelse(as.Date(EXOz.tc[["VDOS"]]$datetimeMT) %in% VDOS_mx, 'metabolism',
                                                                     ifelse(as.Date(EXOz.tc[["VDOS"]]$datetimeMT) %in% VDOS_lt, 'lateral transfer',
                                                                            ifelse(as.Date(EXOz.tc[["VDOS"]]$datetimeMT) %in% VDOS_other, 'other', 'NA'))))


### VDOW ###
EXOz.tc[["VDOW"]]$event <- with (EXOz.tc[["VDOW"]], ifelse(as.Date(EXOz.tc[["VDOW"]]$datetimeMT) %in% VDOW_mx, 'metabolism',
                                                                     ifelse(as.Date(EXOz.tc[["VDOW"]]$datetimeMT) %in% VDOW_lt, 'lateral transfer',
                                                                            ifelse(as.Date(EXOz.tc[["VDOW"]]$datetimeMT) %in% VDOW_other, 'other', 'NA'))))


# add new column of start/end of each event
# new vector for each well, one for start one for end, add datetimeMT corresponding to each
# replot with multiple days, ask about Manuela's code for salt curves
# data frame of just events, open it and look for inflection points
# quantifying - do we want to get a difference or integrate under the curve, tbd
# DO will be integration under curve, DOC maybe difference

#### Creating dataframe of just the DO events ####

#SLOC#
#SLOC_DOx= BEGI_EXO.or2[["SLOC"]][BEGI_EXO.or2[["SLOC"]]$datetimeMT > as.POSIXct("YYYY-MM-DD HH:MM:00",tz= "US/Mountain")
#             &BEGI_EXO.or2[["SLOC"]]$datetimeMT < as.POSIXct("YYYY-MM-DD HH:MM:00",tz= "US/Mountain"),]


#1st event
SLOC_DO1= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-08 22:45:00",tz= "US/Mountain")
              &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-09 00:45:00",tz= "US/Mountain"),]

#2nd event
SLOC_DO2= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-09 22:00:00",tz= "US/Mountain")
              &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-10 02:15:00",tz= "US/Mountain"),]

#3rd event
SLOC_DO3= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-10 22:15:00",tz= "US/Mountain")
              &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-11 01:15:00",tz= "US/Mountain"),]

#4th event
SLOC_DO4= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-16 14:15:00",tz= "US/Mountain")
             &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-16 19:15:00",tz= "US/Mountain"),]

#5th event
SLOC_DO5= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-17 11:00:00",tz= "US/Mountain")
              &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-18 02:15:00",tz= "US/Mountain"),]

#6th event
SLOC_DO6= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-11-17 20:15:00",tz= "US/Mountain")
              &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-11-17 20:45:00",tz= "US/Mountain"),]

#7th event
SLOC_DO7= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-11-21 16:15:00",tz= "US/Mountain")
              &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-11-21 16:45:00",tz= "US/Mountain"),]

#8th event
SLOC_DO8= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-12-05 17:15:00",tz= "US/Mountain")
              &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-12-05 17:45:00",tz= "US/Mountain"),]

#9th event
SLOC_DO9= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-12-18 11:00:00",tz= "US/Mountain")
             &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-12-18 12:00:00",tz= "US/Mountain"),]

#10th event
SLOC_DO10= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-12-28 23:30:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-12-29 02:15:00",tz= "US/Mountain"),]

#11th event
SLOC_DO11= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-19 12:30:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-19 13:00:00",tz= "US/Mountain"),]

#12th event
SLOC_DO12= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-20 18:15:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-20 21:00:00",tz= "US/Mountain"),]

#13th event
SLOC_DO13= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-21 07:45:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-24 14:45:00",tz= "US/Mountain"),]

#14th event
SLOC_DO14= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-02-10 15:00:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-02-10 17:15:00",tz= "US/Mountain"),]

#15th event
SLOC_DO15= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-02-21 10:45:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-02-21 12:00:00",tz= "US/Mountain"),]

#16th event
SLOC_DO16= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-3-20 10:00:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-03-20 14:30:00",tz= "US/Mountain"),]

#17th event
SLOC_DO17= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-04-14 06:45:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-04-14 12:15:00",tz= "US/Mountain"),]

#18th event
SLOC_DO18= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-04-15 03:30:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-04-15 12:15:00",tz= "US/Mountain"),]

#19th event
SLOC_DO19= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-05-23 10:45:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-05-23 12:30:00",tz= "US/Mountain"),]

#20th event
SLOC_DO20= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-07-17 14:00:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-07-17 15:15:00",tz= "US/Mountain"),]

#21st event
SLOC_DO21= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-08-19 10:15:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-08-19 11:15:00",tz= "US/Mountain"),]

SLOC_DO <-list(SLOC_DO1,SLOC_DO2,SLOC_DO3,SLOC_DO4,SLOC_DO5,SLOC_DO6,SLOC_DO7,SLOC_DO8,SLOC_DO9,SLOC_DO10,
            SLOC_DO11,SLOC_DO12,SLOC_DO13,SLOC_DO14,SLOC_DO15,SLOC_DO16,SLOC_DO17,SLOC_DO18,SLOC_DO19,SLOC_DO20,
            SLOC_DO21)
names(SLOC_DO)<-c('SLOC_DO1','SLOC_DO2','SLOC_DO3','SLOC_DO4','SLOC_DO5','SLOC_DO6','SLOC_DO7','SLOC_DO8','SLOC_DO9','SLOC_DO10',
                  'SLOC_DO11','SLOC_DO12','SLOC_DO13','SLOC_DO14','SLOC_DO15','SLOC_DO16','SLOC_DO17','SLOC_DO18','SLOC_DO19','SLOC_DO20',
                  'SLOC_DO21')

SLOC_dates <- as.POSIXct(character(0))
for (i in seq_along(SLOC_DO)){
  temptime <- as.POSIXct(SLOC_DO[[i]]$datetimeMT[1])
  SLOC_dates<-c(SLOC_dates,temptime)
}


#SLOW#

#1st event
SLOW_DO1= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2023-09-20 10:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2023-09-20 12:15:00",tz= "US/Mountain"),]

#2nd event
SLOW_DO2= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2023-10-16 14:30:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2023-10-16 15:15:00",tz= "US/Mountain"),]

#3rd event
SLOW_DO3= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2023-11-21 16:30:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2023-11-21 17:15:00",tz= "US/Mountain"),]

#4th event
SLOW_DO4= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2023-12-18 10:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2023-12-18 11:15:00",tz= "US/Mountain"),]

#5th event
SLOW_DO5= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-01-19 12:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-01-19 13:45:00",tz= "US/Mountain"),]

#6th event
SLOW_DO6= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-02-21 10:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-02-21 11:30:00",tz= "US/Mountain"),]

#7th event
SLOW_DO7= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-03-20 10:00:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-03-20 10:45:00",tz= "US/Mountain"),]

#8th event
SLOW_DO8= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-05-14 14:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-05-14 17:30:00",tz= "US/Mountain"),]

#9th event
SLOW_DO9= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-07-17 14:00:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-07-17 14:30:00",tz= "US/Mountain"),]

#10th event
SLOW_DO10= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-08-19 10:30:00",tz= "US/Mountain")
                                 &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-08-19 17:15:00",tz= "US/Mountain"),]

SLOW_DO<-list(SLOW_DO1,SLOW_DO2,SLOW_DO3,SLOW_DO4,SLOW_DO5,SLOW_DO6,SLOW_DO7,SLOW_DO8,SLOW_DO9,SLOW_DO10)
names(SLOW_DO)<-c('SLOW_DO1','SLOW_DO2','SLOW_DO3','SLOW_DO4','SLOW_DO5','SLOW_DO6','SLOW_DO7','SLOW_DO8','SLOW_DO9','SLOW_DO10')

SLOW_dates <- as.POSIXct(character(0))
for (i in seq_along(SLOW_DO)){
  temptime <- as.POSIXct(SLOW_DO[[i]]$datetimeMT[1])
  SLOW_dates<-c(SLOW_dates,temptime)
}


#VDOW#

#1st event
VDOW_DO1= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-03 11:30:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-03 12:45:00",tz= "US/Mountain"),]

#2nd event
VDOW_DO2= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-04 12:00:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-04 12:30:00",tz= "US/Mountain"),]

#3rd event
VDOW_DO3= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-04 16:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-04 21:30:00",tz= "US/Mountain"),]

#4th event
VDOW_DO4= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-05 13:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-05 21:15:00",tz= "US/Mountain"),]

#5th event
VDOW_DO5= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-16 14:00:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-16 15:15:00",tz= "US/Mountain"),]

#6th event
VDOW_DO6= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-20 14:30:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-20 17:15:00",tz= "US/Mountain"),]

#7th event
VDOW_DO7= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-27 09:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-27 13:15:00",tz= "US/Mountain"),]

#8th event
VDOW_DO8= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-28 10:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-28 13:15:00",tz= "US/Mountain"),]

#9th event
VDOW_DO9= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-29 10:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-29 12:00:00",tz= "US/Mountain"),]

#10th event
VDOW_DO10= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-12-18 09:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-12-18 11:00:00",tz= "US/Mountain"),]

#11th event
VDOW_DO11= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-02-21 13:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-02-21 15:30:00",tz= "US/Mountain"),]

#12th event
VDOW_DO12= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-03-20 09:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-03-20 13:15:00",tz= "US/Mountain"),]

#13th event
VDOW_DO13= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-04-17 10:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-04-17 18:15:00",tz= "US/Mountain"),]

#14th event
VDOW_DO14= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-05-14 13:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-05-15 01:30:00",tz= "US/Mountain"),]

#15th event
VDOW_DO15= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-05-23 10:00:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-05-23 12:45:00",tz= "US/Mountain"),]

#16th event
VDOW_DO16= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-06-17 12:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-06-17 14:15:00",tz= "US/Mountain"),]

#17th event
VDOW_DO17= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-07-17 13:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-07-17 21:15:00",tz= "US/Mountain"),]

#18th event
VDOW_DO18= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-08-19 09:30:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-08-19 10:45:00",tz= "US/Mountain"),]

VDOW_DO<-list(VDOW_DO1,VDOW_DO2,VDOW_DO3,VDOW_DO4,VDOW_DO5,VDOW_DO6,VDOW_DO7,VDOW_DO8,VDOW_DO9,VDOW_DO10,
           VDOW_DO11,VDOW_DO12,VDOW_DO13,VDOW_DO14,VDOW_DO15,VDOW_DO16,VDOW_DO17,VDOW_DO18)
names(VDOW_DO)<-c('VDOW_DO1','VDOW_DO2','VDOW_DO3','VDOW_DO4','VDOW_DO5','VDOW_DO6','VDOW_DO7','VDOW_DO8','VDOW_DO9','VDOW_DO10',
                  'VDOW_DO11','VDOW_DO12','VDOW_DO13','VDOW_DO14','VDOW_DO15','VDOW_DO16','VDOW_DO17','VDOW_DO18')

VDOW_dates <- as.POSIXct(character(0))
for (i in seq_along(VDOW_DO)){
  temptime <- as.POSIXct(VDOW_DO[[i]]$datetimeMT[1])
  VDOW_dates<-c(VDOW_dates,temptime)
}


#VDOS#

#1st event
VDOS_DO1= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-10-15 23:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-10-16 18:15:00",tz= "US/Mountain"),]

#2nd event
VDOS_DO2= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-27 09:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-27 14:30:00",tz= "US/Mountain"),]

#3rd event
VDOS_DO3= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-29 10:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-29 13:00:00",tz= "US/Mountain"),]

#4th event
VDOS_DO4= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-30 10:30:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-30 11:00:00",tz= "US/Mountain"),]

#5th event
VDOS_DO5= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-02-21 13:30:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-02-21 17:15:00",tz= "US/Mountain"),]

#6th event
VDOS_DO6= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-04-17 10:00:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-04-17 12:15:00",tz= "US/Mountain"),]

#7th event
VDOS_DO7= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-05-23 10:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-05-23 17:15:00",tz= "US/Mountain"),]

#8th event
VDOS_DO8= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-07-16 10:00:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-07-16 14:15:00",tz= "US/Mountain"),]

#9th event
VDOS_DO9= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-07-17 13:15:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-07-17 14:00:00",tz= "US/Mountain"),]

#10th event
VDOS_DO10= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-08-19 09:45:00",tz= "US/Mountain")
                                 &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-08-19 11:30:00",tz= "US/Mountain"),]

VDOS_DO<-list(VDOS_DO1,VDOS_DO2,VDOS_DO3,VDOS_DO4,VDOS_DO5,VDOS_DO6,VDOS_DO7,VDOS_DO8,VDOS_DO9,VDOS_DO10)
names(VDOS_DO)<-c('VDOS_DO1','VDOS_DO2','VDOS_DO3','VDOS_DO4','VDOS_DO5','VDOS_DO6','VDOS_DO7','VDOS_DO8','VDOS_DO9','VDOS_DO10')

VDOS_dates <- as.POSIXct(character(0))
for (i in seq_along(VDOS_DO)){
  temptime <- as.POSIXct(VDOS_DO[[i]]$datetimeMT[1])
  VDOS_dates<-c(VDOS_dates,temptime)
}

#### DO area under curve ####

#SLOC#

SLOC_AUC_results <- vector("numeric", length(SLOC_DO))

for (i in seq_along(SLOC_DO)) {
  SLOC_AUC_results[i] <- AUC(
    x = SLOC_DO[[i]]$datetimeMT, 
    y = SLOC_DO[[i]]$ODO.mg.L.mn, 
    method = "trapezoid",
    na.rm = FALSE
  )
}

view(SLOC_AUC_results)


#SLOW#

SLOW_AUC_results <- vector("numeric", length(SLOW_DO))

for (i in seq_along(SLOW_DO)) {
  SLOW_AUC_results[i] <- AUC(
    x = SLOW_DO[[i]]$datetimeMT, 
    y = SLOW_DO[[i]]$ODO.mg.L.mn, 
    method = "trapezoid",
    na.rm = FALSE
  )
}

view(SLOW_AUC_results)


#VDOW#

VDOW_AUC_results <- vector("numeric", length(VDOW_DO))

for (i in seq_along(VDOW_DO)) {
  VDOW_AUC_results[i] <- AUC(
    x = VDOW_DO[[i]]$datetimeMT, 
    y = VDOW_DO[[i]]$ODO.mg.L.mn, 
    method = "trapezoid",
    na.rm = FALSE
  )
}

view(VDOW_AUC_results)


#VDOS#

VDOS_AUC_results <- vector("numeric", length(VDOS_DO))


for (i in seq_along(VDOS_DO)) {
  VDOS_AUC_results[i] <- AUC(
    x = VDOS_DO[[i]]$datetimeMT, 
    y = VDOS_DO[[i]]$ODO.mg.L.mn, 
    method = "trapezoid",
    na.rm = FALSE
  )
}

view(VDOS_AUC_results)


#### DO Boxplots auc ####

DO_AUC<-data.frame(DO=c(SLOC_AUC_results,SLOW_AUC_results,VDOW_AUC_results,VDOS_AUC_results),
                      Well=rep(c("SLOC","SLOW","VDOW","VDOS"),
                               times=c(length(SLOC_AUC_results),length(SLOW_AUC_results),length(VDOW_AUC_results),length(VDOS_AUC_results))))
DO_AUC$Event<-c('SLOC_DO1','SLOC_DO2','SLOC_DO3','SLOC_DO4','SLOC_DO5','SLOC_DO6','SLOC_DO7','SLOC_DO8','SLOC_DO9','SLOC_DO10',
                'SLOC_DO11','SLOC_DO12','SLOC_DO13','SLOC_DO14','SLOC_DO15','SLOC_DO16','SLOC_DO17','SLOC_DO18','SLOC_DO19','SLOC_DO20',
                'SLOC_DO21','SLOW_DO1','SLOW_DO2','SLOW_DO3','SLOW_DO4','SLOW_DO5','SLOW_DO6','SLOW_DO7','SLOW_DO8','SLOW_DO9','SLOW_DO10',
                'VDOW_DO1','VDOW_DO2','VDOW_DO3','VDOW_DO4','VDOW_DO5','VDOW_DO6','VDOW_DO7','VDOW_DO8','VDOW_DO9','VDOW_DO10',
                'VDOW_DO11','VDOW_DO12','VDOW_DO13','VDOW_DO14','VDOW_DO15','VDOW_DO16','VDOW_DO17','VDOW_DO18',
                'VDOS_DO1','VDOS_DO2','VDOS_DO3','VDOS_DO4','VDOS_DO5','VDOS_DO6','VDOS_DO7','VDOS_DO8','VDOS_DO9','VDOS_DO10')
DO_AUC$Eventdate <-c(SLOC_dates,SLOW_dates,VDOW_dates,VDOS_dates) 

DO_AUC$DO_log <- log(DO_AUC$DO)

saveRDS(DO_AUC,"EXO_compiled/DO_AUC.rds")  
  
  
DO_AUC_bp<-ggplot(data=DO_AUC,mapping=aes(x=Well, y=DO))+geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+labs(y = "Respiration Event Size (DO Consumption)")
print(DO_AUC_bp)

#removed outliers from plot
DO_AUC_no<-ggplot(data=DO_AUC,mapping=aes(x=Well, y=DO))+geom_boxplot(outlier.shape=NA,fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+coord_cartesian(ylim=c(0,1250))+labs(y = "Respiration Event Size (DO Consumption)")
print(DO_AUC_no)

#log scale
DO_AUC_log<-ggplot(data=DO_AUC,mapping=aes(x=Well, y=DO_log))+geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+labs(y = "Respiration Event Size (log DO Consumption)")
print(DO_AUC_log)

#### DO negative rates of change ####

#SLOC#
SLOC_rates <- list()
SLOC_avg_rate <- numeric()

for (i in seq_along(SLOC_DO)) {
  events <- SLOC_DO[[i]]
  events <- events %>%
    mutate(
      rate_of_change = c(NA, diff(ODO.mg.L.mn)),
      negative_rate_of_change = ifelse(rate_of_change < 0, rate_of_change, NA)
    )
 
  SLOC_rates[[i]] <- events
  avg_neg_rate <- mean(events$negative_rate_of_change, na.rm = TRUE)
  SLOC_avg_rate <- c(SLOC_avg_rate, avg_neg_rate)
}

names(SLOC_rates) <- names(SLOC_DO)
names(SLOC_avg_rate) <- names(SLOC_DO)
View(SLOC_rates)
print(SLOC_avg_rate)


#SLOW#
SLOW_rates <- list()
SLOW_avg_rate <- numeric()

for (i in seq_along(SLOW_DO)) {
  events <- SLOW_DO[[i]]
  events <- events %>%
    mutate(
      rate_of_change = c(NA, diff(ODO.mg.L.mn)),
      negative_rate_of_change = ifelse(rate_of_change < 0, rate_of_change, NA)
    )
  
  SLOW_rates[[i]] <- events
  avg_neg_rate <- mean(events$negative_rate_of_change, na.rm = TRUE)
  SLOW_avg_rate <- c(SLOW_avg_rate, avg_neg_rate)
}

names(SLOW_rates) <- names(SLOW_DO)
names(SLOW_avg_rate) <- names(SLOW_DO)
View(SLOW_rates)
print(SLOW_avg_rate)


#VDOW#
VDOW_rates <- list()
VDOW_avg_rate <- numeric()

for (i in seq_along(VDOW_DO)) {
  events <- VDOW_DO[[i]]
  events <- events %>%
    mutate(
      rate_of_change = c(NA, diff(ODO.mg.L.mn)),
      negative_rate_of_change = ifelse(rate_of_change < 0, rate_of_change, NA)
    )
  
  VDOW_rates[[i]] <- events
  avg_neg_rate <- mean(events$negative_rate_of_change, na.rm = TRUE)
  VDOW_avg_rate <- c(VDOW_avg_rate, avg_neg_rate)
}

names(VDOW_rates) <- names(VDOW_DO)
names(VDOW_avg_rate) <- names(VDOW_DO)
View(VDOW_rates)
print(VDOW_avg_rate)


#VDOS#
VDOS_rates <- list()
VDOS_avg_rate <- numeric()

for (i in seq_along(VDOS_DO)) {
  events <- VDOS_DO[[i]]
  events <- events %>%
    mutate(
      rate_of_change = c(NA, diff(ODO.mg.L.mn)),
      negative_rate_of_change = ifelse(rate_of_change < 0, rate_of_change, NA)
    )
  
  VDOS_rates[[i]] <- events
  avg_neg_rate <- mean(events$negative_rate_of_change, na.rm = TRUE)
  VDOS_avg_rate <- c(VDOS_avg_rate, avg_neg_rate)
}

names(VDOS_rates) <- names(VDOS_DO)
names(VDOS_avg_rate) <- names(VDOS_DO)
View(VDOS_rates)
print(VDOS_avg_rate)

#save compiled RDS of each event rate of change#
roc_all<-list(SLOC_rates,SLOW_rates,VDOW_rates,VDOS_rates)
names(roc_all)<-c("SLOC_rates","SLOW_rates","VDOW_rates","VDOS_rates")

saveRDS(roc_all,"EXO_compiled/roc_all.rds")
# dataframe of negative roc and average roc #

#### DO boxplots negative roc ####
DO_roc<-data.frame(DO=c(SLOC_avg_rate,SLOW_avg_rate,VDOW_avg_rate,VDOS_avg_rate),
                   Well=rep(c("SLOC","SLOW","VDOW","VDOS"),
                            times=c(length(SLOC_avg_rate),length(SLOW_avg_rate),length(VDOW_avg_rate),length(VDOS_avg_rate))))
DO_roc$Eventdate <-c(SLOC_dates,SLOW_dates,VDOW_dates,VDOS_dates)

saveRDS(DO_roc,"EXO_compiled/DO_roc.rds")

DO_roc_bp<-ggplot(data=DO_roc, mapping=aes(x=Well, y=DO))+geom_boxplot()
print(DO_roc_bp)

#### fDOM to DOC calibration ####
#Match DOC grab sample values to fDOM (make sure you use the last fDOM value before the well was disturbed for sampling!)
#Make a linear regression between fDOM and DOC for each well (see water level script for an example if you need it)
#Calculate R-sq
#Use linear equation to convert all fDOM values to DOC
#options to look at fdom daily mean, fdom values preceeding each well servicing, and first fdom after each well servicing (with post-service data removed)

#Import temp corrected fDOM data
EXOz.tc = readRDS("EXO_compiled/BEGI_EXOz.tc.rds")

#get DOC data from google drive
doc_tibble <- googledrive::as_id("https://drive.google.com/drive/folders/1zdzsIXO5LIzbcg2RzfE4mz3dBKvBmrO-")

doc <- googledrive::drive_ls(path = doc_tibble, type = "xlsx")
2

googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2025-01-22_BEGI_rerun.xlsx"], 
                            path = "NPOC-TN_2025-01-22_BEGI_rerun.xlsx",
                            overwrite = T)
docdata1 <- read_xlsx("NPOC-TN_2025-01-22_BEGI_rerun.xlsx", sheet = 9, skip = 1)

googledrive::drive_download(file = doc$id[doc$name=="240620_BEGI_Data.xlsx"], 
                            path = "240620_BEGI_Data.xlsx",
                            overwrite = T)
docdata2 <- read_xlsx("240620_BEGI_Data.xlsx")

#clean up
docdata1 <- select(docdata1,-6:-8)
names(docdata1)[names(docdata1) == '...1'] <- 'date'
names(docdata1)[names(docdata1) == '...2'] <- 'WellID'
names(docdata1)[names(docdata1) == '...3'] <- 'Sample'

names(docdata2)[names(docdata2) == 'Collection Date'] <- 'date'
names(docdata2)[names(docdata2) == 'NPOC (mg C/L)'] <- 'NPOC'
names(docdata2)[names(docdata2) == 'TDN (mg N/L)'] <- 'TN'

#stitch
docdata <- merge(docdata1, docdata2, all = TRUE)
rm(docdata1, docdata2)
#filter by well
docdata <- docdata %>%
  spread (WellID, NPOC)
docdata$date <- as.Date(docdata$date)

#read in servicing data#
# read in file and filter to EXO1 removal and deployments
service = readxl::read_excel("googledrive/sensor_event_log.xlsx")
service = service[service$model=="EXO1",]
service = service[service$observation=="removed" | service$observation=="deployed",]

# format date and time
service$datetime = paste(service$date,  service$time, sep = " ")
# convert to POIXct and set timezone
service$datetimeMT<-as.POSIXct(service$datetime, 
                               format = "%Y-%m-%d %H:%M",
                               tz="US/Mountain")
service$date = as.Date(service$date)

# remove rows with no exact times
servicetimes = service[!is.na(service$datetimeMT),]

# service dates

service.VDOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOW"]
service.VDOS = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOS"]
service.SLOC = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOC"]
service.SLOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOW"]

# filter docdata to df of each well
docVDOW <- data.frame(docdata$date,
                      docdata$Sample,
                      docdata$TN,
                      docdata$VDOW)
docVDOW <- na.omit(docVDOW)
docVDOW <- docVDOW[-1,] #no 9/15 measurements

docVDOS <- data.frame(docdata$date,
                      docdata$Sample,
                      docdata$TN,
                      docdata$VDOS)
docVDOS <- na.omit(docVDOS)
docVDOS <- docVDOS[-1,] 

docSLOC <- data.frame(docdata$date,
                      docdata$Sample,
                      docdata$TN,
                      docdata$SLOC)
docSLOC <- na.omit(docSLOC)
docSLOC <- docSLOC[-1,] 

docSLOW <- data.frame(docdata$date,
                      docdata$Sample,
                      docdata$TN,
                      docdata$SLOW)
docSLOW <- na.omit(docSLOW)
docSLOW <- docSLOW[-1,]


#VDOW
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(EXOz.tc[["VDOW"]]$datetimeMT),
  date = as.Date(EXOz.tc[["VDOW"]]$datetimeMT),
  fDOM = EXOz.tc[["VDOW"]]$fDOM.QSU.mn.Tc)

#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)
#

#daily mean of VDOW fDOM
#daily_fDOM <- aggregate(fDOM ~ date, data = fDOM_df, FUN = mean)
#merged_VDOW <- merge(daily_fDOM, docdata[, c("date", "VDOW")], by = "date")
#merged_VDOW <- na.omit(merged_VDOW)
#plot(merged_VDOW$VDOW ~ merged_VDOW$fDOM)
#m.VDOW = lm(fDOM ~ VDOW, data = merged_VDOW)
#abline(m.VDOW, col = "blue", lwd = 2)
#summary(m.VDOW)
#cf <- coef(m.VDOW)
#Intercept <- cf[1]
#Slope <- cf[2]

# Index of last fDOM measurement before service datetime
prev_index <- findInterval(service.VDOW, fDOM_df$datetimeMT) -1
valid <- prev_index > 0

#Index of fDOM measurement AFTER service datetime (and after fdom measurements returned to "normal")
#should consider using T-corrected fdom data WITHOUT post-service data being removed
next_index <- findInterval(service.VDOW, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_service_time <- service.VDOW[valid]
matched_fDOM_time <- fDOM_df$datetimeMT[prev_index[valid]]
matched_fDOM_vals <- fDOM_df$fDOM[prev_index[valid]]

npoc_vals <- docVDOW$docdata.VDOW[valid]

merged_df <- data.frame(
  service_time = matched_service_time,
  fDOM_time = matched_fDOM_time,
  NPOC = npoc_vals,
  fDOM = matched_fDOM_vals
)

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (VDOW)",
     ylab = "fDOM (before sample)",
     main = "fDOM vs NPOC (preceeding fDOM measurement)")
m.VDOW <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.VDOW, col = "blue", lwd = 2)
summary(m.VDOW)


#VDOS
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(EXOz.tc[["VDOS"]]$datetimeMT),
  date = as.Date(EXOz.tc[["VDOS"]]$datetimeMT),
  fDOM = EXOz.tc[["VDOS"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)
#

# Index of last fDOM measurement before service datetime
prev_index <- findInterval(service.VDOS, fDOM_df$datetimeMT) -1
valid <- prev_index > 0 #or next_index

#Index of fDOM measurement AFTER service datetime (and after fdom measurements returned to "normal")
#should consider using T corrected fdom data WITHOUT post-service data being removed
next_index <- findInterval(service.VDOS, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_service_time <- service.VDOS[valid]
matched_fDOM_time <- fDOM_df$datetimeMT[prev_index[valid]]
matched_fDOM_vals <- fDOM_df$fDOM[prev_index[valid]]

npoc_vals <- docVDOS$docdata.VDOS[valid]

merged_df <- data.frame(
  service_time = matched_service_time,
  fDOM_time = matched_fDOM_time,
  NPOC = npoc_vals,
  fDOM = matched_fDOM_vals
)

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (VDOS)",
     ylab = "fDOM (before sample)",
     main = "fDOM vs NPOC (preceding fDOM measurement)")
m.VDOS <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.VDOS, col = "blue", lwd = 2)
summary(m.VDOS)

#SLOC
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(EXOz.tc[["SLOC"]]$datetimeMT),
  date = as.Date(EXOz.tc[["SLOC"]]$datetimeMT),
  fDOM = EXOz.tc[["SLOC"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)
#

# Index of last fDOM measurement before service datetime
prev_index <- findInterval(service.SLOC, fDOM_df$datetimeMT) -1
valid <- prev_index > 0

#Index of fDOM measurement AFTER service datetime (and after fdom measurements returned to "normal")
#should consider using T corrected fdom data WITHOUT post-service data being removed
next_index <- findInterval(service.SLOC, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_service_time <- service.SLOC[valid]
matched_fDOM_time <- fDOM_df$datetimeMT[prev_index[valid]]
matched_fDOM_vals <- fDOM_df$fDOM[prev_index[valid]]

npoc_vals <- docSLOC$docdata.SLOC[valid]

merged_df <- data.frame(
  service_time = matched_service_time,
  fDOM_time = matched_fDOM_time,
  NPOC = npoc_vals,
  fDOM = matched_fDOM_vals
)

#remove outlier to see if R2 improves. it doesn't..
#merged_df <- merged_df[-23,]

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (SLOC)",
     ylab = "fDOM (before sample)",
     main = "fDOM vs NPOC (preceding fDOM measurement)")
m.SLOC <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.SLOC, col = "blue", lwd = 2)
summary(m.SLOC)

#SLOW
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(EXOz.tc[["SLOW"]]$datetimeMT),
  date = as.Date(EXOz.tc[["SLOW"]]$datetimeMT),
  fDOM = EXOz.tc[["SLOW"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)
#

# Index of last fDOM measurement before service datetime
prev_index <- findInterval(service.SLOW, fDOM_df$datetimeMT) -1
valid <- prev_index > 0

#Index of fDOM measurement AFTER service datetime (and after fdom measurements returned to "normal")
#should consider using T corrected fdom data WITHOUT post-service data being removed
next_index <- findInterval(service.SLOW, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_service_time <- service.SLOW[valid]
matched_fDOM_time <- fDOM_df$datetimeMT[prev_index[valid]]
matched_fDOM_vals <- fDOM_df$fDOM[prev_index[valid]]

npoc_vals <- docSLOW$docdata.SLOW[valid]

merged_df <- data.frame(
  service_time = matched_service_time,
  fDOM_time = matched_fDOM_time,
  NPOC = npoc_vals,
  fDOM = matched_fDOM_vals
)

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (SLOW)",
     ylab = "fDOM (before sample)",
     main = "fDOM vs NPOC (preceding fDOM measurement)")
m.SLOW <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.SLOW, col = "blue", lwd = 2)
summary(m.SLOW)

#### fDOM to DOC calibration without service times removed ####
#Import temp corrected fDOM data
EXOz.ts.tc = readRDS("EXO_compiled/BEGI_EXOz.ts.tc.rds")

#get DOC data from google drive
doc_tibble <- googledrive::as_id("https://drive.google.com/drive/folders/1zdzsIXO5LIzbcg2RzfE4mz3dBKvBmrO-")

doc <- googledrive::drive_ls(path = doc_tibble, type = "xlsx")
2

googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2025-01-22_BEGI_rerun.xlsx"], 
                            path = "NPOC-TN_2025-01-22_BEGI_rerun.xlsx",
                            overwrite = T)
docdata1 <- read_xlsx("NPOC-TN_2025-01-22_BEGI_rerun.xlsx", sheet = 9, skip = 1)

googledrive::drive_download(file = doc$id[doc$name=="240620_BEGI_Data.xlsx"], 
                            path = "240620_BEGI_Data.xlsx",
                            overwrite = T)
docdata2 <- read_xlsx("240620_BEGI_Data.xlsx")

#clean up
docdata1 <- select(docdata1,-6:-8)
names(docdata1)[names(docdata1) == '...1'] <- 'date'
names(docdata1)[names(docdata1) == '...2'] <- 'WellID'
names(docdata1)[names(docdata1) == '...3'] <- 'Sample'

names(docdata2)[names(docdata2) == 'Collection Date'] <- 'date'
names(docdata2)[names(docdata2) == 'NPOC (mg C/L)'] <- 'NPOC'
names(docdata2)[names(docdata2) == 'TDN (mg N/L)'] <- 'TN'

#stitch
docdata <- merge(docdata1, docdata2, all = TRUE)
rm(docdata1, docdata2)
#filter by well
docdata <- docdata %>%
  spread (WellID, NPOC)
docdata$date <- as.Date(docdata$date)

#read in servicing data#
# read in file and filter to EXO1 removal and deployments
service = readxl::read_excel("googledrive/sensor_event_log.xlsx")
service = service[service$model=="EXO1",]
service = service[service$observation=="removed" | service$observation=="deployed",]

# format date and time
service$datetime = paste(service$date,  service$time, sep = " ")
# convert to POIXct and set timezone
service$datetimeMT<-as.POSIXct(service$datetime, 
                               format = "%Y-%m-%d %H:%M",
                               tz="US/Mountain")
service$date = as.Date(service$date)

# remove rows with no exact times
servicetimes = service[!is.na(service$datetimeMT),]

# service dates

service.VDOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOW"]
service.VDOS = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOS"]
service.SLOC = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOC"]
service.SLOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOW"]

# filter docdata to df of each well
docVDOW <- data.frame(docdata$date,
                      docdata$Sample,
                      docdata$TN,
                      docdata$VDOW)
docVDOW <- na.omit(docVDOW)
docVDOW <- docVDOW[-1,] #no 9/15 measurements

docVDOS <- data.frame(docdata$date,
                      docdata$Sample,
                      docdata$TN,
                      docdata$VDOS)
docVDOS <- na.omit(docVDOS)
docVDOS <- docVDOS[-1,] 

docSLOC <- data.frame(docdata$date,
                      docdata$Sample,
                      docdata$TN,
                      docdata$SLOC)
docSLOC <- na.omit(docSLOC)
docSLOC <- docSLOC[-1,] 

docSLOW <- data.frame(docdata$date,
                      docdata$Sample,
                      docdata$TN,
                      docdata$SLOW)
docSLOW <- na.omit(docSLOW)
docSLOW <- docSLOW[-1,]


#VDOW
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(EXOz.ts.tc[["VDOW"]]$datetimeMT),
  date = as.Date(EXOz.ts.tc[["VDOW"]]$datetimeMT),
  fDOM = EXOz.ts.tc[["VDOW"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)

#Index of fDOM measurement AFTER service datetime
next_index <- findInterval(service.VDOW, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_fDOM_time <- fDOM_df$datetimeMT[next_index]
matched_fDOM_vals <- fDOM_df$fDOM[next_index]

merged_df <- data.frame(
  service.VDOW,
  fDOM_time = matched_fDOM_time,
  NPOC = docVDOW$docdata.VDOW,
  fDOM = matched_fDOM_vals
)

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (VDOW)",
     ylab = "fDOM (after sample)",
     main = "fDOM vs NPOC (following fDOM measurement)")
m.VDOW <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.VDOW, col = "blue", lwd = 2)
summary(m.VDOW)


#VDOS
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(EXOz.ts.tc[["VDOS"]]$datetimeMT),
  date = as.Date(EXOz.ts.tc[["VDOS"]]$datetimeMT),
  fDOM = EXOz.ts.tc[["VDOS"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)

#Index of fDOM measurement AFTER service datetime
next_index <- findInterval(service.VDOS, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_fDOM_time <- fDOM_df$datetimeMT[next_index]
matched_fDOM_vals <- fDOM_df$fDOM[next_index]

merged_df <- data.frame(
  service.VDOS,
  fDOM_time = matched_fDOM_time,
  NPOC = docVDOS$docdata.VDOS,
  fDOM = matched_fDOM_vals
)

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (VDOS)",
     ylab = "fDOM (after sample)",
     main = "fDOM vs NPOC (following fDOM measurement)")
m.VDOS <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.VDOS, col = "blue", lwd = 2)
summary(m.VDOS)


#SLOC
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(EXOz.ts.tc[["SLOC"]]$datetimeMT),
  date = as.Date(EXOz.ts.tc[["SLOC"]]$datetimeMT),
  fDOM = EXOz.ts.tc[["SLOC"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)

#Index of fDOM measurement AFTER service datetime
next_index <- findInterval(service.SLOC, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_fDOM_time <- fDOM_df$datetimeMT[next_index]
matched_fDOM_vals <- fDOM_df$fDOM[next_index]
matched_fDOM_time <- head(matched_fDOM_time, -1)
matched_fDOM_vals <- head(matched_fDOM_vals, -1)
service.SLOCc <- head(service.SLOC, -1)

merged_df <- data.frame(
  service.SLOCc,
  fDOM_time = matched_fDOM_time,
  NPOC = docSLOC$docdata.SLOC,
  fDOM = matched_fDOM_vals
)

#remove outlier to see if R2 improves. it doesn't..
#merged_df <- merged_df[-24,]

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (SLOC)",
     ylab = "fDOM (after sample)",
     main = "fDOM vs NPOC (following fDOM measurement)")
m.SLOC <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.SLOC, col = "blue", lwd = 2)
summary(m.SLOC)


#SLOW
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(EXOz.ts.tc[["SLOW"]]$datetimeMT),
  date = as.Date(EXOz.ts.tc[["SLOW"]]$datetimeMT),
  fDOM = EXOz.ts.tc[["SLOW"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)

#Index of fDOM measurement AFTER service datetime
next_index <- findInterval(service.SLOW, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_fDOM_time <- fDOM_df$datetimeMT[next_index]
matched_fDOM_vals <- fDOM_df$fDOM[next_index]


merged_df <- data.frame(
  service.SLOW,
  fDOM_time = matched_fDOM_time,
  NPOC = docSLOW$docdata.SLOW,
  fDOM = matched_fDOM_vals
)

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (SLOW)",
     ylab = "fDOM (after sample)",
     main = "fDOM vs NPOC (following fDOM measurement)")
m.SLOW <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.SLOW, col = "blue", lwd = 2)
summary(m.SLOW)



#### Dataframes fDOM events ####

##SLOC##

SLOC_fDOM1= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-08 18:15:00",tz= "US/Mountain")
             &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-09 11:45:00",tz= "US/Mountain"),]

SLOC_fDOM2= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-09 21:15:00",tz= "US/Mountain")
            &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-10 09:15:00",tz= "US/Mountain"),]

SLOC_fDOM3= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-10 21:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-11 07:15:00",tz= "US/Mountain"),]

SLOC_fDOM4= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-16 14:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-18 09:15:00",tz= "US/Mountain"),]

SLOC_fDOM5= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-11-17 17:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-11-18 04:00:00",tz= "US/Mountain"),]

SLOC_fDOM6= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-11-25 21:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-11-26 12:45:00",tz= "US/Mountain"),]

SLOC_fDOM7= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-12-05 17:15:00",tz= "US/Mountain")
                                  &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-12-05 18:15:00",tz= "US/Mountain"),]

SLOC_fDOM8= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-12-18 10:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-12-18 12:45:00",tz= "US/Mountain"),]

SLOC_fDOM9= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-12-22 17:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-12-25 16:45:00",tz= "US/Mountain"),]

SLOC_fDOM10= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2023-12-29 00:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2023-12-29 13:00:00",tz= "US/Mountain"),]

SLOC_fDOM11= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-11 10:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-11 18:15:00",tz= "US/Mountain"),]

SLOC_fDOM12= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-12 11:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-12 15:00:00",tz= "US/Mountain"),]

SLOC_fDOM13= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-19 12:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-19 15:45:00",tz= "US/Mountain"),]

SLOC_fDOM14= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-20 23:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-24 07:15:00",tz= "US/Mountain"),]

SLOC_fDOM15= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-02-10 13:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-02-10 16:45:00",tz= "US/Mountain"),]

SLOC_fDOM16= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-02-21 10:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-02-21 15:30:00",tz= "US/Mountain"),]

SLOC_fDOM17= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-03-08 20:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-03-09 12:00:00",tz= "US/Mountain"),]

SLOC_fDOM18= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-03-16 18:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-03-17 14:15:00",tz= "US/Mountain"),]

SLOC_fDOM19= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-03-20 10:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-03-20 14:00:00",tz= "US/Mountain"),]

SLOC_fDOM20= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-04-01 08:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-04-02 03:15:00",tz= "US/Mountain"),]

SLOC_fDOM21= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-04-14 00:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-04-15 13:30:00",tz= "US/Mountain"),]

SLOC_fDOM22= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-05-23 10:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-05-23 14:00:00",tz= "US/Mountain"),]

SLOC_fDOM23= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-07-04 06:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-07-05 17:45:00",tz= "US/Mountain"),]

SLOC_fDOM24= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-07-12 18:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-07-13 06:30:00",tz= "US/Mountain"),]

SLOC_fDOM25= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-07-17 13:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-07-17 17:15:00",tz= "US/Mountain"),]

SLOC_fDOM26= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-08-19 10:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-08-19 17:45:00",tz= "US/Mountain"),]

SLOC_fDOM27= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-08-25 19:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-08-27 07:00:00",tz= "US/Mountain"),]

SLOC_fDOM28= EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT >= as.POSIXct("2024-08-29 09:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOC"]]$datetimeMT <= as.POSIXct("2024-08-30 06:45:00",tz= "US/Mountain"),]

#compile list 
SLOC_fDOM <- list(SLOC_fDOM1,SLOC_fDOM2,SLOC_fDOM3,SLOC_fDOM4,SLOC_fDOM5,SLOC_fDOM6,SLOC_fDOM7,SLOC_fDOM8,SLOC_fDOM9,SLOC_fDOM10,
                  SLOC_fDOM11,SLOC_fDOM12,SLOC_fDOM13,SLOC_fDOM14,SLOC_fDOM15,SLOC_fDOM16,SLOC_fDOM17,SLOC_fDOM18,SLOC_fDOM19,SLOC_fDOM20,
                  SLOC_fDOM21,SLOC_fDOM22,SLOC_fDOM23,SLOC_fDOM24,SLOC_fDOM25,SLOC_fDOM26,SLOC_fDOM27,SLOC_fDOM28)
names(SLOC_fDOM)<-c('SLOC_fDOM1','SLOC_fDOM2','SLOC_fDOM3','SLOC_fDOM4','SLOC_fDOM5','SLOC_fDOM6','SLOC_fDOM7','SLOC_fDOM8','SLOC_fDOM9','SLOC_fDOM10',
                    'SLOC_fDOM11','SLOC_fDOM12','SLOC_fDOM13','SLOC_fDOM14','SLOC_fDOM15','SLOC_fDOM16','SLOC_fDOM17','SLOC_fDOM18','SLOC_fDOM19','SLOC_fDOM20',
                    'SLOC_fDOM21','SLOC_fDOM22','SLOC_fDOM23','SLOC_fDOM24','SLOC_fDOM25','SLOC_fDOM26','SLOC_fDOM27','SLOC_fDOM28')

##SLOW##

SLOW_fDOM1= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2023-09-20 10:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2023-09-20 12:30:00",tz= "US/Mountain"),]

SLOW_fDOM2= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2023-10-16 14:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2023-10-16 16:15:00",tz= "US/Mountain"),]

SLOW_fDOM3= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2023-11-21 16:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2023-11-21 17:00:00",tz= "US/Mountain"),]

SLOW_fDOM4= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2023-12-18 10:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2023-12-18 12:00:00",tz= "US/Mountain"),]

SLOW_fDOM5= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2023-12-24 17:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2023-12-29 03:15:00",tz= "US/Mountain"),]

SLOW_fDOM6= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-01-19 12:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-01-19 14:45:00",tz= "US/Mountain"),]

SLOW_fDOM7= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-02-10 13:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-02-11 03:30:00",tz= "US/Mountain"),]

SLOW_fDOM8= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-02-21 10:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-02-21 12:00:00",tz= "US/Mountain"),]

SLOW_fDOM9= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-04-23 18:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-04-28 17:30:00",tz= "US/Mountain"),]

SLOW_fDOM10= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-06-29 20:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-06-30 04:15:00",tz= "US/Mountain"),]

SLOW_fDOM11= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-07-12 18:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-07-13 11:00:00",tz= "US/Mountain"),]

SLOW_fDOM12= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-07-17 14:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-07-17 16:30:00",tz= "US/Mountain"),]

SLOW_fDOM13= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-07-21 19:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-07-22 04:15:00",tz= "US/Mountain"),]

SLOW_fDOM14= EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT >= as.POSIXct("2024-08-19 10:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["SLOW"]]$datetimeMT <= as.POSIXct("2024-08-19 17:15:00",tz= "US/Mountain"),]

#compile list
SLOW_fDOM<-list(SLOW_fDOM1,SLOW_fDOM2,SLOW_fDOM3,SLOW_fDOM4,SLOW_fDOM5,SLOW_fDOM6,SLOW_fDOM7,SLOW_fDOM8,SLOW_fDOM9,SLOW_fDOM10,
                SLOW_fDOM11,SLOW_fDOM12,SLOW_fDOM13,SLOW_fDOM14)
names(SLOW_fDOM)<-c('SLOW_fDOM1','SLOW_fDOM2','SLOW_fDOM3','SLOW_fDOM4','SLOW_fDOM5','SLOW_fDOM6','SLOW_fDOM7','SLOW_fDOM8','SLOW_fDOM9','SLOW_fDOM10',
                    'SLOW_fDOM11','SLOW_fDOM12','SLOW_fDOM13','SLOW_fDOM14')



##VDOW##

VDOW_fDOM1= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-09-17 02:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-09-19 10:00:00",tz= "US/Mountain"),]

VDOW_fDOM2= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-09-29 19:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-02 00:15:00",tz= "US/Mountain"),]

VDOW_fDOM3= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-02 15:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-05 16:15:00",tz= "US/Mountain"),]

VDOW_fDOM4= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-05 17:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-06 07:15:00",tz= "US/Mountain"),]

VDOW_fDOM5= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-07 08:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-07 15:30:00",tz= "US/Mountain"),]

VDOW_fDOM6= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-11 14:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-11 17:45:00",tz= "US/Mountain"),]

VDOW_fDOM7= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-16 14:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-17 16:15:00",tz= "US/Mountain"),]

VDOW_fDOM8= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-02 21:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-03 00:45:00",tz= "US/Mountain"),]

VDOW_fDOM9= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-03 18:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-04 11:15:00",tz= "US/Mountain"),]

VDOW_fDOM10= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-04 19:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-06 15:30:00",tz= "US/Mountain"),]

VDOW_fDOM11= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-18 12:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-18 17:15:00",tz= "US/Mountain"),]

VDOW_fDOM12= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-19 17:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-20 14:30:00",tz= "US/Mountain"),]

VDOW_fDOM13= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-20 14:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-21 10:45:00",tz= "US/Mountain"),]

VDOW_fDOM14= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-21 17:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-22 12:45:00",tz= "US/Mountain"),]

VDOW_fDOM15= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-27 00:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-28 10:00:00",tz= "US/Mountain"),]

VDOW_fDOM16= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-28 10:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-29 07:00:00",tz= "US/Mountain"),]

VDOW_fDOM17= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-29 10:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-29 16:45:00",tz= "US/Mountain"),]

VDOW_fDOM18= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-12-04 04:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-12-04 23:45:00",tz= "US/Mountain"),]

VDOW_fDOM19= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2023-12-18 09:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2023-12-19 09:00:00",tz= "US/Mountain"),]

VDOW_fDOM20= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-02-21 13:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-02-29 06:00:00",tz= "US/Mountain"),]

VDOW_fDOM21= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-03-05 20:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-03-10 15:15:00",tz= "US/Mountain"),]

VDOW_fDOM22= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-03-20 10:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-03-22 07:45:00",tz= "US/Mountain"),]

VDOW_fDOM23= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-04-17 10:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-04-17 17:15:00",tz= "US/Mountain"),]

VDOW_fDOM24= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-05-14 14:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-05-15 06:00:00",tz= "US/Mountain"),]

VDOW_fDOM25= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-05-23 10:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-05-23 21:45:00",tz= "US/Mountain"),]

VDOW_fDOM26= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-06-17 13:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-06-19 11:45:00",tz= "US/Mountain"),]

VDOW_fDOM27= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-07-17 07:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-07-18 13:00:00",tz= "US/Mountain"),]

VDOW_fDOM28= EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT >= as.POSIXct("2024-08-19 09:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOW"]]$datetimeMT <= as.POSIXct("2024-08-19 23:45:00",tz= "US/Mountain"),]

#compile list
VDOW_fDOM<-list(VDOW_fDOM1,VDOW_fDOM2,VDOW_fDOM3,VDOW_fDOM4,VDOW_fDOM5,VDOW_fDOM6,VDOW_fDOM7,VDOW_fDOM8,VDOW_fDOM9,VDOW_fDOM10,
                VDOW_fDOM11,VDOW_fDOM12,VDOW_fDOM13,VDOW_fDOM14,VDOW_fDOM15,VDOW_fDOM16,VDOW_fDOM17,VDOW_fDOM18,VDOW_fDOM19,VDOW_fDOM20,
                VDOW_fDOM21,VDOW_fDOM22,VDOW_fDOM23,VDOW_fDOM24,VDOW_fDOM25,VDOW_fDOM26,VDOW_fDOM27,VDOW_fDOM28)
names(VDOW_fDOM)<-c('VDOW_fDOM1','VDOW_fDOM2','VDOW_fDOM3','VDOW_fDOM4','VDOW_fDOM5','VDOW_fDOM6','VDOW_fDOM7','VDOW_fDOM8','VDOW_fDOM9','VDOW_fDOM10',
                    'VDOW_fDOM11','VDOW_fDOM12','VDOW_fDOM13','VDOW_fDOM14','VDOW_fDOM15','VDOW_fDOM16','VDOW_fDOM17','VDOW_fDOM18','VDOW_fDOM19','VDOW_fDOM20',
                    'VDOW_fDOM21','VDOW_fDOM22','VDOW_fDOM23','VDOW_fDOM24','VDOW_fDOM25','VDOW_fDOM26','VDOW_fDOM27','VDOW_fDOM28')



##VDOS##

VDOS_fDOM1= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-09-21 15:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-09-21 23:45:00",tz= "US/Mountain"),]

VDOS_fDOM2= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-10-10 06:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-10-10 06:45:00",tz= "US/Mountain"),]

VDOS_fDOM3= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-10-16 08:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-10-16 14:30:00",tz= "US/Mountain"),]

VDOS_fDOM4= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-10-25 01:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-10-26 14:15:00",tz= "US/Mountain"),]

VDOS_fDOM5= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-10-27 18:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-10-29 14:45:00",tz= "US/Mountain"),]

VDOS_fDOM6= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-03 02:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-03 03:00:00",tz= "US/Mountain"),]

VDOS_fDOM7= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-03 16:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-04 15:15:00",tz= "US/Mountain"),]

VDOS_fDOM8= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-11 00:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-15 12:00:00",tz= "US/Mountain"),]

VDOS_fDOM9= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-15 12:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-17 04:00:00",tz= "US/Mountain"),]

VDOS_fDOM10= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-24 22:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-25 11:00:00",tz= "US/Mountain"),]

VDOS_fDOM11= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-27 09:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-27 11:15:00",tz= "US/Mountain"),]

VDOS_fDOM12= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-29 10:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-29 11:15:00",tz= "US/Mountain"),]

VDOS_fDOM13= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-30 10:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-30 11:30:00",tz= "US/Mountain"),]

VDOS_fDOM14= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-02-21 13:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-02-21 15:00:00",tz= "US/Mountain"),]

VDOS_fDOM15= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-05-23 10:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-05-23 10:45:00",tz= "US/Mountain"),]

VDOS_fDOM16= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-06-11 09:45:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-06-12 04:15:00",tz= "US/Mountain"),]

VDOS_fDOM17= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-07-09 20:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-07-14 09:15:00",tz= "US/Mountain"),]

VDOS_fDOM18= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-07-16 10:00:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-07-17 13:15:00",tz= "US/Mountain"),]

VDOS_fDOM19= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-07-17 13:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-07-18 16:30:00",tz= "US/Mountain"),]

VDOS_fDOM20= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-08-07 00:15:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-08-12 08:15:00",tz= "US/Mountain"),]

VDOS_fDOM21= EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT >= as.POSIXct("2024-08-19 09:30:00",tz= "US/Mountain")
                                   &EXOz.tc[["VDOS"]]$datetimeMT <= as.POSIXct("2024-08-19 10:30:00",tz= "US/Mountain"),]

#compile list
VDOS_fDOM<-list(VDOS_fDOM1,VDOS_fDOM2,VDOS_fDOM3,VDOS_fDOM4,VDOS_fDOM5,VDOS_fDOM6,VDOS_fDOM7,VDOS_fDOM8,VDOS_fDOM9,VDOS_fDOM10,
                VDOS_fDOM11,VDOS_fDOM12,VDOS_fDOM13,VDOS_fDOM14,VDOS_fDOM15,VDOS_fDOM16,VDOS_fDOM17,VDOS_fDOM18,VDOS_fDOM19,VDOS_fDOM20,
                VDOS_fDOM21)
names(VDOS_fDOM)<-c('VDOS_fDOM1','VDOS_fDOM2','VDOS_fDOM3','VDOS_fDOM4','VDOS_fDOM5','VDOS_fDOM6','VDOS_fDOM7','VDOS_fDOM8','VDOS_fDOM9','VDOS_fDOM10',
                    'VDOS_fDOM11','VDOS_fDOM12','VDOS_fDOM13','VDOS_fDOM14','VDOS_fDOM15','VDOS_fDOM16','VDOS_fDOM17','VDOS_fDOM18','VDOS_fDOM19','VDOS_fDOM20',
                    'VDOS_fDOM21')






#### Save lists of event dataframes ####
#DO

Eventdate <-list(SLOC_dates,SLOW_dates,VDOW_dates,VDOS_dates)  
names(Eventdate)<-c('SLOC_dates','SLOW_dates','VDOW_dates','VDOS_dates')

DO_events<-list(SLOC_DO,SLOW_DO,VDOW_DO,VDOS_DO)
names(DO_events)<-c('SLOC_DO','SLOW_DO','VDOW_DO','VDOS_DO')
#fDOM
fDOM_events<-list(SLOC_fDOM,SLOW_fDOM,VDOW_fDOM,VDOS_fDOM)
names(fDOM_events)<-c('SLOC_fDOM','SLOW_fDOM','VDOW_fDOM','VDOS_fDOM')
#compile all events
BEGI_events<-list(DO_events,fDOM_events,Eventdate)
names(BEGI_events)<-c('DO_events','fDOM_events','Eventdate')

#save list
saveRDS(BEGI_events,"EXO_compiled/BEGI_events.rds")


#### Import data to plot all events ####
#First, run through above code of "Read in .csv files of service dates and times" and "Vector of dates"
#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#import EXOz.tc
EXOz.tc = readRDS("EXO_compiled/BEGI_EXOz.tc.rds")
#correct negative DO values
EXOz.tc[["VDOW"]]$ODO.mg.L.mn <- EXOz.tc[["VDOW"]]$ODO.mg.L.mn + 0.36
EXOz.tc[["VDOS"]]$ODO.mg.L.mn <- EXOz.tc[["VDOS"]]$ODO.mg.L.mn + 0.42
EXOz.tc[["SLOW"]]$ODO.mg.L.mn <- EXOz.tc[["SLOW"]]$ODO.mg.L.mn + 0.32
EXOz.tc[["SLOC"]]$ODO.mg.L.mn <- EXOz.tc[["SLOC"]]$ODO.mg.L.mn + 2.2

#Import DTW data
DTW_df = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")

### spread DTW_m in DTW_df to each well
DTW_df <- DTW_df %>%
  spread (wellID, DTW_m)

#dtw df for each well
DTW_SLOC <- data.frame(DTW_df$datetimeMT,
                       DTW_df$SLOC)
DTW_SLOC <- na.omit(DTW_SLOC)

DTW_SLOW <- data.frame(DTW_df$datetimeMT,
                       DTW_df$SLOW)
DTW_SLOW <- na.omit(DTW_SLOW)

DTW_VDOW <- data.frame(DTW_df$datetimeMT,
                       DTW_df$VDOW)
DTW_VDOW <- na.omit(DTW_VDOW)

DTW_VDOS <- data.frame(DTW_df$datetimeMT,
                       DTW_df$VDOS)
DTW_VDOS <- na.omit(DTW_VDOS)

#Define shaded am/pm
shade_df <- data.frame(
  xmin = pm.pts,
  xmax = am.pts,
  ymin = -Inf,
  ymax = Inf
)



#### Plot all SLOC events####

for (i in seq_along(BEGI_events[["DO_events"]][["SLOC_DO"]])) {
  dz <- BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]
  
  #Time window: 2 days before event to event end
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 60*60*50
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 60*60
  
  #Subset data
  tempdat <- EXOz.tc[["SLOC"]][
    EXOz.tc[["SLOC"]]$datetimeMT >= start_time &
      EXOz.tc[["SLOC"]]$datetimeMT <= end_time, ]
  
  tempdtw <- DTW_SLOC[
    DTW_SLOC$DTW_df.datetimeMT >= start_time &
      DTW_SLOC$DTW_df.datetimeMT <= end_time, ]
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))

  
  g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.SLOC)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/SLOC/events/SLOC_event_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}


#### Plot all SLOW events####

for (i in seq_along(BEGI_events[["DO_events"]][["SLOW_DO"]])) {
  dz <- BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]
  
  #Time window: 2 days before event to event end
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 60*60*50
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 60*60
  
  #Subset data
  tempdat <- EXOz.tc[["SLOW"]][
    EXOz.tc[["SLOW"]]$datetimeMT >= start_time &
      EXOz.tc[["SLOW"]]$datetimeMT <= end_time, ]
  
  tempdtw <- DTW_SLOW[
    DTW_SLOW$DTW_df.datetimeMT >= start_time &
      DTW_SLOW$DTW_df.datetimeMT <= end_time, ]
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.SLOW)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/SLOW/events/SLOW_event_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

#### Plot all VDOW events####

for (i in seq_along(BEGI_events[["DO_events"]][["VDOW_DO"]])) {
  dz <- BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]
  
  #Time window: 2 days before event to event end
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 60*60*50
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 60*60
  
  #Subset data
  tempdat <- EXOz.tc[["VDOW"]][
    EXOz.tc[["VDOW"]]$datetimeMT >= start_time &
      EXOz.tc[["VDOW"]]$datetimeMT <= end_time, ]
  
  tempdtw <- DTW_VDOW[
    DTW_VDOW$DTW_df.datetimeMT >= start_time &
      DTW_VDOW$DTW_df.datetimeMT <= end_time, ]
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.VDOW)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/VDOW/events/VDOW_event_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

#### Plot all VDOS events####

for (i in seq_along(BEGI_events[["DO_events"]][["VDOS_DO"]])) {
  dz <- BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]
  
  #Time window: 2 days before event to event end
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 60*60*50
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 60*60
  
  #Subset data
  tempdat <- EXOz.tc[["VDOS"]][
    EXOz.tc[["VDOS"]]$datetimeMT >= start_time &
      EXOz.tc[["VDOS"]]$datetimeMT <= end_time, ]
  
  tempdtw <- DTW_VDOS[
    DTW_VDOS$DTW_df.datetimeMT >= start_time &
      DTW_VDOS$DTW_df.datetimeMT <= end_time, ]
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.VDOS)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/VDOS/events/VDOS_event_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}
