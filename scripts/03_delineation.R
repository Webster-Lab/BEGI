#### read me ####

# the purpose of this script is to delineate respiration events from the compiled EXO1 RDS files in the Webster BEGI project

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)

#### Import compiled EXO1 RDS file ####
BEGI_EXO.or2 = readRDS("EXO_compiled/BEGI_EXO.or2.rds")

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

SLOW_events <- c("2023-09-20","2023-10-16","2023-11-21","2023-12-18","2023-12-25","2023-12-26","2023-12-27","2023-12-28","2023-12-29","2024-01-19","2024-02-10","2024-02-11","2024-02-21","2024-03-20","2024-04-24","2024-04-25","2024-04-26","2024-04-27","2024-04-28","2024-05-14","2024-06-30","2024-07-13","2024-07-17","2024-07-22","2024-08-19")
SLOW_events <- as.Date(SLOW_events)

VDOS_events <- c("2023-09-21","2023-09-22","2023-10-10","2023-10-16","2023-10-25","2023-10-26","2023-10-28","2023-10-29","2023-11-03","2023-11-04","2023-11-04","2023-11-11","2023-11-12","2023-11-13","2023-11-14","2023-11-15","2023-11-16","2023-11-17","2023-11-25","2023-11-27","2023-11-29","2023-11-30","2024-02-21","2024-04-17","2024-05-23","2024-06-12","2024-07-10","2024-07-11","2024-07-12","2024-07-13","2024-07-14","2024-07-16","2024-07-17","2024-07-18","2024-08-07","2024-08-08","2024-08-09","2024-08-10","2024-08-11","2024-08-12","2024-08-19")
VDOS_events <- as.Date(VDOS_events)

VDOW_events <- c("2023-09-17","2023-09-18","2023-09-19","2023-09-30","2023-10-01","2023-10-02","2023-10-03","2023-10-04","2023-10-05","2023-10-06","2023-10-07","2023-10-11","2023-10-16","2023-10-17","2023-10-17","2023-11-02","2023-11-03","2023-11-04","2023-11-05","2023-11-06","2023-11-18","2023-11-19","2023-11-20","2023-11-21","2023-11-22","2023-11-27","2023-11-28","2023-11-29","2023-12-04","2023-12-05","2023-12-18","2023-12-19","2024-02-21","2024-02-22","2024-02-23","2024-02-24","2024-02-25","2024-02-26","2024-02-27","2024-02-28","2024-02-29","2024-02-21","2024-03-06","2024-03-07","2024-03-08","2024-03-09","2024-03-10","2024-03-20","2024-03-21","2024-03-22","2024-04-17","2024-05-14","2024-05-15","2024-05-23","2024-05-24","2024-06-17","2024-06-18","2024-06-19","2024-07-17","2024-07-18","2024-08-19","2024-08-20")
VDOW_events <- as.Date(VDOW_events)
#pulse of fDOM and turbidity in VDOW 11/2023
#It seems like fDOM starts to decrease significantly over several days after a sonde event. It makes me think that a sonde event introduces enough oxygen into the wells (which wouldn't show up as DO) to spur aerobic respiration/DOC consumption