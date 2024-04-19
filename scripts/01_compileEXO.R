#### read me ####

# the purpose of this script is to compile and plot EXO1 files from the Webster Lab BEGI project

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)


#### load data from google drive ####

ls_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1w9mhxwI4FU7Nu9HEFmrdH56FufOrv3Lp")
2
for (file_id in ls_tibble$id) {
  try({googledrive::drive_download(as_id(file_id))})
}
# add overwrite = TRUE if for some reason you want to replace files previously downloaded. 



#### load and stitch EXO data ####

# import data
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
BEGI_EXOz = list()
for(i in siteIDz){
  file_list <- list.files(recursive=F, pattern=paste(i, ".csv", sep=""))
  BEGI_EXOz[[i]] = lapply(file_list, read.csv, 
                          stringsAsFactors=FALSE, skip=8,header=T)
}

# use one file as a template and match columns in all other files to that one
# here, BEGI_EXOz[["VDOS"]][[19]] is the template because it does not have the "Depth.m", "Pressure.psi.a", and "Vertical.Position.m" columns
for(i in siteIDz){
  for(n in 1:20){
    BEGI_EXOz[[i]][[n]] = 
      BEGI_EXOz[[i]][[n]] [, intersect(names(BEGI_EXOz[["VDOS"]][[19]] ), names(BEGI_EXOz[[i]][[n]] )), drop=FALSE]
  }
}

# bind files within sites into one dataframe per site
for(i in siteIDz){
  BEGI_EXOz[[i]] = do.call("rbind", BEGI_EXOz[[i]])
}


#
#### format dates ####

for(i in siteIDz){
  # put date and time in same column
  BEGI_EXOz[[i]]$datetime = paste( BEGI_EXOz[[i]]$Date..MM.DD.YYYY.,  BEGI_EXOz[[i]]$Time..HH.mm.ss., sep = " ")
  # convert to POIXct and set timezone
  BEGI_EXOz[[i]]$datetimeMT<-as.POSIXct( BEGI_EXOz[[i]]$datetime, 
                                         format = "%m/%d/%Y %H:%M:%S",
                                         tz="US/Mountain")
  # replace two digit years that are converted incorrectly
  BEGI_EXOz[[i]]$year = year(BEGI_EXOz[[i]]$datetimeMT)
  BEGI_EXOz[[i]]$year[BEGI_EXOz[[i]]$year==0023] = 2023
  BEGI_EXOz[[i]]$year[BEGI_EXOz[[i]]$year==0024] = 2024
  year(BEGI_EXOz[[i]]$datetimeMT) = BEGI_EXOz[[i]]$year
}


#
#### Check variable names ####
#check the variable order for each sonde and edit names if necessary

names(BEGI_EXOz[["VDOW"]]) == names(BEGI_EXOz[["VDOS"]])
names(BEGI_EXOz[["VDOW"]]) == names(BEGI_EXOz[["SLOW"]])
names(BEGI_EXOz[["VDOW"]]) == names(BEGI_EXOz[["SLOC"]])



#### Compile bursts within 1 min ####

# make sure all columns with numeric data data are numeric
BEGI_EXOz <- lapply(BEGI_EXOz, function(x) {x[5:19] <- lapply(x[5:19], as.numeric);x})

# get means and standard deviations of numeric burst values
BEGI_EXO.stz = list()
for(i in siteIDz){
  min<-round_date(BEGI_EXOz[[i]]$datetimeMT, "minute") # note rounding instead of using the function cut()!! cut was what was causing our memory issues!!
  BEGI_EXO.stz[[i]] <- as.data.frame(as.list(aggregate(cbind(Cond.µS.cm, fDOM.QSU, fDOM.RFU,
                                                   nLF.Cond.µS.cm,
                                                   ODO...sat,ODO...local,ODO.mg.L,
                                                   Sal.psu,SpCond.µS.cm,
                                                   TDS.mg.L,Turbidity.FNU,TSS.mg.L,Temp..C,
                                                   Battery.V,Cable.Pwr.V) 
                                             ~ min, data=BEGI_EXOz[[i]], na.action=na.pass, FUN=function(x) c(mn=mean(x), SD=sd(x)))))
  BEGI_EXO.stz[[i]]$datetimeMT<-as.POSIXct(BEGI_EXO.stz[[i]]$min, "%Y-%m-%d %H:%M:%S", tz="US/Mountain")
}

#### save and re-add burst-compiled files ####

saveRDS(BEGI_EXO.stz, "EXO_compiled/BEGI_EXO.rds")
rm(list = ls())
BEGI_EXO.stz = readRDS("EXO_compiled/BEGI_EXO.rds")



#### stitch in water level data ####

# get data from googledrive
beeper_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1L5ywkdYUOxhE3GPm7vbMiwgObOyn3awF")
2
googledrive::drive_download(as_id(beeper_tibble$id[beeper_tibble$name=="BEGI_beeper"]), overwrite = TRUE,
                            path="googledrive/BEGI_beeper.csv")
beeper = read.csv("googledrive/BEGI_beeper.csv")

# format date/times
# beeper$time[is.na(beeper$time)] = "12:00"
# beeper$datetimeMT = as.POSIXct(
#   paste(beeper$date, beeper$time, sep=" "), 
#   "%Y-%m-%d %H:%M", tz="US/Mountain")
beeper$date = as.Date(beeper$date)

# format siteID to be same as wellID
beeper$siteID = beeper$wellID

# join
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz){
  BEGI_EXO.stz[[i]]$date = as.Date(BEGI_EXO.stz[[i]]$datetimeMT, tz="US/Mountain")
  BEGI_EXO.stz[[i]]$siteID = i
  BEGI_EXO.stz[[i]] = left_join(BEGI_EXO.stz[[i]], beeper, by=c("date", "siteID"))
}

saveRDS(BEGI_EXO.stz, "EXO_compiled/BEGI_EXO.rds")
# rm(list = ls())
BEGI_EXO.stz = readRDS("EXO_compiled/BEGI_EXO.rds")



#### plot to check ####

# service dates

service = as.POSIXct(paste ( unique(beeper$date), "12:00:00"), tz="US/Mountain")

suntimes = 
  getSunlightTimes(date = seq.Date(from = as.Date("2023-09-14"), to = as.Date("2024-04-17"), by = 1),
                 keep = c("sunrise", "sunset"),
                 lat = 34.9, lon = -106.7, tz = "US/Mountain")

pm.pts = suntimes$sunset[-(nrow(suntimes))]
am.pts = suntimes$sunrise[-1]


## Plotting ONLY SLOC DO, zoomed in ##

#save plot

jpeg("plots/SLOC_DO.jpg", width = 20, height = 8, units="in", res=1000)

plot.new()

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,.2), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")


dev.off()


## SLOC ##

# Save plot 
jpeg("plots/SLOC.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-10, 10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim = c(0,30))
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,10), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n",ylim=c(0,120), ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,1300), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()

# 
# 
# # plot inset of DO and water depth or fDOM
# plot.new()
# par(mfrow=c(3,1), mar=c(7,4,2,1.5))
# plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$waterlevelbelowsurface_cm*-1),
#      pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-10, 5))
# #rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey")
# lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$waterlevelbelowsurface_cm*-1),
#       pch=20,col="black", xlab="", xaxt = "n", type="b")
# abline(v=as.POSIXct(service), col="red")
# #abline(h=-300, col="red")
# abline(h=0, col="green")
# axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
# title(main="Water depth below surface (cm)")
# 
# plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
#      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="n", ylab="",
#      xlim=c(as.POSIXct("2023-10-05"),as.POSIXct("2023-10-14")))
# rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey")
# lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
#       pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
# abline(v=as.POSIXct(service), col="red")
# axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d %R", las=2)
# title(main="Dissolved Oxygen (mg/L)")
# 
# plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$fDOM.QSU.mn),
#      pch=20,col="black", xlab="", xaxt = "n",ylim=c(20,80), type="n", ylab="",
#      xlim=c(as.POSIXct("2023-10-05"),as.POSIXct("2023-10-14")))
# rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=250, col="lightgrey")
# lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$fDOM.QSU.mn),
#       pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
# abline(v=as.POSIXct(service), col="red")
# axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d %R", las=2)
# title(main="fDOM (QSU)")
# 
# 


## SLOW ##

# Save plot 
jpeg("plots/SLOW.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim = c(0,30))
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,10), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n",ylim=c(0,120), ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,1300), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()



## VDOS ##

# Save plot 
jpeg("plots/VDOS.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim = c(0,30))
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,10), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n",ylim=c(0,120), ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,1300), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()



## VDOW ##

# Save plot 
jpeg("plots/VDOW.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim = c(0,30))
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,10), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n",ylim=c(0,120), ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,1300), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()





#
#### just well depths ####

# plot all together
plot.new()
par(mfrow=c(2,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="VDO South")

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="VDO West")

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="SLO Center")

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="SLO West")


#### extra plots ####

# smoothed DO close up
BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm =
  c(rollmean(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn, 4, align="left"),
    NA,NA,NA)

BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc =
  BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm +
  abs(min(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm, na.rm = T))

BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc_c = BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc

# BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc_c[BEGI_EXO.stz[["SLOC"]]$datetimeMT >
#                                              as.POSIXct("2023-11-01 08:00:00") &
#                                              BEGI_EXO.stz[["SLOC"]]$datetimeMT <
#                                              as.POSIXct("2023-12-09 15:00:00")   ] = NA
plot.new()
par(mfrow=c(1,1), mar=c(7,4,2,1.5))
plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),
     BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc_c,
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(0,0.8), type="n", ylab="",
     xlim=c(as.POSIXct("2023-11-01"),as.POSIXct("2024-01-06")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),
      BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc_c,
      pch=20,col="black", xlab="", xaxt = "n", type="o")
#abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d %R", las=2)
title(main="Dissolved Oxygen (mg/L)")


### just latest DO and fDOM ###

# SLOC
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

# SLOW
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

# VDOW
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")


# VDOS
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")


