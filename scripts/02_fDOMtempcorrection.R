#### read me ####

# the purpose of this script is to to temperature-correct fDOM data for the Webster Lab BEGI project

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)


#### load data ####

EXOz = readRDS("EXO_compiled/BEGI_EXO.or2.rds")

# get data from googledrive
tempcal_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1u4yAZIoqYC2d1BSkt8iG5IT3lPUp3ALo")
2

# import info from sonde 3231 experiment 
googledrive::drive_download(as_id(tempcal_tibble$id[tempcal_tibble$name=="20241204_3231_fdom.csv"]), overwrite = TRUE,
                            path="googledrive/20241204_3231_fdom.csv")
tempcal1 = read.csv("googledrive/20241204_3231_fdom.csv", skip=8)
tempcal1_sn = read.csv("googledrive/20241204_3231_fdom.csv", skip=7, head=FALSE)
tempcal1_sn = tempcal1_sn[1:2,]

# import info from sonde 5009 experiment 
googledrive::drive_download(as_id(tempcal_tibble$id[tempcal_tibble$name=="20241204_5009_fdom.csv"]), overwrite = TRUE,
                            path="googledrive/20241204_5009_fdom.csv")
tempcal2 = read.csv("googledrive/20241204_5009_fdom.csv", skip=8)
tempcal2_sn = read.csv("googledrive/20241204_5009_fdom.csv", skip=7, head=FALSE)
tempcal2_sn = tempcal2_sn[1:2,]


#### data wrangling ####

tempcal1 = tempcal1[ , which(names(tempcal1) %in% c("fDOM.QSU", "fDOM.QSU.1",
                                                    "Temp..C","Temp..C.1"))]
names(tempcal1) = c("fDOM.QSU_23C101705", "fDOM.QSU_23C101758",
                    "Temp.C_23G102566","Temp.C_23G102567")

tempcal2 = tempcal2[ , which(names(tempcal2) %in% c("fDOM.QSU", "fDOM.QSU.1",
                                                    "Temp..C","Temp..C.1"))]
names(tempcal2) = c("fDOM.QSU_23C101759", "fDOM.QSU_23C101760",
                    "Temp.C_23G102560","Temp.C_23G102568")

tempcal2 = tempcal2[1:259,]

tempcalall = cbind(tempcal1,tempcal2)

# make data frames

sonde_3231_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102566"],tempcalall[,"fDOM.QSU_23C101705"]))
names(sonde_3231_tempcal) = c("temp_C","fDOM_QSU")
sonde_3231_tempcal$t = c(1:259)

sonde_5009_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102568"],tempcalall[,"fDOM.QSU_23C101760"]))
names(sonde_5009_tempcal) = c("temp_C","fDOM_QSU")

sonde_3230_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102567"],tempcalall[,"fDOM.QSU_23C101758"]))
names(sonde_3230_tempcal) = c("temp_C","fDOM_QSU")

sonde_3229_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102560"],tempcalall[,"fDOM.QSU_23C101759"]))
names(sonde_3229_tempcal) = c("temp_C","fDOM_QSU")


# trim data frames

plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$t)
sonde_3231_tempcal = sonde_3231_tempcal[-c(250:259),]
plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$t)

plot(sonde_5009_tempcal$fDOM_QSU ~ sonde_5009_tempcal$t)
#sonde_5009_tempcal = sonde_5009_tempcal[-c(250:259),]
#plot(sonde_5009_tempcal$fDOM_QSU ~ sonde_5009_tempcal$t)

plot(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$t)
sonde_3230_tempcal = sonde_3230_tempcal[-c(250:259),]
plot(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$t)

plot(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$t)
sonde_3229_tempcal = sonde_3229_tempcal[-c(1:70),]
plot(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$t)

#

#### Define linear relationships and rhos ####

# sonde_3231 #
plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$temp_C)
m.3231 = lm(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$temp_C)
abline(m.3231)
summary(m.3231)

# sonde_5009 #
plot(sonde_5009_tempcal$fDOM_QSU ~ sonde_5009_tempcal$temp_C)
m.5009 = lm(sonde_5009_tempcal$fDOM_QSU ~ sonde_5009_tempcal$temp_C)
abline(m.5009)
summary(m.5009)

# sonde_3230 #
plot(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$temp_C)
m.3230 = lm(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$temp_C)
abline(m.3230)
summary(m.3230)

# sonde_3229 #
plot(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$temp_C)
m.3229 = lm(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$temp_C)
abline(m.3229)
summary(m.3229)

# define rhos

sondeID = c("s3231", "s5009", "s3230", "s3229")
siteID = c("SLOW", "SLOC", "VDOS", "VDOW")
Tref = c(25, 25, 25, 25) #Tref (reference temperature) in fDOM correction is a standard reference temp. Most people seem to use 25 deg C (Watras et al., 2011, Saraceno et al., 2017...).
rho = c(m.3231$coefficients[2]/m.3231$coefficients[1],
        m.5009$coefficients[2]/m.5009$coefficients[1],
        m.3230$coefficients[2]/m.3230$coefficients[1],
        m.3229$coefficients[2]/m.3229$coefficients[1])
rhos = data.frame(siteID, sondeID, rho, Tref)


#### set rho ####

EXOz.tc = EXOz
# join
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz){
  EXOz.tc[[i]]$siteID = i
  EXOz.tc[[i]] = left_join(EXOz.tc[[i]], rhos, by=c("siteID"))
}


#### apply temp correction ####

siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz){
  EXOz.tc[[i]]$siteID = i
  EXOz.tc[[i]]$fDOM.QSU.mn.Tc = EXOz.tc[[i]]$fDOM.QSU.mn / ( 1 + (EXOz.tc[[i]]$rho * (EXOz.tc[[i]]$Temp..C.mn - EXOz.tc[[i]]$Tref)))
}


#### plot to check ####

# SLOC
tempdat = EXOz.tc[["SLOC"]][EXOz.tc[["SLOC"]]$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
                              EXOz.tc[["SLOC"]]$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"),]
plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="l")#,ylim=c(22.5,24.5))
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn.Tc),
      pch=20,col="blue", xlab="", xaxt = "n", type="l")


# SLOW
tempdat = EXOz.tc[["SLOW"]][EXOz.tc[["SLOW"]]$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
                              EXOz.tc[["SLOW"]]$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"),]
plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="l")#,ylim=c(22.5,24.5))
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn.Tc),
      pch=20,col="blue", xlab="", xaxt = "n", type="l")

# VDOS
tempdat = EXOz.tc[["VDOS"]][EXOz.tc[["VDOS"]]$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
                              EXOz.tc[["VDOS"]]$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"),]
plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="l")#,ylim=c(22.5,24.5))
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn.Tc),
      pch=20,col="blue", xlab="", xaxt = "n", type="l")

# VDOW
tempdat = EXOz.tc[["VDOW"]][EXOz.tc[["VDOW"]]$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
                              EXOz.tc[["VDOW"]]$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"),]
plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="l")#,ylim=c(22.5,24.5))
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn.Tc),
      pch=20,col="blue", xlab="", xaxt = "n", type="l")

#### save rds of all data with temp-corrected fDOM (fDOM.QSU.mn.Tc) ####

saveRDS(EXOz.tc, "EXO_compiled/BEGI_EXOz.tc.rds")
#rm(list = ls())



