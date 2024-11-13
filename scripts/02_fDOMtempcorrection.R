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

# import info from sonde 3229 experiment 
googledrive::drive_download(as_id(tempcal_tibble$id[tempcal_tibble$name=="20241106_3229_fdom.csv"]), overwrite = TRUE,
                            path="googledrive/20241106_3229_fdom.csv")
tempcal1 = read.csv("googledrive/20241106_3229_fdom.csv", skip=8)
tempcal1_sn = read.csv("googledrive/20241106_3229_fdom.csv", skip=7, head=FALSE)
tempcal1_sn = tempcal1_sn[1:2,]

# import info from sonde 3231 experiment 
googledrive::drive_download(as_id(tempcal_tibble$id[tempcal_tibble$name=="20241106_3231_fdom.csv"]), overwrite = TRUE,
                            path="googledrive/20241106_3231_fdom.csv")
tempcal2 = read.csv("googledrive/20241106_3231_fdom.csv", skip=8)
tempcal2_sn = read.csv("googledrive/20241106_3231_fdom.csv", skip=7, head=FALSE)
tempcal2_sn = tempcal2_sn[1:2,]


#### data wrangling ####

tempcal1 = tempcal1[ , which(names(tempcal1) %in% c("fDOM.QSU", "fDOM.QSU.1",
                                                    "Temp..C","Temp..C.1"))]
names(tempcal1) = c("fDOM.QSU_23C101758", "fDOM.QSU_23C101759",
                    "Temp.C_23G102560","Temp.C_23G102567")

tempcal2 = tempcal2[ , which(names(tempcal2) %in% c("fDOM.QSU", "fDOM.QSU.1",
                                                    "Temp..C","Temp..C.1"))]
names(tempcal2) = c("fDOM.QSU_23C101705", "fDOM.QSU_23C101760",
                    "Temp.C_23G102566","Temp.C_23G102568")

tempcal2[1099:1538,] = NA

tempcalall = cbind(tempcal1,tempcal2)

# make data frames

sonde_3231_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102566"],tempcalall[,"fDOM.QSU_23C101705"]))
names(sonde_3231_tempcal) = c("temp_C","fDOM_QSU")
sonde_3231_tempcal$t = c(1:1538)

sonde_5009_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102568"],tempcalall[,"fDOM.QSU_23C101760"]))
names(sonde_5009_tempcal) = c("temp_C","fDOM_QSU")

sonde_3230_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102567"],tempcalall[,"fDOM.QSU_23C101758"]))
names(sonde_3230_tempcal) = c("temp_C","fDOM_QSU")

sonde_3229_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102560"],tempcalall[,"fDOM.QSU_23C101759"]))
names(sonde_3229_tempcal) = c("temp_C","fDOM_QSU")


# trim data frames

plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$t)
sonde_3231_tempcal = sonde_3231_tempcal[-c(1:50, 950:1538),]
plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$t)

#

#### Define linear relationships and rhos ####

# sonde_3231 #
plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$temp_C)
m.3231 = lm(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$temp_C)
abline(m.3231)
summary(m.3231)

# sonde_5009 #
# plot(temp_cal$fDOM.16F.QSU ~ temp_cal$Temp_avg)
# m.16F = lm(temp_cal$fDOM.16F.QSU ~ temp_cal$Temp_avg)
# abline(m.16F)
# summary(m.16F)
# 
# # sonde_3230 #
# plot(temp_cal$fDOM.14B.QSU ~ temp_cal$Temp_avg)
# m.14B = lm(temp_cal$fDOM.14B.QSU ~ temp_cal$Temp_avg)
# abline(m.14B)
# summary(m.14B)
# 
# # sonde_3229 #
# plot(temp_cal$fDOM.14K.QSU ~ temp_cal$Temp_avg)
# m.14K = lm(temp_cal$fDOM.14K.QSU ~ temp_cal$Temp_avg)
# abline(m.14K)
# summary(m.14K)
# 
# fDOM_sensorID = c("15C", "16F", "14B", "14K")
# rho = c(m.15C$coefficients[2]/m.15C$coefficients[1], 
#         m.16F$coefficients[2]/m.16F$coefficients[1], 
#         m.14B$coefficients[2]/m.14B$coefficients[1], 
#         m.14K$coefficients[2]/m.14K$coefficients[1])
# rhos = data.frame(fDOM_sensorID, rho)
