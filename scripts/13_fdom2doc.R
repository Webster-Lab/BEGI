#### read me ####
# The purpose of this script is to calculate the conversion of EXO fDOM measurements to DOC and then apply the conversion

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(readxl)

#### import EXO data ####
ls_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1HnxuqNZlaXqVKSLhgcibviPokNH2-lom")
2

for (file_id in ls_tibble$id) {
  try({googledrive::drive_download(as_id(file_id))})
}

#### load and stitch EXO data ####

# import data
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
BEGI_EXOz.fd = list()
for(i in siteIDz){
  file_list <- list.files(recursive=F, pattern=paste("20250421")) #i, ".csv", sep=""
  BEGI_EXOz.fd[[i]] = lapply(file_list, read.csv, 
                          stringsAsFactors=FALSE, skip=8,header=T,
                          fileEncoding="utf-8") # this line makes it such that if there are any offending utf-16 encodings, it will show the offending file in the error message. If any utf-16 files are found, be sure to fix them in the Google Drive, not just your locally saved file!!
}

# use a set of column names as a template and match columns in all other files to that one. Note that this drops columns like Depth where the sensor isn't avialable on all sondes
universalnames = c("Date..MM.DD.YYYY.","Time..HH.mm.ss.","Time..Fract..Sec.","Site.Name","Cond.µS.cm","fDOM.QSU","fDOM.RFU","nLF.Cond.µS.cm","ODO...sat","ODO...local","ODO.mg.L","Sal.psu",  "SpCond.µS.cm","TDS.mg.L","Turbidity.FNU","TSS.mg.L","Temp..C","Battery.V","Cable.Pwr.V")
for(i in siteIDz){
  for(n in 1:length(BEGI_EXOz.fd[[i]])){
    BEGI_EXOz.fd[[i]][[n]] = 
      BEGI_EXOz.fd[[i]][[n]] [, intersect(universalnames, names(BEGI_EXOz.fd[[i]][[n]] )), drop=FALSE]
  }
}

# bind files within sites into one dataframe per site
for(i in siteIDz){
  BEGI_EXOz.fd[[i]] = do.call(plyr::rbind.fill, BEGI_EXOz.fd[[i]])
}

#### format dates ####

for(i in siteIDz){
  # put date and time in same column
  BEGI_EXOz.fd[[i]]$datetime = paste( BEGI_EXOz.fd[[i]]$Date..MM.DD.YYYY.,  BEGI_EXOz.fd[[i]]$Time..HH.mm.ss., sep = " ")
  # convert to POIXct and set timezone
  BEGI_EXOz.fd[[i]]$datetimeMT<-as.POSIXct( BEGI_EXOz.fd[[i]]$datetime, 
                                         format = "%m/%d/%Y %H:%M:%S",
                                         tz="US/Mountain")
  # replace two digit years that are converted incorrectly
  BEGI_EXOz.fd[[i]]$year = year(BEGI_EXOz.fd[[i]]$datetimeMT)
  BEGI_EXOz.fd[[i]]$year[BEGI_EXOz.fd[[i]]$year==0025] = 2025
  year(BEGI_EXOz.fd[[i]]$datetimeMT) = BEGI_EXOz.fd[[i]]$year
}

#### Check variable names ####
#check the variable order for each sonde and edit names if necessary

names(BEGI_EXOz.fd[["VDOW"]]) == names(BEGI_EXOz.fd[["VDOS"]])
names(BEGI_EXOz.fd[["VDOW"]]) == names(BEGI_EXOz.fd[["SLOW"]])
names(BEGI_EXOz.fd[["VDOW"]]) == names(BEGI_EXOz.fd[["SLOC"]])

#### Compile bursts within 1 min ####

# make sure all columns with numeric data data are numeric
BEGI_EXOz.fd <- lapply(BEGI_EXOz.fd, function(x) {x[5:19] <- lapply(x[5:19], as.numeric);x})

# get means and standard deviations of numeric burst values
BEGI_EXO.stz.fd = list()
for(i in siteIDz){
  min<-round_date(BEGI_EXOz.fd[[i]]$datetimeMT, "minute") # note rounding instead of using the function cut()!! cut was what was causing our memory issues!!
  BEGI_EXO.stz.fd[[i]] <- as.data.frame(as.list(aggregate(cbind(Cond.µS.cm, fDOM.QSU, fDOM.RFU,
                                                             nLF.Cond.µS.cm,
                                                             ODO...sat,ODO.mg.L,
                                                             Sal.psu,SpCond.µS.cm,
                                                             TDS.mg.L,Turbidity.FNU,TSS.mg.L,Temp..C,
                                                             Battery.V,Cable.Pwr.V) 
                                                       ~ min, data=BEGI_EXOz.fd[[i]], na.action=na.pass, FUN=function(x) c(mn=mean(x), SD=sd(x)))))
  BEGI_EXO.stz.fd[[i]]$datetimeMT<-as.POSIXct(BEGI_EXO.stz.fd[[i]]$min, "%Y-%m-%d %H:%M:%S", tz="US/Mountain")
}
#### save and re-add burst-compiled files ####

saveRDS(BEGI_EXO.stz.fd, "EXO_compiled/BEGI_EXO.stz.fd.rds")
rm(list = ls())
BEGI_EXO.stz.fd = readRDS("EXO_compiled/BEGI_EXO.stz.fd.rds")


#### plot to check ####
## SLOC##
tempdat = BEGI_EXO.stz.fd[["SLOC"]]

# Save plot 
jpeg("plots/SLOC_fdom2doc.jpg", width = 12, height = 6, units="in", res=1000)

plot.new()
plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="1 min"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

dev.off()

## SLOW##
tempdat = BEGI_EXO.stz.fd[["SLOW"]]

# Save plot 
jpeg("plots/SLOW_fdom2doc.jpg", width = 12, height = 6, units="in", res=1000)

plot.new()
plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="1 min"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

dev.off()

## VDOW##
tempdat = BEGI_EXO.stz.fd[["VDOW"]]

# Save plot 
jpeg("plots/VDOW_fdom2doc.jpg", width = 12, height = 6, units="in", res=1000)

plot.new()
plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="1 min"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

dev.off()


## VDOS##
tempdat = BEGI_EXO.stz.fd[["VDOS"]]

# Save plot 
jpeg("plots/VDOS_fdom2doc.jpg", width = 12, height = 6, units="in", res=1000)

plot.new()
plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="1 min"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

dev.off()

#### import DOC data ####
#get DOC data from google drive
doc_tibble <- googledrive::as_id("https://drive.google.com/drive/folders/1zdzsIXO5LIzbcg2RzfE4mz3dBKvBmrO-")

doc <- googledrive::drive_ls(path = doc_tibble, type = "xlsx")
2

googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2025-04-21_BEGI-Matrix-Spikes_DataReport_v1.xlsx"], 
                            path = "NPOC-TN_2025-04-21_BEGI-Matrix-Spikes_DataReport_v1.xlsx",
                            overwrite = T)
docdata <- read_xlsx("NPOC-TN_2025-04-21_BEGI-Matrix-Spikes_DataReport_v1.xlsx", sheet = 1, skip = 1)
