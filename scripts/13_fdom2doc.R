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
library(dplyr)
library(lubridate)

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
  file_list <- list.files(recursive=FALSE, pattern=paste0("20250421_.*_", i, "_.*\\.csv$")) #i, ".csv", sep=""
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

googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2025-04-21_BEGI-Matrix-Spikes_DataReport_v2.xlsx"], 
                            path = "NPOC-TN_2025-04-21_BEGI-Matrix-Spikes_DataReport_v2.xlsx",
                            overwrite = T)
docdata <- read_xlsx("NPOC-TN_2025-04-21_BEGI-Matrix-Spikes_DataReport_v2.xlsx", sheet = 2)

#import DOC data without values removed (complete DOC data)
googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2025-04-21_BEGI-Matrix-Spikes_DataReport_v1.xlsx"], 
                            path = "NPOC-TN_2025-04-21_BEGI-Matrix-Spikes_DataReport_v1.xlsx",
                            overwrite = T)
docdata2 <- read_xlsx("NPOC-TN_2025-04-21_BEGI-Matrix-Spikes_DataReport_v1.xlsx", sheet = 2)

#### wrangle DOC data ####

names(docdata2)[names(docdata2) == 'Conc (mg/L)'] <- 'NPOC'
names(docdata)[names(docdata) == 'Conc (mg/L)'] <- 'NPOC'
names(docdata)[names(docdata) == 'Matrix Spike'] <- 'MatrixSpike'

#filter by well
docdata <- docdata %>%
  spread (Well, NPOC)

# filter docdata to df of each well
#VDOW
docVDOW <- data.frame(docdata$Sample,
                      docdata$VDOW,
                      docdata$MatrixSpike)
docVDOW <- na.omit(docVDOW)

#VDOS
docVDOS <- data.frame(docdata$Sample,
                      docdata$VDOS,
                      docdata$MatrixSpike)
docVDOS <- na.omit(docVDOS)

#SLOC
docSLOC <- data.frame(docdata$Sample,
                      docdata$SLOC,
                      docdata$MatrixSpike)
docSLOC <- na.omit(docSLOC)

#SLOW
docSLOW <- data.frame(docdata$Sample,
                      docdata$SLOW,
                      docdata$MatrixSpike)
docSLOW <- na.omit(docSLOW)


##repeat for complete DOC data##
#filter by well
docdata2 <- docdata2 %>%
  spread (Well, NPOC)

# filter docdata to df of each well
#VDOW
docVDOW2 <- data.frame(docdata2$Sample,
                      docdata2$VDOW,
                      docdata2$MatrixSpike)
docVDOW2 <- na.omit(docVDOW2)

#VDOS
docVDOS2 <- data.frame(docdata2$Sample,
                      docdata2$VDOS,
                      docdata2$MatrixSpike)
docVDOS2 <- na.omit(docVDOS2)

#SLOC
docSLOC2 <- data.frame(docdata2$Sample,
                      docdata2$SLOC,
                      docdata2$MatrixSpike)
docSLOC2 <- na.omit(docSLOC2)

#SLOW
docSLOW2 <- data.frame(docdata2$Sample,
                      docdata2$SLOW,
                      docdata2$MatrixSpike)
docSLOW2 <- na.omit(docSLOW2)


#### Partition/clean fDOM data ####
#add a column to label each chunk as Matrix spike
#VDOW#
BEGI_EXO.stz.fd[["VDOW"]] <- BEGI_EXO.stz.fd[["VDOW"]] %>%
  mutate(MatrixSpike = case_when(
    datetimeMT >= as.POSIXct("2025-04-21 14:28:00") & datetimeMT <= as.POSIXct("2025-04-21 14:42:00") ~ 0,
    datetimeMT >= as.POSIXct("2025-04-21 15:23:00") & datetimeMT <= as.POSIXct("2025-04-21 15:40:00") ~ 0.5,
    datetimeMT >= as.POSIXct("2025-04-21 15:51:00") & datetimeMT <= as.POSIXct("2025-04-21 16:05:00") ~ 1,
    datetimeMT >= as.POSIXct("2025-04-21 16:18:00") & datetimeMT <= as.POSIXct("2025-04-21 16:31:00") ~ 2,
    datetimeMT >= as.POSIXct("2025-04-21 16:47:00") & datetimeMT <= as.POSIXct("2025-04-21 17:01:00") ~ 5,
    datetimeMT >= as.POSIXct("2025-04-21 17:15:00") & datetimeMT <= as.POSIXct("2025-04-21 17:28:00") ~ 10,
  ))

#VDOS#
BEGI_EXO.stz.fd[["VDOS"]] <- BEGI_EXO.stz.fd[["VDOS"]] %>%
  mutate(MatrixSpike = case_when(
    datetimeMT >= as.POSIXct("2025-04-21 14:28:00") & datetimeMT <= as.POSIXct("2025-04-21 14:45:00") ~ 0,
    datetimeMT >= as.POSIXct("2025-04-21 15:21:00") & datetimeMT <= as.POSIXct("2025-04-21 15:37:00") ~ 0.5,
    datetimeMT >= as.POSIXct("2025-04-21 15:53:00") & datetimeMT <= as.POSIXct("2025-04-21 16:05:00") ~ 1,
    datetimeMT >= as.POSIXct("2025-04-21 16:21:00") & datetimeMT <= as.POSIXct("2025-04-21 16:33:00") ~ 2,
    datetimeMT >= as.POSIXct("2025-04-21 16:48:00") & datetimeMT <= as.POSIXct("2025-04-21 17:03:00") ~ 5,
    datetimeMT >= as.POSIXct("2025-04-21 17:16:00") & datetimeMT <= as.POSIXct("2025-04-21 17:30:00") ~ 10,
  ))

#SLOC#
BEGI_EXO.stz.fd[["SLOC"]] <- BEGI_EXO.stz.fd[["SLOC"]] %>%
  mutate(MatrixSpike = case_when(
    datetimeMT >= as.POSIXct("2025-04-21 14:30:00") & datetimeMT <= as.POSIXct("2025-04-21 14:47:00") ~ 0,
    datetimeMT >= as.POSIXct("2025-04-21 15:20:00") & datetimeMT <= as.POSIXct("2025-04-21 15:34:00") ~ 0.5,
    datetimeMT >= as.POSIXct("2025-04-21 15:55:00") & datetimeMT <= as.POSIXct("2025-04-21 16:07:00") ~ 1,
    datetimeMT >= as.POSIXct("2025-04-21 16:22:00") & datetimeMT <= as.POSIXct("2025-04-21 16:35:00") ~ 2,
    datetimeMT >= as.POSIXct("2025-04-21 16:50:00") & datetimeMT <= as.POSIXct("2025-04-21 17:05:00") ~ 5,
    datetimeMT >= as.POSIXct("2025-04-21 17:18:00") & datetimeMT <= as.POSIXct("2025-04-21 17:33:00") ~ 10,
  ))

#SLOW#
BEGI_EXO.stz.fd[["SLOW"]] <- BEGI_EXO.stz.fd[["SLOW"]] %>%
  mutate(MatrixSpike = case_when(
    datetimeMT >= as.POSIXct("2025-04-21 14:52:00") & datetimeMT <= as.POSIXct("2025-04-21 15:06:00") ~ 0,
    datetimeMT >= as.POSIXct("2025-04-21 15:18:00") & datetimeMT <= as.POSIXct("2025-04-21 15:32:00") ~ 0.5,
    datetimeMT >= as.POSIXct("2025-04-21 15:56:00") & datetimeMT <= as.POSIXct("2025-04-21 16:09:00") ~ 1,
    datetimeMT >= as.POSIXct("2025-04-21 16:24:00") & datetimeMT <= as.POSIXct("2025-04-21 16:37:00") ~ 2,
    datetimeMT >= as.POSIXct("2025-04-21 16:52:00") & datetimeMT <= as.POSIXct("2025-04-21 17:07:00") ~ 5,
    datetimeMT >= as.POSIXct("2025-04-21 17:19:00") & datetimeMT <= as.POSIXct("2025-04-21 17:35:00") ~ 10,
  ))

#trim beginning/end (?) of each spike chunk + take average of middle (?)
#VDOW#
trim_n <- 4

VDOW_mean_fdom <- BEGI_EXO.stz.fd[["VDOW"]] %>%
  filter(!is.na(MatrixSpike)) %>%
  group_by(MatrixSpike) %>%
  arrange(datetimeMT, .by_group = TRUE) %>%
  mutate(row_num = row_number(),
         group_size = n()) %>%
  filter(row_num > trim_n & row_num <= group_size - trim_n) %>%
  summarise(mean_fDOM = mean(fDOM.QSU.mn, na.rm = TRUE), .groups = "drop") %>%
  deframe()  # turns a 2-col tibble into a named vector: names are MatrixSpike values
#unname(VDOW_mean_fdom)

#VDOS#
trim_n <- 4

VDOS_mean_fdom <- BEGI_EXO.stz.fd[["VDOS"]] %>%
  filter(!is.na(MatrixSpike)) %>%
  group_by(MatrixSpike) %>%
  arrange(datetimeMT, .by_group = TRUE) %>%
  mutate(row_num = row_number(),
         group_size = n()) %>%
  filter(row_num > trim_n & row_num <= group_size - trim_n) %>%
  summarise(mean_fDOM = mean(fDOM.QSU.mn, na.rm = TRUE), .groups = "drop") %>%
  deframe()  # turns a 2-col tibble into a named vector: names are MatrixSpike values
#unname(VDOS_mean_fdom)

#SLOC#
trim_n <- 4

SLOC_mean_fdom <- BEGI_EXO.stz.fd[["SLOC"]] %>%
  filter(!is.na(MatrixSpike)) %>%
  group_by(MatrixSpike) %>%
  arrange(datetimeMT, .by_group = TRUE) %>%
  mutate(row_num = row_number(),
         group_size = n()) %>%
  filter(row_num > trim_n & row_num <= group_size - trim_n) %>%
  summarise(mean_fDOM = mean(fDOM.QSU.mn, na.rm = TRUE), .groups = "drop") %>%
  deframe()  # turns a 2-col tibble into a named vector: names are MatrixSpike values
#unname(SLOC_mean_fdom)

#SLOW#
trim_n <- 4

SLOW_mean_fdom <- BEGI_EXO.stz.fd[["SLOW"]] %>%
  filter(!is.na(MatrixSpike)) %>%
  group_by(MatrixSpike) %>%
  arrange(datetimeMT, .by_group = TRUE) %>%
  mutate(row_num = row_number(),
         group_size = n()) %>%
  filter(row_num > trim_n & row_num <= group_size - trim_n) %>%
  summarise(mean_fDOM = mean(fDOM.QSU.mn, na.rm = TRUE), .groups = "drop") %>%
  deframe()  # turns a 2-col tibble into a named vector: names are MatrixSpike values
#unname(SLOW_mean_fdom)

#### combined doc/fdom df for each well ####
#VDOW#
#remove value not included in docdata
VDOW_mean_fdom <- VDOW_mean_fdom[-4]
docVDOW$fdom <- VDOW_mean_fdom[match(docVDOW$docdata.MatrixSpike,names(VDOW_mean_fdom))]

#VDOS#
#remove value not included in docdata
VDOS_mean_fdom <- VDOS_mean_fdom[-4]
docVDOS$fdom <- VDOS_mean_fdom[match(docVDOS$docdata.MatrixSpike,names(VDOS_mean_fdom))]

#SLOC#
#remove value not included in docdata
SLOC_mean_fdom <- SLOC_mean_fdom[-c(2,4)]
docSLOC$fdom <- SLOC_mean_fdom[match(docSLOC$docdata.MatrixSpike,names(SLOC_mean_fdom))]

#SLOW#
#remove value not included in docdata
SLOW_mean_fdom <- SLOW_mean_fdom[-4]
docSLOW$fdom <- SLOW_mean_fdom[match(docSLOW$docdata.MatrixSpike,names(SLOW_mean_fdom))]


#repeat for complete DOC data
#VDOW#
docVDOW2$fdom <- VDOW_mean_fdom[match(docVDOW2$docdata2.MatrixSpike,names(VDOW_mean_fdom))]

#VDOS#
docVDOS2$fdom <- VDOS_mean_fdom[match(docVDOS2$docdata2.MatrixSpike,names(VDOS_mean_fdom))]

#SLOC#
docSLOC2$fdom <- SLOC_mean_fdom[match(docSLOC2$docdata2.MatrixSpike,names(SLOC_mean_fdom))]

#SLOW#
docSLOW2$fdom <- SLOW_mean_fdom[match(docSLOW2$docdata2.MatrixSpike,names(SLOW_mean_fdom))]

#### linear regression fdom2doc ####
#VDOW#
plot(docVDOW$docdata.VDOW, docVDOW$fdom,
     xlab = "NPOC (VDOW)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.VDOW <- lm(fdom ~ docdata.VDOW, data = docVDOW)
abline(m.VDOW, col = "blue", lwd = 2)
summary(m.VDOW)

#VDOS#
plot(docVDOS$docdata.VDOS, docVDOS$fdom,
     xlab = "NPOC (VDOS)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.VDOS <- lm(fdom ~ docdata.VDOS, data = docVDOS)
abline(m.VDOS, col = "blue", lwd = 2)
summary(m.VDOS)

#SLOC#
plot(docSLOC$docdata.SLOC, docSLOC$fdom,
     xlab = "NPOC (SLOC)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.SLOC <- lm(fdom ~ docdata.SLOC, data = docSLOC)
abline(m.SLOC, col = "blue", lwd = 2)
summary(m.SLOC)

#SLOW#
plot(docSLOW$docdata.SLOW, docSLOW$fdom,
     xlab = "NPOC (SLOW)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.SLOW <- lm(fdom ~ docdata.SLOW, data = docSLOW)
abline(m.SLOW, col = "blue", lwd = 2)
summary(m.SLOW)


##complete DOC data##
#VDOW#
plot(docVDOW2$docdata2.VDOW, docVDOW2$fdom,
     xlab = "NPOC (VDOW)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.VDOW <- lm(fdom ~ docdata2.VDOW, data = docVDOW2)
abline(m.VDOW, col = "blue", lwd = 2)
summary(m.VDOW)

#VDOS#
plot(docVDOS2$docdata2.VDOS, docVDOS2$fdom,
     xlab = "NPOC (VDOS)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.VDOS <- lm(fdom ~ docdata2.VDOS, data = docVDOS2)
abline(m.VDOS, col = "blue", lwd = 2)
summary(m.VDOS)

#SLOC#
plot(docSLOC2$docdata2.SLOC, docSLOC2$fdom,
     xlab = "NPOC (SLOC)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.SLOC <- lm(fdom ~ docdata2.SLOC, data = docSLOC2)
abline(m.SLOC, col = "blue", lwd = 2)
summary(m.SLOC)

#SLOW#
plot(docSLOW2$docdata2.SLOW, docSLOW2$fdom,
     xlab = "NPOC (SLOW)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.SLOW <- lm(fdom ~ docdata2.SLOW, data = docSLOW2)
abline(m.SLOW, col = "blue", lwd = 2)
summary(m.SLOW)







