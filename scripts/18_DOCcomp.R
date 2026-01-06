#### read me ####
#The purpose of this script is to compare the DOC concentrations in the wells with the levels in the Rio Grande
#This script will compile a dataset of the DOC grab samples for each well and plot a boxplot to understand each well's distribution
#Where to find river DOC level?

#### Libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(readxl)
library(dplyr)
library(lubridate)

#### Import DOC data ####
doc_tibble <- googledrive::as_id("https://drive.google.com/drive/folders/1292UXqpLoBdB1uFytiBnxg6zFvO6AUMI")

doc <- googledrive::drive_ls(path = doc_tibble, type = "xlsx")
2

googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2024-10-10_updated.xlsx"],
                            path = "NPOC-TN_2024-10-10_updated.xlsx",
                            overwrite = T)
doc1 <- read_xlsx("NPOC-TN_2024-10-10_updated.xlsx", sheet = 6, skip = 1)

googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2024-10-23_updated.xlsx"],
                            path = "NPOC-TN_2024-10-23_updated.xlsx",
                            overwrite = T)
doc2 <- read_xlsx("NPOC-TN_2024-10-23_updated.xlsx", sheet = 7, skip = 1)

googledrive::drive_download(file = doc$id[doc$name=="240620_BEGI_Data.xlsx"],
                            path = "240620_BEGI_Data.xlsx",
                            overwrite = T)
doc3 <- read_xlsx("240620_BEGI_Data.xlsx")

googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2025-01-14_BEGI_rerun.xlsx"],
                            path = "NPOC-TN_2025-01-14_BEGI_rerun.xlsx",
                            overwrite = T)
doc4 <- read_xlsx("NPOC-TN_2025-01-14_BEGI_rerun.xlsx", sheet = 8)

googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2025-01-22_BEGI_rerun.xlsx"],
                            path = "NPOC-TN_2025-01-22_BEGI_rerun.xlsx",
                            overwrite = T)
doc5 <- read_xlsx("NPOC-TN_2025-01-22_BEGI_rerun.xlsx", sheet = 7)

#### clean up files ####
# remove columns 6-8 for doc1, doc2
doc1 <- doc1[,-(6:8)]
doc2 <- doc2[,-(6:8)]

#make sure column names match
names(doc1)[names(doc1) == '...1'] <- 'date'
names(doc1)[names(doc1) == '...2'] <- 'well'
names(doc1)[names(doc1) == '...3'] <- 'sample'

names(doc2)[names(doc2) == '...1'] <- 'date'
names(doc2)[names(doc2) == '...2'] <- 'well'
names(doc2)[names(doc2) == '...3'] <- 'sample'

# remove columns 6-8 for doc1, doc2
doc1 <- doc1[,-(6:8)]
doc2 <- doc2[,-(6:8)]

#make sure column names match
names(doc1)[names(doc1) == '...1'] <- 'date'
names(doc1)[names(doc1) == '...2'] <- 'well'
names(doc1)[names(doc1) == '...3'] <- 'sample'

names(doc3)[names(doc3) == 'Collection Date'] <- 'date'
names(doc3)[names(doc3) == 'WellID'] <- 'well'
names(doc3)[names(doc3) == 'Sample'] <- 'sample'
names(doc3)[names(doc3) == 'NPOC (mg C/L)'] <- 'NPOC'
names(doc3)[names(doc3) == 'TDN (mg N/L)'] <- 'TN'

names(doc4)[names(doc4) == 'Sample_ID'] <- 'sample'
names(doc4)[names(doc4) == 'NPOC_mg_L'] <- 'NPOC'
names(doc4)[names(doc4) == 'TN_mg_L'] <- 'TN'

names(doc5)[names(doc5) == 'Sample_ID'] <- 'sample'
names(doc5)[names(doc5) == 'NPOC_mg_L'] <- 'NPOC'
names(doc5)[names(doc5) == 'TN_mg_L'] <- 'TN'

#### compile ####
docdata = rbind(doc1, doc2, doc3, doc4, doc5)

#remove NAs
docdata <-na.omit(docdata)

#### Plot ####
doc_bywell<-ggplot(data=docdata,mapping=aes(x=well, y=log(NPOC)))+
  geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+
  labs(y = "log DOC concentration (mg/L)")
print(doc_bywell)

# #### Import prelim (2006-2007) River DOC data ####
# # Package ID: knb-lter-sev.180.374117 Cataloging System:https://pasta.lternet.edu.
# # Data set title: Rio Grande Water Chemistry Data from Bernalillo County, New Mexico (2006-2007).
# # Data set creator:  David Van Horn -  
# # Data set creator:  Clifford Dahm -  
# # Metadata Provider:  Information Manager Sevilleta LTER -  
# # Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# # Contact:  Information Manager Sevilleta LTER -    - data-use@sevilleta.unm.edu
# # Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.180.374117
# # Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# # Uncomment the following lines to have R clear previous work, or set a working directory
# # rm(list=ls())      
# 
# # setwd("C:/users/my_name/my_dir")       
# 
# 
# 
# options(HTTPUserAgent="EDI_CodeGen")
# 
# 
# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/180/374117/0e97405377819cf969264f68533e4b45" 
# infile1 <- tempfile()
# try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# 
# 
# dt1 <-read.csv(infile1,header=F 
#                ,skip=1
#                ,sep=","  
#                , col.names=c(
#                  "site_name",     
#                  "site",     
#                  "monthly",     
#                  "latitude",     
#                  "longitude",     
#                  "elevation",     
#                  "river.hyphen.kilometer",     
#                  "rgtrib",     
#                  "date",     
#                  "time",     
#                  "temp",     
#                  "ph",     
#                  "cond",     
#                  "DOC_Conc",     
#                  "IC_Conc",     
#                  "SUVA_Conc",     
#                  "Na_Conc",     
#                  "K_Conc",     
#                  "Mg_Conc",     
#                  "Ca_Conc",     
#                  "NH4_Conc",     
#                  "NO3_Conc",     
#                  "PO4_Conc",     
#                  "SO4_Conc",     
#                  "Br_Conc",     
#                  "Cl_Conc",     
#                  "Chlorophylla"    ), check.names=TRUE)
# 
# unlink(infile1)
# 
# #### Fix column classes ####
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
# 
# if (class(dt1$site_name)!="factor") dt1$site_name<- as.factor(dt1$site_name)
# if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)
# if (class(dt1$monthly)!="factor") dt1$monthly<- as.factor(dt1$monthly)
# if (class(dt1$latitude)=="factor") dt1$latitude <-as.numeric(levels(dt1$latitude))[as.integer(dt1$latitude) ]               
# if (class(dt1$latitude)=="character") dt1$latitude <-as.numeric(dt1$latitude)
# if (class(dt1$longitude)=="factor") dt1$longitude <-as.numeric(levels(dt1$longitude))[as.integer(dt1$longitude) ]               
# if (class(dt1$longitude)=="character") dt1$longitude <-as.numeric(dt1$longitude)
# if (class(dt1$elevation)=="factor") dt1$elevation <-as.numeric(levels(dt1$elevation))[as.integer(dt1$elevation) ]               
# if (class(dt1$elevation)=="character") dt1$elevation <-as.numeric(dt1$elevation)
# if (class(dt1$river.hyphen.kilometer)=="factor") dt1$river.hyphen.kilometer <-as.numeric(levels(dt1$river.hyphen.kilometer))[as.integer(dt1$river.hyphen.kilometer) ]               
# if (class(dt1$river.hyphen.kilometer)=="character") dt1$river.hyphen.kilometer <-as.numeric(dt1$river.hyphen.kilometer)
# if (class(dt1$rgtrib)!="factor") dt1$rgtrib<- as.factor(dt1$rgtrib)                                   
# # attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
# tmpDateFormat<-"%m/%d/%Y"
# tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# # Keep the new dates only if they all converted correctly
# if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
# 
# if (class(dt1$temp)=="factor") dt1$temp <-as.numeric(levels(dt1$temp))[as.integer(dt1$temp) ]               
# if (class(dt1$temp)=="character") dt1$temp <-as.numeric(dt1$temp)
# if (class(dt1$ph)=="factor") dt1$ph <-as.numeric(levels(dt1$ph))[as.integer(dt1$ph) ]               
# if (class(dt1$ph)=="character") dt1$ph <-as.numeric(dt1$ph)
# if (class(dt1$cond)=="factor") dt1$cond <-as.numeric(levels(dt1$cond))[as.integer(dt1$cond) ]               
# if (class(dt1$cond)=="character") dt1$cond <-as.numeric(dt1$cond)
# if (class(dt1$DOC_Conc)=="factor") dt1$DOC_Conc <-as.numeric(levels(dt1$DOC_Conc))[as.integer(dt1$DOC_Conc) ]               
# if (class(dt1$DOC_Conc)=="character") dt1$DOC_Conc <-as.numeric(dt1$DOC_Conc)
# if (class(dt1$IC_Conc)=="factor") dt1$IC_Conc <-as.numeric(levels(dt1$IC_Conc))[as.integer(dt1$IC_Conc) ]               
# if (class(dt1$IC_Conc)=="character") dt1$IC_Conc <-as.numeric(dt1$IC_Conc)
# if (class(dt1$SUVA_Conc)!="factor") dt1$SUVA_Conc<- as.factor(dt1$SUVA_Conc)
# if (class(dt1$Na_Conc)=="factor") dt1$Na_Conc <-as.numeric(levels(dt1$Na_Conc))[as.integer(dt1$Na_Conc) ]               
# if (class(dt1$Na_Conc)=="character") dt1$Na_Conc <-as.numeric(dt1$Na_Conc)
# if (class(dt1$K_Conc)=="factor") dt1$K_Conc <-as.numeric(levels(dt1$K_Conc))[as.integer(dt1$K_Conc) ]               
# if (class(dt1$K_Conc)=="character") dt1$K_Conc <-as.numeric(dt1$K_Conc)
# if (class(dt1$Mg_Conc)=="factor") dt1$Mg_Conc <-as.numeric(levels(dt1$Mg_Conc))[as.integer(dt1$Mg_Conc) ]               
# if (class(dt1$Mg_Conc)=="character") dt1$Mg_Conc <-as.numeric(dt1$Mg_Conc)
# if (class(dt1$Ca_Conc)=="factor") dt1$Ca_Conc <-as.numeric(levels(dt1$Ca_Conc))[as.integer(dt1$Ca_Conc) ]               
# if (class(dt1$Ca_Conc)=="character") dt1$Ca_Conc <-as.numeric(dt1$Ca_Conc)
# if (class(dt1$NH4_Conc)=="factor") dt1$NH4_Conc <-as.numeric(levels(dt1$NH4_Conc))[as.integer(dt1$NH4_Conc) ]               
# if (class(dt1$NH4_Conc)=="character") dt1$NH4_Conc <-as.numeric(dt1$NH4_Conc)
# if (class(dt1$NO3_Conc)=="factor") dt1$NO3_Conc <-as.numeric(levels(dt1$NO3_Conc))[as.integer(dt1$NO3_Conc) ]               
# if (class(dt1$NO3_Conc)=="character") dt1$NO3_Conc <-as.numeric(dt1$NO3_Conc)
# if (class(dt1$PO4_Conc)=="factor") dt1$PO4_Conc <-as.numeric(levels(dt1$PO4_Conc))[as.integer(dt1$PO4_Conc) ]               
# if (class(dt1$PO4_Conc)=="character") dt1$PO4_Conc <-as.numeric(dt1$PO4_Conc)
# if (class(dt1$SO4_Conc)=="factor") dt1$SO4_Conc <-as.numeric(levels(dt1$SO4_Conc))[as.integer(dt1$SO4_Conc) ]               
# if (class(dt1$SO4_Conc)=="character") dt1$SO4_Conc <-as.numeric(dt1$SO4_Conc)
# if (class(dt1$Br_Conc)=="factor") dt1$Br_Conc <-as.numeric(levels(dt1$Br_Conc))[as.integer(dt1$Br_Conc) ]               
# if (class(dt1$Br_Conc)=="character") dt1$Br_Conc <-as.numeric(dt1$Br_Conc)
# if (class(dt1$Cl_Conc)=="factor") dt1$Cl_Conc <-as.numeric(levels(dt1$Cl_Conc))[as.integer(dt1$Cl_Conc) ]               
# if (class(dt1$Cl_Conc)=="character") dt1$Cl_Conc <-as.numeric(dt1$Cl_Conc)
# if (class(dt1$Chlorophylla)=="factor") dt1$Chlorophylla <-as.numeric(levels(dt1$Chlorophylla))[as.integer(dt1$Chlorophylla) ]               
# if (class(dt1$Chlorophylla)=="character") dt1$Chlorophylla <-as.numeric(dt1$Chlorophylla)
# 
# # Convert Missing Values to NA for non-dates
# 
# 
# 
# # Here is the structure of the input data frame:
# str(dt1)                            
# attach(dt1)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# #### River data summaries ####
# 
# summary(site_name)
# summary(site)
# summary(monthly)
# summary(latitude)
# summary(longitude)
# summary(elevation)
# summary(river.hyphen.kilometer)
# summary(rgtrib)
# summary(date)
# summary(time)
# summary(temp)
# summary(ph)
# summary(cond)
# summary(DOC_Conc)
# summary(IC_Conc)
# summary(SUVA_Conc)
# summary(Na_Conc)
# summary(K_Conc)
# summary(Mg_Conc)
# summary(Ca_Conc)
# summary(NH4_Conc)
# summary(NO3_Conc)
# summary(PO4_Conc)
# summary(SO4_Conc)
# summary(Br_Conc)
# summary(Cl_Conc)
# summary(Chlorophylla) 
# # Get more details on character variables
# 
# summary(as.factor(dt1$site_name)) 
# summary(as.factor(dt1$site)) 
# summary(as.factor(dt1$monthly)) 
# summary(as.factor(dt1$rgtrib)) 
# summary(as.factor(dt1$SUVA_Conc))
# detach(dt1)               
# 

#### Import USGS river data ####
usgsdoc <- read_csv("/Users/etipps/Documents/UNM/Webster Lab/BEGI_data/Data/fullphyschem_VDO.csv")

#### Separate river DOC ####
#dataframe of river DOC labelled as site = river
#combine with docdata for df of site/well and NPOC value

# well <- rep(c("River"), length.out = length(dt1$DOC_Conc))
# NPOC <- dt1$DOC_Conc
well <- rep(c("River"), length.out = length(usgsdoc$Result_Measure))
NPOC <- usgsdoc$Result_Measure
riverdt <- data.frame(well, NPOC)

docdata = select(docdata, 'well','NPOC')

docdata <- rbind(riverdt,docdata)

#### Plot with river data ####
doc_well_river<-ggplot(data=docdata,mapping=aes(x=well, y=log(NPOC)))+
  geom_boxplot(fill=c("#BB3754FF","#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+
  labs(y = "log DOC concentration (mg/L)")
print(doc_well_river)





