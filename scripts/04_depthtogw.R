#### read me ####
# the purpose of this script is to use corrected sensor depth data and bemp gw depth data to create a continuous dataset of depth to gw.
# sensor depth data will first be corrected to account for any jumps in sensor data
# sensor depth data will be plotted against bemp groundwater data. The slope will of the relationship will be calculated to estimate depth to gw
#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)

#### load PT data from google drive ####

ls_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1pAKtMV_qqBv55d-RwbTGTjmpj8loRHFC")
2
for (file_id in ls_tibble$id) {
  try({googledrive::drive_download(as_id(file_id))})
}
# add overwrite = TRUE if for some reason you want to replace files previously downloaded. 

#stitch together data files for each well
#plot PT data to see where jumps are
#edit dataframe, re plot to check
####load in bemp dataset (from google of individual measurements)####
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

#for each well, plot PT data v. BEMP measurements
#calculate slope for each well