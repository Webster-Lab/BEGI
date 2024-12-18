KABQ_pressure_full <- read.csv("~/Downloads/KABQ_pressure_full.csv")

#Parse DateTime column into two columns
KABQ_pressure_full$Date<-as.Date(KABQ_pressure_full$DateTime)
KABQ_pressure_full$Time<-format(as.POSIXct(KABQ_pressure_full$DateTime),format= "%H:%M:%S")
KABQ_pressure_full$DateTime<-NULL

#Convert sea level pressure in mb to mm Hg
mb2mmhg <- function(pressure) {
  mmhg <- pressure*0.750062
  return(mmhg)
}
KABQ_pressure_full$Pressure = mb2mmhg(KABQ_pressure_full$Pressure)

# correct sea level pressure in mm Hg to raw barometric pressure 
sealevel2raw <- function(pressure) {
  raw <- pressure - (2.5*53.51)
  return(raw)
}
KABQ_pressure_full$Pressure = sealevel2raw(KABQ_pressure_full$Pressure)

#export
write.csv(KABQ_pressure_full,"/Users/etipps/Documents/UNM/Webster Lab/BEGI_data/KABQ_pressure_correctedfull.csv")
