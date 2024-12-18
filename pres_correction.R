KABQ_pressure <- read.csv("~/Downloads/KABQ_pressure.csv")

#Parse DateTime column into two columns
KABQ_pressure$Date<-as.Date(KABQ_pressure$DateTime)
KABQ_pressure$Time<-format(as.POSIXct(KABQ_pressure$DateTime),format= "%H:%M:%S")
KABQ_pressure$DateTime<-NULL
KABQ_pressure

#Convert sea level pressure in mb to mm Hg
mb2mmhg <- function(pressure) {
  mmhg <- pressure*0.750062
  return(mmhg)
}
KABQ_pressure$Pressure = mb2mmhg(KABQ_pressure$Pressure)

# correct sea level pressure in mm Hg to raw barometric pressure 
sealevel2raw <- function(pressure) {
  raw <- pressure - (2.5*53.51)
  return(raw)
}
KABQ_pressure$Pressure = sealevel2raw(KABQ_pressure$Pressure)

#export
write.csv(KABQ_pressure,"/Users/etipps/Documents/UNM/Webster Lab/BEGI_data/KABQ_pressure_corrected.csv")
