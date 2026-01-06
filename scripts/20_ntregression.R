#### read me ####
# this is going to be an attempt at the applying the nighttime regression method to estimate gas
# exchange (K) in our wells. This method is based off of the idea that oxygen is out of equilibrium at
# the start of nighttime due to the day's photosynthetic activity. The rate of return to equilibrium
# is a function of the gas exchange rate.
# change in O2/change in t = ER + K * (Osat - O)
# Plot change in O2 over time on y-axis with DO deficit (Osat - O) on x-axis and regress for slope (K)

#### Libraries ####
library(tidyverse)
library(chron)
library(suncalc)
library(dplyr)
library(lubridate)
library(ggplot2)

#### Import compiled EXO1 RDS file ####
#BEGI_EXO.or2 = readRDS("EXO_compiled/BEGI_EXO.or2.rds")

#Changing BEGI_EXO.or2 to EXOz.dtw to include temp corrected fDOM data in each dataframe and depth to water
EXOz.dtw = readRDS("EXO_compiled/BEGI_EXOz.dtw.rds")

#### Correct negative DO values ####
EXOz.dtw[["VDOW"]]$ODO.mg.L.mn <- EXOz.dtw[["VDOW"]]$ODO.mg.L.mn + 0.36
EXOz.dtw[["VDOS"]]$ODO.mg.L.mn <- EXOz.dtw[["VDOS"]]$ODO.mg.L.mn + 0.42
EXOz.dtw[["SLOW"]]$ODO.mg.L.mn <- EXOz.dtw[["SLOW"]]$ODO.mg.L.mn + 0.32
EXOz.dtw[["SLOC"]]$ODO.mg.L.mn <- EXOz.dtw[["SLOC"]]$ODO.mg.L.mn + 0.2

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

#### O2 saturation function ####
## LOAD function to calculate oxygen saturation given water temperature and barometric pressure.

## oxygen saturation. From García and Gordonn (1992).
## calculates mL gas / dm^3 water; the below equation converts to mg/L.    
## BP in mm Hg. 
## 1.42905 is 'correct' and accounts for the fact that O2 is a non-ideal gas.  
## u is the vapor pressure of water

osat<- function(temp, bp) {
  
  tstd<-log((298.15-temp) / (273.15 + temp))
  
  a0<-2.00907
  a1<-3.22014
  a2<-4.0501
  a3<-4.94457
  a4<- -0.256847
  a5<- 3.88767
  
  u<-10^(8.10765-(1750.286/(235+temp)))
  
  sato<-(exp(a0 + a1*tstd + a2*tstd^2 + a3*tstd^3 + a4*tstd^4+a5*tstd^5))*((bp-u)/(760-u))*1.42905
  sato
}

#### Barometric pressure function ####
## Function to correct barometric pressure for altitude. From Colt (2012).
## This function gives bp in mmHg for altitude given nearby measurement of standardized barometric pressure. 
## temp is degC 
## alt is m
## bpst is in inches of Hg and the sort of data you will find from weather stations.  

####function returns mm of Hg
bpcalc<- function(bpst, alt) {
  bpst*25.4*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}
#end of function


#### K600 from oxygen function ####
# NOTE: The functions you use will depend on the methods used to estimate K (O2, propane, SF6). The temperature correction (Kcor) is embedded in the models below, but the Kcor function must be loaded into R before running the model.

# UNITS are day-1

## This code does the opposite of Kcor below; it estimates K600 for KO2 at a given temperature. From Wanninkhof (1992).
K600fromO2<-function (temp, KO2) {
  ((600/(1800.6 - (120.1 * temp) + (3.7818 * temp^2) - (0.047608 * temp^3)))^-0.5) * KO2
}
#### Nighttime regression function ####
### nighttime regression code to estimate K. Approach as in Hornberger and Kelly (1975).
## o2 file is your oxygen data (defined in subsetting)
## bp is barometric pressure in mm Hg for your site, 
## ts is the time step in MINUTES (not days as in metabolism code below)

nightreg<-function(o2file, bp, ts){
  
  temp<-o2file$temp
  oxy<-o2file$oxy
  
  # moving average on oxy data
  oxyf1<-stats::filter(o2file$oxy,rep(1/3,3), sides=2)
  
  # trim the ends of the oxy data
  oxyf2<- oxyf1[c(-1,-length(oxyf1))]
  
  # calculate delO/delt; convert to units of days by ts in min-1*1440
  deltaO2<-((oxyf2[-1]-oxyf2[-length(oxyf2)])/ts)*1440
  
  # Trim the first two and last one from the temp data to match the filter oxy data
  temptrim<-temp[c(-2:-1,-length(temp))]
  
  # calc the dodef
  satdef<-osat(temptrim,bp)-oxyf2[-1]
  
  # fit linear model and plot using linear model fitting (lm) and abline functions in R stats package
  nreg<-lm(deltaO2~satdef)
  plot(satdef,deltaO2)
  abline(nreg)
  
  # use coef function in R stats package to get lm coefficients
  coeff<-coef(nreg)
  
  # output gives lm coeff and K600 (converted from nighttime regression estimate of KO2)
  out<-list(coeff, K600fromO2(mean(temp), coeff[2]))
  out
  
}
#end of function

#### Example input to calculate K with NT regression ####
# NOTE: this approach works better for some sites/dates than others; always check that your model fit is good and that your K600 estimate makes sense!

#Call as:  The first argument in the function defines when to pull data.  In this case on 10/27/204 (for spring creek) between 18:05 and 23:00

nightreg(spring[spring$dtime>=as.numeric(chron(dates="10/27/14", times="18:05:00")) & spring$dtime<=as.numeric(chron(dates="10/27/14", times="23:00:00")), ], bp=595, ts=10)

nightreg(french[french$dtime>=as.numeric(chron(dates="09/21/12", times="19:40:00")) & french$dtime<=as.numeric(chron(dates="09/21/12", times="23:00:00")), ], bp=523, ts=5)

nightreg(french[french$dtime>=as.numeric(chron(dates="09/22/12", times="19:40:00")) & french$dtime<=as.numeric(chron(dates="09/22/12", times="23:00:00")), ], bp=523, ts=5)

#### Calculate bp ####
# average bp abq sunport = 24.75 in Hg
# elevation = 1631 m
BEGIbp = bpcalc(24.75, 1631)

#### Attempt 1 with BEGI - prepping data ####
## for this code to work you need your data looking like the example datasets below:
## temp = water temperature in Celcius
## oxy = dissolved oxygen in mg/L

# Make copy of each df and rename columns
SLOC <- EXOz.dtw[["SLOC"]]
SLOC$oxy <- SLOC$ODO.mg.L.mn
SLOC$temp <- SLOC$Temp..C.mn

SLOW <- EXOz.dtw[["SLOW"]]
SLOW$oxy <- SLOW$ODO.mg.L.mn
SLOW$temp <- SLOW$Temp..C.mn

VDOW <- EXOz.dtw[["VDOW"]]
VDOW$oxy <- VDOW$ODO.mg.L.mn
VDOW$temp <- VDOW$Temp..C.mn

VDOS <- EXOz.dtw[["VDOS"]]
VDOS$oxy <- VDOS$ODO.mg.L.mn
VDOS$temp <- VDOS$Temp..C.mn

#Plot to check
plot(SLOC$datetimeMT, SLOC$oxy)
plot(SLOW$datetimeMT, SLOW$oxy)
plot(VDOW$datetimeMT, VDOW$oxy)
plot(VDOS$datetimeMT, VDOS$oxy)

# Remove outliers #
# Check validity on this????? #
# SLOC #
Q1 <- quantile(SLOC$oxy, .25, na.rm = TRUE)
Q3 <- quantile(SLOC$oxy, .75, na.rm = TRUE)
IQR <- IQR(SLOC$oxy, na.rm = TRUE)

SLOC<- subset(SLOC, SLOC$oxy > (Q1 - 1.5*IQR) & SLOC$oxy < (Q3 + 1.5*IQR))

# SLOW #
Q1 <- quantile(SLOW$oxy, .25, na.rm = TRUE)
Q3 <- quantile(SLOW$oxy, .75, na.rm = TRUE)
IQR <- IQR(SLOW$oxy, na.rm = TRUE)

SLOW<- subset(SLOW, SLOW$oxy > (Q1 - 1.5*IQR) & SLOW$oxy < (Q3 + 1.5*IQR))

# VDOW #
Q1 <- quantile(VDOW$oxy, .25, na.rm = TRUE)
Q3 <- quantile(VDOW$oxy, .75, na.rm = TRUE)
IQR <- IQR(VDOW$oxy, na.rm = TRUE)

VDOW<- subset(VDOW, VDOW$oxy > (Q1 - 1.5*IQR) & VDOW$oxy < (Q3 + 1.5*IQR))

# VDOS #
Q1 <- quantile(VDOS$oxy, .25, na.rm = TRUE)
Q3 <- quantile(VDOS$oxy, .75, na.rm = TRUE)
IQR <- IQR(VDOS$oxy, na.rm = TRUE)

VDOS<- subset(VDOS, VDOS$oxy > (Q1 - 1.5*IQR) & VDOS$oxy < (Q3 + 1.5*IQR))

#Plot to check
plot(SLOC$datetimeMT, SLOC$oxy)
plot(SLOW$datetimeMT, SLOW$oxy)
plot(VDOW$datetimeMT, VDOW$oxy)
plot(VDOS$datetimeMT, VDOS$oxy)

# Initial check
nightreg(SLOC, bp=BEGIbp, ts=15)
nightreg(SLOW, bp=BEGIbp, ts=15)

#### Define nighttime windows -SLOC- ####
SLOC <- SLOC %>%
  mutate(date = as.Date(datetimeMT),
         hour = hour(datetimeMT),
         night = ifelse(hour >= 18, date, date - 1))

#### Apply nightreg() by night ####
night_results <- SLOC %>%
  group_by(night) %>%
  group_modify(~ {
    o2file <- .x
    
    # Skip if too few data points
    if (nrow(o2file) < 10) {
      return(tibble(
        night      = unique(.x$night),
        start_time = min(.x$datetimeMT),
        end_time   = max(.x$datetimeMT),
        intercept  = NA,
        slope      = NA,
        K600       = NA
      ))
    }
    
    res <- tryCatch(
      nightreg(o2file, bp = BEGIbp, ts = 15),
      error = function(e) list(c("(Intercept)" = NA, "satdef" = NA), NA)
    )
    
    tibble(
      night      = unique(.x$night),
      start_time = min(.x$datetimeMT),
      end_time   = max(.x$datetimeMT),
      intercept  = res[[1]][1],
      slope      = res[[1]][2],
      K600       = res[[2]]
    )
  }) %>%
  ungroup()


#### Average of nightly K slopes -SLOC-####
composite_slope_SLOC <- mean(night_results$slope, na.rm = TRUE)
composite_K600_SLOC <- mean(night_results$K600, na.rm = TRUE)
K_sd_SLOC <- sd(night_results$slope, na.rm = TRUE)

#### Plot K over time -SLOC-####
SLOC_Ktime<-ggplot(night_results, aes(x = start_time, y = K600)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression(K[600]),
       title = "K over time in SLOC ")

#### Define nighttime windows -SLOW- ####
SLOW <- SLOW %>%
  mutate(date = as.Date(datetimeMT),
         hour = hour(datetimeMT),
         night = ifelse(hour >= 18, date, date - 1))

#### Apply nightreg() by night ####
night_results <- SLOW %>%
  group_by(night) %>%
  group_modify(~ {
    o2file <- .x
    
    # Skip if too few data points
    if (nrow(o2file) < 10) {
      return(tibble(
        night      = unique(.x$night),
        start_time = min(.x$datetimeMT),
        end_time   = max(.x$datetimeMT),
        intercept  = NA,
        slope      = NA,
        K600       = NA
      ))
    }
    
    res <- tryCatch(
      nightreg(o2file, bp = BEGIbp, ts = 15),
      error = function(e) list(c("(Intercept)" = NA, "satdef" = NA), NA)
    )
    
    tibble(
      night      = unique(.x$night),
      start_time = min(.x$datetimeMT),
      end_time   = max(.x$datetimeMT),
      intercept  = res[[1]][1],
      slope      = res[[1]][2],
      K600       = res[[2]]
    )
  }) %>%
  ungroup()


#### Average of nightly K slopes -SLOW-####
composite_slope_SLOW <- mean(night_results$slope, na.rm = TRUE)
composite_K600_SLOW <- mean(night_results$K600, na.rm = TRUE)
K_sd_SLOW <- sd(night_results$slope, na.rm = TRUE)

#### Plot K over time -SLOW-####
SLOW_Ktime<-ggplot(night_results, aes(x = start_time, y = K600)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression(K[600]),
       title = "K over time in SLOW ")
print(SLOW_Ktime)
#### Define nighttime windows -VDOW- ####
VDOW <- VDOW %>%
  mutate(date = as.Date(datetimeMT),
         hour = hour(datetimeMT),
         night = ifelse(hour >= 18, date, date - 1))

#### Apply nightreg() by night ####
night_results <- VDOW %>%
  group_by(night) %>%
  group_modify(~ {
    o2file <- .x
    
    # Skip if too few data points
    if (nrow(o2file) < 10) {
      return(tibble(
        night      = unique(.x$night),
        start_time = min(.x$datetimeMT),
        end_time   = max(.x$datetimeMT),
        intercept  = NA,
        slope      = NA,
        K600       = NA
      ))
    }
    
    res <- tryCatch(
      nightreg(o2file, bp = BEGIbp, ts = 15),
      error = function(e) list(c("(Intercept)" = NA, "satdef" = NA), NA)
    )
    
    tibble(
      night      = unique(.x$night),
      start_time = min(.x$datetimeMT),
      end_time   = max(.x$datetimeMT),
      intercept  = res[[1]][1],
      slope      = res[[1]][2],
      K600       = res[[2]]
    )
  }) %>%
  ungroup()


#### Average of nightly K slopes -VDOW-####
composite_slope_VDOW <- mean(night_results$slope, na.rm = TRUE)
composite_K600_VDOW <- mean(night_results$K600, na.rm = TRUE)
K_sd_VDOW <- sd(night_results$slope, na.rm = TRUE)

#### Plot K over time -VDOW-####
VDOW_Ktime<-ggplot(night_results, aes(x = start_time, y = K600)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression(K[600]),
       title = "K over time in VDOW ")
print(VDOW_Ktime)

#### Define nighttime windows -VDOS- ####
VDOS <- VDOS %>%
  mutate(date = as.Date(datetimeMT),
         hour = hour(datetimeMT),
         night = ifelse(hour >= 18, date, date - 1))

#### Apply nightreg() by night ####
night_results <- VDOS %>%
  group_by(night) %>%
  group_modify(~ {
    o2file <- .x
    
    # Skip if too few data points
    if (nrow(o2file) < 10) {
      return(tibble(
        night      = unique(.x$night),
        start_time = min(.x$datetimeMT),
        end_time   = max(.x$datetimeMT),
        intercept  = NA,
        slope      = NA,
        K600       = NA
      ))
    }
    
    res <- tryCatch(
      nightreg(o2file, bp = BEGIbp, ts = 15),
      error = function(e) list(c("(Intercept)" = NA, "satdef" = NA), NA)
    )
    
    tibble(
      night      = unique(.x$night),
      start_time = min(.x$datetimeMT),
      end_time   = max(.x$datetimeMT),
      intercept  = res[[1]][1],
      slope      = res[[1]][2],
      K600       = res[[2]]
    )
  }) %>%
  ungroup()


#### Average of nightly K slopes -VDOS-####
composite_slope_VDOS <- mean(night_results$slope, na.rm = TRUE)
composite_K600_VDOS <- mean(night_results$K600, na.rm = TRUE)
K_sd_VDOS <- sd(night_results$slope, na.rm = TRUE)

#### Plot K over time -VDOS-####
VDOS_Ktime<-ggplot(night_results, aes(x = start_time, y = K600)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression(K[600]),
       title = "K over time in VDOS ")
print(VDOS_Ktime)
#### All plots K over time ####
print(SLOC_Ktime)
print(SLOW_Ktime)
print(VDOW_Ktime)
print(VDOS_Ktime)

