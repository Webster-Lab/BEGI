#### read me ####
# The purpose of this script is to take the events determined to not be chemical
# oxidation from script 21 and re-run statistical analyses on them, assuming they
# more precisely represent aerobic respiration in our wells.
# This will use a dataframe of 30 events where fDOM does not rebound following a
# DO event (possibly indicative of chemical oxidation).
# The analyses to be re-done are:
# Testing for effect of site differences on DO event size, ER, D
# Testing for effect of groundwater mean and variance on DO event size, ER, D
# Rerunning DTW cluster analysis groundwater depth preceding a DO event for
# respiration events
# Calculating confidence intervals for annual carbon respiration

#### Libraries and functions ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(forecast)
library(zoo)
library(xts)
library(nlme)
library(visreg)
library(psych)
library(ggeffects)
library(margins)
library(car)
library(tsibble)
library(lme4)
library(lmerTest)
library(MuMIn)
library(DescTools)
library(dtwclust)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(patchwork)


# replace NaNs with NA
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))


#### Load data ####
roc_cluster2 <- readRDS("EXO_compiled/roc_cluster2.rds")

#### Calculate DO event size ####
## DO event size ##
AUC_results_all <- list()

for (rate_list in names(roc_cluster2)) {
  df_names <- names(roc_cluster2[[rate_list]])
  results <- setNames(vector("numeric", length(df_names)), df_names)
  
  for (nm in df_names) {
    results[nm] <- AUC(
      x = roc_cluster2[[rate_list]][[nm]]$datetimeMT,
      y = roc_cluster2[[rate_list]][[nm]]$ODO.mg.L.mn,
      method = "trapezoid",
      na.rm = FALSE
    )
  }
  AUC_results_all[[rate_list]] <- results
}

# df
AUC_df <- do.call(rbind, lapply(names(AUC_results_all), function(rate_list) {
  do.call(rbind, lapply(names(AUC_results_all[[rate_list]]), function(nm) {
    df <- roc_cluster2[[rate_list]][[nm]]
    data.frame(
      rate_list   = rate_list,
      event_name  = nm,
      eventdate  = min(df$datetimeMT, na.rm = TRUE),
      AUC         = as.numeric(AUC_results_all[[rate_list]][[nm]])
    )
  }))
}))

#### Combine datasets for by well model ####

## ER and D ##
# Apparently I already did this... will have to clean up this workflow
odumER_subset <- readRDS("EXO_compiled/odumER_subset.rds")

# mean and variance summary of depth to water (DTW) for each well for whole period of %DO observation
gwmv_well = read.csv("DTW_compiled/gwmv_well.csv", row.names = 1)
names(gwmv_well)[names(gwmv_well) == 'wells'] <- 'Well'
gwmv_well$siteID = substr(gwmv_well$wellID, start = 1, stop = 3)
gwmv_well[sapply(gwmv_well, is.character)] <- lapply(gwmv_well[sapply(gwmv_well, is.character)],  as.factor)


# join dataframes
names(AUC_df)[names(AUC_df) == 'event_name'] <- 'Event'
resp_events = left_join(AUC_df,odumER_subset, by="Event")
resp_events = left_join(resp_events, gwmv_well, by=c("Well"))

# siteID column
resp_events$siteID <- substr(resp_events$Well, 1, 3)

# replace NaNs with NA
resp_events[is.nan(resp_events)] <- NA

# clean up environment
rm(odumER_subset); rm(roc_cluster2); rm(gwmv_well)

#### explore data for group means with AUC, ER, D ####

# explore structure
with(resp_events, table(Well))
#SLOC SLOW VDOS VDOW 
#8    2    6   14 

resp_events = 
  resp_events %>% 
  group_by(Well) %>% 
  arrange(Event)
summary(resp_events)

# explore correlations
psych::pairs.panels(resp_events[c("AUC", "gwmean_well", "gwvar_well")], stars = TRUE)
psych::pairs.panels(resp_events[c("ER", "gwmean_well", "gwvar_well")], stars = TRUE)
psych::pairs.panels(resp_events[c("D", "gwmean_well", "gwvar_well")], stars = TRUE)
# gw mean and gw var are highly correlated, so we can't include them in the same model or test for an interaction. This is good motivation to simply test for group mean differences by well and site and interpret effects of gw mean and var qualitatively. 

# explore normality of data
# DO event size
plot.new()
qqPlot(resp_events$AUC[resp_events$Well=="SLOC"]); shapiro.test(resp_events$AUC[resp_events$Well=="SLOC"]) # not normal
qqPlot(resp_events$AUC[resp_events$Well=="SLOW"]); shapiro.test(resp_events$AUC[resp_events$Well=="SLOW"]) # Only 2 observations
qqPlot(resp_events$AUC[resp_events$Well=="VDOW"]); shapiro.test(resp_events$AUC[resp_events$Well=="VDOW"]) # not normal
qqPlot(resp_events$AUC[resp_events$Well=="VDOS"]); shapiro.test(resp_events$AUC[resp_events$Well=="VDOS"]) # not normal

qqPlot(resp_events$AUC); shapiro.test(resp_events$AUC) # not normal :|

# ER
plot.new()
qqPlot(resp_events$ER[resp_events$Well=="SLOC"]); shapiro.test(resp_events$ER[resp_events$Well=="SLOC"]) # not normal
qqPlot(resp_events$ER[resp_events$Well=="SLOW"]); shapiro.test(resp_events$ER[resp_events$Well=="SLOW"]) # Only 2 observations
qqPlot(resp_events$ER[resp_events$Well=="VDOW"]); shapiro.test(resp_events$ER[resp_events$Well=="VDOW"]) # normal
qqPlot(resp_events$ER[resp_events$Well=="VDOS"]); shapiro.test(resp_events$ER[resp_events$Well=="VDOS"]) # normal

qqPlot(resp_events$ER); shapiro.test(resp_events$ER) # not normal :|

# D
plot.new()
qqPlot(resp_events$D[resp_events$Well=="SLOC"]); shapiro.test(resp_events$D[resp_events$Well=="SLOC"]) # not normal
qqPlot(resp_events$D[resp_events$Well=="SLOW"]); shapiro.test(resp_events$D[resp_events$Well=="SLOW"]) # Only 2 observations
qqPlot(resp_events$D[resp_events$Well=="VDOW"]); shapiro.test(resp_events$D[resp_events$Well=="VDOW"]) # not normal
qqPlot(resp_events$D[resp_events$Well=="VDOS"]); shapiro.test(resp_events$D[resp_events$Well=="VDOS"]) # normal

qqPlot(resp_events$D); shapiro.test(resp_events$D) # not normal :|

# check for temporal autocorrelation
# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer

### subset data to be one site and one parameter
temp = resp_events[resp_events$Well=="SLOC" ,]
temp = temp[,c(2,3)]
### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(eventdate) 
# there are two  duplicated dates, which I will assign to the next day to avoid duplicated time stamps
duplicated(as.Date(temp$eventdatedate))
temp$eventdate[temp$eventdate=="2024-01-21 07:45:00"] = temp$eventdate[temp$eventdate=="2024-01-21 07:45:00"] + 4*60*60
temp$eventdate = as.Date(substr(temp$eventdate,1,10))
any(duplicated(temp$eventdate))
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(eventdate = seq(min(eventdate), max(eventdate), by = "1 day"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = eventdate)
head (temp_ts)
temp_ts = ts(temp_ts$AUC, frequency=365, start=c(2023, yday(temp_ts$eventdate[1])) ) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp)

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)
# it is challenging to interpret this given the gappines of the data, since na/pass doesn't work and I don't trust the other 2 na dealing methods since the gaps are so large. I think we'll need to rely on model residuals rather than this analysis to assess the issue of temporal autocorrelation

#### group means AUC ####

#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(AUC) ~ 1, data=resp_events, random=~1|siteID, method="ML")
m.1 = nlme::lme(log(AUC) ~ Well, data=resp_events, random=~1|siteID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# Results: 
# m.1 now better!

Anova(m.1, type=2) #Chi-Square test for influence of wellID on AUC fitted with type 2 error
# strong diffs between wells

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks better

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks good!

#3 temporal autocorrelation in data
forecast::Acf(residuals(m.1))


# GET P-VALUES AND COEFFICIENT ESTIMATES WITH 95% CONFIDENCE INTERVALS
#F-tests 
anova.lme(m.1,type = "marginal", adjustSigma = F)

#95% CI gives you LOWER and UPPER bound around the MEAN ESTIMATE for each parameter
#linear, quadratic terms with 95% Confidence Intervals
conf_int <- intervals(m.1, level = 0.95, which = "fixed")  # Get confidence intervals on transformed scale
# conf_int = unlist(conf_int)
# back_transformed_ci <- exp(conf_int) # Back-transform using exp() function
# # need to format this better!
# above code did NOT work to back-transform data, so each value needs to be back-transformed as exp(#)
exp(5.186942)
exp(6.2970787)
# etc....


## Finally, look at model summary: m.1
summary(m.1)

### reorder wellID to set intercept to other wells:
## SLOC
m.1.SLOCint = m.1
# interpretation: SLOC is significantly different from all other wells. The negative Value estimates indicate that all the other wells have lower mean AUC than SLOC.

resp_events_SLOWint <- within(resp_events, Well <- relevel(factor(Well), ref = "SLOW"))
## SLOW
resp_events_SLOWint <- within(resp_events, Well <- relevel(Well, ref = "SLOW"))
m.1.SLOWint = nlme::lme(log(AUC) ~ Well, data=resp_events_SLOWint, random=~1|siteID, method="ML")
summary(m.1.SLOWint)
# interpretation: SLOW is significantly different from all other wells. The all positive Value estimates indicate that all the other wells have HIGHER mean AUC than SLOC.

resp_events_VDOSint <- within(resp_events, Well <- relevel(factor(Well), ref = "VDOS"))
## VDOS
resp_events_VDOSint <- within(resp_events, Well <- relevel(Well, ref = "VDOS"))
m.1.VDOSint = nlme::lme(log(AUC) ~ Well, data=resp_events_VDOSint, random=~1|siteID, method="ML")
summary(m.1.VDOSint)
# interpretation: VDOS is significantly different from SLOC and SLOW, but NOT VDOW. AUC in VDOS is lower than in SLOC,  higher than SLOW, and is not sig. different than VDOW. 

resp_events_VDOWint <- within(resp_events, Well <- relevel(factor(Well), ref = "VDOW"))
## VDOW
resp_events_VDOWint <- within(resp_events, Well <- relevel(Well, ref = "VDOW"))
m.1.VDOWint = nlme::lme(log(AUC) ~ Well, data=resp_events_VDOWint, random=~1|siteID, method="ML")
summary(m.1.VDOWint)
# interpretation: VDOW is significantly different from SLOC, but NOT from VDOW or SLOW (this last one is marginal... p-value is 0.0556 so it is ALMOST a sig difference, but not quite). AUC in VDOW is lower than in SLOC and is not sig. different than VDOW or SLOW (though it is marginally higher than SLOW, but not significantly so). 




#### group means ER ####

#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(ER) ~ 1, data=resp_events, random=~1|siteID, method="ML")
m.1 = nlme::lme(log(ER) ~ Well, data=resp_events, random=~1|siteID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# Results: 
# m.1 now better!

Anova(m.1, type=2) #Chi-Square test for influence of wellID on ER fitted with type 2 error
# strong diffs between wells

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks better

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks good!

#3 temporal autocorrelation in data
forecast::Acf(residuals(m.1))


# GET P-VALUES AND COEFFICIENT ESTIMATES WITH 95% CONFIDENCE INTERVALS
#F-tests 
anova.lme(m.1,type = "marginal", adjustSigma = F)

#95% CI gives you LOWER and UPPER bound around the MEAN ESTIMATE for each parameter
#linear, quadratic terms with 95% Confidence Intervals
conf_int <- intervals(m.1, level = 0.95, which = "fixed")  # Get confidence intervals on transformed scale
# conf_int = unlist(conf_int)
# back_transformed_ci <- exp(conf_int) # Back-transform using exp() function
# # need to format this better!
# above code did NOT work to back-transform data, so each value needs to be back-transformed as exp(#)
exp(5.186942)
exp(6.2970787)
# etc....


## Finally, look at model summary: m.1
summary(m.1)

### reorder wellID to set intercept to other wells:
## SLOC
m.1.SLOCint = m.1
# interpretation: SLOC is significantly different from all other wells. The negative Value estimates indicate that all the other wells have lower mean ER than SLOC.

resp_events_SLOWint <- within(resp_events, Well <- relevel(factor(Well), ref = "SLOW"))
## SLOW
resp_events_SLOWint <- within(resp_events, Well <- relevel(Well, ref = "SLOW"))
m.1.SLOWint = nlme::lme(log(ER) ~ Well, data=resp_events_SLOWint, random=~1|siteID, method="ML")
summary(m.1.SLOWint)
# interpretation: SLOW is significantly different from all other wells. The all positive Value estimates indicate that all the other wells have HIGHER mean ER than SLOC.

resp_events_VDOSint <- within(resp_events, Well <- relevel(factor(Well), ref = "VDOS"))
## VDOS
resp_events_VDOSint <- within(resp_events, Well <- relevel(Well, ref = "VDOS"))
m.1.VDOSint = nlme::lme(log(ER) ~ Well, data=resp_events_VDOSint, random=~1|siteID, method="ML")
summary(m.1.VDOSint)
# interpretation: VDOS is significantly different from SLOC and SLOW, but NOT VDOW. ER in VDOS is lower than in SLOC,  higher than SLOW, and is not sig. different than VDOW. 

resp_events_VDOWint <- within(resp_events, Well <- relevel(factor(Well), ref = "VDOW"))
## VDOW
resp_events_VDOWint <- within(resp_events, Well <- relevel(Well, ref = "VDOW"))
m.1.VDOWint = nlme::lme(log(ER) ~ Well, data=resp_events_VDOWint, random=~1|siteID, method="ML")
summary(m.1.VDOWint)
# interpretation: VDOW is significantly different from SLOC, but NOT from VDOW or SLOW (this last one is marginal... p-value is 0.0556 so it is ALMOST a sig difference, but not quite). ER in VDOW is lower than in SLOC and is not sig. different than VDOW or SLOW (though it is marginally higher than SLOW, but not significantly so). 




#### group means D ####

#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(D) ~ 1, data=resp_events, random=~1|siteID, method="ML")
m.1 = nlme::lme(log(D) ~ Well, data=resp_events, random=~1|siteID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# Results: 
# m.1 now better!

Anova(m.1, type=2) #Chi-Square test for influence of wellID on D fitted with type 2 error
# strong diffs between wells

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks better

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks good!

#3 temporal autocorrelation in data
forecast::Acf(residuals(m.1))


# GET P-VALUES AND COEFFICIENT ESTIMATES WITH 95% CONFIDENCE INTERVALS
#F-tests 
anova.lme(m.1,type = "marginal", adjustSigma = F)

#95% CI gives you LOWER and UPPER bound around the MEAN ESTIMATE for each parameter
#linear, quadratic terms with 95% Confidence Intervals
conf_int <- intervals(m.1, level = 0.95, which = "fixed")  # Get confidence intervals on transformed scale
# conf_int = unlist(conf_int)
# back_transformed_ci <- exp(conf_int) # Back-transform using exp() function
# # need to format this better!
# above code did NOT work to back-transform data, so each value needs to be back-transformed as exp(#)
exp(5.186942)
exp(6.2970787)
# etc....


## Finally, look at model summary: m.1
summary(m.1)

### reorder wellID to set intercept to other wells:
## SLOC
m.1.SLOCint = m.1
# interpretation: SLOC is significantly different from all other wells. The negative Value estimates indicate that all the other wells have lower mean D than SLOC.

resp_events_SLOWint <- within(resp_events, Well <- relevel(factor(Well), ref = "SLOW"))
## SLOW
resp_events_SLOWint <- within(resp_events, Well <- relevel(Well, ref = "SLOW"))
m.1.SLOWint = nlme::lme(log(D) ~ Well, data=resp_events_SLOWint, random=~1|siteID, method="ML")
summary(m.1.SLOWint)
# interpretation: SLOW is significantly different from all other wells. The all positive Value estimates indicate that all the other wells have HIGHER mean D than SLOC.

resp_events_VDOSint <- within(resp_events, Well <- relevel(factor(Well), ref = "VDOS"))
## VDOS
resp_events_VDOSint <- within(resp_events, Well <- relevel(Well, ref = "VDOS"))
m.1.VDOSint = nlme::lme(log(D) ~ Well, data=resp_events_VDOSint, random=~1|siteID, method="ML")
summary(m.1.VDOSint)
# interpretation: VDOS is significantly different from SLOC and SLOW, but NOT VDOW. D in VDOS is lower than in SLOC,  higher than SLOW, and is not sig. different than VDOW. 

resp_events_VDOWint <- within(resp_events, Well <- relevel(factor(Well), ref = "VDOW"))
## VDOW
resp_events_VDOWint <- within(resp_events, Well <- relevel(Well, ref = "VDOW"))
m.1.VDOWint = nlme::lme(log(D) ~ Well, data=resp_events_VDOWint, random=~1|siteID, method="ML")
summary(m.1.VDOWint)
# interpretation: VDOW is significantly different from SLOC, but NOT from VDOW or SLOW (this last one is marginal... p-value is 0.0556 so it is ALMOST a sig difference, but not quite). D in VDOW is lower than in SLOC and is not sig. different than VDOW or SLOW (though it is marginally higher than SLOW, but not significantly so). 








#### Combine datasets for event dtw model ####

## ER and D ##
# Apparently I already did this... will have to clean up this workflow
odumER_subset <- readRDS("EXO_compiled/odumER_subset.rds")

# mean and variance summary of depth to water (DTW) for each event
dtw_events = readRDS("DTW_compiled/DO_mv.rds")
names(dtw_events)[names(dtw_events) == 'WellID'] <- 'Well'
names(dtw_events)[names(dtw_events) == 'Eventdates'] <- 'eventdate'
dtw_events$date = as.POSIXct(paste(substr(dtw_events$eventdate, start=1,stop=10),
                                        substr(dtw_events$eventdate, start=12,stop=19), sep=" "),
                                  tz="US/Mountain")
dtw_events$siteID = substr(dtw_events$Well, start = 1, stop = 3)
dtw_events[sapply(dtw_events, is.character)] <- lapply(dtw_events[sapply(dtw_events, is.character)],  as.factor)

# join dataframes
names(AUC_df)[names(AUC_df) == 'event_name'] <- 'Event'
resp_events = left_join(AUC_df,odumER_subset, by="Event")
resp_events = left_join(resp_events, dtw_events, by=c("Well","eventdate"))

# replace NaNs with NA
resp_events[is.nan(resp_events)] <- NA

# clean up environment
rm(odumER_subset); rm(roc_cluster2); rm(dtw_events); rm(AUC_df)

#### explore data for event dtw with AUC, ER, D ####

# explore structure
with(resp_events, table(Well))
# SLOC SLOW VDOS VDOW 
# 8    2    6   14 

resp_events = 
  resp_events %>% 
  group_by(siteID, Well) %>% 
  arrange(eventdate)
summary(resp_events)

# explore correlations
psych::pairs.panels(resp_events[c("AUC", "DO_event_mean", "DO_event_cv")], stars = TRUE)
# event gw mean and gw var are fairly correlated (-0.31), so we probably shouldn't include them in the same model or test for an interaction without testing for variance inflations factors. 
# no obvious correlation between DO AUC and dtw stats, but we'll see how it looks when we group it by site and well in the models!
psych::pairs.panels(resp_events[c("ER", "DO_event_mean", "DO_event_cv")], stars = TRUE)
psych::pairs.panels(resp_events[c("D", "DO_event_mean", "DO_event_cv")], stars = TRUE)


#### event dtw and AUC ####
#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(AUC) ~ 1, data=resp_events, random=~1|Well, method="ML")
m.1 = nlme::lme(log(AUC) ~ DO_event_mean, 
                data=resp_events, random=~1|siteID/Well, method="ML")
m.2 = nlme::lme(log(AUC) ~ DO_event_cv, 
                data=resp_events, random=~1|siteID/Well, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# df     AICc
# m.null  3 104.9779
# m.1     5 109.7095
# m.2     5 107.7567

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks good
plot(m.2) #looks good

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks great
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# looks great

#3 temporal autocorrelation in data
forecast::Acf(residuals(m.1))
# looks great
forecast::Acf(residuals(m.2))
# looks great

# GET P-VALUES AND COEFFICIENT ESTIMATES WITH 95% CONFIDENCE INTERVALS
#F-tests 
anova.lme(m.1,type = "marginal", adjustSigma = F)
anova.lme(m.2,type = "marginal", adjustSigma = F)

#95% CI gives you LOWER and UPPER bound around the MEAN ESTIMATE for each parameter
#linear, quadratic terms with 95% Confidence Intervals
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") 
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") 

# view model summaries
summary(m.1)
summary(m.2)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)

#### event dtw and ER ####
#need to multiply ER by -1 to be able to log transform
resp_events$posER <- resp_events$ER * -1
#filter out 0 values
resp_events_filtered <- resp_events[resp_events$posER > 0, ]

#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(posER) ~ 1, data=resp_events_filtered, random=~1|Well, method="ML")
m.1 = nlme::lme(log(posER) ~ DO_event_mean, 
                data=resp_events_filtered, random=~1|siteID/Well, method="ML")
m.2 = nlme::lme(log(posER) ~ DO_event_cv, 
                data=resp_events_filtered, random=~1|siteID/Well, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# df     AICc
# m.null  3 82.40326
# m.1     5 87.92649
# m.2     5 87.52709

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks good
plot(m.2) #looks good

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks great
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# looks great

#3 temporal autocorrelation in data
forecast::Acf(residuals(m.1))
# looks great
forecast::Acf(residuals(m.2))
# looks great

# GET P-VALUES AND COEFFICIENT ESTIMATES WITH 95% CONFIDENCE INTERVALS
#F-tests 
anova.lme(m.1,type = "marginal", adjustSigma = F)
anova.lme(m.2,type = "marginal", adjustSigma = F)

#95% CI gives you LOWER and UPPER bound around the MEAN ESTIMATE for each parameter
#linear, quadratic terms with 95% Confidence Intervals
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") 
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") 

# view model summaries
summary(m.1)
summary(m.2)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

#### event dtw and D ####
#filter out 0 values
resp_events_filtered <- resp_events[resp_events$D > 0, ]
#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(D) ~ 1, data=resp_events_filtered, random=~1|Well, method="ML")
m.1 = nlme::lme(log(D) ~ DO_event_mean, 
                data=resp_events_filtered, random=~1|siteID/Well, method="ML")
m.2 = nlme::lme(log(D) ~ DO_event_cv, 
                data=resp_events_filtered, random=~1|siteID/Well, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# df     AICc
# m.null  3 102.9469
# m.1     5 108.5065
# m.2     5 108.5666

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks good
plot(m.2) #looks good

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks great
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# looks great

#3 temporal autocorrelation in data
forecast::Acf(residuals(m.1))
# looks great
forecast::Acf(residuals(m.2))
# looks great

# GET P-VALUES AND COEFFICIENT ESTIMATES WITH 95% CONFIDENCE INTERVALS
#F-tests 
anova.lme(m.1,type = "marginal", adjustSigma = F)
anova.lme(m.2,type = "marginal", adjustSigma = F)

#95% CI gives you LOWER and UPPER bound around the MEAN ESTIMATE for each parameter
#linear, quadratic terms with 95% Confidence Intervals
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") 
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") 

# view model summaries
summary(m.1)
summary(m.2)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

#### dtw cluster analysis of gw depth ####
# is this worth doing?
