#### read me ####
# the purpose of this script is to test for relationships between depth to water mean and variance vs. magnitudes and rates of respiration events (modeled as Odum's ER) between wells OVER TIME for the Webster Lab BEGI project

# There are 2 sites (SLO and VOD).

# There are 2 wells within each site (SLOC, SLOW, VDOS, and VDOW).

# There are multiple respiration events in each well: 
# SLOC SLOW VDOS VDOW 
# 18    9   10   15

# For each respiration event, we have quantified the mean and variance of depth to water for a time period preceeding the respiration event. We will start by testing 2 days before the event, and later wee will try shorter and longer time periods for relationships with respiration.

#### Libraries and fxn ####
library(tidyverse)
library(lubridate)
library(forecast)
library(zoo)
library(xts)
library(nlme)
library(visreg)
library(psych)
library(ggeffects)
library(margins)
library(psych)
library(car)
library(tsibble)
library(lme4)
library(lmerTest)
library(MuMIn)
library(lattice)
library(patchwork)


# replace NaNs with NA
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

#### load and wrangle data ####

# Odum's method of quantifying the size of respiration events from %DO timeseries
odumER = readRDS("EXO_compiled/odumER.rds")
odumER$siteID = substr(odumER$Well, start = 1, stop = 3)
names(odumER)[names(odumER) == 'Well'] <- 'wellID'
odumER[sapply(odumER, is.character)] <- lapply(odumER[sapply(odumER, is.character)],  as.factor)
names(odumER)[names(odumER) == 'Event'] <- 'eventID'

# # eventIDs
# DOevents = DO_AUC[,2:5]

# mean and variance summary of depth to water (DTW) for each event
#dtw_events = read.csv("DTW_compiled/DO_mv.csv")
dtw_events = readRDS("DTW_compiled/DO_mv.rds")
names(dtw_events)[names(dtw_events) == 'WellID'] <- 'wellID'
names(dtw_events)[names(dtw_events) == 'Eventdates'] <- 'Eventdate'
dtw_events$Eventdate = as.POSIXct(paste(substr(dtw_events$Eventdate, start=1,stop=10),
                                        substr(dtw_events$Eventdate, start=12,stop=19), sep=" "),
                                  tz="US/Mountain")
dtw_events$siteID = substr(dtw_events$wellID, start = 1, stop = 3)
dtw_events[sapply(dtw_events, is.character)] <- lapply(dtw_events[sapply(dtw_events, is.character)],  as.factor)

# join data
# the current dtw data by event does not have the same date/times as the DO events. Need to fix, but for now I will join to the nearest date/time
ER_events = odumER %>% 
  arrange(Eventdate)  %>%  group_by(siteID, wellID)  
dtw_events = dtw_events %>% 
  arrange(Eventdate) %>%  group_by(siteID, wellID) 
ER_events$Eventdate_dtw = dtw_events$Eventdate
ER_events$Eventdate - ER_events$Eventdate_dtw # check that date/times are close
names(dtw_events)[names(dtw_events) == 'Eventdate'] <- 'Eventdate_dtw'
ER_events = left_join(ER_events, dtw_events, by=c("siteID","wellID","Eventdate_dtw"))
names(ER_events)[names(ER_events) == 'DO_event_mean'] <- 'dtw_ER_event_mean2'
names(ER_events)[names(ER_events) == 'DO_event_cv'] <- 'dtw_ER_event_cv2'
names(ER_events)[names(ER_events) == 'DO_event_mean1'] <- 'dtw_ER_event_mean1'
names(ER_events)[names(ER_events) == 'DO_event_cv1'] <- 'dtw_ER_event_cv1'
names(ER_events)[names(ER_events) == 'DO_event_mean5'] <- 'dtw_ER_event_mean5'
names(ER_events)[names(ER_events) == 'DO_event_cv5'] <- 'dtw_ER_event_cv5'

# replace NaNs with NA
ER_events[is.nan(ER_events)] <- NA

# clean up environment
rm(odumER); rm(dtw_events)

#### load and wrangle fDOM data ####
fDOM_loss = readRDS("EXO_compiled/fDOM_loss.rds")
dtw_fdom = read_csv("DTW_compiled/fdom_mv_2days.csv")

names(fDOM_loss)[names(fDOM_loss) == 'Well'] <- 'wellID'
names(fDOM_loss)[names(fDOM_loss) == 'Event'] <- 'eventID_fdom'

fDOM_loss$Eventdate <- as.POSIXct(format(fDOM_loss$Eventdate, "%Y-%m-%d %H:%M:%S",tz="US/Mountain"))
fDOM_loss$siteID = substr(fDOM_loss$wellID, start = 1, stop = 3)
fDOM_loss[sapply(fDOM_loss, is.character)] <- lapply(fDOM_loss[sapply(fDOM_loss, is.character)],  as.factor)

names(dtw_fdom)[names(dtw_fdom) == 'WellID'] <- 'wellID'
names(dtw_fdom)[names(dtw_fdom) == 'Eventdates'] <- 'Eventdate'
dtw_fdom$siteID = substr(dtw_fdom$wellID, start = 1, stop = 3)
dtw_fdom$Eventdate <- as.POSIXct(format(dtw_fdom$Eventdate, "%Y-%m-%d %H:%M:%S",tz="US/Mountain"))
dtw_fdom[sapply(dtw_fdom, is.character)] <- lapply(dtw_fdom[sapply(dtw_fdom, is.character)],  as.factor)

#join data
fDOM_loss = fDOM_loss %>% 
  arrange(Eventdate)  %>%  group_by(siteID, wellID)  
dtw_fdom = dtw_fdom %>% 
  arrange(Eventdate) %>%  group_by(siteID, wellID) 
fdom_events = left_join(fDOM_loss, dtw_fdom, by=c("siteID","wellID","Eventdate"))

rm(fDOM_loss); rm(dtw_fdom)

fdom_events <- na.omit(fdom_events[, c("fDOM", "fdom_event_mean", "siteID", "wellID", "fdom_event_cv")])

#### explore data ####
# explore structure (based on 2 day dtw mean/var)
with(ER_events, table(wellID))

ER_events = 
  ER_events %>% 
  group_by(siteID, wellID) %>% 
  arrange(Eventdate)
summary(ER_events)

# explore correlations
# 2 days
psych::pairs.panels(ER_events[c("ER", "dtw_ER_event_mean2", "dtw_ER_event_cv2")], stars = TRUE)
psych::pairs.panels(ER_events[c("D", "dtw_ER_event_mean2", "dtw_ER_event_cv2")], stars = TRUE)
# event gw mean and gw var are fairly correlated (-0.35), so we probably shouldn't include them in the same model or test for an interaction without testing for variance inflations factors. 
# no obvious correlation between DO ER or D and dtw stats, but we'll see how it looks when we group it by site and well in the models!

# 1 day
psych::pairs.panels(ER_events[c("ER", "dtw_ER_event_mean1", "dtw_ER_event_cv1")], stars = TRUE)
psych::pairs.panels(ER_events[c("D", "dtw_ER_event_mean1", "dtw_ER_event_cv1")], stars = TRUE)

# 5 days
psych::pairs.panels(ER_events[c("ER", "dtw_ER_event_mean5", "dtw_ER_event_cv5")], stars = TRUE)
psych::pairs.panels(ER_events[c("D", "dtw_ER_event_mean5", "dtw_ER_event_cv5")], stars = TRUE)
#event gw mean and var are not significantly correlated

#### explore fdom data ####
# explore structure (based on 2 day dtw mean/var)
with(fdom_events, table(wellID))

fdom_events = 
  fdom_events %>% 
  group_by(siteID, wellID) %>% 
  arrange(Eventdate)
summary(fdom_events)

# explore correlations
# 2 days
psych::pairs.panels(fdom_events[c("fDOM", "fdom_event_mean", "fdom_event_cv")], stars = TRUE)
# event gw mean and gw var are fairly correlated (-0.52), so we probably shouldn't include them in the same model or test for an interaction without testing for variance inflations factors. 
# nfDOM loss and gw mean seem fairly correlated (0.30)


#### test for relationship btw fdom loss & dtw stats (2 days) ####
#based on 2 day dtw mean/var
#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(fDOM ~ 1, data=fdom_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(fDOM ~ fdom_event_mean, 
                data=fdom_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(fDOM ~ fdom_event_cv, 
                data=fdom_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# m1 slightly better

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - clumped with some outliers

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. Not normal.
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# pretty much the same..


#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++
m.null = nlme::lme(log(fDOM) ~ 1, data=fdom_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(fDOM) ~ fdom_event_mean, 
                data=fdom_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(fDOM) ~ fdom_event_cv, 
                data=fdom_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is slightly better than m.2 and about the same as m.2

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
anova.lme(m.1,type = "marginal", adjustSigma = F) #***
anova.lme(m.2,type = "marginal", adjustSigma = F)

#95% CI gives you LOWER and UPPER bound around the MEAN ESTIMATE for each parameter
#linear, quadratic terms with 95% Confidence Intervals
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") #ns
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") #ns

# view model summaries
summary(m.1)
summary(m.2)
#it seems like groundwater mean is significantly positively correlated with fDOM loss (p = .0231)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

### plot of predicted model
# for all sites
visreg(m.1,"fdom_event_mean",type="conditional",
       fill=list(col="lightgrey"),
       points.par=list(cex=2, col=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF")),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceding Event (2 days)", cex=1.2),
       ylab=list("fDOM Loss (log QSU)", cex=1.2),
       by='wellID',overlay=TRUE, legend = FALSE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"fdom_event_cv",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("fDOM loss (log QSU)", cex=1.8),
       by='wellID',overlay=TRUE)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors



#### test for relationship btw odum's ER & dtw stats (2 days) ####
#based on 2 day dtw mean/var
#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(ER ~ 1, data=ER_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(ER ~ dtw_ER_event_mean2, 
                data=ER_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(ER ~ dtw_ER_event_cv2, 
                data=ER_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is lowest by 4 points...

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - clumped with some outliers

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. Not normal.
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# pretty much the same..


#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++
#need to multiply ER by -1 to be able to log transform
ER_events$posER <- ER_events$ER * -1
#filter out 0 values
ER_events_filtered <- ER_events[ER_events$posER > 0, ]

m.null = nlme::lme(log(posER) ~ 1, data=ER_events_filtered, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(posER) ~ dtw_ER_event_mean2, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(posER) ~ dtw_ER_event_cv2, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv2<3,]
m.3 = nlme::lme(log(posER) ~ dtw_ER_event_cv2, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is still better than m.1 and m.2

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
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") #ns
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") #ns

# view model summaries
summary(m.1)
summary(m.2)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

### plot of predicted model
# for all sites
visreg(m.1,"dtw_ER_event_mean2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_ER_event_cv2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_ER_event_cv2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors


#### test for relationship btw D & dtw stats (2 days) ####
#based on 2 day dtw mean/var

#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(D ~ 1, data=ER_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(D ~ dtw_ER_event_mean2, 
                data=ER_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(D ~ dtw_ER_event_cv2, 
                data=ER_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is lowest by 4 points... m.1 and m.2 are about the same

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - clumped with some outliers

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. Not normal.
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# pretty much the same..

#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++
#need to filter out 0 values to be able to log transform
ER_events_filtered <- ER_events[ER_events$D > 0, ]

m.null = nlme::lme(log(D) ~ 1, data=ER_events_filtered, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(D) ~ dtw_ER_event_mean2, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(D) ~ dtw_ER_event_cv2, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv2<3,]
m.3 = nlme::lme(log(D) ~ dtw_ER_event_cv2, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is still better than m.1 and m.2, m1 and m2 still about the same

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
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") #ns
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") #ns

# view model summaries
summary(m.1)
summary(m.2)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

### plot of predicted model
# for all sites
visreg(m.1,"dtw_ER_event_mean2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_ER_event_cv2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_ER_event_cv2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors




#### test for relationship btw ER & dtw (1 day) ####
#based on 1 day dtw mean/var
#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(ER ~ 1, data=ER_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(ER ~ dtw_ER_event_mean1, 
                data=ER_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(ER ~ dtw_ER_event_cv1, 
                data=ER_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is lowest by 4 points...

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - clumped with some outliers

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. Not normal.
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# pretty much the same..

#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++
#need to multiply ER by -1 to be able to log transform
ER_events$posER <- ER_events$ER * -1
#filter out 0 values
ER_events_filtered <- ER_events[ER_events$posER > 0, ]

m.null = nlme::lme(log(posER) ~ 1, data=ER_events_filtered, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(posER) ~ dtw_ER_event_mean1, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(posER) ~ dtw_ER_event_cv1, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv1<3,]
m.3 = nlme::lme(log(posER) ~ dtw_ER_event_cv1, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is still better than m.1 and m.2 but only slightly so

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks good
plot(m.2) #looks good, one outlier

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
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") #ns
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") #ns

# view model summaries
summary(m.1)
summary(m.2)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

### plot of predicted model
# for all sites
visreg(m.1,"dtw_ER_event_mean1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_ER_event_cv1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_ER_event_cv1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)



#### test for relationship btw D & dtw stats (1 day) ####
#based on 1 day dtw mean/var

#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(D ~ 1, data=ER_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(D ~ dtw_ER_event_mean1, 
                data=ER_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(D ~ dtw_ER_event_cv1, 
                data=ER_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is lowest by 4 points... m.1 and m.2 are about the same

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - clumped with some outliers

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. Not normal.
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# pretty much the same..

#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++
#need to filter out 0 values to be able to log transform
ER_events_filtered <- ER_events[ER_events$D > 0, ]

m.null = nlme::lme(log(D) ~ 1, data=ER_events_filtered, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(D) ~ dtw_ER_event_mean1, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(D) ~ dtw_ER_event_cv1, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv1<3,]
m.3 = nlme::lme(log(D) ~ dtw_ER_event_cv1, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is still better than m.1 and m.2, m1 and m2 still about the same

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
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") #ns
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") #ns

# view model summaries
summary(m.1)
summary(m.2)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

### plot of predicted model
# for all sites
visreg(m.1,"dtw_ER_event_mean1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_ER_event_cv1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_ER_event_cv1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors

#### test for relationship btw ER & dtw (5 days) ####
#based on 5 day dtw mean/var
#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(ER ~ 1, data=ER_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(ER ~ dtw_ER_event_mean5, 
                data=ER_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(ER ~ dtw_ER_event_cv5, 
                data=ER_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is lowest by 4 points...

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - clumped with some outliers

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. Not normal.
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# pretty much the same..

#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++
#need to multiply ER by -1 to be able to log transform
ER_events$posER <- ER_events$ER * -1
#filter out 0 values
ER_events_filtered <- ER_events[ER_events$posER > 0, ]

m.null = nlme::lme(log(posER) ~ 1, data=ER_events_filtered, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(posER) ~ dtw_ER_event_mean5, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(posER) ~ dtw_ER_event_cv5, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv5<3,]
m.3 = nlme::lme(log(posER) ~ dtw_ER_event_cv5, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is still better than m.1 and m.2 but only slightly so

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks good
plot(m.2) #looks good, one outlier

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
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") #ns
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") #ns

# view model summaries
summary(m.1)
summary(m.2)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

### plot of predicted model
# for all sites
visreg(m.1,"dtw_ER_event_mean5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_ER_event_cv5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_ER_event_cv5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)



#### test for relationship btw D & dtw stats (5 days) ####
#based on 5 day dtw mean/var

#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(D ~ 1, data=ER_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(D ~ dtw_ER_event_mean5, 
                data=ER_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(D ~ dtw_ER_event_cv5, 
                data=ER_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is lowest by 4 points... m.1 and m.2 are about the same

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - clumped with some outliers

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. Not normal.
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# pretty much the same..

#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++
#need to filter out 0 values to be able to log transform
ER_events_filtered <- ER_events[ER_events$D > 0, ]

m.null = nlme::lme(log(D) ~ 1, data=ER_events_filtered, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(D) ~ dtw_ER_event_mean5, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(D) ~ dtw_ER_event_cv5, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv5<3,]
m.3 = nlme::lme(log(D) ~ dtw_ER_event_cv5, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is still better than m.1 and m.2, m1 and m2 still about the same

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
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") #ns
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") #ns

# view model summaries
summary(m.1)
summary(m.2)

## look at random effects to varify that intercepts are being estimated differenlt
ranef(m.1)
ranef(m.2)

### plot of predicted model
# for all sites
visreg(m.1,"dtw_ER_event_mean5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_ER_event_cv5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_ER_event_cv5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors


#### Save and export model results ####
#reset each model

#Log transformed ER and dtw over 2 days
#need to multiply ER by -1 to be able to log transform
ER_events$posER <- ER_events$ER * -1
#filter out 0 values
ER_events_filtered <- ER_events[ER_events$posER > 0, ]

m.null = nlme::lme(log(posER) ~ 1, data=ER_events_filtered, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(posER) ~ dtw_ER_event_mean2, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(posER) ~ dtw_ER_event_cv2, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv2<3,]
m.3 = nlme::lme(log(posER) ~ dtw_ER_event_cv2, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

#Log transformed ER and dtw over 1 day
m.4 = nlme::lme(log(posER) ~ dtw_ER_event_mean1, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.5 = nlme::lme(log(posER) ~ dtw_ER_event_cv1, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv1<3,]
m.6 = nlme::lme(log(posER) ~ dtw_ER_event_cv1, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

#Log transformed ER and dtw over 5 days
m.7 = nlme::lme(log(posER) ~ dtw_ER_event_mean5, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.8 = nlme::lme(log(posER) ~ dtw_ER_event_cv5, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv5<3,]
m.9 = nlme::lme(log(posER) ~ dtw_ER_event_cv5, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

#Log transformed D and dtw over 2 days
ER_events_filtered <- ER_events[ER_events$D > 0, ]

m.null = nlme::lme(log(D) ~ 1, data=ER_events_filtered, random=~1|wellID, method="ML")
m.10 = nlme::lme(log(D) ~ dtw_ER_event_mean2, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.11 = nlme::lme(log(D) ~ dtw_ER_event_cv2, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv2<3,]
m.12 = nlme::lme(log(D) ~ dtw_ER_event_cv2, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

#Log transformed D and dtw over 1 day
m.13 = nlme::lme(log(D) ~ dtw_ER_event_mean1, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.14 = nlme::lme(log(D) ~ dtw_ER_event_cv1, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv1<3,]
m.15 = nlme::lme(log(D) ~ dtw_ER_event_cv1, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")

#Log transformed D and dtw over 5 days
m.16 = nlme::lme(log(D) ~ dtw_ER_event_mean5, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
m.17 = nlme::lme(log(D) ~ dtw_ER_event_cv5, 
                data=ER_events_filtered, random=~1|siteID/wellID, method="ML")
ER_events_r =ER_events_filtered[ER_events_filtered$dtw_ER_event_cv5<3,]
m.18 = nlme::lme(log(D) ~ dtw_ER_event_cv5, 
                data=ER_events_r, random=~1|siteID/wellID, method="ML")


#Summarize
s.1 <- summary(m.1)
s.2 <- summary(m.2)
s.3 <- summary(m.3)
s.4 <- summary(m.4)
s.5 <- summary(m.5)
s.6 <- summary(m.6)
s.7 <- summary(m.7)
s.8 <- summary(m.8)
s.9 <- summary(m.9)
s.10 <- summary(m.10)
s.11 <- summary(m.11)
s.12 <- summary(m.12)
s.13 <- summary(m.13)
s.14 <- summary(m.14)
s.15 <- summary(m.15)
s.16 <- summary(m.16)
s.17 <- summary(m.17)
s.18 <- summary(m.18)

#compile and save
