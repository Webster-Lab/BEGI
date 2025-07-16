#### read me ####

# the purpose of this script is to test for relationships between depth to water mean and variance vs. magnitudes and rates of respiration events between wells OVER TIME for the Webster Lab BEGI project

# There are 2 sites (SLO and VOD).

# There are 2 wells within each site (SLOC, SLOW, VDOS, and VDOW).

# There are multiple respiration events in each well: 
# SLOC SLOW VDOS VDOW 
# 21   10   10   18 

# For each respiration event, we have quantified the mean and variance of depth to water for a time period preceeding the respiration event. We will start by testing 2 days before the event, and later wee will try shorter and longer time periods for relationships with respiration.

#### libraries & fxns ####

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

# area under the curve method of quantifying the relative size of respiration events from %DO timeseries
DO_AUC = readRDS("EXO_compiled/DO_AUC.rds")
DO_AUC$siteID = substr(DO_AUC$Well, start = 1, stop = 3)
names(DO_AUC)[names(DO_AUC) == 'Well'] <- 'wellID'
DO_AUC[sapply(DO_AUC, is.character)] <- lapply(DO_AUC[sapply(DO_AUC, is.character)],  as.factor)
names(DO_AUC)[names(DO_AUC) == 'Event'] <- 'eventID'
names(DO_AUC)[names(DO_AUC) == 'DO'] <- 'DO_AUC'

# # eventIDs
# DOevents = DO_AUC[,2:5]

# rate of change method of quantifying the rate of respiration events from %DO timeseries
DO_ROC = readRDS("EXO_compiled/DO_roc.rds")
DO_ROC$siteID = substr(DO_ROC$Well, start = 1, stop = 3)
names(DO_ROC)[names(DO_ROC) == 'Well'] <- 'wellID'
DO_ROC[sapply(DO_ROC, is.character)] <- lapply(DO_ROC[sapply(DO_ROC, is.character)],  as.factor)
#DO_ROC = left_join(DO_ROC, DOevents, by=c("siteID","wellID","Eventdate"))
names(DO_ROC)[names(DO_ROC) == 'DO'] <- 'DO_ROC'

# mean and variance summary of depth to water (DTW) for each event
dtw_events = readRDS("DTW_compiled/DO_mv.rds")
#dtw_events = read_csv("DTW_compiled/DO_mv.csv")
names(dtw_events)[names(dtw_events) == 'WellID'] <- 'wellID'
names(dtw_events)[names(dtw_events) == 'Eventdates'] <- 'Eventdate'
dtw_events$Eventdate = as.POSIXct(paste(substr(dtw_events$Eventdate, start=1,stop=10),
                              substr(dtw_events$Eventdate, start=12,stop=19), sep=" "),
                              tz="US/Mountain")
dtw_events$siteID = substr(dtw_events$wellID, start = 1, stop = 3)
dtw_events[sapply(dtw_events, is.character)] <- lapply(dtw_events[sapply(dtw_events, is.character)],  as.factor)

# join data
DO_events_all = full_join(DO_AUC, DO_ROC, by=c("siteID","wellID","Eventdate"))
# the current dtw data by event does not have the same date/times as the DO events. Need to fix, but for now I will join to the nearest date/time
DO_events_all = DO_events_all %>% 
 arrange(Eventdate)  %>%  group_by(siteID, wellID)  
dtw_events = dtw_events %>% 
  arrange(Eventdate) %>%  group_by(siteID, wellID) 
DO_events_all$Eventdate_dtw = dtw_events$Eventdate
DO_events_all$Eventdate - DO_events_all$Eventdate_dtw # check that date/times are close
names(dtw_events)[names(dtw_events) == 'Eventdate'] <- 'Eventdate_dtw'
DO_events_all = left_join(DO_events_all, dtw_events, by=c("siteID","wellID","Eventdate_dtw"))
names(DO_events_all)[names(DO_events_all) == 'DO_event_mean'] <- 'dtw_DO_event_mean2'
names(DO_events_all)[names(DO_events_all) == 'DO_event_cv'] <- 'dtw_DO_event_cv2'
names(DO_events_all)[names(DO_events_all) == 'DO_event_mean1'] <- 'dtw_DO_event_mean1'
names(DO_events_all)[names(DO_events_all) == 'DO_event_cv1'] <- 'dtw_DO_event_cv1'
names(DO_events_all)[names(DO_events_all) == 'DO_event_mean5'] <- 'dtw_DO_event_mean5'
names(DO_events_all)[names(DO_events_all) == 'DO_event_cv5'] <- 'dtw_DO_event_cv5'

# replace NaNs with NA
DO_events_all[is.nan(DO_events_all)] <- NA

# clean up environment
rm(DO_AUC); rm(DO_ROC); rm(dtw_events)



#### explore data ####

# explore structure
with(DO_events_all, table(wellID))

DO_events_all = 
  DO_events_all %>% 
  group_by(siteID, wellID) %>% 
  arrange(Eventdate)
summary(DO_events_all)

# explore correlations
psych::pairs.panels(DO_events_all[c("DO_AUC", "dtw_DO_event_mean2", "dtw_DO_event_cv2")], stars = TRUE)
# event gw mean and gw var are fairly correlated (-0.41), so we probably shouldn't include them in the same model or test for an interaction without testing for variance inflations factors. 
# no obvious correlation between DO AUC and dtw stats, but we'll see how it looks when we group it by site and well in the models!


#### test for relationship btw DO AUC & dtw stats (2 days) ####

#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(DO_AUC ~ 1, data=DO_events_all, random=~1|wellID, method="ML")
m.1 = nlme::lme(DO_AUC ~ dtw_DO_event_mean2, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(DO_AUC ~ dtw_DO_event_cv2, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# both models with predictors are marginally better than the null- yay!
# models using mean vs variance of dtw as a predictor are indistinguishable 

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - some outliers, but not a consistent pattern

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. I need to try transforming data!
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# not great. I need to try transforming data!


#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(DO_AUC) ~ 1, data=DO_events_all, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean2, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_cv2, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
DO_events_all_r =DO_events_all[DO_events_all$dtw_DO_event_cv2<3,]
m.3 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_cv2, 
                data=DO_events_all_r, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# model using mean of dtw as a predictor is worst than null- bummer
# model using variance of dtw is indistinguishable from the null- could use

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

### plot of predicted model
# for all sites
visreg(m.1,"dtw_DO_event_mean2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_DO_event_cv2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_DO_event_cv2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)
# that point is influential and makes it significant, but it looks like the trend is there without it

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors
#### test for relationship btw DO ROC & dtw stats (2 days) ####

#+++++++++++++ ROC with nlme::lme #+++++++++++

m.null = nlme::lme(DO_ROC ~ 1, data=DO_events_all, random=~1|wellID, method="ML", na.action=na.omit)
m.1 = nlme::lme(DO_ROC ~ dtw_DO_event_mean2, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)
m.2 = nlme::lme(DO_ROC ~ dtw_DO_event_cv2, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# both models with predictors are better than the null!
# models using mean of dtw is slightly better than variance (essentially indistinguishable) 

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks worse - a lot of clumping of residuals

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks bad, very skewed, will need to be transformed
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# looks bad, very skewed, will need to be transformed


#+++++++++++++ with nlme::lme and POSITIVE LOG TRANSFORMED DATA #+++++++++++
# Multiply DO_ROC data by -1 to handle log transformations
DO_events_all$DO_ROC_s = DO_events_all$DO_ROC * -1

m.null = nlme::lme(log(DO_ROC_s) ~ 1, data=DO_events_all, random=~1|wellID, method="ML", na.action=na.omit)
m.1 = nlme::lme(log(DO_ROC_s) ~ dtw_DO_event_mean2, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)
m.2 = nlme::lme(log(DO_ROC_s) ~ dtw_DO_event_cv2, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)
DO_events_all_r =DO_events_all[DO_events_all$dtw_DO_event_cv2<3,]
m.3 = nlme::lme(log(DO_ROC_s) ~ dtw_DO_event_cv2, 
                data=DO_events_all_r, random=~1|siteID/wellID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# models using mean and variance of dtw as a predictor is worst than null- bummer

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks good
plot(m.2) #looks bad

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

### plot of predicted model
# for all sites
visreg(m.1,"dtw_DO_event_mean2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_DO_event_cv2",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_DO_event_cv2",type="conditional",points.par=list(cex=1.2),
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
#All very low, p-values of each model very high
#### test for relationship btw DO AUC & dtw stats (1 day) ####
#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(DO_AUC ~ 1, data=DO_events_all, random=~1|wellID, method="ML")
m.1 = nlme::lme(DO_AUC ~ dtw_DO_event_mean1, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(DO_AUC ~ dtw_DO_event_cv1, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# both models with predictors are marginally better than the null- yay!
# models using mean vs variance of dtw as a predictor are indistinguishable 

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - some outliers, but not a consistent pattern

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. I need to try transforming data!
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# not great. I need to try transforming data!


#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(DO_AUC) ~ 1, data=DO_events_all, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean1, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_cv1, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
DO_events_all_r =DO_events_all[DO_events_all$dtw_DO_event_cv1<3,]
m.3 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_cv1, 
                data=DO_events_all_r, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model scores lower than other two models

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
anova.lme(m.2,type = "marginal", adjustSigma = F) # marginal?

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

### plot of predicted model
# for all sites
visreg(m.1,"dtw_DO_event_mean1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_DO_event_cv1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_DO_event_cv1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)
# that point is influential and makes it significant, but it looks like the trend is there without it

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors
#### test for relationship btw DO ROC & dtw stats (1 day) ####
#+++++++++++++ ROC with nlme::lme #+++++++++++

m.null = nlme::lme(DO_ROC ~ 1, data=DO_events_all, random=~1|wellID, method="ML", na.action=na.omit)
m.1 = nlme::lme(DO_ROC ~ dtw_DO_event_mean1, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)
m.2 = nlme::lme(DO_ROC ~ dtw_DO_event_cv1, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# both models with predictors are better than the null!
# models using mean of dtw is slightly better than variance (essentially indistinguishable) 

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks worse - a lot of clumping of residuals

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks bad, very skewed, will need to be transformed
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# looks bad, very skewed, will need to be transformed


#+++++++++++++ with nlme::lme and POSITIVE LOG TRANSFORMED DATA #+++++++++++
# Multiply DO_ROC data by -1 to handle log transformations
DO_events_all$DO_ROC_s = DO_events_all$DO_ROC * -1

m.null = nlme::lme(log(DO_ROC_s) ~ 1, data=DO_events_all, random=~1|wellID, method="ML", na.action=na.omit)
m.1 = nlme::lme(log(DO_ROC_s) ~ dtw_DO_event_mean1, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)
m.2 = nlme::lme(log(DO_ROC_s) ~ dtw_DO_event_cv1, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)
DO_events_all_r =DO_events_all[DO_events_all$dtw_DO_event_cv1<3,]
m.3 = nlme::lme(log(DO_ROC_s) ~ dtw_DO_event_cv1, 
                data=DO_events_all_r, random=~1|siteID/wellID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# models using mean and variance of dtw as a predictor is worst than null- bummer

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks good
plot(m.2) #looks bad

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

### plot of predicted model
# for all sites
visreg(m.1,"dtw_DO_event_mean1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_DO_event_cv1",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_DO_event_cv1",type="conditional",points.par=list(cex=1.2),
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
#All very low, p-values of each model very high
#### test for relationship btw DO AUC & dtw stats (5 days) ####

#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(DO_AUC ~ 1, data=DO_events_all, random=~1|wellID, method="ML")
m.1 = nlme::lme(DO_AUC ~ dtw_DO_event_mean5, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(DO_AUC ~ dtw_DO_event_cv5, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# both models with predictors are worse than null

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks bad-ish - some outliers, but not a consistent pattern

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. I need to try transforming data!
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# not great. I need to try transforming data!


#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(DO_AUC) ~ 1, data=DO_events_all, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean5, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_cv5, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
DO_events_all_r =DO_events_all[DO_events_all$dtw_DO_event_cv5<3,]
m.3 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_cv5, 
                data=DO_events_all_r, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# models using mean and variancec of dtw as a predictor is worst than null- bummer

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

### plot of predicted model
# for all sites
visreg(m.1,"dtw_DO_event_mean5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_DO_event_cv5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3) # significant? p-value is 0.0191????
visreg(m.3,"dtw_DO_event_cv5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)
# that point is influential and makes it significant, but it looks like the trend is there without it

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors
#### test for relationship btw DO ROC & dtw stats (5 days) ####

#+++++++++++++ ROC with nlme::lme #+++++++++++

m.null = nlme::lme(DO_ROC ~ 1, data=DO_events_all, random=~1|wellID, method="ML", na.action=na.omit)
m.1 = nlme::lme(DO_ROC ~ dtw_DO_event_mean5, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)
m.2 = nlme::lme(DO_ROC ~ dtw_DO_event_cv5, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# both models with predictors are better than the null!
# models using mean of dtw is slightly better than variance (essentially indistinguishable) 

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern
plot(m.2) #looks worse - a lot of clumping of residuals

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks bad, very skewed, will need to be transformed
qqnorm(residuals(m.2))
qqline(residuals(m.2))
hist(residuals(m.2))
# looks bad, very skewed, will need to be transformed


#+++++++++++++ with nlme::lme and POSITIVE LOG TRANSFORMED DATA #+++++++++++
# Multiply DO_ROC data by -1 to handle log transformations
DO_events_all$DO_ROC_s = DO_events_all$DO_ROC * -1

m.null = nlme::lme(log(DO_ROC_s) ~ 1, data=DO_events_all, random=~1|wellID, method="ML", na.action=na.omit)
m.1 = nlme::lme(log(DO_ROC_s) ~ dtw_DO_event_mean5, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)
m.2 = nlme::lme(log(DO_ROC_s) ~ dtw_DO_event_cv5, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML", na.action=na.omit)
DO_events_all_r =DO_events_all[DO_events_all$dtw_DO_event_cv5<3,]
m.3 = nlme::lme(log(DO_ROC_s) ~ dtw_DO_event_cv5, 
                data=DO_events_all_r, random=~1|siteID/wellID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# models using mean and variance of dtw as a predictor is worst than null- bummer

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks good
plot(m.2) #looks bad

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

### plot of predicted model
# for all sites
visreg(m.1,"dtw_DO_event_mean5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) # this looks significant, but it isn't because it's just driven by different means of each site and well. This is why including random effects is important!
# for all sites
visreg(m.2,"dtw_DO_event_cv5",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_DO_event_cv5",type="conditional",points.par=list(cex=1.2),
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


