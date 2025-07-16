#### read me ####
# the purpose of this script is to test for relationships between water temperature mean and variance vs. magnitudes and rates of respiration events (modeled as Odum's ER) between wells OVER TIME for the Webster Lab BEGI project

# There are 2 sites (SLO and VOD).

# There are 2 wells within each site (SLOC, SLOW, VDOS, and VDOW).

# There are multiple respiration events in each well: 
# SLOC SLOW VDOS VDOW 
# 21   10   10   18 

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
library(scales)
library(ggbreak)
library(viridis)
library(gridExtra)


# replace NaNs with NA
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

#### load and wrangle data ####

# Odum's method of quantifying the size of respiration events from %DO timeseries
odumER = readRDS("EXO_compiled/odumER.rds")
odumER$siteID = substr(odumER$Well, start = 1, stop = 3)
names(odumER)[names(odumER) == 'Well'] <- 'wellID'
odumER[sapply(odumER, is.character)] <- lapply(odumER[sapply(odumER, is.character)],  as.factor)
names(odumER)[names(odumER) == 'Event'] <- 'eventID'

# area under the curve method of quantifying the relative size of respiration events from %DO timeseries
DO_AUC = readRDS("EXO_compiled/DO_AUC.rds")
DO_AUC$siteID = substr(DO_AUC$Well, start = 1, stop = 3)
names(DO_AUC)[names(DO_AUC) == 'Well'] <- 'wellID'
DO_AUC[sapply(DO_AUC, is.character)] <- lapply(DO_AUC[sapply(DO_AUC, is.character)],  as.factor)
names(DO_AUC)[names(DO_AUC) == 'Event'] <- 'eventID'
names(DO_AUC)[names(DO_AUC) == 'DO'] <- 'DO_AUC'

# mean and variance summary of water temperature for each event
temp_events = read.csv("DTW_compiled/temp_mv_2days.csv", stringsAsFactors = FALSE)
temp_events$Eventdates <- as.POSIXct(temp_events$Eventdates, format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain")
names(temp_events)[names(temp_events) == 'WellID'] <- 'wellID'
names(temp_events)[names(temp_events) == 'Eventdates'] <- 'Eventdate'
temp_events$siteID = substr(temp_events$wellID, start = 1, stop = 3)
temp_events[sapply(temp_events, is.character)] <- lapply(temp_events[sapply(temp_events, is.character)],  as.factor)

#join data
wt_events = full_join(DO_AUC, odumER, by=c("siteID","wellID","Eventdate","eventID"))
# the current dtw data by event does not have the same date/times as the DO events. Need to fix, but for now I will join to the nearest date/time
wt_events = wt_events %>% 
  arrange(Eventdate)  %>%  group_by(siteID, wellID)  
temp_events = temp_events %>% 
  arrange(Eventdate) %>%  group_by(siteID, wellID) 
wt_events$Eventdate_temp = temp_events$Eventdate
wt_events$Eventdate - wt_events$Eventdate_temp # check that date/times are close
#join if acceptable
wt_events = left_join(wt_events, temp_events, by=c("siteID","wellID","Eventdate"))

# clean up environment
rm(DO_AUC); rm(odumER); rm(temp_events)

#### explore data ####
# explore structure (based on 2 day water temp mean/var)
with(wt_events, table(wellID))

wt_events = 
  wt_events %>% 
  group_by(siteID, wellID) %>% 
  arrange(Eventdate)
summary(wt_events)

# explore correlations
# 2 days for water temp with ER, D, DO AUC
psych::pairs.panels(wt_events[c("ER", "temp_event_mean", "temp_event_cv")], stars = TRUE)
psych::pairs.panels(wt_events[c("D", "temp_event_mean", "temp_event_cv")], stars = TRUE)
psych::pairs.panels(wt_events[c("DO_AUC", "temp_event_mean", "temp_event_cv")], stars = TRUE)
# event temp mean and temp var are fairly correlated (-0.57), so we probably shouldn't include them in the same model or test for an interaction without testing for variance inflations factors. 
# no obvious correlation between DO ER or D and temp stats, but we'll see how it looks when we group it by site and well in the models!

#### test for relationship btw DO AUC & water temp stats (2 days) ####

#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(DO_AUC ~ 1, data=wt_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(DO_AUC ~ temp_event_mean, 
                data=wt_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(DO_AUC ~ temp_event_cv, 
                data=wt_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is slightly better than both models with predictors
# models using mean vs variance of temp as a predictor are indistinguishable 

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

m.null = nlme::lme(log(DO_AUC) ~ 1, data=wt_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(DO_AUC) ~ temp_event_mean, 
                data=wt_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(DO_AUC) ~ temp_event_cv, 
                data=wt_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model still slightly better than either models with predictors

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
visreg(m.1,"temp_event_mean",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Temperature Mean Preceeding Event", cex=1.8),
       ylab=list("DO Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE) 

visreg(m.2,"temp_event_cv",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Temperature Variation Preceeding Event", cex=1.8),
       ylab=list("DO Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)


## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors

#### test for relationship btw odum's ER & water temp stats (2 days) ####
#based on 2 day temp mean/var
#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(ER ~ 1, data=wt_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(ER ~ temp_event_mean, 
                data=wt_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(ER ~ temp_event_cv, 
                data=wt_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is lowest by 3 points...

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
wt_events$posER <- wt_events$ER * -1
#filter out 0 values
wt_events_filtered <- wt_events[wt_events$posER > 0, ]

m.null = nlme::lme(log(posER) ~ 1, data=wt_events_filtered, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(posER) ~ temp_event_mean, 
                data=wt_events_filtered, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(posER) ~ temp_event_cv, 
                data=wt_events_filtered, random=~1|siteID/wellID, method="ML")

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
visreg(m.1,"temp_event_mean",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Temperature Mean Preceeding Event", cex=1.8),
       ylab=list("Respiration (log)", cex=1.8),
       by='wellID',overlay=TRUE) 

visreg(m.2,"temp_event_cv",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Temperature Variation Preceeding Event", cex=1.8),
       ylab=list("Respiration (log)", cex=1.8),
       by='wellID',overlay=TRUE)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors

#### test for relationship btw D & water temp stats (2 days) ####
#based on 2 day water temp mean/var

#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(D ~ 1, data=wt_events, random=~1|wellID, method="ML")
m.1 = nlme::lme(D ~ temp_event_mean, 
                data=wt_events, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(D ~ temp_event_cv, 
                data=wt_events, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is lowest but all pretty close

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
wt_events_filtered <- wt_events[wt_events$D > 0, ]

m.null = nlme::lme(log(D) ~ 1, data=wt_events_filtered, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(D) ~ temp_event_mean, 
                data=wt_events_filtered, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(D) ~ temp_event_cv, 
                data=wt_events_filtered, random=~1|siteID/wellID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2)
# null model is still slightly better than m.1 and m.2

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
visreg(m.1,"temp_event_mean",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Temperature Mean Preceeding Event", cex=1.8),
       ylab=list("Diffusion (log)", cex=1.8),
       by='wellID',overlay=TRUE) 

visreg(m.2,"temp_event_cv",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Temperature Variation Preceeding Event", cex=1.8),
       ylab=list("Diffusion (log)", cex=1.8),
       by='wellID',overlay=TRUE)

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors






#### plot results ####


temp_gwvar = 
  ggplot(wt_events, aes(x = temp_event_cv, y = log(DO_AUC), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  labs(x = str_wrap("Water temperature Coef. of Variation Preceding Event (2 days)", width=35),
       y = str_wrap("Dissolved Oxygen Consumption Event Size", width=40))+
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(wt_events$DO_AUC)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
temp_gwvar
ggsave("plots/temp_gwvar.png",temp_gwvar, width = 9, height = 8, units = "in")

temp_gwmean = 
  ggplot(wt_events, aes(x = temp_event_mean, y = log(DO_AUC), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  labs(x = str_wrap("Mean Water Temperature Preceding Event (2 days)", width=35),
       y = str_wrap("Dissolved Oxygen Consumption Event Size", width=40))+
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(wt_events$DO_AUC)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
temp_gwmean
ggsave("plots/temp_gwmean.png",temp_gwmean, width = 9, height = 8, units = "in")





