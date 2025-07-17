#### read me ####

# the purpose of this script is to test for differences in magnitudes and rates of respiration events between wells for the Webster Lab BEGI project

# There are 2 sites (SLO and VOD).
# There are 2 wells within each site (SLOC, SLOW, VDOS, and VDOW).
# There are multiple respiration events in each well: 
# SLOC SLOW VDOS VDOW 
# 18    9   10   15 

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

# eventIDs
DOevents = DO_AUC[,2:5]

# rate of change method of quantifying the rate of respiration events from %DO timeseries
DO_ROC = readRDS("EXO_compiled/DO_roc.rds")
DO_ROC$siteID = substr(DO_ROC$Well, start = 1, stop = 3)
names(DO_ROC)[names(DO_ROC) == 'Well'] <- 'wellID'
DO_ROC[sapply(DO_ROC, is.character)] <- lapply(DO_ROC[sapply(DO_ROC, is.character)],  as.factor)
#DO_ROC = left_join(DO_ROC, DOevents, by=c("siteID","wellID","Eventdate"))
names(DO_ROC)[names(DO_ROC) == 'DO'] <- 'DO_ROC'

# mean and variance summary of depth to water (DTW) for each well for whole period of %DO observation
gwmv_well = read.csv("DTW_compiled/gwmv_well.csv", row.names = 1)
names(gwmv_well)[names(gwmv_well) == 'wells'] <- 'wellID'
gwmv_well$siteID = substr(gwmv_well$wellID, start = 1, stop = 3)
gwmv_well[sapply(gwmv_well, is.character)] <- lapply(gwmv_well[sapply(gwmv_well, is.character)],  as.factor)

# join data
DO_events_all = full_join(DO_AUC, DO_ROC, by=c("siteID","wellID","Eventdate"))
DO_events_all = left_join(DO_events_all, gwmv_well, by=c("siteID","wellID"))

# replace NaNs with NA
DO_events_all[is.nan(DO_events_all)] <- NA

# clean up environment
rm(DO_AUC); rm(DO_ROC); rm(DOevents)

# Add + 2 to DOC_ROC to handle log transformations
DO_events_all$DO_ROC_con = DO_events_all$DO_ROC + 2

# Multiply DO_ROC data by -1 to handle log transformations
DO_events_all$DO_ROC_s = DO_events_all$DO_ROC * -1


#### explore data ####

# explore structure
with(DO_events_all, table(wellID))

DO_events_all = 
  DO_events_all %>% 
  group_by(siteID, wellID) %>% 
  arrange(Eventdate)
summary(DO_events_all)

# explore correlations
psych::pairs.panels(DO_events_all[c("DO_AUC", "gwmean_well", "gwvar_well")], stars = TRUE)
# gw mean and gw var are highly correlated, so we can't include them in the same model or test for an interaction. This is good motivation to simply test for group mean differences by well and site and interpret effects of gw mean and var qualitatively. 

# explore normality of data
plot.new()
qqPlot(DO_events_all$DO_AUC[DO_events_all$wellID=="SLOC"]); shapiro.test(DO_events_all$DO_AUC[DO_events_all$wellID=="SLOC"]) # not normal, but close other than 2 (maybe 3) outliers
qqPlot(DO_events_all$DO_AUC[DO_events_all$wellID=="SLOW"]); shapiro.test(DO_events_all$DO_AUC[DO_events_all$wellID=="SLOW"]) # not normal, but close other than 1 outlier
qqPlot(DO_events_all$DO_AUC[DO_events_all$wellID=="VDOW"]); shapiro.test(DO_events_all$DO_AUC[DO_events_all$wellID=="VDOW"]) # not normal, but close  other than 4 outliers
qqPlot(DO_events_all$DO_AUC[DO_events_all$wellID=="VDOS"]); shapiro.test(DO_events_all$DO_AUC[DO_events_all$wellID=="VDOS"]) # not normal, but close  other than 2 outliers. this one is the closest

qqPlot(DO_events_all$DO_ROC[DO_events_all$wellID=="SLOC"]); shapiro.test(DO_events_all$DO_ROC[DO_events_all$wellID=="SLOC"]) # not normal, but close other than 2 (maybe 3) outliers
qqPlot(DO_events_all$DO_ROC[DO_events_all$wellID=="SLOW"]); shapiro.test(DO_events_all$DO_ROC[DO_events_all$wellID=="SLOW"]) # techincally normal, despite 1 outlier
qqPlot(DO_events_all$DO_ROC[DO_events_all$wellID=="VDOW"]); shapiro.test(DO_events_all$DO_ROC[DO_events_all$wellID=="VDOW"]) # not normal, but close  other than 3 outliers
qqPlot(DO_events_all$DO_ROC[DO_events_all$wellID=="VDOS"]); shapiro.test(DO_events_all$DO_ROC[DO_events_all$wellID=="VDOS"]) # not normal, but close  other than 1 outlier

qqPlot(DO_events_all$DO_AUC); shapiro.test(DO_events_all$DO_AUC) 
qqPlot(DO_events_all$DO_ROC); shapiro.test(DO_events_all$DO_ROC) 
# overall, outliers make for a skewed distribution (positive for AUC and negative for ROC). We may need a gamma, exponential, or other generalized linear model to properly fit


# check for temporal autocorrelation
# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer

### subset data to be one site and one parameter
temp = DO_events_all[DO_events_all$wellID=="SLOC" ,]
temp = temp[,c(1,3)]
### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(Eventdate) 
# there are two  duplicated dates, which I will assign to the next day to avoid duplicated time stamps
duplicated(as.Date(temp$Eventdate))
temp$Eventdate[temp$Eventdate=="2023-11-17 20:15:00"] = temp$Eventdate[temp$Eventdate=="2023-11-17 20:15:00"] + 4*60*60
temp$Eventdate = as.Date(substr(temp$Eventdate,1,10))
any(duplicated(temp$Eventdate))
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(Eventdate = seq(min(Eventdate), max(Eventdate), by = "1 day"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = Eventdate)
head (temp_ts)
temp_ts = ts(temp_ts$DO_AUC, frequency=365, start=c(2023, yday(temp_ts$Eventdate[1])) ) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp)

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)
# it is challenging to interpret this given the gappines of the data, since na/pass doesn't work and I don't trust the other 2 na dealing methods since the gaps are so large. I think we'll need to rely on model residuals rather than this analysis to assess the issue of temporal autocorrelation


#### group means DO_AUC ####

#+++++++++++++ trying various model algorithms and structures #+++++++++++

# m.null = lmer(DO_AUC ~ 1 + (1|siteID), data=DO_events_all)
# summary(m.null)
# ranova(m.null) # request test of random effects
# 
# m.1 = lmer(DO_AUC ~ wellID + (1|siteID), REML = FALSE, data=DO_events_all)
# # model is too complex for the data
# 
# m.2 = lmer(DO_AUC ~ 1 + (wellID|siteID), data=DO_events_all)
# # model is too complex for the data
# 
# m.3 = lmer(DO_AUC ~ 1 + (1|wellID), data=DO_events_all)
# summary(m.3)
# ranova(m.3) # request test of random effects
# confint(m.3) # request test of random effects (variance displayed as SD)
# 
# m.4 = lm(DO_AUC ~ wellID, data=DO_events_all)
# summary(m.4)
# # this works but is not respecting the nesting of wells within sites, so is not correct
# 
# m.5 = lm(DO_AUC ~ siteID + wellID, data=DO_events_all)
# # model is too complex for the data
# 
# m.6 = nlme::lme(DO_AUC ~wellID, data=DO_events_all, random=~1|siteID, method="ML")
# # it seems that lme has a more robust estimation and works!
# summary(m.6)


#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(DO_AUC ~ 1, data=DO_events_all, random=~1|siteID, method="ML")
m.1 = nlme::lme(DO_AUC ~ wellID, data=DO_events_all, random=~1|siteID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# Results: 
# AICc is <2 pts different, so wellID does not provide the model significantly more information, but is also not a worst model. So, we can use it.  

Anova(m.1, type=2) #Chi-Square test for influence of wellID on AUC fitted with type 2 error
# this tells us that all wells aren't significantly different from each other (but we know from the main summary that the well representing the intercept SLOC IS different)

#you could also get the p-value via a Chi-Square likelihood ratio test against the null model
#remember these models have to be nested - with the more complex model listed first
anova(m.1, m.null)

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad - large increase in variance between groups 

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. I need to try transforming data!


#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++

m.null = nlme::lme(log(DO_AUC) ~ 1, data=DO_events_all, random=~1|siteID, method="ML")
m.1 = nlme::lme(log(DO_AUC) ~ wellID, data=DO_events_all, random=~1|siteID, method="ML")

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# Results: 
# m.1 performs much better than null now!  

Anova(m.1, type=2) #Chi-Square test for influence of wellID on AUC fitted with type 2 error
# strong differences between wells

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks great!

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# looks great!

#3 temporal autocorrelation in data
forecast::Acf(residuals(m.1))
# looks shockingly good, though we may need to consider if data is structured correctly!


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

# NOTES ABOUT HOW TO INTERPRET COEFFICENT VALUES:
# The intercept is defined as the expected outcome (DO_AUC) when all predictors are zero. In this case, the intercept will be the mean AUC for SLOC. The other coefficients in the model will be differences between this mean and the means for the comparison groups.
# So:
# mean AUC for SLOC = exp(5.742010) = 311.6903
# mean AUC for SLOW = exp(5.742010-2.682254) = 21.32235
# mean AUC for VDOS = exp(5.742010-1.419104) = 75.40744
# mean AUC for VDOW = exp(5.742010-1.668458) = 58.76533

# the p values and confidence intervals for each coefficient I think just indicate that AUC in SLOC is different from all the others. We need to reorder factors to set what the intercept is so that we can interpret the other differences between wells. 

### reorder wellID to set intercept to other wells:
## SLOC
m.1.SLOCint = m.1
# interpretation: SLOC is significantly different from all other wells. The negative Value estimates indicate that all the other wells have lower mean AUC than SLOC.

## SLOW
DO_events_all_SLOWint <- within(DO_events_all, wellID <- relevel(wellID, ref = "SLOW"))
m.1.SLOWint = nlme::lme(log(DO_AUC) ~ wellID, data=DO_events_all_SLOWint, random=~1|siteID, method="ML")
summary(m.1.SLOWint)
# interpretation: SLOW is significantly different from all other wells. The all positive Value estimates indicate that all the other wells have HIGHER mean AUC than SLOC.

## VDOS
DO_events_all_VDOSint <- within(DO_events_all, wellID <- relevel(wellID, ref = "VDOS"))
m.1.VDOSint = nlme::lme(log(DO_AUC) ~ wellID, data=DO_events_all_VDOSint, random=~1|siteID, method="ML")
summary(m.1.VDOSint)
# interpretation: VDOS is significantly different from SLOC and SLOW, but NOT VDOW. AUC in VDOS is lower than in SLOC,  higher than SLOW, and is not sig. different than VDOW. 

## VDOW
DO_events_all_VDOWint <- within(DO_events_all, wellID <- relevel(wellID, ref = "VDOW"))
m.1.VDOWint = nlme::lme(log(DO_AUC) ~ wellID, data=DO_events_all_VDOWint, random=~1|siteID, method="ML")
summary(m.1.VDOWint)
# interpretation: VDOW is significantly different from SLOC, but NOT from VDOW or SLOW (this last one is marginal... p-value is 0.0556 so it is ALMOST a sig difference, but not quite). AUC in VDOW is lower than in SLOC and is not sig. different than VDOW or SLOW (though it is marginally higher than SLOW, but not significantly so). 

#### group means DO_ROC ####

#+++++++++++++ with nlme::lme #+++++++++++

m.null = nlme::lme(DO_ROC ~ 1, data=DO_events_all, random=~1|siteID, method="ML", na.action=na.omit)
m.1 = nlme::lme(DO_ROC ~ wellID, data=DO_events_all, random=~1|siteID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# Results: 
# AICc is >2 pts different with m.1 being lower, so wellID does provide the model significantly more information. So, we can use it.  

Anova(m.1, type=2) #Chi-Square test for influence of wellID on ROC fitted with type 2 error
# All wells aren't significantly different from each other 

#you could also get the p-value via a Chi-Square likelihood ratio test against the null model
#remember these models have to be nested - with the more complex model listed first
anova(m.1, m.null)

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #not great 

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great, pretty skewed. Will need to be transformed.


#+++++++++++++ with nlme::lme and LOG TRANSFORMED DATA #+++++++++++
# taking the log of DOC_ROC data +2
m.null = nlme::lme(log(DO_ROC_con) ~ 1, data=DO_events_all, random=~1|siteID, method="ML", na.action=na.omit)
m.1 = nlme::lme(log(DO_ROC_con) ~ wellID, data=DO_events_all, random=~1|siteID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# Results: 
# m.1 performs better than null  

Anova(m.1, type=2) #Chi-Square test for influence of wellID on AUC fitted with type 2 error
# still no significant difference between wells

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #not great

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# does not look good. Will need to try a different transformation


#+++++++++++++ with nlme::lme and POSITIVE TRANSFORMED DATA #+++++++++++
# testing model fit on DOC_ROC * -1
m.null = nlme::lme(DO_ROC_s ~ 1, data=DO_events_all, random=~1|siteID, method="ML", na.action=na.omit)
m.1 = nlme::lme(DO_ROC_s ~ wellID, data=DO_events_all, random=~1|siteID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# Results: 
# m.1 performs better than null  

Anova(m.1, type=2) #Chi-Square test for influence of wellID on AUC fitted with type 2 error
# still no significant difference between wells

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #not great

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# does not look good. Will need to try a different transformation


#+++++++++++++ with nlme::lme and LOG OF POSITIVE TRANSFORMED DATA #+++++++++++
# taking the log of DOC_ROC_s
m.null = nlme::lme(log(DO_ROC_s) ~ 1, data=DO_events_all, random=~1|siteID, method="ML", na.action=na.omit)
m.1 = nlme::lme(log(DO_ROC_s) ~ wellID, data=DO_events_all, random=~1|siteID, method="ML", na.action=na.omit)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# Results: 
# AICc is <2 pts different, so wellID does not provide the model significantly more information, but is also not a worst model. So, we can use it.  


Anova(m.1, type=2) #Chi-Square test for influence of wellID on AUC fitted with type 2 error
# still no significant difference between wells

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #not great

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great but better than previous transformations. Less skewed.

#3 temporal autocorrelation in data
forecast::Acf(residuals(m.1))
# looks good, though we may need to consider if data is structured correctly


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
exp()
exp()
# etc....


## Finally, look at model summary: m.1
summary(m.1)

# NOTES ABOUT HOW TO INTERPRET COEFFICENT VALUES:
# The intercept is defined as the expected outcome (DO_AUC) when all predictors are zero. In this case, the intercept will be the mean AUC for SLOC. The other coefficients in the model will be differences between this mean and the means for the comparison groups.
# So:
# mean AUC for SLOC = exp(5.742010) = 311.6903
# mean AUC for SLOW = exp(5.742010-2.682254) = 21.32235
# mean AUC for VDOS = exp(5.742010-1.419104) = 75.40744
# mean AUC for VDOW = exp(5.742010-1.668458) = 58.76533

# the p values and confidence intervals for each coefficient I think just indicate that AUC in SLOC is different from all the others. We need to reorder factors to set what the intercept is so that we can interpret the other differences between wells. 

### reorder wellID to set intercept to other wells:
## SLOC
m.1.SLOCint = m.1
# interpretation: SLOC is significantly different from all other wells. The negative Value estimates indicate that all the other wells have lower mean AUC than SLOC.

## SLOW
DO_events_all_SLOWint <- within(DO_events_all, wellID <- relevel(wellID, ref = "SLOW"))
m.1.SLOWint = nlme::lme(log(DO_ROC_s) ~ wellID, data=DO_events_all_SLOWint, random=~1|siteID, method="ML", na.action=na.omit)
summary(m.1.SLOWint)
# interpretation: SLOW is significantly different from VDOW. SLOW is marginally different from SLOC

## VDOS
DO_events_all_VDOSint <- within(DO_events_all, wellID <- relevel(wellID, ref = "VDOS"))
m.1.VDOSint = nlme::lme(log(DO_ROC_s) ~ wellID, data=DO_events_all_VDOSint, random=~1|siteID, method="ML", na.action=na.omit)
summary(m.1.VDOSint)
# interpretation: VDOS is not significantly different  from any other well in ROC 

## VDOW
DO_events_all_VDOWint <- within(DO_events_all, wellID <- relevel(wellID, ref = "VDOW"))
m.1.VDOWint = nlme::lme(log(DO_ROC_s) ~ wellID, data=DO_events_all_VDOWint, random=~1|siteID, method="ML", na.action=na.omit)
summary(m.1.VDOWint)
# interpretation: VDOW is significantly different from SLOW, but NOT from VDOS or SLOC 
