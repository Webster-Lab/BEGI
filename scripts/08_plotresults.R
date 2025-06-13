#### read me ####

# the purpose of this script is to plot data and model results thus far for presentations from the Webster Lab BEGI project

# There are 2 sites (SLO and VOD).

# There are 2 wells within each site (SLOC, SLOW, VDOS, and VDOW).

# There are multiple respiration events in each well: 
# SLOC SLOW VDOS VDOW 
# 21   10   10   18 

# For each respiration event, we have quantified the mean and variance of depth to water for a time period preceding the respiration event. We will start by testing 2 days before the event, and later we will try shorter and longer time periods for relationships with respiration.

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
library(scales)
library(ggbreak)
library(viridis)
library(gridExtra)

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
dtw_events = read.csv("DTW_compiled/DO_mv_2days.csv")
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
names(DO_events_all)[names(DO_events_all) == 'DO_event_mean'] <- 'dtw_DO_event_mean'
names(DO_events_all)[names(DO_events_all) == 'DO_event_cv'] <- 'dtw_DO_event_cv'

# replace NaNs with NA
DO_events_all[is.nan(DO_events_all)] <- NA

# clean up environment
rm(DO_AUC); rm(DO_ROC); rm(dtw_events)




#### plot respriation events over time ####

DO_AUC_ts = 
  DO_events_all %>%
  ggplot()+
  geom_line(aes(x = Eventdate, y = (DO_AUC), color=wellID),key_glyph = "timeseries",linewidth=1)+
  geom_point(aes(x = Eventdate, y = (DO_AUC), color=wellID), alpha = 0.7, size=5)+
  # scale_y_continuous(breaks = seq(0,10000,1000), limits = c(0,10000)) +
  # scale_y_break(c(4000,8000), space = 0.4, ticklabels = c(seq(0,10000,1000)), expand = c(0, 0)) +
  labs(x = "Time", 
       y = str_wrap("Dissolved Oxygen Consumption Event Size", width=25))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_AUC_ts
ggsave("plots/DO_AUC_overtime.png",DO_AUC_ts, width = 15, height = 8, units = "in")

#### plot respriation events vs gw variance ####

DO_AUC_gwvar_log = 
  ggplot(DO_events_all, aes(x = dtw_DO_event_cv, y = log(DO_AUC), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  geom_smooth(method = "lm", fill=NA) +
  labs(x = str_wrap("Depth to Groundwater Coef. of Variation Preceeding Event (2 days)", width=35),
       y = str_wrap("Dissolved Oxygen Consumption Event Size (log scale)", width=40))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_AUC_gwvar_log
ggsave("plots/DO_AUC_gwvar_log.png",DO_AUC_gwvar_log, width = 9, height = 8, units = "in")

DO_ROC_gwvar = 
  ggplot(DO_events_all, aes(x = dtw_DO_event_cv, y = log(DO_ROC*-1), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  geom_smooth(method = "lm", fill=NA) +
  labs(x = "Groundwater Variation Preceeding Event", 
       y = str_wrap("Dissolved Oxygen Consumption Rate (log % saturation change per 15 min)", width=36))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_ROC_gwvar
ggsave("plots/DO_ROC_gwvar_log.png",DO_ROC_gwvar, width = 9, height = 8, units = "in")


#### plot respriation events vs gw mean ####

# NOTE: Eve used the dtw_m with +1 constant added that I made to calcualte cv (see 05_gwmeanvar.R script), so all these gw mean values have +1 added to them, and therefore look deeper than they actually were. I will fix this in the plots, but we should probably fix this in the modeling too. 

DO_AUC_gwmean_log = 
  ggplot(DO_events_all, aes(x = dtw_DO_event_mean-1, y = log(DO_AUC), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  geom_smooth(method = "lm", fill=NA) +
  labs(x = "Mean Depth to Groundwater Preceeding Event (2 days)", 
       y = str_wrap("Dissolved Oxygen Event Size (log DO consumption)", width=25))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_AUC_gwmean_log
ggsave("plots/DO_AUC_gwmean_log.png",DO_AUC_gwmean_log, width = 9, height = 8, units = "in")

DO_AUC_gwmean = 
  ggplot(DO_events_all, aes(x = dtw_DO_event_mean-1, y = (DO_AUC), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  geom_smooth(method = "lm", fill=NA) +
  labs(x = "Mean Depth to Groundwater Preceeding Event (2 days)", 
       y = str_wrap("Dissolved Oxygen Event Size (DO consumption)", width=25))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_AUC_gwmean
ggsave("plots/DO_AUC_gwmean.png",DO_AUC_gwmean, width = 9, height = 8, units = "in")

DO_ROC_gwmean_log = 
  ggplot(DO_events_all, aes(x = dtw_DO_event_mean-1, y = log(DO_ROC*-1), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  geom_smooth(method = "lm", fill=NA) +
  labs(x = "Mean Depth to Groundwater (m) Preceeding Event (2 days)", 
       y = str_wrap("Dissolved Oxygen Consumption Rate (log % saturation change per 15 min)", width=36))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_ROC_gwmean_log
ggsave("plots/DO_ROC_gwmean_log.png",DO_ROC_gwmean_log, width = 9, height = 8, units = "in")

# DTW_df = readRDS( "DTW_compiled/BEGI_PT_DTW_all.rds")
# dtw_events = read.csv("DTW_compiled/DO_mv_2days.csv")
# gwmv_well = read.csv("DTW_compiled/gwmv_well.csv", row.names = 1)
# summary(DTW_df$DTW_m)

#### model DO_AUC events and visualize ####

# I went a little wild with trying different model structures in here. The one you really want is m.2 and the visreg plot, but feel free to explore other things I did in here

m.null = nlme::lme(log(DO_AUC) ~ 1, data=DO_events_all, random=~1|wellID, method="ML")
m.1 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
m.2 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_cv, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
DO_events_all_r =DO_events_all[DO_events_all$dtw_DO_event_cv<3,]
m.3 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_cv, 
                data=DO_events_all_r, random=~1|siteID/wellID, method="ML")
m.4 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean*dtw_DO_event_cv, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
# allow random slopes by well
#m.5 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean, 
#                data=DO_events_all, random=~wellID|siteID/wellID, method="ML") # this is the proper way to specify the nesting of wells within sites, but it doesn't converge
m.5 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean, 
                data=DO_events_all, random=~1+wellID|wellID, method="ML") # this allows slopes and intercepts to vary by well, but treats wells within sites as independent, which is incorrect. but, it converges.
#m.6 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean * wellID, 
#                data=DO_events_all, random=~1|siteID/wellID, method="ML") # this allows the effect of mean gw to vary by well using an interaction term instead of a random effect. However, it doesn't converge as indicated by it being unable to produce results for CIs and p values below
m.6 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean, 
                data=DO_events_all, random=~1|wellID, method="ML") # to compare to m.5 to see if random slopes improve model fit

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1, m.2, m.4, m.5, m.6)
# all models that include mean of dtw as a predictor are both worst than null- bummer
# model using variance of dtw is indistinguishable from the null- could use
# random slopes + intercepts (m.5) do not improve the mean gw model compared to just random intercepts (m.6)

## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks good
plot(m.2) #looks good
plot(m.4)#looks good
plot(m.5)#looks good

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
qqnorm(residuals(m.4))
qqline(residuals(m.4))
hist(residuals(m.4))
# looks great
qqnorm(residuals(m.5)); qqline(residuals(m.5))
hist(residuals(m.5))
# looks great


#3 temporal autocorrelation in data
forecast::Acf(residuals(m.1))
# looks great
forecast::Acf(residuals(m.2))
# looks great
forecast::Acf(residuals(m.4))
# looks great
forecast::Acf(residuals(m.5))
# looks great


# GET P-VALUES AND COEFFICIENT ESTIMATES WITH 95% CONFIDENCE INTERVALS
#F-tests 
anova.lme(m.1,type = "marginal", adjustSigma = F)
anova.lme(m.2,type = "marginal", adjustSigma = F)
anova.lme(m.4,type = "marginal", adjustSigma = F)
anova.lme(m.5,type = "marginal", adjustSigma = F)

#95% CI gives you LOWER and UPPER bound around the MEAN ESTIMATE for each parameter
#linear, quadratic terms with 95% Confidence Intervals
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") 
m.2_conf_int <- intervals(m.2, level = 0.95, which = "fixed") 
m.4_conf_int <- intervals(m.4, level = 0.95, which = "fixed") 
m.5_conf_int <- intervals(m.5, level = 0.95, which = "fixed") 

# view model summaries
summary(m.1)
summary(m.2)
summary(m.4)
summary(m.5)

## look at random effects to varify that intercepts and/or slopes are being estimated differently as expected
ranef(m.1)
ranef(m.2)
ranef(m.5)

### plot of predicted model

visreg(m.2,"dtw_DO_event_cv",type="conditional",
       fill=list(col="lightgrey"),
       points.par=list(cex=2, col=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF")),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Depth to Groundwater Coef. of Variation Preceeding Event (2 days)", cex=1.2),
       ylab=list("Dissolved Oxygen Consumption Event Size (log scale)", cex=1.2),
       by='wellID',overlay=TRUE, legend=FALSE)

visreg(m.5,"dtw_DO_event_mean",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Depth to Groundwater Preceeding Event", cex=1.8),
       ylab=list("Dissolved Oxygen Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)
# this doesn't show the random slopes or intercepts, which I really want but need to probably do some prediction to get
ranef(m.5)

# look at model without the far high variability point to make sure it isn't driving the result
summary(m.3)
visreg(m.3,"dtw_DO_event_cv",type="conditional",points.par=list(cex=1.2),
       fill=list(col="lightgrey"),
       cex.axis=1.4, line.par=list(col="black"),
       xlab=list("Groundwater Variation Preceeding Event", cex=1.8),
       ylab=list("Dissolved Oxygen Event Magnitude (log)", cex=1.8),
       by='wellID',overlay=TRUE)
# that point is influential and makes it significant, but it looks like the trend is there without it

## get Pseudo-R-squares
r.squaredGLMM(m.1)
r.squaredGLMM(m.2)
r.squaredGLMM(m.3)
r.squaredGLMM(m.5)
# #Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: the proportion of variance explained by both the fixed and random factors


#### plot respiration events and DTW data ts together ####

DTW_df = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")
DO_events_all$datetimeMT = DO_events_all$Eventdate

DTW_DOevents = left_join(DTW_df, DO_events_all, by=c("siteID","wellID","datetimeMT"))
summary(DTW_DOevents$datetimeMT)

# summary(DO_events_all$Eventdate)
# DTW_df_r = DTW_df[DTW_df$datetimeMT>= as.POSIXct("2023-09-20 10:45:00", tz="US/Mountain") &
#                     DTW_df$datetimeMT<= as.POSIXct("2024-08-19 10:30:00", tz="US/Mountain"),]

Q = 
  ggplot(DTW_DOevents, aes(datetimeMT, Q_Lsec)) +
  xlab("") +
  ylab("Q (L/sec)") +
  geom_line(linewidth=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15))

DTW = 
  ggplot(DTW_DOevents, aes(datetimeMT, DTW_m*-1, color=wellID)) +
  xlab("") +
  ylab(str_wrap("Water Depth Below Surface (m)", width=23))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_line(key_glyph = "timeseries",linewidth=1,alpha=0.75) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15))+
  scale_color_viridis(discrete = TRUE, option = "D")

DO_AUC_ts = 
  ggplot(DTW_DOevents[!is.na(DTW_DOevents$DO_AUC),], 
         aes(x = datetimeMT, y = (DO_AUC), color=wellID))+
  geom_line(key_glyph = "timeseries",linewidth=1,)+
  geom_point(alpha = 0.7, size=5)+
  labs(x = "", 
       y = str_wrap("Dissolved Oxygen Consumption Event Size", width=23))+
  xlim(c(as.POSIXct("2023-09-15 00:00:00", tz="US/Mountain"), 
         as.POSIXct("2024-12-03 00:00:00", tz="US/Mountain")))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15))+
  scale_color_viridis(discrete = TRUE, option = "D")

DO_AUC_ts_log = 
  ggplot(DTW_DOevents[!is.na(DTW_DOevents$DO_AUC),], 
         aes(x = datetimeMT, y = log(DO_AUC), color=wellID))+
  geom_line(key_glyph = "timeseries",linewidth=1,)+
  geom_point(alpha = 0.7, size=5)+
  labs(x = "", 
       y = str_wrap("Dissolved Oxygen Consumption Event Size (log scale)", width=23))+
  xlim(c(as.POSIXct("2023-09-15 00:00:00", tz="US/Mountain"), 
         as.POSIXct("2024-12-03 00:00:00", tz="US/Mountain")))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 15))+
  scale_color_viridis(discrete = TRUE, option = "D")


Q_DTW_DOevents = 
  Q+ DTW+ DO_AUC_ts +DO_AUC_ts_log +
  plot_layout(ncol = 1, widths = c(1,.84,1,1), heights=c(1,1.5,1.5,1.5))
ggsave("plots/RGdischarge_allwellsDTW_DOAUC.png", Q_DTW_DOevents, width=11,height=10, units="in")

#### load and wrangle updated data ####
#using compiled dtw dataset with varying time periods and odum's ER

# Odum's method of quantifying the size of respiration events from %DO timeseries
odumER = readRDS("EXO_compiled/odumER.rds")
odumER$siteID = substr(odumER$Well, start = 1, stop = 3)
names(odumER)[names(odumER) == 'Well'] <- 'wellID'
odumER[sapply(odumER, is.character)] <- lapply(odumER[sapply(odumER, is.character)],  as.factor)
names(odumER)[names(odumER) == 'Event'] <- 'eventID'

# # eventIDs
# DOevents = DO_AUC[,2:5]

# mean and variance summary of depth to water (DTW) for each event
dtw_events = read.csv("DTW_compiled/DO_mv.csv")
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
#need to multiply ER by -1 to be able to log transform
ER_events$posER <- ER_events$ER * -1


# replace NaNs with NA
ER_events[is.nan(ER_events)] <- NA

# clean up environment
rm(odumER); rm(dtw_events)

#### plot respiration events over time ####
#D
D_gwvar_log = 
  ggplot(ER_events, aes(x = dtw_ER_event_cv2, y = log(D), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  labs(x = str_wrap("Depth to Groundwater Coef. of Variation Preceeding Event (2 days)", width=35),
       y = str_wrap("Oxygen Uptake via Diffusion (log scale)", width=40))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
D_gwvar_log
ggsave("plots/D_gwvar_log.png",D_gwvar_log, width = 9, height = 8, units = "in")

#ER
ER_gwvar_log = 
  ggplot(ER_events, aes(x = dtw_ER_event_cv2, y = log(posER), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  labs(x = str_wrap("Depth to Groundwater Coef. of Variation Preceeding Event (2 days)", width=35),
       y = str_wrap("Ecosystem Respiration (log scale)", width=40))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
ER_gwvar_log
ggsave("plots/ER_gwvar_log.png",ER_gwvar_log, width = 9, height = 8, units = "in")


#AUC, ER, and D
poster_results <- grid.arrange(DO_AUC_gwvar_log, ER_gwvar_log, D_gwvar_log, ncol=3)
ggsave("plots/poster_results.png", poster_results, width = 20, height = 8, units = "in")

#ER and D
pres_results <- grid.arrange(ER_gwvar_log, D_gwvar_log, ncol=2)
ggsave("plots/pres_results.png", pres_results, width = 20, height = 8, units = "in")

#### DO and FDOM event example ####
### Import compiled EXO1 RDS file ###
BEGI_EXO.or2 = readRDS("EXO_compiled/BEGI_EXO.or2.rds")

### Correct negative DO values ###
BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn <- BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn + 0.3


### Read in .csv files of service dates and times ###
service.SLOC = read.csv("EXO_compiled/service.SLOC.csv", row.names = 1)
names(service.SLOC) = "datetimeMT"
service.SLOC$datetimeMT = as.POSIXct(service.SLOC$datetimeMT, tz="US/Mountain")

# sunrise/sunset
suntimes = 
  getSunlightTimes(date = seq.Date(from = as.Date("2023-09-14"), to = as.Date("2024-09-5"), by = 1),
                   keep = c("sunrise", "sunset"),
                   lat = 34.9, lon = -106.7, tz = "US/Mountain")

pm.pts = suntimes$sunset[-(nrow(suntimes))]
am.pts = suntimes$sunrise[-1]


## SLOC 10-06 17:00 to 10-12 17:00 ##
tempdat = BEGI_EXO.or2[["SLOC"]][BEGI_EXO.or2[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-17 11:15:01", tz="US/Mountain") & 
                                   BEGI_EXO.or2[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-18 00:15:00", tz="US/Mountain"), ]

  
  #save plot 
jpeg("plots/SLOC_event.jpg", width = 8, height = 8, units="in", res=1000)
plot.new()
  
  par(mfrow=c(2,1))
  
  plot(tempdat$datetimeMT, tempdat$ODO.mg.L.mn,
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,6.0))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="Dissolved Oxygen (mg/L)")

  plot(tempdat$datetimeMT, tempdat$fDOM.QSU.mn,
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,80))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="fDOM (QSU)")
  
dev.off()


#### lateral transfer example ####

# read in and wrangle data from previous section

## SLOC 10-06 17:00 to 10-12 17:00 ##
tempdat = BEGI_EXO.or2[["SLOC"]][BEGI_EXO.or2[["SLOC"]]$datetimeMT >= as.POSIXct("2024-08-25 10:15:01", tz="US/Mountain") & 
                                   BEGI_EXO.or2[["SLOC"]]$datetimeMT <= as.POSIXct("2024-08-26 10:15:00", tz="US/Mountain"), ]


#save plot 
jpeg("plots/lt_event.jpg", width = 8, height = 8, units="in", res=1000)
plot.new()

par(mfrow=c(2,1))

plot(tempdat$datetimeMT, tempdat$ODO.mg.L.mn,
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,6.0))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(tempdat$datetimeMT, tempdat$fDOM.QSU.mn,
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,90))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

dev.off()


#### sonde error example ####
# read in and wrangle data from previous section

## VDOW 6/5 - 6/13 ##
tempdat = BEGI_EXO.or2[["VDOW"]][BEGI_EXO.or2[["VDOW"]]$datetimeMT >= as.POSIXct("2024-06-05 10:15:01", tz="US/Mountain") & 
                                   BEGI_EXO.or2[["VDOW"]]$datetimeMT <= as.POSIXct("2024-06-13 10:15:00", tz="US/Mountain"), ]


#save plot 
jpeg("plots/error_event.jpg", width = 8, height = 8, units="in", res=1000)
plot.new()

par(mfrow=c(3,1))

plot(tempdat$datetimeMT, tempdat$ODO.mg.L.mn,
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,9.0))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(tempdat$datetimeMT, tempdat$fDOM.QSU.mn,
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(0,90))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")


dev.off()


