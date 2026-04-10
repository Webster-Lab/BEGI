#### read me ####
# the purpose of this script is to create pub-ready figures for BEGI manuscript

#### libraries ####
library(tidyverse)
library(ggplot2)
library(DescTools)
library(nlme)
library(lme4)
library(lmerTest)
library(viridis)

#### Load data for aerobic respiration events ####
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

#### Combine datasets for event dtw model (aerobic respiration events) ####

## ER and D ##
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

# clean up environment
rm(odumER_subset); rm(dtw_events); rm(AUC_df); rm(AUC_results_all)

#### Load data for all DO events ####
# area under the curve method of quantifying the relative size of respiration events from %DO timeseries
DO_AUC = readRDS("EXO_compiled/DO_AUC.rds")
DO_AUC$siteID = substr(DO_AUC$Well, start = 1, stop = 3)
names(DO_AUC)[names(DO_AUC) == 'Well'] <- 'wellID'
DO_AUC[sapply(DO_AUC, is.character)] <- lapply(DO_AUC[sapply(DO_AUC, is.character)],  as.factor)
names(DO_AUC)[names(DO_AUC) == 'Event'] <- 'eventID'
names(DO_AUC)[names(DO_AUC) == 'DO'] <- 'DO_AUC'

# Odum's method of quantifying the size of respiration events from %DO timeseries
odumER = readRDS("EXO_compiled/odumER.rds")
odumER$siteID = substr(odumER$Well, start = 1, stop = 3)
names(odumER)[names(odumER) == 'Well'] <- 'wellID'
odumER[sapply(odumER, is.character)] <- lapply(odumER[sapply(odumER, is.character)],  as.factor)
names(odumER)[names(odumER) == 'Event'] <- 'eventID'

# mean and variance summary of depth to water (DTW) for each event
dtw_events = readRDS("DTW_compiled/DO_mv.rds")
#dtw_events = read.csv("DTW_compiled/DO_mv_2days.csv")
names(dtw_events)[names(dtw_events) == 'WellID'] <- 'wellID'
names(dtw_events)[names(dtw_events) == 'Eventdates'] <- 'Eventdate'
dtw_events$Eventdate = as.POSIXct(paste(substr(dtw_events$Eventdate, start=1,stop=10),
                                        substr(dtw_events$Eventdate, start=12,stop=19), sep=" "),
                                  tz="US/Mountain")
dtw_events$siteID = substr(dtw_events$wellID, start = 1, stop = 3)
dtw_events[sapply(dtw_events, is.character)] <- lapply(dtw_events[sapply(dtw_events, is.character)],  as.factor)

# join data
DO_events_all = full_join(DO_AUC, odumER, by=c("siteID","wellID","Eventdate"))
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
rm(DO_AUC); rm(dtw_events); rm(odumER)

#### groundwater models ####
# respiration subset model results #
# AUC
m.1 = nlme::lme(log(AUC) ~ DO_event_mean, 
                data=resp_events, random=~1|siteID/Well, method="ML")
m.2 = nlme::lme(log(AUC) ~ DO_event_cv, 
                data=resp_events, random=~1|siteID/Well, method="ML")
# ER
#need to multiply ER by -1 to be able to log transform
resp_events$posER <- resp_events$ER * -1
#filter out 0 values
resp_events_filtered <- resp_events[resp_events$posER > 0, ]

m.3 = nlme::lme(log(posER) ~ DO_event_mean, 
                data=resp_events_filtered, random=~1|siteID/Well, method="ML")
m.4 = nlme::lme(log(posER) ~ DO_event_cv, 
                data=resp_events_filtered, random=~1|siteID/Well, method="ML")

# D
#filter out 0 values
resp_events_filtered <- resp_events[resp_events$D > 0, ]

m.5 = nlme::lme(log(D) ~ DO_event_mean, 
                data=resp_events_filtered, random=~1|siteID/Well, method="ML")
m.6 = nlme::lme(log(D) ~ DO_event_cv, 
                data=resp_events_filtered, random=~1|siteID/Well, method="ML")

# all DO events #
# DO event size
m.7 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_mean, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")
m.8 = nlme::lme(log(DO_AUC) ~ dtw_DO_event_cv, 
                data=DO_events_all, random=~1|siteID/wellID, method="ML")

# ER
#need to multiply ER by -1 to be able to log transform
DO_events_all$posER <- DO_events_all$ER * -1
#filter out 0 values
DO_events_all_filtered <- DO_events_all[DO_events_all$posER > 0, ]

m.9 = nlme::lme(log(posER) ~ dtw_DO_event_mean, 
                data=DO_events_all_filtered, random=~1|siteID/wellID, method="ML")
m.10 = nlme::lme(log(posER) ~ dtw_DO_event_cv, 
                data=DO_events_all_filtered, random=~1|siteID/wellID, method="ML")


# D
#filter out 0 values
DO_events_all_filtered <- DO_events_all[DO_events_all$D > 0, ]

m.11 = nlme::lme(log(D) ~ dtw_DO_event_mean, 
                data=DO_events_all_filtered, random=~1|siteID/wellID, method="ML")
m.12 = nlme::lme(log(D) ~ dtw_DO_event_cv, 
                data=DO_events_all_filtered, random=~1|siteID/wellID, method="ML")

#### Plot all DO events model results ####

# DO AUC and gw var #
#create new data frame for prediction
new_data_DO <- DO_events_all %>% 
  select(dtw_DO_event_cv, wellID) %>% 
  distinct()

#add predictions
new_data_DO$pred <- predict(m.8, newdata = new_data_DO, level = 0)

#standard erros
X <- model.matrix(~ dtw_DO_event_cv, new_data_DO)
betas <- fixef(m.8)
vcov_mat <- vcov(m.8)
se_fit <- sqrt(diag(X %*% vcov_mat %*% t(X)))

#95% CI
new_data_DO <- new_data_DO %>%
  mutate(
    lower = pred - 1.96 * se_fit,
    upper = pred + 1.96 * se_fit
  )

### FIX HERE: PREDICTED VALUES DON'T LINE UP TO CREATE CONFIDENCE INTERVALS FOR 
### DO EVENTS AND GW VAR

DO_AUC_gwvar_log = 
  ggplot(DO_events_all, aes(x = dtw_DO_event_cv, y = log(DO_AUC), color=wellID))+
  #geom_point(alpha = 0.7, size=5)+
  geom_abline(intercept = 4.056911, slope = 0.617108, color="#440154FF", size = 1.5) + 
  geom_point(alpha = 0.7, size=3,aes(colour=factor(wellID)))+
  geom_ribbon(data = new_data, 
              aes(x = dtw_DO_event_cv, ymin = lower, ymax = upper),
              inherit.aes = FALSE, fill = "#440154FF", alpha = 0.3) +
  #geom_smooth(method=lm, colour="#440154FF", se=T, size=1.5)+
  #geom_smooth(method=lm, aes(colour=factor(wellIDID)), se=F, size=0.5)
  labs(x = str_wrap("Depth to Groundwater Coef. of Variation Preceding Event (2 days)", width=35),
       y = str_wrap("Dissolved Oxygen Consumption Event Size (g O2 m-3 15 min-1)", width=40))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_AUC_gwvar_log

# D and gw var #
DO_D_gwvar_log = 
  ggplot(DO_events_all_filtered, aes(x = dtw_DO_event_cv, y = log(D), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Depth to Groundwater Coef. of Variation \nPreceding Event (2 days)") +
  ylab(bquote("Diffusion (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(DO_events_all_filtered$D)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_D_gwvar_log

# ER and gw var #
DO_ER_gwvar_log = 
  ggplot(DO_events_all_filtered, aes(x = dtw_DO_event_cv, y = log(posER), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Depth to Groundwater Coef. of Variation \nPreceding Event (2 days)") +
  ylab(bquote("Aerobic Respiration (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(DO_events_all_filtered$posER)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_ER_gwvar_log

# DO AUC and gw mean #
DO_AUC_gwmean_log = 
  ggplot(DO_events_all, aes(x = dtw_DO_event_mean-1, y = log(DO_AUC), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceeding\n Event (2 days)") +
  ylab(bquote("Dissolved Oxygen Event Size (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(DO_events_all$DO_AUC)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_AUC_gwmean_log

# D and gw mean #
DO_D_gwmean_log = 
  ggplot(DO_events_all_filtered, aes(x = dtw_DO_event_mean-1, y = log(D), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceeding\n Event (2 days)") +
  ylab(bquote("Diffusion (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(DO_events_all_filtered$D)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_D_gwmean_log

# ER and gw mean #
DO_ER_gwmean_log = 
  ggplot(DO_events_all_filtered, aes(x = dtw_DO_event_mean-1, y = log(posER), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceeding\n Event (2 days)") +
  ylab(bquote("Aerobic Respiration (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(DO_events_all_filtered$posER)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_ER_gwmean_log


#### Plot aerobic respiration model results ####

resp_AUC_gwvar_log = 
  ggplot(resp_events, aes(x = DO_event_cv, y = log(AUC), color=Well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Depth to Groundwater Coef. of Variation \nPreceding Event (2 days)") +
  ylab(bquote("DO Event Size (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(resp_events$AUC)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_AUC_gwvar_log

# D and gw var #
resp_D_gwvar_log = 
  ggplot(resp_events_filtered, aes(x = DO_event_cv, y = log(D), color=Well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Depth to Groundwater Coef. of Variation \nPreceding Event (2 days)") +
  ylab(bquote("Diffusion (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(resp_events_filtered$D)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_D_gwvar_log

# ER and gw var #
resp_ER_gwvar_log = 
  ggplot(resp_events_filtered, aes(x = DO_event_cv, y = log(posER), color=Well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Depth to Groundwater Coef. of Variation \nPreceding Event (2 days)") +
  ylab(bquote("Aerobic Respiration (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(resp_events_filtered$posER)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_ER_gwvar_log

# DO AUC and gw mean #
resp_AUC_gwmean_log = 
  ggplot(resp_events, aes(x = DO_event_mean-1, y = log(AUC), color=Well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceeding\n Event (2 days)") +
  ylab(bquote("Dissolved Oxygen Event Size (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(resp_events$AUC)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_AUC_gwmean_log

# D and gw mean #
resp_D_gwmean_log = 
  ggplot(resp_events_filtered, aes(x = DO_event_mean-1, y = log(D), color=Well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceeding\n Event (2 days)") +
  ylab(bquote("Diffusion (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(resp_events_filtered$D)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_D_gwmean_log

# ER and gw mean #
resp_ER_gwmean_log = 
  ggplot(resp_events_filtered, aes(x = DO_event_mean-1, y = log(posER), color=Well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceeding\n Event (2 days)") +
  ylab(bquote("Aerobic Respiration (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(resp_events_filtered$posER)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 21))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_ER_gwmean_log

#### Boxplots ####



#### Plot DO time series ####
