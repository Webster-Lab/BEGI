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
library(patchwork)
library(cowplot)
library(grid)
library(ggplotify)
library(stringr)
library(dtwclust)
library(reshape2)
library(zoo)
library(xts)
library(gridExtra)


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
# Add se_fit as a column first, then mutate (ungroup to avoid size mismatch)
new_data_DO <- new_data_DO %>%
  ungroup() %>%
  mutate(
    se_fit = sqrt(diag(X %*% vcov_mat %*% t(X))),
    lower  = pred - 1.96 * se_fit,
    upper  = pred + 1.96 * se_fit
  )

DO_AUC_gwvar_log = 
  ggplot(DO_events_all, aes(x = dtw_DO_event_cv, y = log(DO_AUC), color=wellID))+
  #geom_point(alpha = 0.7, size=5)+
  geom_abline(intercept = 4.056911, slope = 0.617108, color="#440154FF", linewidth = 1.5) + 
  geom_point(alpha = 0.7, size=3,aes(colour=factor(wellID)))+
  geom_ribbon(data = new_data_DO, 
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
        text = element_text(size = 12))+
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
        text = element_text(size = 12))+
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
        text = element_text(size = 12))+
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
        text = element_text(size = 12))+
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
        text = element_text(size = 12))+
  scale_color_viridis(discrete = TRUE, option = "D")
DO_D_gwmean_log

# ER and gw mean #
DO_ER_gwmean_log = 
  ggplot(DO_events_all_filtered, aes(x = dtw_DO_event_mean-1, y = log(posER), color=wellID))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceeding\n Event (2 days)") +
  ylab(bquote("Aerobic Respiration  (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")"))+
  geom_vline(xintercept=0, linetype = 'dashed') +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(log(DO_events_all_filtered$posER)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 12))+
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
        text = element_text(size = 12))+
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
        text = element_text(size = 12))+
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
        text = element_text(size = 12))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_ER_gwvar_log

# DO AUC and gw mean #
resp_AUC_gwmean_log = 
  ggplot(resp_events, aes(x = DO_event_mean-1, y = log(AUC), color=Well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceding\n Event (2 days)") +
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
        text = element_text(size = 12))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_AUC_gwmean_log

# D and gw mean #
resp_D_gwmean_log = 
  ggplot(resp_events_filtered, aes(x = DO_event_mean-1, y = log(D), color=Well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceding\n Event (2 days)") +
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
        text = element_text(size = 12))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_D_gwmean_log

# ER and gw mean #
resp_ER_gwmean_log = 
  ggplot(resp_events_filtered, aes(x = DO_event_mean-1, y = log(posER), color=Well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  xlab( "Mean Depth to Groundwater Preceding\n Event (2 days)") +
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
        text = element_text(size = 12))+
  scale_color_viridis(discrete = TRUE, option = "D")
resp_ER_gwmean_log

#### All model results layout ####
# ── Updated helpers: strip axes AND reduce text size ──────────────────────────
base_text_size <- 19   # adjust this one number to scale all plot text

shrink_text <- function(p) p + theme(
  text        = element_text(size = base_text_size, family = "sans"),
  axis.title  = element_text(size = base_text_size),
  axis.text   = element_text(size = base_text_size - 1, color = "black"),
  legend.text = element_text(size = base_text_size)
)

no_x <- function(p) p + theme(axis.title.x = element_blank(),
                              axis.text.x  = element_blank(),
                              axis.ticks.x = element_blank())

no_y <- function(p) p + theme(axis.title.y = element_blank(),
                              axis.text.y  = element_blank(),
                              axis.ticks.y = element_blank())

no_xy       <- function(p) no_x(no_y(p))
hide_legend <- function(p) p + theme(legend.position = "none")

# ── Wrap y-axis titles on the col-1 plots (only ones keeping y-axis) ──────────
# str_wrap() width controls how many characters before a line break
wrap_y <- function(p, width = 18) {
  lbl <- p$labels$y
  # Only wrap if the label is a plain string; skip bquote/expression labels
  if (is.character(lbl)) {
    p + ylab(str_wrap(lbl, width = width))
  } else {
    p  # return unchanged if it's an expression
  }
}

# ── Clean theme ────────────────────────────────────────────────────────────────
clean_theme <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "black", linewidth = 0.5),
    axis.ticks       = element_line(color = "black", linewidth = 0.3),
    axis.title       = element_blank(),
    axis.text        = element_text(size = base_text_size - 1, color = "black"),
    legend.title     = element_blank(),
    text             = element_text(size = base_text_size)
  )

apply_clean <- function(p) p + clean_theme
# ── Build plots: shrink all, wrap y on col-1 only ─────────────────────────────

# Row 1 - DO event size
r1c1 <- hide_legend(no_x(apply_clean(DO_AUC_gwmean_log)))
r1c2 <- hide_legend(no_xy(apply_clean(DO_AUC_gwvar_log)))
r1c3 <- hide_legend(no_xy(apply_clean(resp_AUC_gwmean_log)))
r1c4 <- hide_legend(no_xy(apply_clean(resp_AUC_gwvar_log)))

# Row 2 - Diffusion
r2c1 <- hide_legend(no_x(apply_clean(DO_D_gwmean_log)))
r2c2 <- hide_legend(no_xy(apply_clean(DO_D_gwvar_log)))
r2c3 <- hide_legend(no_xy(apply_clean(resp_D_gwmean_log)))
r2c4 <- hide_legend(no_xy(apply_clean(resp_D_gwvar_log)))

# Row 3 - Ecosystem respiration (keep x, col4 keeps legend)
r3c1 <- hide_legend(apply_clean(DO_ER_gwmean_log))
r3c2 <- hide_legend(no_y(apply_clean(DO_ER_gwvar_log)))
r3c3 <- hide_legend(no_y(apply_clean(resp_ER_gwmean_log)))
r3c4 <- hide_legend(no_y(apply_clean(resp_ER_gwvar_log))) 
  # + theme(legend.position  = "bottome",
  #       legend.text      = element_text(size = base_text_size),
  #       legend.margin    = margin(t = 20, unit = "pt"))  # increase t to push legend down

# ── Assemble ──────────────────────────────────────────────────────────────────
combined_fig <- (
  r1c1 | r1c2 | r1c3 | r1c4 |
    r2c1 | r2c2 | r2c3 | r2c4 |
    r3c1 | r3c2 | r3c3 | r3c4
) +
  plot_layout(ncol = 4, nrow = 3)

combined_with_margins <- combined_fig +
  plot_annotation(theme = theme(
    plot.margin = margin(t = 50, r = 10, b = 90, l = 80, unit = "pt")
  ))

# ── Overlay labels with cowplot ───────────────────────────────────────────────
final_fig <- ggdraw(combined_with_margins) +
  
  draw_label("All DO Events",
             x = 0.29, y = 0.99, hjust = 0.5, vjust = 1,
             size = 31, fontface = "bold") +
  draw_label("Aerobic Respiration Events",
             x = 0.78, y = 0.99, hjust = 0.5, vjust = 1,
             size = 31, fontface = "bold") +
  
  # column labels
  draw_label("Mean Depth to Groundwater\nPreceding Event (2 days)",
             x = 0.2, y = 0.023, hjust = 0.5, vjust = 0, size = 27.5) +
  draw_label("Depth to Groundwater Coef. of Var.\nPreceding Event (2 days)",
             x = 0.415, y = 0.023, hjust = 0.5, vjust = 0, size = 27.5) +
  draw_label("Mean Depth to Groundwater\nPreceding Event (2 days)",
             x = 0.64, y = 0.023, hjust = 0.5, vjust = 0, size = 27.5) +
  draw_label("Depth to Groundwater Coef. of Var.\nPreceding Event (2 days)",
             x = 0.883, y = 0.023, hjust = 0.5, vjust = 0, size = 27.5) +
  
  # Row 1 label — two lines via two draw_label calls
  draw_label("DO Event Size",
             x = 0.008, y = 0.81, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  draw_label(expression("(g O"[2]~"m"^-3~"15 min"^-1*")"),
             x = 0.019, y = 0.805, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  
  # Row 2 label
  draw_label("Diffusion",
             x = 0.008, y = 0.52, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  draw_label(expression("(g O"[2]~"m"^-3~"15 min"^-1*")"),
             x = 0.019, y = 0.515, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  
  # Row 3 label
  draw_label("Ecosystem Respiration",
             x = 0.008, y = 0.23, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  draw_label(expression("(g O"[2]~"m"^-3~"15 min"^-1*")"),
             x = 0.019, y = 0.235, angle = 90, hjust = 0.5, vjust = 1, size = 27.5)
  
  # # Row labels (rotated, in left margin)
  # draw_label(expression("DO Event Size \n(g O"[2]~"m"^-3~"15 min"^-1*")"),
  #            x = 0.012, y = 0.79, angle = 90, hjust = 0.5, vjust = 1, size = 27) +
  # draw_label(expression("Diffusion \n(g O"[2]~"m"^-3~"15 min"^-1*")"),
  #            x = 0.012, y = 0.50, angle = 90, hjust = 0.5, vjust = 1, size = 27) +
  # draw_label(expression("Ecosystem Respiration \n(g O"[2]~"m"^-3~"15 min"^-1*")"),
  #            x = 0.012, y = 0.21, angle = 90, hjust = 0.5, vjust = 1, size = 27)
  # 
  # draw_line(x = c(0.495, 0.495), y = c(0.04, 0.94),
  #           color = "grey50", linetype = "dashed", size = 0.4)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("biggermanuscript_modelfig.pdf", plot = final_fig,
       width = 28, height = 17, device = "pdf")

ggsave("biggermanuscript_modelfig.png", plot = final_fig,
       width = 28, height = 17, dpi = 300)
#### Boxplots ####
### All DO events ###
# Load data for boxplots
DO_AUC<-readRDS("EXO_compiled/DO_AUC.rds")  
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")
odumER<- readRDS("EXO_compiled/odumER.rds")

# DO event size log scale
DO_AUC_log <- ggplot(data = DO_AUC, mapping = aes(x = Well, y = log(DO))) +
  geom_boxplot(fill = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")) +
  scale_y_continuous(
    breaks = pretty(log(DO_AUC$DO)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme_grey(base_size = 18) +
  labs(y = "DO Event Size (g O2 m-3 15 min-1)")
DO_AUC_log

# ER log scale
odumER_bp<-ggplot(data=odumER,mapping=aes(x=Well, y=ER))+geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+
  theme_grey(base_size = 18) +
  ylab(bquote("Ecosystem Respiration (g" ~ O[2] ~ m^-2 ~ "event"^-1 * ")"))+
  xlab("Well")
print(odumER_bp)

# D log scale 
odumD_bp<-ggplot(data=odumER,mapping=aes(x=Well, y=D))+geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+
  theme_grey(base_size = 18) +
  ylab(bquote("Oxygen Uptake via Diffusion (g" ~ O[2] ~ m^-2 ~ "event"^-1 * ")"))+
  xlab("Well")
print(odumD_bp)


### Aerobic respiration events ###
# DO event size #
resp_DO_AUC_log <- ggplot(data = resp_events, mapping = aes(x = Well, y = log(AUC))) +
  geom_boxplot(fill = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")) +
  scale_y_continuous(
    breaks = pretty(log(resp_events$AUC)),
    labels = function(x) round(exp(x), 1)
  ) +
  theme_grey(base_size = 18) +
  labs(y = "DO Event Size (g O2 m-3 15 min-1)")
resp_DO_AUC_log

# ER #
resp_odumER_bp<-ggplot(data=resp_events,mapping=aes(x=Well, y=ER))+
  geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+
  theme_grey(base_size = 18) +
  ylab(bquote("Ecosystem Respiration (g" ~ O[2] ~ m^-2 ~ "event"^-1 * ")"))+
  xlab("Well")
print(resp_odumER_bp)

# D #
resp_odumD_bp<-ggplot(data=resp_events,mapping=aes(x=Well, y=D))+
  geom_boxplot(fill=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))+
  theme_grey(base_size = 18) +
  ylab(bquote("Oxygen Uptake via Diffusion (g" ~ O[2] ~ m^-2 ~ "event"^-1 * ")"))+
  xlab("Well")
print(resp_odumD_bp)



#### Plot all boxplots together ####
base_text_size <- 19   # adjust this one number to scale all plot text

shrink_text <- function(p) p + theme(
  text        = element_text(size = base_text_size, family = "sans"),
  axis.title  = element_text(size = base_text_size),
  axis.text   = element_text(size = base_text_size - 1, color = "black"),
  legend.text = element_text(size = base_text_size)
)

no_x <- function(p) p + theme(axis.title.x = element_blank(),
                              axis.text.x  = element_blank(),
                              axis.ticks.x = element_blank())

no_y <- function(p) p + theme(axis.title.y = element_blank(),
                              axis.text.y  = element_blank(),
                              axis.ticks.y = element_blank())

no_xy       <- function(p) no_x(no_y(p))
hide_legend <- function(p) p + theme(legend.position = "none")

# ── Wrap y-axis titles on the col-1 plots (only ones keeping y-axis) ──────────
# str_wrap() width controls how many characters before a line break
wrap_y <- function(p, width = 18) {
  lbl <- p$labels$y
  # Only wrap if the label is a plain string; skip bquote/expression labels
  if (is.character(lbl)) {
    p + ylab(str_wrap(lbl, width = width))
  } else {
    p  # return unchanged if it's an expression
  }
}

# ── Clean theme ────────────────────────────────────────────────────────────────
clean_theme <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "black", linewidth = 0.5),
    axis.ticks       = element_line(color = "black", linewidth = 0.3),
    axis.title       = element_blank(),
    axis.text        = element_text(size = base_text_size - 1, color = "black"),
    legend.title     = element_blank(),
    text             = element_text(size = base_text_size)
  )

apply_clean <- function(p) p + clean_theme

# ── Build plots: shrink all, wrap y on col-1 only ─────────────────────────────

# Row 1 - DO event size
r1c1 <- hide_legend(no_x(apply_clean(DO_AUC_log)))
r1c2 <- hide_legend(no_xy(apply_clean(resp_DO_AUC_log)))

# Row 2 - Diffusion
r2c1 <- hide_legend(no_x(apply_clean(odumD_bp)))
r2c2 <- hide_legend(no_xy(apply_clean(resp_odumD_bp)))

# Row 3 - Ecosystem respiration (keep x, col4 keeps legend)
r3c1 <- hide_legend(apply_clean(odumER_bp))
r3c2 <- hide_legend(no_y(apply_clean(resp_odumER_bp))) 
  # +theme(legend.position = "bottom",
  #       legend.text     = element_text(size = base_text_size))

# ── Assemble ──────────────────────────────────────────────────────────────────
combined_fig <- (
  r1c1 | r1c2 |
    r2c1 | r2c2 |
    r3c1 | r3c2
) +
  plot_layout(ncol = 2, nrow = 3)

combined_with_margins <- combined_fig +
  plot_annotation(theme = theme(
    plot.margin = margin(t = 40, r = 10, b = 10, l = 55, unit = "pt")
  ))

# ── Overlay labels with cowplot ───────────────────────────────────────────────
final_fig <- ggdraw(combined_with_margins) +
  
  draw_label("All DO Events",
             x = 0.3, y = 0.995, hjust = 0.5, vjust = 1,
             size = 31, fontface = "bold") +
  draw_label("Aerobic Respiration Events",
             x = 0.75, y = 0.995, hjust = 0.5, vjust = 1,
             size = 31, fontface = "bold") +


  # Row 1 label — two lines via two draw_label calls
  draw_label("DO Event Size",
             x = 0.007, y = 0.81, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  draw_label(expression("(g O"[2]~"m"^-3~"15 min"^-1*")"),
             x = 0.02, y = 0.805, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  
  # Row 2 label
  draw_label("Diffusion",
             x = 0.007, y = 0.52, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  draw_label(expression("(g O"[2]~"m"^-3~"15 min"^-1*")"),
             x = 0.02, y = 0.515, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  
  # Row 3 label
  draw_label("Ecosystem Respiration",
             x = 0.007, y = 0.23, angle = 90, hjust = 0.5, vjust = 1, size = 27.5) +
  draw_label(expression("(g O"[2]~"m"^-3~"15 min"^-1*")"),
             x = 0.02, y = 0.235, angle = 90, hjust = 0.5, vjust = 1, size = 27.5)

# draw_line(x = c(0.495, 0.495), y = c(0.04, 0.94),
#           color = "grey50", linetype = "dashed", size = 0.4)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("manuscript_bpfig.pdf", plot = final_fig,
       width = 24, height = 14, device = "pdf")

ggsave("manuscript_bpfig.png", plot = final_fig,
       width = 24, height = 14, dpi = 300)
#### groundwater depth cluster plot ####
cluster_DTW_data_k3 <- read_csv("DTW_compiled/DTW_clusters_k3_smoothed_norm.csv")

#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#Match event_time with Eventdate for each well
#Turns out the events in the csv are in order for each well's Eventdate list :D
cluster_DTW_data_k3$well_id <- rep(c("SLOC","SLOW","VDOW","VDOS"),
                                   times = c(length(BEGI_events[["Eventdate"]][["SLOC_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["SLOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOS_dates"]])))

#count of what clusters occurred in each well
cluster_by_well <- data.frame(SLOC = c(sum(cluster_DTW_data_k3$cluster == 1 & cluster_DTW_data_k3$well_id == 'SLOC'),
                                       sum(cluster_DTW_data_k3$cluster == 2 & cluster_DTW_data_k3$well_id == 'SLOC'),
                                       sum(cluster_DTW_data_k3$cluster == 3 & cluster_DTW_data_k3$well_id == 'SLOC')),
                              SLOW = c(sum(cluster_DTW_data_k3$cluster == 1 & cluster_DTW_data_k3$well_id == 'SLOW'),
                                       sum(cluster_DTW_data_k3$cluster == 2 & cluster_DTW_data_k3$well_id == 'SLOW'),
                                       sum(cluster_DTW_data_k3$cluster == 3 & cluster_DTW_data_k3$well_id == 'SLOW')),
                              VDOW = c(sum(cluster_DTW_data_k3$cluster == 1 & cluster_DTW_data_k3$well_id == 'VDOW'),
                                       sum(cluster_DTW_data_k3$cluster == 2 & cluster_DTW_data_k3$well_id == 'VDOW'),
                                       sum(cluster_DTW_data_k3$cluster == 3 & cluster_DTW_data_k3$well_id == 'VDOW')),
                              VDOS = c(sum(cluster_DTW_data_k3$cluster == 1 & cluster_DTW_data_k3$well_id == 'VDOS'),
                                       sum(cluster_DTW_data_k3$cluster == 2 & cluster_DTW_data_k3$well_id == 'VDOS'),
                                       sum(cluster_DTW_data_k3$cluster == 3 & cluster_DTW_data_k3$well_id == 'VDOS')))

# plot all curves of each cluster
cluster_DTW_data_k3_long = cluster_DTW_data_k3 %>% pivot_longer(cols='t12':'t192',
                                                                names_to = "timestep",
                                                                values_to = "DTW_m")
cluster_DTW_data_k3_long$timestep = as.numeric(gsub('t', '', cluster_DTW_data_k3_long$timestep))


# Rename clusters for meaningful facet labels
cluster_DTW_data_k3_long$cluster_label <- factor(
  cluster_DTW_data_k3_long$cluster,
  labels = c("Cluster 1", "Cluster 2", "Cluster 3")
)

well_clusters <- ggplot(
  cluster_DTW_data_k3_long,
  aes(x = timestep, y = DTW_m, group = ename, color = well_id)
) +
  geom_line(linewidth = 0.6, alpha = 0.75) +
  facet_wrap(~cluster_label) +
  scale_color_manual(
    values = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
    name = "Well ID"
  ) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 1)) +
  labs(
    x = "Time (min)",
    y = "Normalized Depth (m)"
  ) +
  theme_classic(base_size = 11) +
  theme(
    strip.background  = element_rect(fill = "grey92", color = "black", linewidth = 0.4),
    strip.text        = element_text(size = 11, face = "bold"),
    axis.line         = element_line(linewidth = 0.4, color = "black"),
    axis.ticks        = element_line(linewidth = 0.3, color = "black"),
    axis.ticks.length = unit(2, "pt"),
    axis.title        = element_text(size = 11),
    axis.text         = element_text(size = 9, color = "black"),
    legend.title      = element_text(size = 10, face = "bold"),
    legend.text       = element_text(size = 9),
    legend.key.size   = unit(12, "pt"),
    legend.key        = element_rect(fill = NA),
    legend.background = element_rect(fill = NA),
    panel.spacing     = unit(8, "pt"),
    plot.margin       = margin(6, 6, 6, 6, "pt")
  )

ggsave("well_clusters.pdf",  well_clusters, width = 6.5, height = 3.5, units = "in")
ggsave("well_clusters.png",  well_clusters, width = 6.5, height = 3.5, units = "in", dpi = 300)
#### Plot DO time series ####
