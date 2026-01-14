#### read me ####
#The purpose of this script is to align delineated DO and fDOM events to test their correlation
#DO events and fDOM consumption were independently delineated but share some overlap.
#This script will index each DO event to align each one with its corresponding fDOM consumption
# The break down of DO events is   SLOC (18), SLOW (9), VDOW (15), VDOS (10)
# The break down of fDOM events is SLOC (28), SLOW (14), VDOW (28), VDOS (21)


#### Libraries ####
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)
library(dplyr)
library(ggplot2)
library(lme4)
library(DHARMa)
library(MuMIn)
library(forecast)
library(nlme)
library(visreg)
library(viridis)

#### Import all events ####
#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#### Create event data table ####
# DO
summarize_DO_event <- function(df,
                               time_col = "datetimeMT",
                               DO_col   = "ODO.mg.L.mn") {
  
  # ensure ordered by time
  df <- df[order(df[[time_col]]), ]
  
  DO_auc <- AUC(
    x = df[[time_col]],
    y = df[[DO_col]],
    method = "trapezoid",
    na.rm = FALSE
  )
  
  data.frame(
    t_start = min(df[[time_col]]),
    t_end   = max(df[[time_col]]),
    DO_min  = min(df[[DO_col]], na.rm = TRUE),
    DO_AUC  = DO_auc
  )
}

# fDOM
# excludes fDOM events that increase (transport)
summarize_fDOM_event <- function(df,
                                 time_col = "datetimeMT",
                                 fDOM_col = "fDOM.QSU.mn") {
  
  stopifnot(time_col %in% names(df))
  stopifnot(fDOM_col %in% names(df))
  
  df <- df[order(df[[time_col]]), ]
  
  valid <- !is.na(df[[fDOM_col]])
  if (sum(valid) == 0) {
    return(NULL)  # safer than stop() for batch processing
  }
  
  fDOM_start <- df[[fDOM_col]][which(valid)[1]]
  fDOM_end   <- df[[fDOM_col]][tail(which(valid), 1)]
  
  # ---- EXCLUDE EVENTS WITH NET INCREASE ----
  if (fDOM_end > fDOM_start) {
    return(NULL)
  }
  
  fDOM_max <- max(df[[fDOM_col]], na.rm = TRUE)
  fDOM_min <- min(df[[fDOM_col]], na.rm = TRUE)
  
  data.frame(
    t_start        = min(df[[time_col]]),
    t_end          = max(df[[time_col]]),
    fDOM_start     = fDOM_start,
    fDOM_end       = fDOM_end,
    fDOM_max       = fDOM_max,
    fDOM_min       = fDOM_min,
    fDOM_magnitude = fDOM_max - fDOM_min,
    fDOM_net_change = fDOM_end - fDOM_start
  )
}



# DO events
DO_event_tables <- list()

for (w in names(BEGI_events$DO_events)) {
  cat("\n=== Processing DO events for", w, "===\n")
  DO_site_list <- BEGI_events$DO_events[[w]]
  cat("  Number of events:", length(DO_site_list), "\n")
  DO_rows <- vector("list", length(DO_site_list))
  
  for (i in seq_along(DO_site_list)) {
    df_event <- DO_site_list[[i]]
    s <- summarize_DO_event(df_event)
    s$event_id <- names(DO_site_list)[i]
    
    # derive well name without suffix
    s$well <- sub("_DO$", "", w)
    DO_rows[[i]] <- s
  }
  
  DO_event_tables[[sub("_DO$", "", w)]] <- do.call(rbind, DO_rows)
}

#fDOM
fDOM_event_tables <- list()

for (w in names(BEGI_events$fDOM_events)) {
  
  cat("\n=== Processing fDOM events for", w, "===\n")
  
  fDOM_site_list <- BEGI_events$fDOM_events[[w]]
  fDOM_interest  <- vector("list", length(fDOM_site_list))
  
  for (i in seq_along(fDOM_site_list)) {
    
    s <- summarize_fDOM_event(
      fDOM_site_list[[i]],
      fDOM_col = "fDOM.QSU.mn"
    )
    
    if (!is.null(s)) {
      s$event_id <- names(fDOM_site_list)[i]
      s$well     <- sub("_fDOM$", "", w)
      fDOM_interest[[i]] <- s
    }
  }
  
  # drop NULLs
  fDOM_interest <- Filter(Negate(is.null), fDOM_interest)
  
  fDOM_event_tables[[sub("_fDOM$", "", w)]] <-
    do.call(rbind, fDOM_interest)
}


#### Function to match fDOM events to DO ####
# Function
match_DO_fDOM_events <- function(DO_events, fDOM_events, lag_hours = 72,
                                 DO_metric = "DO_AUC",
                                 fDOM_metric = "fDOM_magnitude") {
  
  # Safety checks
  if (!fDOM_metric %in% names(fDOM_events)) {
    stop(paste("Missing column in fDOM_events:", fDOM_metric))
  }
  if (!DO_metric %in% names(DO_events)) {
    stop(paste("Missing column in DO_events:", DO_metric))
  }
  
  matched_list <- vector("list", nrow(fDOM_events))
  
  for (i in seq_len(nrow(fDOM_events))) {
    
    fdom_row <- fDOM_events[i, ]
    
    # Lag relative to DO *start*
    lag_diff <- as.numeric(
      difftime(fdom_row$t_start, DO_events$t_start, units = "hours")
    )
    
    # Valid DO candidates
    candidates <- DO_events[lag_diff >= 0 & lag_diff <= lag_hours, ]
    
    if (nrow(candidates) == 0) {
      
      matched_list[[i]] <- data.frame(
        fDOM_event_id  = fdom_row$event_id,
        fDOM_start     = fdom_row$t_start,
        fDOM_end       = fdom_row$t_end,
        fDOM_magnitude = fdom_row[[fDOM_metric]],
        DO_event_id    = NA,
        DO_start       = as.POSIXct(NA),
        DO_end         = as.POSIXct(NA),
        DO_AUC         = NA,
        lag_hours      = NA
      )
      
    } else {
      
      # Closest prior DO event by start time
      idx <- which.min(lag_diff[lag_diff >= 0 & lag_diff <= lag_hours])
      closest <- candidates[idx, , drop = FALSE]
      
      matched_list[[i]] <- data.frame(
        fDOM_event_id  = fdom_row$event_id,
        fDOM_start     = fdom_row$t_start,
        fDOM_end       = fdom_row$t_end,
        fDOM_magnitude = fdom_row[[fDOM_metric]],
        DO_event_id    = closest$event_id,
        DO_start       = closest$t_start,
        DO_end         = closest$t_end,
        DO_AUC         = closest[[DO_metric]],
        lag_hours      = as.numeric(
          difftime(fdom_row$t_start, closest$t_start, units = "hours")
        )
      )
    }
  }
  
  do.call(rbind, matched_list)
}

#### Match events across wells ####
matched_tables <- list()

for (w in names(DO_event_tables)) {
  cat("Matching events for well:", w, "\n")
  
  matched_tables[[w]] <- match_DO_fDOM_events(
    DO_events   = DO_event_tables[[w]],
    fDOM_events = fDOM_event_tables[[w]],
    lag_hours   = 72,  
    DO_metric   = "DO_AUC",
    fDOM_metric = "fDOM_magnitude"
  )
}

#### Prelim plot to check correlation ####
ggplot(matched_tables$SLOC, aes(x = DO_AUC, y = fDOM_magnitude)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(
    title = "SLOC DO vs fDOM events",
    x = "DO event AUC",
    y = "fDOM event magnitude"
  )
##############################################
#### Test if DO events trigger fDOM events ####
# Logistic regression
# Response: whether an fDOM event occurred within lag time L
# Predictor: DO magnitude
# OR contingency/permutation test: count DO events followed by fDOM events within L
# Compare to random event timings using permutation to answer if the co-occurrence is greater than expected by chance


##############################################
#### Build DO event table from matched table ####
build_DO_trigger_table <- function(DO_events, fDOM_events, lag_hours = 36, well_name = NULL) {
  
  # DO-centric table
  out <- vector("list", nrow(DO_events))
  
  for (i in seq_len(nrow(DO_events))) {
    DO_row <- DO_events[i, ]
    
    # Compute lag from DO start to fDOM starts
    lag_diff <- as.numeric(difftime(fDOM_events$t_start, DO_row$t_start, units = "hours"))
    
    # fDOM events that start after DO start but within lag_hours
    hits <- lag_diff >= 0 & lag_diff <= lag_hours
    
    out[[i]] <- data.frame(
      DO_event_id    = DO_row$event_id,
      DO_start       = DO_row$t_start,
      DO_AUC         = DO_row$DO_AUC,
      triggered_fDOM = as.integer(any(hits)),
      min_lag_hours  = if(any(hits)) min(lag_diff[hits]) else NA,
      well           = well_name
    )
  }
  do.call(rbind, out)
}

# Apply across wells
DO_trigger_all <- lapply(names(DO_event_tables), function(w) {
  build_DO_trigger_table(
    DO_events   = DO_event_tables[[w]],
    fDOM_events = fDOM_event_tables[[w]],
    lag_hours   = 36,
    well_name   = w
  )
}) %>% bind_rows()

# Inspect
head(DO_trigger_all)

#### Fit mixed-effects logistic regression ####
m.null <- glmer(triggered_fDOM ~ 1 + (1 | well),
                data = DO_trigger_all,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa"))

m.1 <- glmer(triggered_fDOM ~ DO_AUC + (1 | well),
             data = DO_trigger_all,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"))

m.2 <- glmer(triggered_fDOM ~ DO_AUC + (DO_AUC | well),
             data = DO_trigger_all,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"))
summary(m.null)
summary(m.1)
summary(m.2)

#### Evaluate model assumptions ####
# Compare AICc
AICc(m.null, m.1, m.2)
# null model and m.1 seem to perform best

# Raw residuals (Pearson residuals)
res <- residuals(m.1, type = "pearson")
# Homogeneity (residuals vs fitted)
plot(fitted(m.1), res,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)
# Normality
qqnorm(res)
qqline(res)
hist(res)

sim_res1 <- simulateResiduals(m.1)
plot(sim_res1)


# Raw residuals (Pearson residuals)
res <- residuals(m.2, type = "pearson")
# Homogeneity (residuals vs fitted)
plot(fitted(m.2), res,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)
# Normality
qqnorm(res)
qqline(res)
hist(res)

sim_res2 <- simulateResiduals(m.2)
plot(sim_res2) 

#### Odds ratios and CI ####
OR <- exp(fixef(m.1))
CI <- exp(confint(m.1, parm = "beta_"))
cat("Odds ratios:\n")
print(OR)
# suggests that DO events and fDOM events are independent....
cat("95% CI:\n")
print(CI)

OR <- exp(fixef(m.2))
CI <- exp(confint(m.2, parm = "beta_"))
cat("Odds ratios:\n")
print(OR)
# suggests that DO events and fDOM events are independent....
cat("95% CI:\n")
print(CI)

#### Plot ####
ggplot(DO_trigger_all, aes(x = DO_AUC, y = triggered_fDOM)) +
  geom_jitter(height = 0.05, width = 0) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  theme_bw() +
  labs(x = "Dissolved Oxygen Event Size (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")", y = "Probability of fDOM event")

####################################################
#### Test if DO magnitude drives fDOM magnitude ####
# core regression
# log(fDOM_metric) ~ log(DO_metric) + lag
# Spearman rank correlation or linear regression with bootstrapped CI

####################################################
#### Build paired table ####
DO_fDOM_paired <- lapply(names(matched_tables), function(w) {
  matched_tables[[w]] %>%
    filter(!is.na(fDOM_event_id)) %>%  # only paired events
    mutate(well = w) %>%               # add the well name as a column
    select(
      DO_event_id, DO_start, DO_AUC,
      fDOM_event_id, fDOM_start, fDOM_magnitude,
      well
    )
}) %>% bind_rows()

# Remove rows with NA
DO_fDOM_paired_clean <- DO_fDOM_paired %>%
  filter(!is.na(fDOM_magnitude) & !is.na(DO_AUC))

#### Fit linear mixed-effects model ####
m.null <- nlme::lme(fDOM_magnitude ~ 1,
              random = ~ 1 | well,
              data = DO_fDOM_paired_clean,
              method = "REML")
m.1 <- nlme::lme(fDOM_magnitude ~ DO_AUC,
                 random = ~ 1 | well,
                 data = DO_fDOM_paired_clean,
                 method = "REML")

summary(m.1)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# null model is better...


#### Evaluate model assumptions ####
## EVALUATE MODEL ASSUMPTIONS
#1) Homogeneity of Variances (of best model)
#This assumption is the most important
#You do not want to see strong decrease or increase of residuals vs. predicteds
plot(m.1) #looks bad-ish - some outliers, but not a consistent pattern

#2) Normality of Residuals (of best model)
#If these look close, it's probably NOT worth trying data transformation
#Because you complicate interpretability
qqnorm(residuals(m.1))
qqline(residuals(m.1))
hist(residuals(m.1))
# not great. Not normal.

# Temporal autocorrelation
Acf(res)

#### Fit linear mixed-effects model (LOG TRANSFORMED) ####
DO_fDOM_paired_clean$log_fDOM <- log(DO_fDOM_paired_clean$fDOM_magnitude)

m.null <- nlme::lme(log_fDOM ~ 1,
              random = ~ 1 | well,
              data = DO_fDOM_paired_clean,
              method = "REML")
m.1 <- nlme::lme(log_fDOM ~ DO_AUC,
           random = ~ 1 | well,
           data = DO_fDOM_paired_clean,
           method = "REML")

summary(m.1)

# Model Selection Procedures
# compare the  models: lowest AICc wins; difference <2 is a tie
AICc(m.null, m.1)
# null model is still better

#### Evaluate model assumptions ####
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
# Normal

# Temporal autocorrelation
Acf(residuals(m.1))

#### Confidence intervals and model summaries ####
# GET P-VALUES AND COEFFICIENT ESTIMATES WITH 95% CONFIDENCE INTERVALS
#F-tests 
anova.lme(m.1,type = "marginal", adjustSigma = F)

#95% CI gives you LOWER and UPPER bound around the MEAN ESTIMATE for each parameter
#linear, quadratic terms with 95% Confidence Intervals
m.1_conf_int <- intervals(m.1, level = 0.95, which = "fixed") #ns

summary(m.1)
ranef(m.1)

#### Plot for predicted model ####
ggplot(DO_fDOM_paired, aes(x = DO_AUC, y = fDOM_magnitude)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~well) +
  theme_bw() +
  labs(x = "Dissolved Oxygen Event Size (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")",
       y = "fDOM magnitude")

well_colors <- c("#440154FF","#31688EFF","#35B779FF","#FDE725FF")
DO_fDOM_paired_clean$well <- factor(DO_fDOM_paired_clean$well)
pred <- predict(m.1, level = 0)  # population-level prediction
DO_fDOM_paired_clean$pred <- pred

ggplot(DO_fDOM_paired_clean, aes(x = DO_AUC, y = log_fDOM, color = well)) +
  geom_point(size = 2) +
  geom_line(aes(y = pred)) +
  scale_color_manual(values = well_colors) +
  theme_bw() +
  labs(x = "Dissolved Oxygen Event Size (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")",
       y = "fDOM magnitude")

fdom_corr = 
  ggplot(DO_fDOM_paired_clean, aes(x = DO_AUC, y = log_fDOM, color = well))+
  geom_point(alpha = 0.7, size=5)+                                      
  #geom_smooth(method = "lm", fill=NA) +
  labs(x = "Dissolved Oxygen Event Size (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")", 
       y = str_wrap("fDOM magnitude", width=25))+
  geom_abline(intercept = 2.3261223-0.2297795, slope = 0.0005883, color="#440154FF", size = 1.5) + 
  geom_abline(intercept = 2.3261223-0.6132270, slope = 0.0005883, color="#31688EFF", size = 1.5) +
  geom_abline(intercept = 2.3261223+0.1065753, slope = 0.0005883, color="#35B779FF", size = 1.5) +
  geom_abline(intercept = 2.3261223+0.7364312, slope = 0.0005883, color="#FDE725FF", size = 1.5) +
  theme_bw()+
  scale_y_continuous(
    breaks = pretty(DO_fDOM_paired_clean$log_fDOM),
    labels = function(x) round(exp(x), 1)
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis(discrete = TRUE, option = "D")
fdom_corr

#### final figures ####
# Scatter plot
#   DO magnitude vs fDOM magnitude
#   Colored by well, size = lag

DO_fDOM_paired <- bind_rows(matched_tables, .id = "well") %>%
  filter(!is.na(fDOM_event_id)) %>%
  mutate(
    well = factor(well),
    log_fDOM = log(fDOM_magnitude)
  )

ggplot(DO_fDOM_paired,
       aes(x = DO_AUC,
           y = log_fDOM,
           color = well,
           size = lag_hours)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(name = "Lag (hours)", range = c(2, 6)) +
  scale_color_manual(values = well_colors) +
  theme_bw() +
  labs(
    x = "Dissolved Oxygen Event Size (g" ~ O[2] ~ m^-3 ~ "15 min"^-1 * ")",
    y = "fDOM magnitude (log QSU)",
    color = "Well"
   ) #+
  # geom_abline(intercept = fixef(m.1)[1],
  #             slope     = fixef(m.1)[2],
  #             linetype  = "dashed",
  #             color     = "black")






# Lag distribution plot
#   Histogram or density of DO → fDOM lag times
ggplot(DO_fDOM_paired, aes(x = lag_hours)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20,
                 fill = "grey80",
                 color = "black") +
  geom_density(linewidth = 1) +
  theme_bw() +
  labs(
    x = "Lag between DO start and fDOM start (hours)",
    y = "Density"
  )

#per well structure
ggplot(DO_fDOM_paired, aes(x = lag_hours, fill = well)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = well_colors) +
  theme_bw() +
  labs(x = "Lag (hours)", y = "Density")





# Boxplot
#   fDOM magnitude: matched vs unmatched events
fDOM_all <- bind_rows(fDOM_event_tables, .id = "well")

matched_ids <- DO_fDOM_paired$fDOM_event_id

fDOM_compare <- fDOM_all %>%
  mutate(
    matched = ifelse(event_id %in% matched_ids,
                     "Matched to DO",
                     "Unmatched"),
    log_fDOM = log(fDOM_magnitude)
  )

ggplot(fDOM_compare,
       aes(x = matched,
           y = log_fDOM,
           fill = matched)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("Matched to DO" = "#31688EFF",
                               "Unmatched"     = "grey70")) +
  theme_bw() +
  labs(
    x = "",
    y = "fDOM magnitude (log QSU)"
  )

