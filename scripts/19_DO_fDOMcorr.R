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
summarize_fDOM_event <- function(df,
                                 time_col = "datetimeMT",
                                 fDOM_col = "fDOM.QSU.mn") {
  
  stopifnot(time_col %in% names(df))
  stopifnot(fDOM_col %in% names(df))
  
  df <- df[order(df[[time_col]]), ]
  
  n_valid <- sum(!is.na(df[[fDOM_col]]))
  
  if (n_valid == 0) {
    stop("No non-missing fDOM values in this event")
  }
  
  fDOM_max <- max(df[[fDOM_col]], na.rm = TRUE)
  fDOM_min <- min(df[[fDOM_col]], na.rm = TRUE)
  
  data.frame(
    t_start       = min(df[[time_col]]),
    t_end         = max(df[[time_col]]),
    fDOM_max      = fDOM_max,
    fDOM_min      = fDOM_min,
    fDOM_magnitude = fDOM_max - fDOM_min
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
  fDOM_rows <- vector("list", length(fDOM_site_list))
  
  for (i in seq_along(fDOM_site_list)) {
    df_event <- fDOM_site_list[[i]]
    s <- summarize_fDOM_event(
      df_event,
      fDOM_col = "fDOM.QSU.mn"   # adjust if needed
    )
    
    s$event_id <- names(fDOM_site_list)[i]
    s$well     <- sub("_fDOM$", "", w)
    fDOM_rows[[i]] <- s
  }
  
  fDOM_event_tables[[sub("_fDOM$", "", w)]] <- do.call(rbind, fDOM_rows)
}

#### Match fDOM events to DO ####
match_DO_fDOM_events <- function(DO_events, fDOM_events, lag_hours = 36,
                                 DO_metric = "DO_AUC", fDOM_metric = "fDOM_magnitude") {
  
  # Convert lag to seconds
  lag_secs <- lag_hours * 3600
  
  # Initialize list for matches
  matched_list <- list()
  
  # Loop over each fDOM event
  for (i in seq_len(nrow(fDOM_events))) {
    fdom_row <- fDOM_events[i, ]
    
    # Candidate DO events: must end before fDOM start and within lag window
    candidates <- DO_events[
      (DO_events$t_end <= fdom_row$t_start) & 
        (fdom_row$t_start - DO_events$t_end <= lag_secs),
    ]
    
    if (nrow(candidates) == 0) {
      # No match
      matched_list[[i]] <- data.frame(
        fDOM_event_id = fdom_row$event_id,
        fDOM_start = fdom_row$t_start,
        fDOM_end = fdom_row$t_end,
        fDOM_magnitude = fdom_row[[fDOM_metric]],
        DO_event_id = NA,
        DO_start = as.POSIXct(NA),
        DO_end = as.POSIXct(NA),
        DO_AUC = NA
      )
    } else {
      # Match to the **closest previous DO event**
      closest <- candidates[which.max(candidates$t_end), ]
      matched_list[[i]] <- data.frame(
        fDOM_event_id = fdom_row$event_id,
        fDOM_start = fdom_row$t_start,
        fDOM_end = fdom_row$t_end,
        fDOM_magnitude = fdom_row[[fDOM_metric]],
        DO_event_id = closest$event_id,
        DO_start = closest$t_start,
        DO_end = closest$t_end,
        DO_AUC = closest[[DO_metric]]
      )
    }
  }
  
  # Combine all matches
  matched_table <- do.call(rbind, matched_list)
  matched_table
}

#### Match events across wells ####
matched_tables <- list()

for (w in names(DO_event_tables)) {
  cat("Matching events for well:", w, "\n")
  
  matched_tables[[w]] <- match_DO_fDOM_events(
    DO_events   = DO_event_tables[[w]],
    fDOM_events = fDOM_event_tables[[w]],
    lag_hours   = 36,  
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

#### To see actual lag of events ####
# Add a column: lag = as.numeric(fDOM_start - DO_end, units = "hours") to see actual lag


#### Test if DO events trigger fDOM events ####
# Logistic regression
# Response: whether an fDOM event occurred within lag time L
# Predictor: DO magnitude
# OR contingency/permutation test: count DO events followed by fDOM events within L
# Compare to random event timings using permutation to answer if the co-occurrence is greater than expected by chance


#### Test if DO magnitude drives fDOM magnitude ####
# core regression
# log(fDOM_metric) ~ log(DO_metric) + lag
# Spearman rank correlation or linear regression with bootstrapped CI

#### final figures ####
# Scatter plot
#   DO magnitude vs fDOM magnitude
#   Colored by well, size = lag
# 
# Lag distribution plot
#   Histogram or density of DO → fDOM lag times
# 
# Boxplot
#   fDOM magnitude: matched vs unmatched events

