#### Read me ####
# This script uses a sample DO event and calculates ER using Odum's (1956) / Hall & Hotchkiss's (2017) direct-calculation approach, adapted for our shallow groundwater setting (no light mean GPP = 0; no air-water interface or turbulence = gas exchange (K) assumed negligible)
# Corrected from the original ER_calc.R, which had a units bug: raw mg/L differences (change per 15-minute sampling step) were integrated via AUC() against raw seconds, without being converted to a rate in matching time units. That mismatch (per-15-min values multiplied against a per-seconds) inflated the result by ~900x.
# Because our DO events are sporadic and don't align with a diel cycle, a "per day" rate isn't a natural unit to report. This version instead reports TWO distinct quantities per event:
# an average hourly rate that conveys an "intensity" that allows comparisons among events and sites/wells within this study. Units are g02/m2/hr.
# an event total (a total amount, g02/m2 (or converted to gC02/m2 with a respiratory quotient for respiration)). This is NOT expressed "per" any time unit -- it's the full integrated total amount of accural or respiration for that one event, and should not be thought of as a rate.
#
# Accrual/ER classification is done per-interval, by the sign (+/-) of each 15-min DO change (rising = accrual, declining = ER). Sign-based classification allows quantification of additional accrual dispersed throughout the event. It does have a cost of being more sensitive to sensor noise. We should consider adding a noise threshold to address this - see noise_threshold_mgL below.

#### Libraries ####
library(tidyverse)
library(dplyr)
library(patchwork)

#### Import DO event file ####
BEGI_ex = read.csv("EXO_compiled/BEGI_event_ex.csv")
BEGI_ex$datetimeMT = as.POSIXct(BEGI_ex$datetimeMT)

#### Time step constants ####
interval_min = 15                        # sampling interval, minutes
steps_per_hour = 60 / interval_min        # number of sampling intervals per hour (4 for 15-min data)
interval_hr = interval_min / 60           # width of one sampling interval, in hours

#### Rate of change: per-interval, then converted to an hourly rate ####
# rate_of_change_per_interval: change in DO (mg/L) from one 15-min sample to the next 
# rate_of_change_hourly: the same per-interval quantity converted to mgO2/L/hour by multiplying by steps_per_hour. This is converts the raw amount (mg/L) to a rate (mg/L/hr), and is a conversion step that was missing in the original code
BEGI_ex <- BEGI_ex %>%
  mutate(
    rate_of_change_per_interval = c(NA, diff(ODO.mg.L.mn)),         # mg/L per 15-min step (an amount)
    rate_of_change_hourly = rate_of_change_per_interval * steps_per_hour  # mg/L per hour (a rate)
  )

View(BEGI_ex)

#### Classify each interval by sign (+/-) ####
# noise_threshold_mgL: any per-interval DO change smaller in magnitude than this (mg/L) is treated as neither accrual nor respiration -- i.e. within sensor noise, not a real signal -- and excluded from both totals. 
# EXO manual says resolution is 0.01 mg/L, so we will use that as the noise threshold
noise_threshold_mgL <- 0.01

accrual_idx <- which(BEGI_ex$rate_of_change_per_interval > noise_threshold_mgL)
decline_idx <- which(BEGI_ex$rate_of_change_per_interval < -noise_threshold_mgL)

#### Event duration (hours) ####
# We might want to report the event duration alongside the hourly rate so that it's clear the rate applies to THIS event's own length.
# this is the TOTAL time spent rising (or falling) -- the sum of however many separate intervals were classified into that phase -- not necessarily one continuous stretch of the event.
accrual_duration_hr <- length(accrual_idx) * interval_hr
decline_duration_hr <- length(decline_idx) * interval_hr

#### Average hourly rate (mg/L/hr) ####
# Average of the per-interval hourly rates.
accrual_rate_hourly <- mean(BEGI_ex$rate_of_change_hourly[accrual_idx], na.rm = TRUE)
ER_rate_hourly <- mean(BEGI_ex$rate_of_change_hourly[decline_idx], na.rm = TRUE)
# Gas exchange (K) is assumed ~0 here, so ER_rate_hourly is ecosystem respiration -- no K term to subtract, unlike in streams

#### Event total (mg/L) ####
# Sum of the raw per-interval changes (amounts, not rates). 
accrual_total <- sum(BEGI_ex$rate_of_change_per_interval[accrual_idx], na.rm = TRUE)
ER_total <- sum(BEGI_ex$rate_of_change_per_interval[decline_idx], na.rm = TRUE)

# Print out summary:
cat("Accrual: average hourly rate =", accrual_rate_hourly, "mg/L/hr, over", accrual_duration_hr, "hr\n")
cat("Accrual: event total =", accrual_total, "mg/L (= g/m3)\n")
cat("ER: average hourly rate =", ER_rate_hourly, "mg/L/hr, over", decline_duration_hr, "hr\n")
cat("ER: event total =", ER_total, "mg/L (= g/m3)\n")

#### Convert to areal quantities (g/m2 or g/m2/hr) ####
# z = assumed depth of the well-mixed zone that these volumetric quantities apply over. The DO sensor floats to stay ~1 m below the water table surface at all times -- z is set to that value below as the best available proxy for the mixing-zone thickness.
#
# >>> CHANGE THIS VALUE if you confirm a different sensor offset <<<
z_m <- 1   # meters; sensor depth below water table surface -- VERIFY THIS NUMBER

# mg/L is already numerically equal to g/m3 (1 mg/L = 1 g/m3 exactly), so no conversion factor is needed here -- only the depth multiplication below. It is 1, so no change, but I'm coding it here in case we need to change 1 to something else.
accrual_rate_hourly_areal <- accrual_rate_hourly * z_m   # g O2 / m2 / hr
ER_rate_hourly_areal <- ER_rate_hourly * z_m             # g O2 / m2 / hr

accrual_total_areal <- accrual_total * z_m               # g O2 / m2 (event total, not a rate)
ER_total_areal <- ER_total * z_m                         # g O2 / m2 (event total, not a rate)

# Print out summary:
cat("\nAreal (z =", z_m, "m):\n")
cat("Accrual: average hourly rate =", accrual_rate_hourly_areal, "g O2/m2/hr\n")
cat("Accrual: event total =", accrual_total_areal, "g O2/m2\n")
cat("ER: average hourly rate =", ER_rate_hourly_areal, "g O2/m2/hr\n")
cat("ER: event total =", ER_total_areal, "g O2/m2\n")


#### Convert respiration in O2 to CO2 units via respiratory quotients (RQ) ####
# RQ = mol CO2 produced / mol O2 consumed. Default RQ = 1.2 
# RQ_range (0.5-4.0) is the full theoretical range reported in Berggren et al. (2012), This range includes more plausible CO2 total/rate given uncertainty in substrate
MW_O2 <- 32    # g/mol
MW_CO2 <- 44   # g/mol
RQ_default <- 1.2
RQ_range <- c(min = 0.5, max = 4.0)

# Converts an O2 quantity (any of mg/L, mg/L/hr, g/m2, g/m2/hr -- units carry through unchanged) to the corresponding CO2 quantity at a given RQ. Sign is flipped so CO2 PRODUCTION is reported as positive (ER_* values above are negative, representing O2 loss).
O2_to_CO2 <- function(O2_value, RQ) {
  -O2_value * RQ * (MW_CO2 / MW_O2)
}

#### Point estimate (RQ = 1.2)
ER_rate_hourly_CO2 <- O2_to_CO2(ER_rate_hourly, RQ_default)
ER_total_CO2 <- O2_to_CO2(ER_total, RQ_default)
ER_rate_hourly_areal_CO2 <- O2_to_CO2(ER_rate_hourly_areal, RQ_default)
ER_total_areal_CO2 <- O2_to_CO2(ER_total_areal, RQ_default)

#### Full literature RQ range 
ER_rate_hourly_CO2_range <- O2_to_CO2(ER_rate_hourly, RQ_range)
ER_total_CO2_range <- O2_to_CO2(ER_total, RQ_range)
ER_rate_hourly_areal_CO2_range <- O2_to_CO2(ER_rate_hourly_areal, RQ_range)
ER_total_areal_CO2_range <- O2_to_CO2(ER_total_areal, RQ_range)

# Print out summary as table:
RQ_CO2_table <- data.frame(
  Quantity = c("ER average hourly rate (areal)",
               "ER event total (areal)"),
  Units = c("g CO2/m2/hr",
            "g CO2/m2"),
  RQ_0.5_min = c(ER_rate_hourly_areal_CO2_range["min"],
                 ER_total_areal_CO2_range["min"]),
  RQ_1.2_point = c(ER_rate_hourly_areal_CO2,
                   ER_total_areal_CO2),
  RQ_4.0_max = c(ER_rate_hourly_areal_CO2_range["max"],
                 ER_total_areal_CO2_range["max"])
)
RQ_CO2_table[ , 3:5] <- round(RQ_CO2_table[ , 3:5], 3)
row.names(RQ_CO2_table) <- NULL

print(RQ_CO2_table)
View(RQ_CO2_table)


#### Conceptual figure: DO curve + rate-of-change + cumulative total curves ####
# This is in response to Bob's request for a conceptual figure of the calculations. It uses the example data to make a two-panel figure in the style of Odum (1956) and Hall & Hotchkiss (2017).

# forming new df with one row per interval, linking each interval's start/end DO values and time bounds to its rate and classification.
seg <- data.frame(
  t0  = BEGI_ex$datetimeMT[-nrow(BEGI_ex)],
  t1  = BEGI_ex$datetimeMT[-1],
  DO0 = BEGI_ex$ODO.mg.L.mn[-nrow(BEGI_ex)],
  DO1 = BEGI_ex$ODO.mg.L.mn[-1],
  q   = BEGI_ex$rate_of_change_hourly[-1],
  dO  = BEGI_ex$rate_of_change_per_interval[-1]
)
seg$phase <- ifelse(seg$dO > noise_threshold_mgL, "Accrual",
                    ifelse(seg$dO < -noise_threshold_mgL, "ER", "Noise"))

phase_colors <- c(Accrual = "#1f6feb", ER = "#d1453b", Noise = "#999999")

#### Panel A: DO concentration curve, colored by interval classification
panelA <- ggplot(seg) +
  geom_segment(aes(x = t0, y = DO0, xend = t1, yend = DO1, color = phase),
               linewidth = 1.1, lineend = "round") +
  geom_point(data = BEGI_ex, aes(x = datetimeMT, y = ODO.mg.L.mn),
             size = 1.3, color = "black") +
  scale_color_manual(values = phase_colors,
                     breaks = c("Accrual", "ER", "Noise"),
                     labels = c("rising interval (q_i > 0): accrual",
                                "falling interval (q_i < 0): respiration",
                                "(qᵢ < [0.01]): no change"),
                     name = NULL) +
  labs(y = expression(paste("Dissolved oxygen, ", italic(O), "  (mg L"^-1, ")")),
       title = "Direct-Calculation Method Applied to a Groundwater DO Event") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "top",
        plot.title = element_text(face = "bold"))

#### Panel B: rate-of-change curve as bars where bar height = interval's Delta O2
panelB <- ggplot(seg) +
  geom_rect(aes(xmin = t0, xmax = t1, ymin = 0, ymax = q, fill = phase),
            color = "white", linewidth = 0.2) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.6) +
  scale_fill_manual(values = phase_colors, guide = "none") +
  scale_x_datetime(date_labels = "%H:%M") +
  labs(y = expression(paste("Rate of change, ", italic(q)[i], " = ", Delta,
                            italic(O), "/", Delta, italic(t), "  (mg L"^-1, " hr"^-1, ")"))) +
  theme_minimal(base_size = 12)

### Panel C: cumulative sum of Delta O_i, by phase -- explicitly builds ER_total
cumdf <- data.frame(
  time = c(seg$t0[1], seg$t1),
  cum_ER = c(0, cumsum(ifelse(seg$phase == "ER", seg$dO, 0))) * z_m,
  cum_Accrual = c(0, cumsum(ifelse(seg$phase == "Accrual", seg$dO, 0))) * z_m
)

panelC <- ggplot(cumdf) +
  geom_step(aes(x = time, y = cum_Accrual), color = phase_colors[["Accrual"]],
            direction = "hv", linewidth = 1.1) +
  geom_step(aes(x = time, y = cum_ER), color = phase_colors[["ER"]],
            direction = "hv", linewidth = 1.1) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.5) +
  geom_hline(yintercept = ER_total_areal, color = phase_colors[["ER"]],
             linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = accrual_total_areal, color = phase_colors[["Accrual"]],
             linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = cumdf$time[nrow(cumdf)],
           y = ER_total_areal, label = paste0("ER total = ", round(ER_total_areal, 2), " g/m2"),
           color = phase_colors[["ER"]], size = 3, hjust = 1, vjust = 1.4) +
  annotate("text", x = cumdf$time[nrow(cumdf)],
           y = accrual_total_areal, label = paste0("accrual total = ", round(accrual_total_areal, 2), " g/m2"),
           color = phase_colors[["Accrual"]], size = 3, hjust = 1, vjust = -0.6) +
  annotate("text", x = -Inf, y = Inf,
           label = paste0('paste("areal conversion: ", Delta, italic(O)[i], "  (mg L"^-1, ',
                          '" = g m"^-3, ") ', "×", ' z (", ', z_m, ', " m) = g m"^-2)'),
           parse = TRUE,
           hjust = -0.02, vjust = 1.6, size = 3, color = "grey30", fontface = "italic") +
  scale_x_datetime(date_labels = "%H:%M") +
  labs(x = "Time",
       y = expression(paste("Cumulative ", Sigma, Delta, italic(O)[i], "  (g m"^-2, ")"))) +
  theme_minimal(base_size = 12)


#### Combine panels and view
do_figure <- (panelA / panelB / panelC) +
  plot_layout(heights = c(1.1, 0.85, 1)) +
  plot_annotation(tag_levels = "A")
do_figure
