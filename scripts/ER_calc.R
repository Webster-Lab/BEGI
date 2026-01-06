#### Read me ####
# This script reads in a sample DO event and calculates ER using Odum's model (1956)

#### Libraries ####
library(tidyverse)
library(dplyr)
library(DescTools)

#### Import DO event file ####
BEGI_ex <-read.csv("BEGI_event_ex.csv")
BEGI_ex$datetimeMT <- as.POSIXct(BEGI_ex$datetimeMT)

#### Calculate event negative rate of change ####
BEGI_ex <- BEGI_ex %>%
  mutate(
    rate_of_change = c(NA, diff(ODO.mg.L.mn)),
    negative_rate_of_change = ifelse(rate_of_change < 0, rate_of_change, NA)
  )

avg_neg_rate <- mean(BEGI_ex$negative_rate_of_change, na.rm = TRUE)

View(BEGI_ex)
print(avg_neg_rate)

#### Integral of rate of change ####
# Extract event values
x_vals <- as.numeric(BEGI_ex$datetimeMT)
y_vals <- BEGI_ex$rate_of_change

# Remove NA values
valid <- !is.na(y_vals)
x_vals_clean <- x_vals[valid]
y_vals_clean <- y_vals[valid]

# Area under the curve (Q)
Q <- AUC(
  x = x_vals_clean,
  y = y_vals_clean,
  method = "trapezoid",
  na.rm = FALSE
)


#### Calculate positive integral of rate of change (D) ####
# Positive-only y values
y_pos <- y_vals_clean
y_pos[y_pos < 0] <- 0   # Zero out the negative limb

D <- AUC(
  x = x_vals_clean,
  y = y_pos,
  method = "trapezoid",
  na.rm = FALSE
)


#### Q - D = -ER ####
ER <- Q - D




