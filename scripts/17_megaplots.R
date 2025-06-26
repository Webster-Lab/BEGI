#### read me ####

# the purpose of this script is to clean and plot all DO event data for interpretation and publication

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)
library(readxl)
library(ggplot2)
library(patchwork)

#### get sunrise/sunset times ####

suntimes = getSunlightTimes(date = seq.Date(
  from = as.Date("2023-09-14"), 
  to = as.Date("2024-09-5"), by = 1),
  keep = c("sunrise", "sunset"),
  lat = 34.9, lon = -106.7, tz = "US/Mountain")
pm.pts = suntimes$sunset[-(nrow(suntimes))]
am.pts = suntimes$sunrise[-1]

#Define shaded am/pm
shade_df <- data.frame(
  xmin = pm.pts,
  xmax = am.pts,
  ymin = -Inf,
  ymax = Inf
)




#### Read in .csv files of service dates and times ####
service.VDOW = read.csv("EXO_compiled/service.VDOW.csv", row.names = 1)
names(service.VDOW) = "datetimeMT"
service.VDOW$datetimeMT = as.POSIXct(service.VDOW$datetimeMT, tz="US/Mountain")

service.VDOS = read.csv("EXO_compiled/service.VDOS.csv", row.names = 1)
names(service.VDOS) = "datetimeMT"
service.VDOS$datetimeMT = as.POSIXct(service.VDOS$datetimeMT, tz="US/Mountain")

service.SLOC = read.csv("EXO_compiled/service.SLOC.csv", row.names = 1)
names(service.SLOC) = "datetimeMT"
service.SLOC$datetimeMT = as.POSIXct(service.SLOC$datetimeMT, tz="US/Mountain")

service.SLOW = read.csv("EXO_compiled/service.SLOW.csv", row.names = 1)
names(service.SLOW) = "datetimeMT"
service.SLOW$datetimeMT = as.POSIXct(service.SLOW$datetimeMT, tz="US/Mountain")

#### Import EXO data ####
#First, run through above code of "Read in .csv files of service dates and times" and "Vector of dates"
#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#import EXOz.tc
EXOz.tc = readRDS("EXO_compiled/BEGI_EXOz.tc.rds")



# check time zone formatting
lubridate::tz(EXOz.tc[["SLOC"]]$datetimeMT)
lubridate::tz(EXOz.tc[["SLOW"]]$datetimeMT)
lubridate::tz(EXOz.tc[["VDOS"]]$datetimeMT)
lubridate::tz(EXOz.tc[["VDOW"]]$datetimeMT)






#### Remove in-air and obvious faulty measurements from all sites ####

sitenamez = c("SLOC","SLOW","VDOS","VDOW")


# replace pts outside spc and turbidity data thresholds with NA
# these thresholds indicate times sensors were in air or in muck/highly turbid conditions. Whole rows of data are replaced with NA for the sensors being in either of these conditions.
EXOz.ic = EXOz.tc
for(i in sitenamez){
  dat = EXOz.ic[[i]]
  ## replace pts below specific conductivity theshold with NA ##
  dat$Cond.µS.cm.mn[dat$SpCond.µS.cm.mn < 10] = NA
  dat$Cond.µS.cm.SD[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$fDOM.QSU.mn[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$fDOM.QSU.SD[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$fDOM.RFU.mn[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$fDOM.RFU.SD[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$nLF.Cond.µS.cm.mn[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$nLF.Cond.µS.cm.SD[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$ODO...sat.mn[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$ODO...sat.SD[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$ODO.mg.L.mn[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$ODO.mg.L.SD[dat$SpCond.µS.cm.mn < 10] = NA
  dat$Sal.psu.mn[dat$SpCond.µS.cm.mn < 10] = NA
  dat$Sal.psu.SD[dat$SpCond.µS.cm.mn < 10] = NA
  dat$SpCond.µS.cm.mn[dat$SpCond.µS.cm.mn < 10] = NA
  dat$SpCond.µS.cm.SD[dat$SpCond.µS.cm.mn < 10] = NA
  dat$TDS.mg.L.mn[dat$SpCond.µS.cm.mn < 10] = NA
  dat$TDS.mg.L.SD[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$Turbidity.FNU.mn[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$Turbidity.FNU.SD[dat$SpCond.µS.cm.mn < 10] = NA
  dat$TSS.mg.L.mn[dat$SpCond.µS.cm.mn < 10] = NA
  dat$TSS.mg.L.SD[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$Temp..C.mn[dat$SpCond.µS.cm.mn < 10] = NA
  # dat$Temp..C.SD[dat$SpCond.µS.cm.mn < 10] = NA
  ## replace pts above turbidity threshold of 200 FNU with NA. Keep DO since it is not as sensitive to high turbidity ##
  dat$Cond.µS.cm.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$Cond.µS.cm.SD[dat$Turbidity.FNU.mn > 200] = NA
  dat$fDOM.QSU.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$fDOM.QSU.SD[dat$Turbidity.FNU.mn > 200] = NA
  dat$fDOM.RFU.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$fDOM.RFU.SD[dat$Turbidity.FNU.mn > 200] = NA
  dat$nLF.Cond.µS.cm.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$nLF.Cond.µS.cm.SD[dat$Turbidity.FNU.mn > 200] = NA
  #dat$ODO...sat.mn[dat$Turbidity.FNU.mn > 200] = NA
  #dat$ODO...sat.SD[dat$Turbidity.FNU.mn > 200] = NA
  #dat$ODO.mg.L.mn[dat$Turbidity.FNU.mn > 200] = NA
  #dat$ODO.mg.L.SD[dat$Turbidity.FNU.mn > 200] = NA
  dat$Sal.psu.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$Sal.psu.SD[dat$Turbidity.FNU.mn > 200] = NA
  dat$SpCond.µS.cm.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$SpCond.µS.cm.SD[dat$Turbidity.FNU.mn > 200] = NA
  dat$TDS.mg.L.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$TDS.mg.L.SD[dat$Turbidity.FNU.mn > 200] = NA
  dat$Turbidity.FNU.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$Turbidity.FNU.SD[dat$Turbidity.FNU.mn > 200] = NA
  dat$TSS.mg.L.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$TSS.mg.L.SD[dat$Turbidity.FNU.mn > 200] = NA
  dat$Temp..C.mn[dat$Turbidity.FNU.mn > 200] = NA
  dat$Temp..C.SD[dat$Turbidity.FNU.mn > 200] = NA
  # replace in list
  EXOz.ic[[i]] = dat
}

#correct negative DO values
EXOz.ic[["VDOW"]]$ODO.mg.L.mn <- EXOz.ic[["VDOW"]]$ODO.mg.L.mn + (min(EXOz.ic[["VDOW"]]$ODO.mg.L.mn, na.rm = TRUE)*-1)
EXOz.ic[["VDOS"]]$ODO.mg.L.mn <- EXOz.ic[["VDOS"]]$ODO.mg.L.mn + (min(EXOz.ic[["VDOS"]]$ODO.mg.L.mn, na.rm = TRUE)*-1)
EXOz.ic[["SLOW"]]$ODO.mg.L.mn <- EXOz.ic[["SLOW"]]$ODO.mg.L.mn + (min(EXOz.ic[["SLOW"]]$ODO.mg.L.mn, na.rm = TRUE)*-1)
EXOz.ic[["SLOC"]]$ODO.mg.L.mn <- EXOz.ic[["SLOC"]]$ODO.mg.L.mn + (min(EXOz.ic[["SLOC"]]$ODO.mg.L.mn, na.rm = TRUE)*-1)

# replace pts outside burst standard dev data thresholds with NA
# these thresholds indicate faulty readings on individual sensors for our most used data columns. Only data for that sensor and column is replaced with NA.
EXOz.sdc = EXOz.ic
col.mn = c("fDOM.QSU.mn", "SpCond.µS.cm.mn", "Turbidity.FNU.mn", "Temp..C.mn")
col.SD = c("fDOM.QSU.SD", "SpCond.µS.cm.SD", "Turbidity.FNU.SD", "Temp..C.SD")

for(i in sitenamez){
  
  dat = EXOz.sdc[[i]]
  
  for(c in 1:4) {
    dat[[col.mn[c]]][dat[[col.SD[c]]] > (10*mean(dat[[col.SD[c]]], na.rm = T))] = NA # replaces individual values with NA
  }
  
  # replace in list
  EXOz.sdc[[i]] = dat
}

#### Import and format depth to water data for plotting ####

#Import DTW data
DTW_df = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")

# check time zone formatting
lubridate::tz(DTW_df$datetimeMT)

### spread DTW_m in DTW_df to each well
DTW_df <- DTW_df %>%
  spread (wellID, DTW_m)

#dtw df for each well
DTW_SLOC <- data.frame(DTW_df$datetimeMT,
                       DTW_df$SLOC)
DTW_SLOC <- na.omit(DTW_SLOC)

DTW_SLOW <- data.frame(DTW_df$datetimeMT,
                       DTW_df$SLOW)
DTW_SLOW <- na.omit(DTW_SLOW)

DTW_VDOW <- data.frame(DTW_df$datetimeMT,
                       DTW_df$VDOW)
DTW_VDOW <- na.omit(DTW_VDOW)

DTW_VDOS <- data.frame(DTW_df$datetimeMT,
                       DTW_df$VDOS)
DTW_VDOS <- na.omit(DTW_VDOS)




#### Plot all events ####

# #### with delineation periods highlighted on DO plots
# # SLOC
# for (i in seq_along(BEGI_events[["DO_events"]][["SLOC_DO"]])) {
#   
#   dz <- BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]
#   
#   #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
#   start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
#   end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
#   
#   start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
#   end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
#   
#   #Subset data
#   tempdat <- EXOz.sdc[["SLOC"]][
#     EXOz.sdc[["SLOC"]]$datetimeMT >= start_time &
#       EXOz.sdc[["SLOC"]]$datetimeMT <= end_time, ]
# 
#   tempdatDOe <- EXOz.sdc[["SLOC"]][
#     EXOz.sdc[["SLOC"]]$datetimeMT >= start_time_event &
#       EXOz.sdc[["SLOC"]]$datetimeMT <= end_time_event, ]  
#   
#   tempdtw <- DTW_SLOC[
#     DTW_SLOC$DTW_df.datetimeMT >= start_time &
#       DTW_SLOC$DTW_df.datetimeMT <= end_time, ]
#   
#   #Plots
#   g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))+
#     geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
#   
#   g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   
#   g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.SLOC)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits = c(0,200))+
#     geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     labs(y = "SpCond (µS/cm)", x = "Datetime") +
#     geom_line(na.rm = TRUE) + theme_minimal() +
#     theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
#   
#   # Combine with patchwork
#   full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
#     plot_layout(ncol = 1, heights = rep(1, 6)) & 
#     theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
#   
#   # Save
#   ggsave(
#     filename = paste0("plots/delineations/SLOC/events/SLOC_megaplot_v2_", i, ".pdf"),
#     plot = full_plot,
#     width = 6, height = 11 
#   )
# }
# 
# # SLOW
# for (i in seq_along(BEGI_events[["DO_events"]][["SLOW_DO"]])) {
#   
#   dz <- BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]
#   
#   #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
#   start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
#   end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
#   
#   start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
#   end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
#   
#   #Subset data
#   tempdat <- EXOz.sdc[["SLOW"]][
#     EXOz.sdc[["SLOW"]]$datetimeMT >= start_time &
#       EXOz.sdc[["SLOW"]]$datetimeMT <= end_time, ]
#   
#   tempdatDOe <- EXOz.sdc[["SLOW"]][
#     EXOz.sdc[["SLOW"]]$datetimeMT >= start_time_event &
#       EXOz.sdc[["SLOW"]]$datetimeMT <= end_time_event, ]  
#   
#   tempdtw <- DTW_SLOW[
#     DTW_SLOW$DTW_df.datetimeMT >= start_time &
#       DTW_SLOW$DTW_df.datetimeMT <= end_time, ]
#   
#   #Plots
#   g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))+
#     geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
#   
#   g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   
#   g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.SLOW)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#    # scale_y_continuous(limits = c(0,200))+
#     geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     labs(y = "SpCond (µS/cm)", x = "Datetime") +
#     geom_line(na.rm = TRUE) + theme_minimal() +
#     theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
#   
#   # Combine with patchwork
#   full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
#     plot_layout(ncol = 1, heights = rep(1, 6)) & 
#     theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
#   
#   # Save
#   ggsave(
#     filename = paste0("plots/delineations/SLOW/events/SLOW_megaplot_v2_", i, ".pdf"),
#     plot = full_plot,
#     width = 6, height = 11 
#   )
# }
# 
# # VDOW
# for (i in seq_along(BEGI_events[["DO_events"]][["VDOW_DO"]])) {
#   
#   dz <- BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]
#   
#   #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
#   start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
#   end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
#   
#   start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
#   end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
#   
#   #Subset data
#   tempdat <- EXOz.sdc[["VDOW"]][
#     EXOz.sdc[["VDOW"]]$datetimeMT >= start_time &
#       EXOz.sdc[["VDOW"]]$datetimeMT <= end_time, ]
#   
#   tempdatDOe <- EXOz.sdc[["VDOW"]][
#     EXOz.sdc[["VDOW"]]$datetimeMT >= start_time_event &
#       EXOz.sdc[["VDOW"]]$datetimeMT <= end_time_event, ]  
#   
#   tempdtw <- DTW_VDOW[
#     DTW_VDOW$DTW_df.datetimeMT >= start_time &
#       DTW_VDOW$DTW_df.datetimeMT <= end_time, ]
#   
#   #Plots
#   g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))+
#     geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
#   
#   g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   
#   g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.VDOW)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     labs(y = "SpCond (µS/cm)", x = "Datetime") +
#     geom_line(na.rm = TRUE) + theme_minimal() +
#     theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
#   
#   # Combine with patchwork
#   full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
#     plot_layout(ncol = 1, heights = rep(1, 6)) & 
#     theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
#   
#   # Save
#   ggsave(
#     filename = paste0("plots/delineations/VDOW/events/VDOW_megaplot_v2_", i, ".pdf"),
#     plot = full_plot,
#     width = 6, height = 11 
#   )
# }
# 
# # VDOS
# for (i in seq_along(BEGI_events[["DO_events"]][["VDOS_DO"]])) {
#   
#   dz <- BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]
#   
#   #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
#   start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
#   end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
#   
#   start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
#   end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
#   
#   #Subset data
#   tempdat <- EXOz.sdc[["VDOS"]][
#     EXOz.sdc[["VDOS"]]$datetimeMT >= start_time &
#       EXOz.sdc[["VDOS"]]$datetimeMT <= end_time, ]
#   
#   tempdatDOe <- EXOz.sdc[["VDOS"]][
#     EXOz.sdc[["VDOS"]]$datetimeMT >= start_time_event &
#       EXOz.sdc[["VDOS"]]$datetimeMT <= end_time_event, ]  
#   
#   tempdtw <- DTW_VDOS[
#     DTW_VDOS$DTW_df.datetimeMT >= start_time &
#       DTW_VDOS$DTW_df.datetimeMT <= end_time, ]
#   
#   #Plots
#   g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))+
#     geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
#   
#   g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   
#   g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.VDOS)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     labs(y = "SpCond (µS/cm)", x = "Datetime") +
#     geom_line(na.rm = TRUE) + theme_minimal() +
#     theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
#   
#   # Combine with patchwork
#   full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
#     plot_layout(ncol = 1, heights = rep(1, 6)) & 
#     theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
#   
#   # Save
#   ggsave(
#     filename = paste0("plots/delineations/VDOS/events/VDOS_megaplot_v2_", i, ".pdf"),
#     plot = full_plot,
#     width = 6, height = 11 
#   )
# }


#### withOUT delineation periods highlighted on DO plots ####

# SLOC
for (i in seq_along(BEGI_events[["DO_events"]][["SLOC_DO"]])) {
  
  dz <- BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]
  
  #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
  
  start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
  end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
  
  #Subset data
  tempdat <- EXOz.sdc[["SLOC"]][
    EXOz.sdc[["SLOC"]]$datetimeMT >= start_time &
      EXOz.sdc[["SLOC"]]$datetimeMT <= end_time, ]
  
  tempdatDOe <- EXOz.sdc[["SLOC"]][
    EXOz.sdc[["SLOC"]]$datetimeMT >= start_time_event &
      EXOz.sdc[["SLOC"]]$datetimeMT <= end_time_event, ]  
  
  tempdtw <- DTW_SLOC[
    DTW_SLOC$DTW_df.datetimeMT >= start_time &
      DTW_SLOC$DTW_df.datetimeMT <= end_time, ]
  
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    #geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #          inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
    #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    #geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #          inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.SLOC)) +
    #geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
     #         inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
   # geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #          inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    #geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
     #         inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    #geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
     #         inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time),
                     date_labels = "%b %d %H:%M") +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/SLOC/events/SLOC_megaplot_v3_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

# SLOW
for (i in seq_along(BEGI_events[["DO_events"]][["SLOW_DO"]])) {
  
  dz <- BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]
  
  #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
  
  start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
  end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
  
  #Subset data
  tempdat <- EXOz.sdc[["SLOW"]][
    EXOz.sdc[["SLOW"]]$datetimeMT >= start_time &
      EXOz.sdc[["SLOW"]]$datetimeMT <= end_time, ]
  
  tempdatDOe <- EXOz.sdc[["SLOW"]][
    EXOz.sdc[["SLOW"]]$datetimeMT >= start_time_event &
      EXOz.sdc[["SLOW"]]$datetimeMT <= end_time_event, ]  
  
  tempdtw <- DTW_SLOW[
    DTW_SLOW$DTW_df.datetimeMT >= start_time &
      DTW_SLOW$DTW_df.datetimeMT <= end_time, ]
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
    #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.SLOW)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time),
                     date_labels = "%b %d %H:%M") +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/SLOW/events/SLOW_megaplot_v3_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

# VDOW
for (i in seq_along(BEGI_events[["DO_events"]][["VDOW_DO"]])) {
  
  dz <- BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]
  
  #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
  
  start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
  end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
  
  #Subset data
  tempdat <- EXOz.sdc[["VDOW"]][
    EXOz.sdc[["VDOW"]]$datetimeMT >= start_time &
      EXOz.sdc[["VDOW"]]$datetimeMT <= end_time, ]
  
  tempdatDOe <- EXOz.sdc[["VDOW"]][
    EXOz.sdc[["VDOW"]]$datetimeMT >= start_time_event &
      EXOz.sdc[["VDOW"]]$datetimeMT <= end_time_event, ]  
  
  tempdtw <- DTW_VDOW[
    DTW_VDOW$DTW_df.datetimeMT >= start_time &
      DTW_VDOW$DTW_df.datetimeMT <= end_time, ]
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
    #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.VDOW)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time),
                     date_labels = "%b %d %H:%M") +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/VDOW/events/VDOW_megaplot_v3_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

# VDOS
for (i in seq_along(BEGI_events[["DO_events"]][["VDOS_DO"]])) {
  
  dz <- BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]
  
  #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
  
  start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
  end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
  
  #Subset data
  tempdat <- EXOz.sdc[["VDOS"]][
    EXOz.sdc[["VDOS"]]$datetimeMT >= start_time &
      EXOz.sdc[["VDOS"]]$datetimeMT <= end_time, ]
  
  tempdatDOe <- EXOz.sdc[["VDOS"]][
    EXOz.sdc[["VDOS"]]$datetimeMT >= start_time_event &
      EXOz.sdc[["VDOS"]]$datetimeMT <= end_time_event, ]  
  
  tempdtw <- DTW_VDOS[
    DTW_VDOS$DTW_df.datetimeMT >= start_time &
      DTW_VDOS$DTW_df.datetimeMT <= end_time, ]
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
    #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.VDOS)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time),
                     date_labels = "%b %d %H:%M") +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/VDOS/events/VDOS_megaplot_v3_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

# IGNORE: #### withOUT delineation periods highlighted on DO plots and WITH y axis limits ####
# 
# # determine y axis limits to apply
# summary(EXOz.sdc[["SLOC"]]$ODO.mg.L.mn)
# summary(EXOz.sdc[["SLOW"]]$ODO.mg.L.mn)
# summary(EXOz.sdc[["VDOW"]]$ODO.mg.L.mn)
# summary(EXOz.sdc[["VDOS"]]$ODO.mg.L.mn)
# # 0-12
# 
# summary(EXOz.sdc[["SLOC"]]$fDOM.QSU.mn)
# summary(EXOz.sdc[["SLOW"]]$fDOM.QSU.mn)
# summary(EXOz.sdc[["VDOW"]]$fDOM.QSU.mn)
# summary(EXOz.sdc[["VDOS"]]$fDOM.QSU.mn)
# # 0-150
# 
# summary(DTW_SLOC$DTW_df.SLOC)
# summary(DTW_SLOW$DTW_df.SLOW)
# summary(DTW_VDOW$DTW_df.VDOW)
# summary(DTW_VDOS$DTW_df.VDOS)
# # -0.4-1.82
# 
# summary(EXOz.sdc[["SLOC"]]$Turbidity.FNU.mn)
# summary(EXOz.sdc[["SLOW"]]$Turbidity.FNU.mn)
# summary(EXOz.sdc[["VDOW"]]$Turbidity.FNU.mn)
# summary(EXOz.sdc[["VDOS"]]$Turbidity.FNU.mn)
# #-0.6-200
# 
# summary(EXOz.sdc[["SLOC"]]$Temp..C.mn)
# summary(EXOz.sdc[["SLOW"]]$Temp..C.mn)
# summary(EXOz.sdc[["VDOW"]]$Temp..C.mn)
# summary(EXOz.sdc[["VDOS"]]$Temp..C.mn)
# # -0.2-27
# 
# summary(EXOz.sdc[["SLOC"]]$SpCond.µS.cm.mn)
# summary(EXOz.sdc[["SLOW"]]$SpCond.µS.cm.mn)
# summary(EXOz.sdc[["VDOW"]]$SpCond.µS.cm.mn)
# summary(EXOz.sdc[["VDOS"]]$SpCond.µS.cm.mn)
# # 10-1700
# 
# # SLOC
# for (i in seq_along(BEGI_events[["DO_events"]][["SLOC_DO"]])) {
#   
#   dz <- BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]
#   
#   #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
#   start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
#   end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
#   
#   start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
#   end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
#   
#   #Subset data
#   tempdat <- EXOz.sdc[["SLOC"]][
#     EXOz.sdc[["SLOC"]]$datetimeMT >= start_time &
#       EXOz.sdc[["SLOC"]]$datetimeMT <= end_time, ]
#   
#   tempdatDOe <- EXOz.sdc[["SLOC"]][
#     EXOz.sdc[["SLOC"]]$datetimeMT >= start_time_event &
#       EXOz.sdc[["SLOC"]]$datetimeMT <= end_time_event, ]  
#   
#   tempdtw <- DTW_SLOC[
#     DTW_SLOC$DTW_df.datetimeMT >= start_time &
#       DTW_SLOC$DTW_df.datetimeMT <= end_time, ]
#   
#   #Plots
#   g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,12)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
#   
#   g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,150)) +
#     geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   
#   g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.SLOC)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(-0.4, 1.82)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     scale_y_continuous(limits= c(0,100)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(-0.2,27)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,1700)) +
#     labs(y = "SpCond (µS/cm)", x = "Datetime") +
#     geom_line(na.rm = TRUE) + theme_minimal() +
#     theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
#   
#   # Combine with patchwork
#   full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
#     plot_layout(ncol = 1, heights = rep(1, 6)) & 
#     theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
#   
#   # Save
#   ggsave(
#     filename = paste0("plots/delineations/SLOC/events/SLOC_megaplot_v4_", i, ".pdf"),
#     plot = full_plot,
#     width = 6, height = 11 
#   )
# }
# 
# # SLOW
# for (i in seq_along(BEGI_events[["DO_events"]][["SLOW_DO"]])) {
#   
#   dz <- BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]
#   
#   #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
#   start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
#   end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
#   
#   start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
#   end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
#   
#   #Subset data
#   tempdat <- EXOz.sdc[["SLOW"]][
#     EXOz.sdc[["SLOW"]]$datetimeMT >= start_time &
#       EXOz.sdc[["SLOW"]]$datetimeMT <= end_time, ]
#   
#   tempdatDOe <- EXOz.sdc[["SLOW"]][
#     EXOz.sdc[["SLOW"]]$datetimeMT >= start_time_event &
#       EXOz.sdc[["SLOW"]]$datetimeMT <= end_time_event, ]  
#   
#   tempdtw <- DTW_SLOW[
#     DTW_SLOW$DTW_df.datetimeMT >= start_time &
#       DTW_SLOW$DTW_df.datetimeMT <= end_time, ]
#   
#   #Plots
#   g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,12)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
#   
#   g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,150)) +
#     geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   
#   g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.SLOW)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(-0.4, 1.82)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     scale_y_continuous(limits= c(0,100)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(-0.2,27)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,1700)) +
#     labs(y = "SpCond (µS/cm)", x = "Datetime") +
#     geom_line(na.rm = TRUE) + theme_minimal() +
#     theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
#   
#   # Combine with patchwork
#   full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
#     plot_layout(ncol = 1, heights = rep(1, 6)) & 
#     theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
#   
#   # Save
#   ggsave(
#     filename = paste0("plots/delineations/SLOW/events/SLOW_megaplot_v4_", i, ".pdf"),
#     plot = full_plot,
#     width = 6, height = 11 
#   )
# }
# 
# # VDOW
# for (i in seq_along(BEGI_events[["DO_events"]][["VDOW_DO"]])) {
#   
#   dz <- BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]
#   
#   #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
#   start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
#   end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
#   
#   start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
#   end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
#   
#   #Subset data
#   tempdat <- EXOz.sdc[["VDOW"]][
#     EXOz.sdc[["VDOW"]]$datetimeMT >= start_time &
#       EXOz.sdc[["VDOW"]]$datetimeMT <= end_time, ]
#   
#   tempdatDOe <- EXOz.sdc[["VDOW"]][
#     EXOz.sdc[["VDOW"]]$datetimeMT >= start_time_event &
#       EXOz.sdc[["VDOW"]]$datetimeMT <= end_time_event, ]  
#   
#   tempdtw <- DTW_VDOW[
#     DTW_VDOW$DTW_df.datetimeMT >= start_time &
#       DTW_VDOW$DTW_df.datetimeMT <= end_time, ]
#   
#   #Plots
#   g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,12)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
#   
#   g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,150)) +
#     geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   
#   g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.VDOW)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(-0.4, 1.82)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     scale_y_continuous(limits= c(0,100)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(-0.2,27)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,1700)) +
#     labs(y = "SpCond (µS/cm)", x = "Datetime") +
#     geom_line(na.rm = TRUE) + theme_minimal() +
#     theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
#   
#   # Combine with patchwork
#   full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
#     plot_layout(ncol = 1, heights = rep(1, 6)) & 
#     theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
#   
#   # Save
#   ggsave(
#     filename = paste0("plots/delineations/VDOW/events/VDOW_megaplot_v4_", i, ".pdf"),
#     plot = full_plot,
#     width = 6, height = 11 
#   )
# }
# 
# # VDOS
# for (i in seq_along(BEGI_events[["DO_events"]][["VDOS_DO"]])) {
#   
#   dz <- BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]
#   
#   #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
#   start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
#   end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
#   
#   start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
#   end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
#   
#   #Subset data
#   tempdat <- EXOz.sdc[["VDOS"]][
#     EXOz.sdc[["VDOS"]]$datetimeMT >= start_time &
#       EXOz.sdc[["VDOS"]]$datetimeMT <= end_time, ]
#   
#   tempdatDOe <- EXOz.sdc[["VDOS"]][
#     EXOz.sdc[["VDOS"]]$datetimeMT >= start_time_event &
#       EXOz.sdc[["VDOS"]]$datetimeMT <= end_time_event, ]  
#   
#   tempdtw <- DTW_VDOS[
#     DTW_VDOS$DTW_df.datetimeMT >= start_time &
#       DTW_VDOS$DTW_df.datetimeMT <= end_time, ]
#   
#   #Plots
#   g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,12)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
#   
#   g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,150)) +
#     geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   
#   g3 <- ggplot(tempdtw, aes(x = DTW_df.datetimeMT, y = -DTW_df.VDOS)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(-0.4, 1.82)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     scale_y_continuous(limits= c(0,100)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(-0.2,27)) +
#     geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
#     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#           plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
#   
#   g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
#     geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
#     geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
#     scale_x_datetime(limits = c(start_time, end_time)) +
#     #scale_y_continuous(limits= c(0,1700)) +
#     labs(y = "SpCond (µS/cm)", x = "Datetime") +
#     geom_line(na.rm = TRUE) + theme_minimal() +
#     theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
#   
#   # Combine with patchwork
#   full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
#     plot_layout(ncol = 1, heights = rep(1, 6)) & 
#     theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
#   
#   # Save
#   ggsave(
#     filename = paste0("plots/delineations/VDOS/events/VDOS_megaplot_v4_", i, ".pdf"),
#     plot = full_plot,
#     width = 6, height = 11 
#   )
# }
