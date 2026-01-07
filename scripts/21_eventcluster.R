#### read me #####
# the purpose of this script is to use the r package dtwclust to cluster 
# water depth curves by shape to characterize the nature of the variation that is correlated with DO event size
# The previous application of this package clustered groundwater depth curves preceding a DO event.
# In this application, I will be looking at groundwater depth curves DURING a DO event as well as fDOM curves FOLLOWING a DO event

# resources:
# Manual: https://cran.r-project.org/web/packages/dtwclust/dtwclust.pdf
# R Journal article: https://journal.r-project.org/articles/RJ-2019-023/
# Example paper about turtle dives: https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.4384
# Ethanol example: https://tmastny.github.io/tsrecipes/articles/time-series-clustering.html
# browseVignettes("dtwclust")

#### libraries ####

#library(tsrecipes)
library(tidyverse)
library(dtwclust)
library(reshape2)
library(zoo)
library(xts)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(dplyr)
library(purrr)

####################################################################
##### Starting with fDOM clusters in 48 hours following DO event####
####################################################################
#### load data ####
fdom_df = readRDS("EXO_compiled/BEGI_EXOz.ts.tc.rds")
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#### create timeseries matrix of fDOM following DO event ####
#SLOC#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["SLOC_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["SLOC_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 48 * 60 * 60
  
  tempdat <- fdom_df[["SLOC"]][
    fdom_df[["SLOC"]]$datetimeMT >= start_time & fdom_df[["SLOC"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_SLOC <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_SLOC) >= 192) {
    ts_values <- valid_SLOC[1:192]
  } else {
    ts_values <- c(valid_SLOC, rep(NA, 192 - length(valid_SLOC)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOC_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(SLOC_fdom) <- paste0("t", seq_len(192))
SLOC_fdom$event_time <- BEGI_events[["Eventdate"]][["SLOC_dates"]]


#SLOW#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["SLOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 48 * 60 * 60
  
  tempdat <- fdom_df[["SLOW"]][
    fdom_df[["SLOW"]]$datetimeMT >= start_time & fdom_df[["SLOW"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_SLOW <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_SLOW) >= 192) {
    ts_values <- valid_SLOW[1:192]
  } else {
    ts_values <- c(valid_SLOW, rep(NA, 192 - length(valid_SLOW)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOW_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(SLOW_fdom) <- paste0("t", seq_len(192))
SLOW_fdom$event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]]


#VDOW#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 48 * 60 * 60
  
  tempdat <- fdom_df[["VDOW"]][
    fdom_df[["VDOW"]]$datetimeMT >= start_time & fdom_df[["VDOW"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_VDOW <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_VDOW) >= 192) {
    ts_values <- valid_VDOW[1:192]
  } else {
    ts_values <- c(valid_VDOW, rep(NA, 192 - length(valid_VDOW)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOW_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(VDOW_fdom) <- paste0("t", seq_len(192))
VDOW_fdom$event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]]


#VDOS#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOS_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 48 * 60 * 60
  
  tempdat <- fdom_df[["VDOS"]][
    fdom_df[["VDOS"]]$datetimeMT >= start_time & fdom_df[["VDOS"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_VDOS <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_VDOS) >= 192) {
    ts_values <- valid_VDOS[1:192]
  } else {
    ts_values <- c(valid_VDOS, rep(NA, 192 - length(valid_VDOS)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOS_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(VDOS_fdom) <- paste0("t", seq_len(192))
VDOS_fdom$event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]]



#combine all fdom dataframes
event_fdom <- rbind(SLOC_fdom,SLOW_fdom,VDOW_fdom,VDOS_fdom)
#save
saveRDS(event_fdom, "EXO_compiled/event_fdom.rds")


#### load and wrangle matrix data ####

dat = readRDS("EXO_compiled/event_fdom.rds") 
# I need to make an equivalent for the (estimated) duration of each event. Which is challenging because the timeframe of during each event varies
# I started with the 36 hour period of fDOM following each event.
# then I can take the average event length to look at gw depth curves during a DO event

dat$ename = paste("e", c(1:52), sep="")
rownames(dat) = dat$ename

# save date/time stamps of events separately 
times = dat[,193:194]
dat[,193:194] = NULL

# gap fill fdom series
dat_filled <- t(apply(dat, 1, function(e) {
  na.approx(e, na.rm = FALSE)
}))
dat_filled <- t(apply(dat_filled, 1, function(e) {
  e <- na.locf(e, na.rm = FALSE)
  na.locf(e, fromLast = TRUE)
}))

# make a version with data normalized to make all scales the same
normalized<-function(y) {
  
  x<-y[!is.na(y)]
  
  x<-(x - min(x)) / (max(x) - min(x))
  
  y[!is.na(y)]<-x
  
  return(y)
}
dat_n = t(apply(dat_filled,1,normalized))

# make versions with data smoothed and normalized
dat.xts = data.frame(t(dat_filled))
d = ( seq.POSIXt(
  from = as.POSIXct("2025-06-10 01:00:00", tz="US/Mountain"),
  to = as.POSIXct("2025-06-10 01:00:00", tz="US/Mountain")+172800,
  by = "15 min"))[1:192]
dat.xts = xts(dat.xts, order.by = d)
#dat.xts_s = rollmean(dat.xts, 12, fill=NA, align = "right")
dat.xts_s <- rollmean(dat.xts,
  k     = 12,
  fill  = "extend",   # no NAs at edges
  align = "center"
)

# interpolate any remaining internal gaps
dat.xts_s <- na.approx(dat.xts_s, na.rm = FALSE)
dat_s = as.data.frame(t(dat.xts_s))
names(dat_s) = paste("t", c(1:192), sep="")
#dat_s = dat_s[,12:144]

dat_s_n = t(apply(dat_s,1,normalized))

#### plot data ####

# raw data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat))[,i], type = "l", main=i)
}
#dev.off()

# norm data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat_n))[,i], type = "l", main=i)
}

# smoothed data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat_s))[,i], type = "l", main=i)
}

# smoothed and normalized data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat_s_n))[,i], type = "l", main=i)
}

#### FOR SMOOTHED AND NORMALIZED DATA ####
#### Evaluate Optimal Cluster Number ####

# NOTE: notes are mostly from the turtle dives paper's code
## Evaluate "optimal" cluster number using CVI's (cluster validity indices)
## internal CVI's: consider partioned data and try to define measure of cluster purity
## external CVI's: compare obtained partition to correct one (need a ground truth for this- we won't use these)
## note: which CVI to use is also subjective/needs testing... can go with "majority vote" from indices but you should check that the final result makes biological sense!
## note: can also use "clue" package to evaluate clusters

# look at cluster no. of 2 to 6 max
# NOTE: this can take a long time to run! suggest running on a computer with lots of RAM/memory...
depth_clust_k <- tsclust(series=dat_s_n, k = 2:7, centroid="pam", distance = "dtw_basic")
names(depth_clust_k) <- paste0("k_", 2:7)
k_table<-sapply(depth_clust_k, cvi, type = "internal")
# print table
k_table
# Note:
## some indices should maximized ("Sil","SF","CH","D") and some should be minimized ("DB","DBstar","COP")
# k=2 is optimal, k=4 as secondary option but non ideal

# look at cluster no. of 2 to 6 max
# NOTE: this can take a long time to run! suggest running on a computer with lots of RAM/memory...
depth_clust_k <- tsclust(series=dat_s_n, k = 2:7, centroid="shape", distance = "dtw_basic")
names(depth_clust_k) <- paste0("k_", 2:7)
k_table<-sapply(depth_clust_k, cvi, type = "internal")
# print table
k_table
# Note:
## some indices should maximized ("Sil","SF","CH","D") and some should be minimized ("DB","DBstar","COP")
# k=2 is optimal
#### Performing DTW Clustering ####

# Choices of parameters for dtw clustering:

# window size: limits distance that points can be matched to each other
## I don't want a limit ( I want all observations for a depth to be considered)

# k = no. of clusters
## can use selection criteria with dtwclust package to determine optimal no. for k

# centroid = time series prototype (time-series averaging method = summarizes imp. characteristics for all series in a given cluster)
## PAM centroid is likely the best candidate- time series with minimum sum of distances to others in cluster (also allows series of diff lengths)
## for PAM: cluster centroids are generally one of the time series from the data
## from the manual (Sarda-Espinosa 2019): 
## " partitional clustering creates k number of clusters from data
## k centroids are randomly initialized (choose k objects from dataset at random = k depths)
## each is then assigned to individual clusters
## distance between all data objects (depths) and all centroids (random k depths) is calculated
## each object/depth is assigned to the cluster of its closest centroid (random k depth time series)
## protyping function iS then applied to each cluster to update the corresponding centroid (e.g. median)
## distances and centroids are updated iteratively (until no more objects can change clusters)"
## note: clustering is generally unsupervised but clusters can be evaluated...

# we also use the "dtw_basic" distance measure
## core calculations for distances of dtwclust are performed in C++ (fast)
## basic uses DTW distance measure and has less functionality than other options (?) but is faster

set.seed(666)

# run analysis with k=2 (pam)
depth_clust_k2 <- tsclust(series = dat_s_n, k = 2, distance = 'dtw_basic',centroid="pam")
depth_clust_k2
plot(depth_clust_k2)
# optimal

# run analysis with k=2 (shape)
depth_clust_k2s <- tsclust(series = dat_s_n, k = 2, distance = 'dtw_basic',centroid="shape")
depth_clust_k2s
plot(depth_clust_k2s)
# optimal

# # run analysis with k=3
# depth_clust_k3 <- tsclust(series = dat_s_n, k = 3, distance = 'dtw_basic',centroid="pam")
# depth_clust_k3
# plot(depth_clust_k3)
# 
# # run analysis with k=4
# depth_clust_k4 <- tsclust(series = dat_s_n, k = 4, distance = 'dtw_basic',centroid="pam")
# depth_clust_k4
# plot(depth_clust_k4)


#### Merge Cluster Data With original Data ####

# format cluster data
## 52 was the # of original curves
cluster_data<-as.data.frame(list(DTW=list(cumsum(rep(1,52))),cluster=list(depth_clust_k2@cluster)))
colnames(cluster_data)<-c("DTW_id","cluster")
# merge data df's together
cluster_DTW_data_k2<-cbind(times, cluster_data,dat_s_n)


#### View results ####
# view summary of results
# includes the number of curves in each cluster, and the average distance of curves from the "ideal" curve 
depth_clust_k2

# size   av_dist
# 22 13.174740
# 30  9.539185

# there are a couple ways to plot results

## 1- with the output of the dtwclust function
plot(depth_clust_k2)
# this shows the centroid cluster (the one most representative of the cluster) in thick dashed line
# and the rest of the curves overlain

## 2- extract the centroid from each dtwclust object and plot with ggplot
# can find centroids (by their list number) in the output of the dtwclust function
attr(depth_clust_k2@centroids,"series_id")
## events 27 36 are the centroids for each cluster

dat_s_n_forplot = data.frame(t(dat_s_n[c(27,36),]))
names(dat_s_n_forplot) = colnames(dat_s_n_forplot)
dat_s_n_forplot$t = rownames(dat_s_n_forplot)
dat_s_n_forplot_long = dat_s_n_forplot %>% pivot_longer(cols='e27':'e36',
                                                        names_to = "event",
                                                        values_to = "fDOM.QSU.mn")
dat_s_n_forplot_long$timestep = as.numeric(gsub('t', '', dat_s_n_forplot_long$t))
dat_s_n_forplot_long =
  dat_s_n_forplot_long %>%
  mutate(cluster = case_match(event, 
                              "e27" ~ 1,"e36" ~ 2))

centriodcurvesp = 
  ggplot(dat_s_n_forplot_long, aes(x=timestep,y=fDOM.QSU.mn))+
  geom_line(linewidth=2)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized fDOM (QSU)")+
  facet_wrap(~cluster)


# plot all curves of each cluster
cluster_DTW_data_k2_long = cluster_DTW_data_k2 %>% pivot_longer(cols='t12':'t192',
                                                                names_to = "timestep",
                                                                values_to = "fDOM.QSU.mn")
cluster_DTW_data_k2_long$timestep = as.numeric(gsub('t', '', cluster_DTW_data_k2_long$timestep))

ggplot(cluster_DTW_data_k2_long, aes(x=timestep,y=fDOM.QSU.mn, color=ename))+
  geom_line(linewidth=1)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized fDOM (QSU)")+
  facet_wrap(~cluster)


# plot the mean of all curves in each cluster
mean_k2_cluster = 
  cluster_DTW_data_k2_long %>%
  group_by(cluster,timestep) %>%
  summarise(fdom_mean = mean(fDOM.QSU.mn))

meancurvesp = ggplot(mean_k2_cluster, aes(x=timestep,y=fdom_mean))+
  geom_line(linewidth=2)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized fDOM (QSU)")+
  facet_wrap(~cluster)
meancurvesp
ggsave("plots/meancurves_fdom.png", meancurvesp, width = 9, height = 8, units = "in")
#### save results ####

saveRDS(cluster_DTW_data_k2, "DTW_compiled/fdom_clusters_k2_smoothed_norm.rds")


#### Identify fdom rebounding events ####
cluster_data_k2 <- readRDS("DTW_compiled/fdom_clusters_k2_smoothed_norm.rds")
# events 1-52 are in order of how they were compiled in the timeseries matrix
# event_fdom <- rbind(SLOC_fdom,SLOW_fdom,VDOW_fdom,VDOS_fdom)
# match them up with event ID or datetime
# create another df of just the events in cluster 1 (rebounding fdom)

#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

# create table of events
do_lookup <- imap_dfr(
  BEGI_events$DO_events,
  function(site_events, site) {
    imap_dfr(
      site_events,
      function(event_df, event_name) {
        tibble(
          site = site,
          event_id = event_name,
          event_time = event_df$datetimeMT[1]  
        )
      }
    )
  }
)

# standardize datetime classes
cluster_data_k2$event_time <- as.POSIXct(cluster_data_k2$event_time, tz = "US/Mountain")
do_lookup$event_time <- as.POSIXct(do_lookup$event_time, tz = "US/Mountain")

# confirm match between BEGI events and cluster matrix
match <- cluster_data_k2$event_time - do_lookup$event_time
# should all be 0

# match cluster data with event
cluster_data_k2$event_id <-do_lookup$event_id

# dataframe of just rebounding fdom (cluster 1)
fdom_rebound <- cluster_data_k2[cluster_data_k2$cluster < 2, ]

#### Plot rebounding fDOM with DO ####
fdom_df = readRDS("EXO_compiled/BEGI_EXOz.ts.tc.rds")

# # Correct negative DO values
# fdom_df[["VDOW"]]$ODO.mg.L.mn <- fdom_df[["VDOW"]]$ODO.mg.L.mn + 0.36
# fdom_df[["VDOS"]]$ODO.mg.L.mn <- fdom_df[["VDOS"]]$ODO.mg.L.mn + 0.42
# fdom_df[["SLOW"]]$ODO.mg.L.mn <- fdom_df[["SLOW"]]$ODO.mg.L.mn + 0.32
# fdom_df[["SLOC"]]$ODO.mg.L.mn <- fdom_df[["SLOC"]]$ODO.mg.L.mn + 2.2

# for each event in fdom_rebound$event_id, plot fdom and DO 48 hours following event
for (i in c(1:nrow(fdom_rebound))){
  id = fdom_rebound$event_id[i]
  site = substr(id,1,4)
  start_time = fdom_rebound$event_time[i]
  end_time = start_time + (60*60*48)
  tempdat = fdom_df[[site]][fdom_df[[site]]$datetimeMT < (as.POSIXct(end_time))&
                                fdom_df[[site]]$datetimeMT > as.POSIXct(start_time),]
  
  #save plot 
  file_name = paste("plots/fdom_rebound/", id, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  # Create a sequence of hourly intervals
  hour_intervals <- seq(from = start_time, to = end_time, by = "1 hour")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.5,2.5))
  #rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  #abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(0,100))
  #rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  #abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="fDOM (QSU)")
  
  dev.off()
}
