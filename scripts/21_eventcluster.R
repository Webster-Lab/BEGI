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

####################################################################
##### Starting with fDOM clusters in 36 hours following DO event####
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
  end_time   <- start_time + 36 * 60 * 60
  
  tempdat <- fdom_df[["SLOC"]][
    fdom_df[["SLOC"]]$datetimeMT >= start_time & fdom_df[["SLOC"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_SLOC <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_SLOC) >= 144) {
    ts_values <- valid_SLOC[1:144]
  } else {
    ts_values <- c(valid_SLOC, rep(NA, 144 - length(valid_SLOC)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOC_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(SLOC_fdom) <- paste0("t", seq_len(144))
SLOC_fdom$event_time <- BEGI_events[["Eventdate"]][["SLOC_dates"]]


#SLOW#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["SLOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 36 * 60 * 60
  
  tempdat <- fdom_df[["SLOW"]][
    fdom_df[["SLOW"]]$datetimeMT >= start_time & fdom_df[["SLOW"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_SLOW <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_SLOW) >= 144) {
    ts_values <- valid_SLOW[1:144]
  } else {
    ts_values <- c(valid_SLOW, rep(NA, 144 - length(valid_SLOW)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOW_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(SLOW_fdom) <- paste0("t", seq_len(144))
SLOW_fdom$event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]]


#VDOW#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 36 * 60 * 60
  
  tempdat <- fdom_df[["VDOW"]][
    fdom_df[["VDOW"]]$datetimeMT >= start_time & fdom_df[["VDOW"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_VDOW <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_VDOW) >= 144) {
    ts_values <- valid_VDOW[1:144]
  } else {
    ts_values <- c(valid_VDOW, rep(NA, 144 - length(valid_VDOW)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOW_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(VDOW_fdom) <- paste0("t", seq_len(144))
VDOW_fdom$event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]]


#VDOS#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOS_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 36 * 60 * 60
  
  tempdat <- fdom_df[["VDOS"]][
    fdom_df[["VDOS"]]$datetimeMT >= start_time & fdom_df[["VDOS"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_VDOS <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_VDOS) >= 144) {
    ts_values <- valid_VDOS[1:144]
  } else {
    ts_values <- c(valid_VDOS, rep(NA, 144 - length(valid_VDOS)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOS_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(VDOS_fdom) <- paste0("t", seq_len(144))
VDOS_fdom$event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]]



#combine all dtw dataframes
event_fdom <- rbind(SLOC_fdom,SLOW_fdom,VDOW_fdom,VDOS_fdom)
#save
saveRDS(event_fdom, "EXO_compiled/event_fdom.rds")


#### load and wrangle data ####

dat = readRDS("EXO_compiled/event_fdom.rds") 
# I need to make an equivalent for the (estimated) duration of each event. Which is challenging because the timeframe of during each event varies
# I started with the 36 hour period of fDOM following each event.
# then I can take the average event length to look at gw depth curves during a DO event

dat$ename = paste("e", c(1:52), sep="")
rownames(dat) = dat$ename

# save date/time stamps of events separately 
times = dat[,145:146]
dat[,145:146] = NULL

# gap fill fdom series
dat_filled <- t(apply(dat_qc, 1, function(e) {
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
  to = as.POSIXct("2025-06-10 01:00:00", tz="US/Mountain")+129600,
  by = "15 min"))[1:144]
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
names(dat_s) = paste("t", c(1:144), sep="")
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
for(i in c(1:9)){
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
# looks like with new data, 2 may be ideal, then 3

# look at cluster no. of 2 to 6 max
# NOTE: this can take a long time to run! suggest running on a computer with lots of RAM/memory...
depth_clust_k <- tsclust(series=dat_s_n, k = 2:7, centroid="shape", distance = "dtw_basic")
names(depth_clust_k) <- paste0("k_", 2:7)
# Warning message:
#   In tsclust(series = dat_s_n, k = 2:7, centroid = "shape", distance = "dtw_basic") :
#   At least one clustering did not converge within the allowed iterations.
# "pam" centroid likely more optimal
k_table<-sapply(depth_clust_k, cvi, type = "internal")
# print table
k_table
# Note:
## some indices should maximized ("Sil","SF","CH","D") and some should be minimized ("DB","DBstar","COP")
# looks like with new data, 2 or 4 may be ideal
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
# run analysis with k=4
depth_clust_k4 <- tsclust(series = dat_s_n, k = 4, distance = 'dtw_basic',centroid="pam")
depth_clust_k4
plot(depth_clust_k4)

# run analysis with k=2
depth_clust_k2 <- tsclust(series = dat_s_n, k = 2, distance = 'dtw_basic',centroid="pam")
depth_clust_k2
plot(depth_clust_k2)

# run analysis with k=3
depth_clust_k3 <- tsclust(series = dat_s_n, k = 3, distance = 'dtw_basic',centroid="pam")
depth_clust_k3
plot(depth_clust_k3)
#optimal

# # run analysis with k=4
# depth_clust_k4_dba <- tsclust(series = dat_s_n, k = 4, distance = 'dtw_basic',centroid="dba")
# 
# # run analysis with k=4
# depth_clust_k2_shape <- tsclust(series = dat_s_n, k = 2, distance = 'dtw_basic',centroid="shape")
# depth_clust_k2_shape
# plot(depth_clust_k2_shape)
# # run analysis with k=4
# depth_clust_k4_shape <- tsclust(series = dat_s_n, k = 4, distance = 'dtw_basic',centroid="shape")
# depth_clust_k4_shape
# plot(depth_clust_k4_shape)

#### Merge Cluster Data With original Data ####

# format cluster data
## 52 was the # of original curves
cluster_data1<-as.data.frame(list(DTW=list(cumsum(rep(1,52))),cluster=list(depth_clust_k3@cluster)))
colnames(cluster_data1)<-c("DTW_id","cluster")
# merge data df's together
cluster_DTW_data_k3<-cbind(times, cluster_data1,dat_s_n)
# 
# # format cluster data
# ## 59 was the # of original curves
# cluster_data1_dba<-as.data.frame(list(DTW=list(cumsum(rep(1,59))),cluster=list(depth_clust_k4_dba@cluster)))
# colnames(cluster_data1_dba)<-c("DTW_id","cluster")
# # merge data df's together
# cluster_DTW_k4_dba<-cbind(times, cluster_data1_dba,dat_s_n)
# 
# # format cluster data
# ## 59 was the # of original curves
# cluster_data1_shape<-as.data.frame(list(DTW=list(cumsum(rep(1,59))),cluster=list(depth_clust_k4_shape@cluster)))
# colnames(cluster_data1_shape)<-c("DTW_id","cluster")
# # merge data df's together
# cluster_DTW_k4_shape<-cbind(times, cluster_data1_shape,dat_s_n)


#### View results ####
# view summary of results
# includes the number of curves in each cluster, and the average distance of curves from the "ideal" curve 
depth_clust_k3
# depth_clust_k4_dba
# depth_clust_k4_shape

# there are a couple ways to plot results

## 1- with the output of the dtwclust function
plot(depth_clust_k3)
# plot(depth_clust_k4_dba)
# plot(depth_clust_k4_shape)
# this shows the centroid cluster (the one most representative of the cluster) in thick dashed line
# and the rest of the curves overlain

## 2- extract the centroid from each dtwclust object and plot with ggplot
# can find centroids (by their list number) in the output of the dtwclust function
attr(depth_clust_k3@centroids,"series_id")
## events 8 52 34 are the centroids for each cluster

dat_s_n_forplot = data.frame(t(dat_s_n[c(8,52,34),]))
names(dat_s_n_forplot) = colnames(dat_s_n_forplot)
dat_s_n_forplot$t = rownames(dat_s_n_forplot)
dat_s_n_forplot_long = dat_s_n_forplot %>% pivot_longer(cols='e52':'e8',
                                                        names_to = "event",
                                                        values_to = "DTW_m")
dat_s_n_forplot_long$timestep = as.numeric(gsub('t', '', dat_s_n_forplot_long$t))
dat_s_n_forplot_long =
  dat_s_n_forplot_long %>%
  mutate(cluster = case_match(event, 
                              "e8" ~ 1,
                              "e52" ~ 2,
                              "e34" ~ 3))

centriodcurvesp = 
  ggplot(dat_s_n_forplot_long, aes(x=timestep,y=DTW_m))+
  geom_line(linewidth=2)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized Depth (m)")+
  facet_wrap(~cluster)


# plot all curves of each cluster
cluster_DTW_data_k3_long = cluster_DTW_data_k3 %>% pivot_longer(cols='t1':'t144',
                                                                names_to = "timestep",
                                                                values_to = "DTW_m")
cluster_DTW_data_k3_long$timestep = as.numeric(gsub('t', '', cluster_DTW_data_k3_long$timestep))

ggplot(cluster_DTW_data_k3_long, aes(x=timestep,y=DTW_m, color=ename))+
  geom_line(linewidth=1)+
  theme_classic()+
  xlab("Time (min)")+ylab("Depth (m)")+
  facet_wrap(~cluster)


# plot the mean of all curves in each cluster
mean_k3_cluster = 
  cluster_DTW_data_k3_long %>%
  group_by(cluster,timestep) %>%
  summarise(DTW_m_mean = mean(DTW_m))

meancurvesp = ggplot(mean_k3_cluster, aes(x=timestep,y=DTW_m_mean))+
  geom_line(linewidth=2)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized Depth (m)")+
  facet_wrap(~cluster)
#ggsave("plots/meancurvesp.png", meancurvesp, width = 9, height = 8, units = "in")
#### save results ####

#write.csv(cluster_DTW_data_k3, "DTW_compiled/DTW_clusters_k3_smoothed_norm.csv", row.names = FALSE)

#### FOR SMOOTHED ONLY DATA ####
#### Evaluate Optimal Cluster Number ####

# NOTE: notes are mostly from the turtle dives paper's code
## Evaluate "optimal" cluster number using CVI's (cluster validity indices)
## internal CVI's: consider partioned data and try to define measure of cluster purity
## external CVI's: compare obtained partition to correct one (need a ground truth for this- we won't use these)
## note: which CVI to use is also subjective/needs testing... can go with "majority vote" from indices but you should check that the final result makes biological sense!
## note: can also use "clue" package to evaluate clusters

# look at cluster no. of 2 to 6 max
# NOTE: this can take a long time to run! suggest running on a computer with lots of RAM/memory...
depth_clust_k <- tsclust(series=dat_s, k = 2:7, centroid="pam", distance = "dtw_basic")
names(depth_clust_k) <- paste0("k_", 2:7)
k_table<-sapply(depth_clust_k, cvi, type = "internal")

# print table
k_table
# Note:
## some indices should maximized ("Sil","SF","CH","D") and some should be minimized ("DB","DBstar","COP")
# k=2 and k=4 could be ideal
#### Performing DTW Clustering ####

# Choices of parameters for dtw clustering:

# window size: limits distance that points can be matched to each other
## I don't want a limit ( I want all observations for a depth to be considered)

# k = no. of clusters
## can use selection criteria with dtwclust package to determine optimal no. for k
## we chose k=4 above

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
# run analysis with k=2
depth_clust_k2 <- tsclust(series = dat_s, k = 2, distance = 'dtw_basic',centroid="pam")
depth_clust_k2
plot(depth_clust_k2)

# run analysis with k=4
depth_clust_k4 <- tsclust(series = dat_s, k = 4, distance = 'dtw_basic',centroid="pam")
depth_clust_k4
plot(depth_clust_k4)

#### Merge Cluster Data With original Data ####
###############################################
#### haven't revised from original analysis yet ####
# format cluster data
## 59 was the # of original curves
cluster_data2<-as.data.frame(list(DTW=list(cumsum(rep(1,59))),cluster=list(depth_clust_k3@cluster)))
colnames(cluster_data2)<-c("DTW_id","cluster")

# merge data df's together
cluster_DTW_smoothed_k3<-cbind(times, cluster_data2, dat_s)


#### View Results ####

# view summary of results
# includes the number of curves in each cluster, and the average distance of 
depth_clust_k3


# there are a couple ways to plot results

## 1- with the output of the dtwclust function
plot(depth_clust_k3)
# this shows the centroid cluster (the one most representative of the cluster) in thick dashed line
# and the rest of the curves overlain

## 2- extract the centroid from each dtwclust object and plot with ggplot


#### save results ####

# viewing results compared to the normalized version of this data, this approach is pretty clearly inferior to me since I don't see much coherence in clusters 2 and 3 of the kinds of patterns we are interested in, and their average distances are much larger. I won't save this since I don't think we'll use it. 

#write.csv(cluster_DTW_smoothed_k3, "DTW_compiled/DTW_clusters_k3_smoothed.csv", row.names = FALSE)




#### plot cluster with Well ID ####
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

well_clusters<-ggplot(cluster_DTW_data_k3_long, aes(x=timestep,y=DTW_m, group=ename, color=well_id))+
  geom_line(linewidth=1,)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized Depth (m)")+
  facet_wrap(~cluster) +
  theme(text = element_text(size = 20))+
  scale_color_manual(values=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))

well_clusters

final_cluster <- well_clusters / tableGrob(cluster_by_well) +
  plot_layout(heights = c(4,1))


ggsave("plots/well_clusters.png", well_clusters, width = 12, height = 7, units = "in")
ggsave("plots/final_cluster.png", final_cluster, width = 14, height = 9, units = "in")

