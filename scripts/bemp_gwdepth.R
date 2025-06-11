#### read me ####

# the purpose of this script is to visualize BEMP's depth to gw data

#### libraries ####

library(tidyverse)
library(lubridate)

#### load data ####

dat = read.csv("/BEGI/bemp_data/depth_to_groundwater_data/data", na.strings = c("."))

#### format date/time ####

# examine date format
head(dat$date)
tail(dat$date)
?strptime
OlsonNames()
# date format is "%m/%d/%y %H:%M", tz is "MST"
# test date date
head(dat$Sample_DateTime)
head(as.Date(dat$date))

# create new date/time col with correct format (always keep the original!)
dat$date_f = as.Date(dat$date)

#### filter data ####

dat_f = dat %>% filter(site_name %in% c("State Land Office", "Valle de Oro"))


#### calculate gw depth ####

# bemp data has only casing height and depth to gw from surface or top of casing. 
# I need depth of gw from top of water table to bottom of well. 
# I have total well depth from site visit on 2023-07-06 for most of the wells at SLO and two wells at VDO
# 
# well_depth_df = data.frame(center_well_total_depth_cm = c(300,NA,NA,NA,NA,NA),
#                            west_well_total_depth_cm = c(NA,449,365,NA,NA,NA),
#                            north_well_total_depth_cm = c(NA,NA,NA,275,NA,NA),
#                            south_well_total_depth_cm = c(NA,NA,NA,NA,349,NA),
#                            east_well_total_depth_cm = c(NA,NA,NA,NA,NA,333),
#                            site_name = c("Valle de Oro","Valle de Oro","State Land Office",
#                                          "State Land Office","State Land Office","State Land Office")) 

well_depth_df = data.frame(center_well_total_depth_cm = c(300,170),
                           west_well_total_depth_cm = c(449,365),
                           north_well_total_depth_cm = c(NA,420),
                           south_well_total_depth_cm = c(NA,349),
                           east_well_total_depth_cm = c(NA,333),
                           site_name = c("Valle de Oro","State Land Office")) 

dat_f = left_join(dat_f, well_depth_df, by="site_name")

dat_f$center_subsurface_gw_depth_cm = (dat_f$center_well_total_depth_cm - dat_f$center_case_height) - dat_f$center_groundwater_depth_cm
dat_f$west_subsurface_gw_depth_cm = (dat_f$west_well_total_depth_cm - dat_f$west_case_height) - dat_f$west_groundwater_depth_cm
dat_f$north_subsurface_gw_depth_cm = (dat_f$north_well_total_depth_cm - dat_f$north_case_height) - dat_f$north_groundwater_depth_cm
dat_f$south_subsurface_gw_depth_cm = (dat_f$south_well_total_depth_cm - dat_f$south_case_height) - dat_f$south_groundwater_depth_cm
dat_f$east_subsurface_gw_depth_cm = (dat_f$east_well_total_depth_cm - dat_f$east_case_height) - dat_f$east_groundwater_depth_cm

  

#### plot ####

# 
# table(dat_f$site_name)
# 
# SLO_gw = dat_f %>% 
#   filter(site_name =="State Land Office") %>% 
#   arrange(date_f)
# 

# east wells
east_subwd = 
  ggplot(data=dat_f, aes(x=date_f, y=east_subsurface_gw_depth_cm))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=40, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")
summary(dat_f$east_subsurface_gw_depth_cm)
east_d2w = 
  ggplot(data=dat_f, aes(x=date_f, y=east_groundwater_depth_cm*-1))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=-300, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "green")

# center wells
center_subwd = 
  ggplot(data=dat_f, aes(x=date_f, y=center_subsurface_gw_depth_cm))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=40, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")
summary(dat_f$center_subsurface_gw_depth_cm)
center_d2w = 
  ggplot(data=dat_f, aes(x=date_f, y=center_groundwater_depth_cm*-1))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=-300, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "green")

# west wells
west_subwd = 
  ggplot(data=dat_f, aes(x=date_f, y=west_subsurface_gw_depth_cm))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=40, linetype="dashed", color = "red")
summary(dat_f$west_subsurface_gw_depth_cm)
west_d2w = 
  ggplot(data=dat_f, aes(x=date_f, y=west_groundwater_depth_cm*-1))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=-300, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "green")

# north wells
north_subwd = 
  ggplot(data=dat_f, aes(x=date_f, y=north_subsurface_gw_depth_cm))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=40, linetype="dashed", color = "red")
summary(dat_f$north_subsurface_gw_depth_cm)
north_d2w = 
  ggplot(data=dat_f, aes(x=date_f, y=north_groundwater_depth_cm*-1))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=-300, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "green")

# south wells
south_subwd = 
  ggplot(data=dat_f, aes(x=date_f, y=south_subsurface_gw_depth_cm))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=40, linetype="dashed", color = "red")
summary(dat_f$south_subsurface_gw_depth_cm)
south_d2w = 
  ggplot(data=dat_f, aes(x=date_f, y=south_groundwater_depth_cm*-1))+
  geom_point() + geom_path()+
  facet_wrap(~site_name)+
  geom_hline(yintercept=-300, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "green")

gridExtra::grid.arrange(north_d2w, east_d2w, south_d2w, west_d2w, center_d2w)
gridExtra::grid.arrange(north_subwd, east_subwd, south_subwd, west_subwd, center_subwd)

