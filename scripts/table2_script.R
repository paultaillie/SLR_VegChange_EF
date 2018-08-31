# table of vegetation densities

#clear environment
remove(list=ls())

#load required packages
library(tidyverse)

#read in data
dat.in<-read.csv(file="raw_data/by_plot_veg.csv")
dat<-dat.in%>%
  dplyr::select(-c(X,Ca,dist_to_shore))%>%
  gather(response,value,c(grass.cover:snag.density,Na))%>%
  mutate(comm=factor(substr(plots,3,5)))%>%
  mutate(site=substr(plots,1,2))%>%
  mutate(site=factor(substr(plots,1,2),levels=c("PP","MA","LS","GR","SW")))%>%
  group_by(year,comm,site,response)%>%
  summarize(sd(value))%>%
  spread(comm,`sd(value)`)




