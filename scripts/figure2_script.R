# investigate differences in vegetation between the communities

#clear environment
remove(list=ls())

#load required packages
library(tidyverse)

#read in data
dat_all<-read.csv(file="raw_data/by_plot_veg.csv")

#formatting  --------------
dat_all$site=factor(substr(dat_all$plots,1,2))
dat_all$year=as.factor(dat_all$year)


dat<-dat_all%>%
  gather(key="response",value="value",grass.cover:snag.density)%>%
  dplyr::select(plots,year,site,response,value)%>% 
  spread(key="year",value=value,convert=T)%>%
  #filter(`2004`!=0)%>%  # remove plots with 0 change
  mutate(diff=(`2016`-`2004`))%>%
  mutate(Community=factor(substr(plots,3,5),levels=c("FOR","TRA","MAR")))%>%
  mutate(Site=factor(site,levels=c("PP","MA","LS","GR","SW")))%>%
  mutate(response=factor(response,levels=c("mature.tree.density",
                                           "sapling.density",
                                           "snag.density",
                                           "shrub.density",
                                           "grass.cover")))%>%
  ggplot()+
  geom_point(aes(x=Site,y=diff,color=Community),
             position=position_jitter(width=.2,height=.05))+
  #scale_y_continuous(breaks=c(-1,0,1))+
  facet_grid(response~.,scales="free_y")+
  ylab("Change in Density")
dat
