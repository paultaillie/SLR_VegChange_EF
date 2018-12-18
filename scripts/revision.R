
# investigate differences in vegetation between the communities

#clear environment
remove(list=ls())

#load required packages
library(tidyverse)
library(MASS)
library(gridExtra)
library(lme4)

# helper function
standardize<-function(xx){  
  xx.mean=mean(xx,na.rm=T)
  xx.sd=sd(xx,na.rm=T)
  xx.standardized<-(xx-xx.mean)/xx.sd
  return(xx.standardized)
}

#read in data
elev<-read.csv(file="raw_data/elev_samples.csv")%>%
  filter(elev>.0001)%>%
  mutate(site=substr(Plot_Name,1,2))%>%
  mutate(comm=substr(Plot_Name,3,5))%>%
  group_by(Plot_Name)%>%
  summarize(av_elev=mean(elev))%>%
  mutate(av_elev=standardize(av_elev))%>%
  arrange(Plot_Name)

dat<-read.csv(file="raw_data/by_plot_veg.csv")%>%
  rename(Grass=grass.cover)%>%
  rename(Trees=mature.tree.density)%>%
  rename(Saplings=sapling.density)%>%
  rename(Snags=snag.density)%>%
  rename(Shrubs=shrub.density)%>%
  arrange(year,plots)%>%
  mutate(elev=c(elev$av_elev,elev$av_elev))%>%
  mutate(Na=standardize(Na))%>%
  mutate(Dist=standardize(dist_to_shore))%>%
  mutate(site=factor(substr(plots,1,2)))%>%
  mutate(site=factor(substr(plots,3,5)))%>%
  mutate(yr=factor(year))

response="Snags"
response.index<-which(colnames(dat)==response)
response.dat<-dat[,response.index]

dat_all<-data.frame(
  Na=dat$Na,
  elev=dat$elev,
  dist=dat$Dist
  site=dat$site,
  response=ceiling(response.dat),
  year=dat$year,
  plot=dat$plots)

#separate data into years
dat_2004_all<-dat_all[which(dat_all$year==2004),]
dat_2016_all<-dat_all[which(dat_all$year==2016),] 

#remove rows where 2004 response value = 0
index.row<-which(dat_2004_all$response!=0)
dat_2004<-dat_2004_all[index.row,]
dat_2016<-dat_2016_all[index.row,]


#no proportional change for grass cover
if(response=="Grass"){
  response.diff<-(dat_2016$response-(dat_2004$response))
}else{response.diff<-(dat_2016$response-dat_2004$response)/dat_2004$response}


# build data frame for difference between years
dat_diff<-data.frame(
  plot=dat_2016$plot,
  site=dat_2016$site,
  response=response.diff,
  Na=((dat_2016$Na+dat_2004$Na)/2),
  elev=dat_2016$elev,
  dist=dat_2016$dist)


summary(diff_mod<-lm(response~                 
                        Na+elev+dist,
                      data=dat_diff))

################
#interaction
interaction.mod<-lm(Saplings~Na+elev,data=dat)
summary(interaction.mod)







