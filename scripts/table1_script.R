
# investigate relationship between vegetation change and each of elevation
# and soil sodium concentration.

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
  mutate(Na=standardize(Na))


#formatting  --------------
dat$site=factor(substr(dat$plots,1,2))
dat$comm=factor(substr(dat$plots,3,5))
dat$year=as.factor(dat$year)

######

# correlation between elevation and salt
plot(dat$Na~dat$elev,ylim=c(-2,2.5),xlim=c(-2,2.5))
cor(data.frame(Na=dat$Na,elev=dat$elev))
dat04<-filter(dat,year==2004)
cor(data.frame(Na=dat04$Na,elev=dat04$elev))
dat16<-filter(dat,year==2016)
cor(data.frame(Na=dat16$Na,elev=dat16$elev))


# ANOVA to test for differences in elevation and salt among plots and between 
# years (salt only)

#salt
dat.anova<-data.frame(year=dat$year,Na=dat$Na,site=dat$site)
summary(out.anova<-aov(Na~year*site,data=dat.anova))
summary(out.anova<-aov(Na~year,data=dat.anova))

TukeyHSD(out.anova, which = "site")
#elev
dat.anova.elev<-dat%>%
  filter(year==2016)%>%
  dplyr::select(site,elev)
summary(out.anova.elev<-aov(elev~site,data=dat.anova.elev))
TukeyHSD(out.anova.elev, which = "site")
summary(glm(elev~site,data=dat.anova.elev))
#difference in salt between years
dat%>%
  group_by(year)%>%
  summarize(av=mean(Na))


 ######################################################################
# Function to calculate differences in each of the vegetation metrics, model
# results, output figure
saltelev<-function(dat,community,response){
  response.index<-which(colnames(dat)==response)
  if(community!="ALL"){
    community.index<-which(dat$comm==community)
    dat.comm<-dat[community.index,]}else{dat.comm<-dat}
  response.dat<-dat.comm[,response.index]
  
  dat_all<-data.frame(
    Na=dat.comm$Na,
    elev=dat.comm$elev,
    site=dat.comm$site,
    response=ceiling(response.dat),
    year=dat.comm$year,
    plot=dat.comm$plots)
  
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
    elev=dat_2016$elev)
  dat_diff2<-dat_diff%>%
    mutate(Na.standard=standardize(Na))%>%
    mutate(elev.standard=standardize(elev))
  
  summary(diff_mod<-glm(response~                 
                          Na.standard+
                          elev.standard,
                        data=dat_diff2))
  
  #plot linear relationship
  dat_diff%>%
    ggplot()+
    geom_point(aes(x=Na,y=response))+
    geom_smooth(aes(x=Na,y=response),method=lm)
  
  
  #store and output results
  results<-c(summary(diff_mod)$coefficients[2,c(1,2,4)],
             summary(diff_mod)$coefficients[3,c(1,2,4)])
  plot.temp<-
    
    
  return(round(results,3))
}

# Table 1
table.out<-rbind(
  saltelev(dat,"ALL","Trees"),
  saltelev(dat,"ALL","Saplings"),
  saltelev(dat,"ALL","Snags"),
  saltelev(dat,"ALL","Shrubs"),
  saltelev(dat,"ALL","Grass"))

round(table.out,3)




