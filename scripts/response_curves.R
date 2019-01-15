
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

##########################################################
# fit models and calculate predictions

# Create empty dataframe to store predictions
predictions.Na<-data.frame(matrix(NA,100,7))
predictions.elev<-data.frame(matrix(NA,100,7))

# Loop through 5 metrics to get predicted values 
responses=c("Trees",
           "Saplings",
           "Snags",
           "Shrubs",
           "Grass")
for (i in 1:length(responses)){
  response.index<-which(colnames(dat)==responses[i])
  response.dat<-dat[,response.index]
  
  dat_all<-data.frame(
    Na=dat$Na,
    elev=dat$elev,
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
  #if(responses[i]=="Grass"){
  #  response.diff<-(dat_2016$response-(dat_2004$response))
  #}else{response.diff<-(dat_2016$response-dat_2004$response)/dat_2004$response}
  response.diff<-(dat_2016$response-dat_2004$response)/dat_2004$response 
  
  # build data frame for difference between years
  dat_diff<-data.frame(
    plot=dat_2016$plot,
    site=dat_2016$site,
    response=response.diff,
    Na=((dat_2016$Na+dat_2004$Na)/2),
    elev=dat_2016$elev)
  
  # Fit the model
  summary(mod<-lm(response~                 
                       Na+elev,
                       data=dat_diff))
  
  # build new dataframe with which to calculate predictions for sodium
  Na.range1=seq(139,11547,length.out=20) #max and min are from original (unstandardized) data
  elev.range1=rep(0,20)
  newdata1<-data.frame(
    Na=standardize(Na.range1),
    elev=elev.range1)
  # Repeat for elevation predictions
  Na.range2=rep(0,20) #max and min are from original (unstandardized) data
  elev.range2=seq(0.026,0.962,length.out=20)
  newdata2<-data.frame(
    Na=Na.range2,
    elev=standardize(elev.range2))
  # Calculate predictions
  predictions.Na.temp<-as.data.frame(predict(mod,newdata1,se.fit=T))%>%
    mutate(fit=fit/10)%>%
    mutate(Metric=rep(responses[i],20))%>%
    mutate(Sodium=Na.range1)%>%
    mutate(Elevation=elev.range1)
  predictions.elev.temp<-as.data.frame(predict(mod,newdata2,se.fit=T))%>%
    mutate(fit=fit/10)%>%
    mutate(Metric=rep(responses[i],20))%>%
    mutate(Sodium=Na.range2)%>%
    mutate(Elevation=elev.range2)
  # Assign predictions to empty dataframe
  predictions.Na[((20*(i-1))+1):(i*20),]<-predictions.Na.temp
  predictions.elev[((20*(i-1))+1):(i*20),]<-predictions.elev.temp
  
}# End loop
colnames(predictions.Na)<-colnames(predictions.Na.temp)
colnames(predictions.elev)<-colnames(predictions.elev.temp)

####################################################
###  Plot
tiff("figure2.tiff",
     width=5,
     height=8,
     units="in",
     res=600,
     compression="lzw")

plot1<-ggplot(predictions.Na)+
  geom_line(aes(x=Sodium,y=fit,color=Metric),size=2)+
  ylab("Proportional change in vegetation")+
  ylim(-1,1)+xlab("Soil sodium concentration (mg-Na/kg-soil)")

plot2<-ggplot(predictions.elev)+
  geom_line(aes(x=Elevation,y=fit,color=Metric),size=2)+
  ylab("Proportional change in vegetation")+
  ylim(-1,1)+xlab("Elevation (m)")

grid.arrange(plot1,plot2,ncol=1)
dev.off()