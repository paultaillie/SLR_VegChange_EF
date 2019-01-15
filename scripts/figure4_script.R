# summarize soil data

#clear environment
remove(list=ls())

#load required packages
library(tidyverse)
library(gridExtra)


#read in data
#read in data
elev<-read.csv(file="raw_data/elev_samples.csv")%>%
  filter(elev>.0001)%>%
  mutate(site=substr(Plot_Name,1,2))%>%
  mutate(comm=substr(Plot_Name,3,5))%>%
  group_by(Plot_Name)%>%
  summarize(av_elev=mean(elev))%>%
  arrange(Plot_Name)

soil_2004<-read.csv(file="raw_data/soils_report_2004.csv")
soil_2017a<-read.csv(file="raw_data/soils_report_2017a.csv")
soil_2017b<-read.csv(file="raw_data/soils_report_2017b.csv")
soil_2017<-rbind(soil_2017a,soil_2017b)
soil_2017$PlotID=as.character(soil_2017$Sample.Description..1)

# create PlotID for 2004
soil_2004$PlotID<-rep(NA,nrow(soil_2004))
plotnum=as.character(soil_2004$SampleDescription1)
numchar<-nchar(plotnum)
for (i in 1:length(numchar)){
  if (numchar[i]==1){soil_2004$PlotID[i]<-paste(soil_2004$Comm[i],"0",plotnum[i],sep="")}
  else {soil_2004$PlotID[i]<-paste(soil_2004$Comm[i],plotnum[i],sep="")}
}



soil_all<-data.frame(
  Na=c(soil_2004$Na_ppm,soil_2017$Na...mg.kg.),
  Ca=c(soil_2004$Ca_ppm,soil_2017$Ca...mg.kg.),
  site=as.factor(c(substr(soil_2004$PlotID,1,2),substr(soil_2017$Sample.Description..1,1,2))),
  comm=as.factor(c(substr(soil_2004$PlotID,3,5),substr(soil_2017$Sample.Description..1,3,5))),
  year=as.factor(c(rep("2004",nrow(soil_2004)),rep("2017",nrow(soil_2017)))),
  plot=c(soil_2004$PlotID,soil_2017$Sample.Description..1))%>%
  arrange(year,plot)%>%
  mutate(elev=c(elev$av_elev,elev$av_elev))


#reorder factors
soil_all$Site<-factor(soil_all$site,levels(soil_all$site)[c(4,3,2,1,5)])
soil_all$comm<-factor(soil_all$comm,levels(soil_all$comm)[c(1,3,2)])

tiff("figure4.tiff",
     width=5,
     height=8,
     units="in",
     res=600,
     compression="lzw")
#salt
plot.salt<-soil_all%>%
  ggplot(aes(x=Site,y=Na))+
  geom_violin(position=position_dodge(width=.5),aes(fill=year),size=1)+
  scale_fill_grey()+
  theme(legend.position="bottom")+
  ylab("Soil sodium concentration (mg-Na/kg-soil)")
#elevation
plot.elev<-soil_all%>%
  filter(year==2017)%>%
  ggplot(aes(x=Site,y=elev))+
  geom_violin()+
  ylab("Elevation (m)")
grid.arrange(plot.salt,plot.elev,nrow=2)
dev.off()