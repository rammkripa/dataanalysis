library(dplyr)
library(ggplot2)
data2019 = read.csv("~/Downloads/berkeleypollutiondata2019.csv")
data2010 = read.csv("~/Downloads/berkeleypollutiondata2010.csv")
berkeley2019 = filter(data2019,Site.Name=="Berkeley Aquatic Park")
berkeley2010 = filter(data2010,Site.Name=="Berkeley")
m1 = mean(berkeley2010$Daily.Mean.PM2.5.Concentration,na.rm=TRUE)
m2 = mean(berkeley2019$Daily.Mean.PM2.5.Concentration,na.rm=TRUE)
sd1 = sd(berkeley2010$Daily.Mean.PM2.5.Concentration,na.rm=TRUE)
sd2 = sd(berkeley2019$Daily.Mean.PM2.5.Concentration,na.rm=TRUE)
se1 = sd1/sqrt(length(berkeley2010$Daily.Mean.PM2.5.Concentration))
se2 = sd2/sqrt(length(berkeley2019$Daily.Mean.PM2.5.Concentration))
setot = sqrt((se1^2)+(se2^2))
z = (m2-m1)/setot
p = 1-pnorm(z)
# reject null at 5% level
# looks like not chance
# Pollution increased in the past 10 years
ggplot()+
  geom_point(data=berkeley2019,color="green",aes(x=Date,y=Daily.Mean.PM2.5.Concentration))+
  geom_point(data=berkeley2010,color="red",aes(x=Date,y=Daily.Mean.PM2.5.Concentration))
## Do maps
library(ggmap)
library(maps)
library(mapdata)

us_states = map_data("state")

usa = map_data("usa")

america_map = ggplot()+
  geom_polygon(data=usa, aes(x=long,y=lat,group=group),fill="green",color="red")+
  coord_fixed(1.3)

states_map = ggplot()+
  geom_polygon(data=us_states, aes(x=long,y=lat,group=group,fill=factor(region)),color="red")+
  coord_fixed(1.3)+
  guides(fill=FALSE)

california = filter(us_states,region=='california')

site_mean = function(city){
  mean(filter(data2019,Site.Name==city)$DAILY_AQI_VALUE)
}
site_mean("Berkeley Aquatic Park")
site_means_2019 = c(site_mean("Berkeley Aquatic Park"),site_mean("Pleasanton - Owens Ct"),site_mean("Livermore")
                    ,site_mean("Laney College"),site_mean("Oakland"), site_mean("Oakland West"))
sites = c("Berkeley Aquatic Park","Pleasanton - Owens Ct","Livermore","Laney College","Oakland","Oakland West")

ggplot()+
  geom_polygon(data=california,aes(x=lat,y=long),fill="skyblue",color="black")+
  geom_point(data=data2019,aes(x=SITE_LATITUDE,y=SITE_LONGITUDE,color="red"))+
  coord_fixed(1.3)+
  guides(fill=FALSE)
  
