

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(grid)

#---------------------df_China:China Region-----------------------------------

dataProjected <- readOGR("China_adm_shp/gadm34_CHN_shp/gadm34_CHN_2.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_mainland <- full_join(watershedPoints, dataProjected@data, by = "id")
df_mainland$group<-as.numeric(as.character(df_mainland$group))

dataProjected <- readOGR("China_adm_shp/gadm34_TWN_shp/gadm34_TWN_2.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_taiwan <- full_join(watershedPoints, dataProjected@data, by = "id")
df_taiwan$group<-as.numeric(as.character(df_taiwan$group))+max(df_mainland$group)

df_China<-rbind(df_mainland,df_taiwan)
df_China$group<-as.factour(df_China$group)
df_China$class<-rep("Mainland",nrow(df_China))


#---------------------df_Nanhai:Nanhai Region-----------------------------------
df_NanHaiLine <- read.csv("中国南海九段线.csv")  
colnames(df_NanHaiLine)<-c("long","lat","ID")

Width<-9
Height<-9
long_Start<-124
lat_Start<-16

df_Nanhai<-df_China[df_China$long>106.55 & df_China$long<123.58,]
df_Nanhai<-df_Nanhai[df_Nanhai$lat>4.61 & df_Nanhai$lat<25.45,]

min_long<-min(min(df_Nanhai$long, na.rm = TRUE),min(df_NanHaiLine$long))
min_lat<-min(min(df_Nanhai$lat, na.rm = TRUE),min(df_NanHaiLine$lat))
max_long<-max(min(df_Nanhai$long, na.rm = TRUE),max(df_NanHaiLine$long))
max_lat<-max(max(df_Nanhai$lat, na.rm = TRUE),max(df_NanHaiLine$lat))

df_Nanhai$long<-(df_Nanhai$long-min_long)/(max_long-min_long)*Width+long_Start
df_Nanhai$lat<-(df_Nanhai$lat-min_lat)/(max_lat-min_lat)*Height+lat_Start
df_Nanhai$class<-rep("NanHai",nrow(df_Nanhai))

df_China<-rbind(df_China,df_Nanhai)


df_NanHaiLine$long<-(df_NanHaiLine$long-min_long)/(max_long-min_long)*Width+long_Start
df_NanHaiLine$lat<-(df_NanHaiLine$lat-min_lat)/(max_lat-min_lat)*Height+lat_Start

#---------------------------------df_China:China Region-----------------------------------
dataProjected <- readOGR("China_adm_shp/bou1_4m/bou1_4p.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map4 <- full_join(watershedPoints, dataProjected@data, by = "id")

ggplot()+
  geom_polygon(data=df_map4, aes(x=long, y=lat, group=group), fill="gray",colour="black",size=0.25)+ 
  geom_polygon(data=df_China, aes(x=long, y=lat, group=interaction(class,group)),
               fill="gray",colour="black",size=0.25)+ #中国地图
  geom_rect(aes(xmin=124, xmax=125+Width, ymin=15, ymax=16+Height),fill=NA,colour="black",size=0.25)+
  geom_line(data=df_NanHaiLine, aes(x=long, y=lat, group=ID), colour="black", size=1)+ #中国南海九段线
  ylim(15,55)+
  theme_void()



