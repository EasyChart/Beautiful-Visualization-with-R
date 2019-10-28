
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts


library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(scatterpie)
#library(wesanderson)

dataProjected <- readOGR("Virtual_Map1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
watershedDF <- full_join(watershedPoints, dataProjected@data,by='id')
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_Map <- full_join(watershedPoints, dataProjected@data, by = "id")

df_city<-read.csv("Virtual_City.csv")  

Map_Scale<-0.75
min_lat<-min(df_Map$lat) #30.11628
max_lat<-max(df_Map$lat) #59.94186
min_long<-min(df_Map$long) #105.2945
max_long<-max(df_Map$long) #134.8317

df_Map$x<-(df_Map$long-min_long)/(max_long-min_long)
df_Map$y<-(df_Map$lat-min_lat)/(max_lat-min_lat)*Map_Scale


df_city$x<-(df_city$long-min_long)/(max_long-min_long)
df_city$y<-(df_city$lat-min_lat)/(max_lat-min_lat)*Map_Scale

labels_x<-seq(110,135,10)
breaks_x<-(labels_x-min_long)/(max_long-min_long)

labels_y<-seq(30,60,10)
breaks_y<-(labels_y-min_lat)/(max_lat-min_lat)


df_city$Sumindex<-rowSums(df_city[,c("orange","apple","banana","watermelon")])
Bubble_Scale<-0.04
radius<-sqrt(df_city$Sumindex/pi)
Max_radius<-max(radius)
df_city$radius<-radius/Max_radius*Bubble_Scale

#(a) 散点复合饼图
ggplot()+
  geom_polygon(data=df_Map, aes(x=x, y=y, group=group),fill='white',colour="black",size=0.25)+ 
  geom_scatterpie(data=df_city,aes(x=x, y=y, group=city,r=0.03),# 0.03
                  cols=c("orange","apple","banana","watermelon"), color="black", alpha=1,size=0.1)+
  scale_x_continuous(breaks=breaks_x,labels=labels_x)+
  scale_y_continuous(breaks=breaks_y,labels=labels_y)+
  xlab("long") + 
  ylab("lat") +
  coord_fixed()

#(b) 气泡复合饼图
ggplot()+
  geom_polygon(data=df_Map, aes(x=x, y=y, group=group),fill='white',colour="black",size=0.25)+ 
  geom_scatterpie(data=df_city,aes(x=x, y=y, group=city,r=radius),# 0.03
                   cols=c("orange","apple","banana","watermelon"), color="black", alpha=1,size=0.1)+
  geom_scatterpie_legend(df_city$radius, x=0.9, y=0.1, 
                          n=5,
                         labeller=function(x) round((x* Max_radius/Bubble_Scale)^2*pi))+
  scale_x_continuous(breaks=breaks_x,labels=labels_x)+
  scale_y_continuous(breaks=breaks_y,labels=labels_y)+
  xlab("long") + 
  ylab("lat") +
  coord_fixed()

