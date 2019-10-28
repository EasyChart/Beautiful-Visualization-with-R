
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#Reference: https://stackoverflow.com/questions/47493197/how-to-draw-two-half-circles-in-ggplot-in-r/47493452#47493452

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggforce)
library(reshape2)


dataProjected <- readOGR("Virtual_Map1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
watershedDF <- full_join(watershedPoints, dataProjected@data,by='id')

df_city<-read.csv("Virtual_City.csv")  

Map_Scale<-0.75
min_lat<-min(df_Map$lat) #30.11628
max_lat<-max(df_Map$lat) #59.94186
min_long<-min(df_Map$long) #105.2945
max_long<-max(df_Map$long) #134.8317

df_Map$x<-(df_Map$long-min_long)/(max_long-min_long)
df_Map$y<-(df_Map$lat-min_lat)/(max_lat-min_lat)*Map_Scale


df_city<-df_city[c("lat","long","city","orange","apple")]
df_city<-melt(df_city,id.vars=c("lat","long","city"))


#(a) 水平双气泡（两个数据系列）------------------------------------------------------------------
df_city$start<- rep(c(-pi/2, pi/2), each=nrow(df_city)/2)
df_city$vjust<- rep(c(1, -1), each=nrow(df_city)/2)
r <- 0.04
scale <- r/max(sqrt(df_city$value))

df_city$x<-(df_city$long-min_long)/(max_long-min_long)
df_city$y<-(df_city$lat-min_lat)/(max_lat-min_lat)*Map_Scale

labels_x<-seq(110,135,10)
breaks_x<-(labels_x-min_long)/(max_long-min_long)

labels_y<-seq(30,60,10)
breaks_y<-(labels_y-min_lat)/(max_lat-min_lat)

ggplot()+
  geom_polygon(data=df_Map, aes(x=x, y=y, group=group),fill='white',colour="black",size=0.25)+ 
  geom_arc_bar(data=df_city,aes(x0 = x, y0 = y, r0 = 0, r = sqrt(value)*scale,
                   start = start, end = start + pi, fill = variable),
                  color = "black",size=0.2) +
  geom_text(data=df_city,aes(label = city, x = x, y = y),vjust=2.25,size =3)+
  #geom_text(data=df_city,aes(label = value, x = x, y = y + vjust*scale*sqrt(value)*1.3),
  #          size =3)#+ 
  scale_x_continuous(breaks=breaks_x,labels=labels_x)+
  scale_y_continuous(breaks=breaks_y,labels=labels_y)+
  xlab("long") + 
  ylab("lat") +
  coord_fixed()+
  theme(legend.position = c(0.9,0.15),
        legend.background = element_blank())

#(b) 竖直双气泡（两个数据系列）------------------------------------------------------------------
df_city$start<- rep(c(0, pi), each=nrow(df_city)/2)
df_city$vjust<- rep(c(1, -1), each=nrow(df_city)/2)
r <- 0.04
scale <- r/max(sqrt(df_city$value))

df_city$x<-(df_city$long-min_long)/(max_long-min_long)
df_city$y<-(df_city$lat-min_lat)/(max_lat-min_lat)*Map_Scale

labels_x<-seq(110,135,10)
breaks_x<-(labels_x-min_long)/(max_long-min_long)

labels_y<-seq(30,60,10)
breaks_y<-(labels_y-min_lat)/(max_lat-min_lat)

ggplot()+
  geom_polygon(data=df_Map, aes(x=x, y=y, group=group),fill='white',colour="black",size=0.25)+ 
  geom_arc_bar(data=df_city,aes(x0 = x, y0 = y, r0 = 0, r = sqrt(value)*scale,
                                start = start, end = start + pi, fill = variable),
               color = "black",size=0.2) +
  geom_text(data=df_city,aes(label = city, x = x, y = y),vjust=2.25,size =3)+
  #geom_text(data=df_city,aes(label = value, x = x, y = y + vjust*scale*sqrt(value)*1.3),
  #          size =3)#+ 
  scale_x_continuous(breaks=breaks_x,labels=labels_x)+
  scale_y_continuous(breaks=breaks_y,labels=labels_y)+
  xlab("long") + 
  ylab("lat") +
  coord_fixed()+
  theme(legend.position = c(0.9,0.15),
        legend.background = element_blank())


