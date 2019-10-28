
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts


library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidyr)

dataProjected <- readOGR("Virtual_Map1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map <- full_join(watershedPoints, dataProjected@data, by = "id")


df_city<-read.csv("Virtual_City.csv") [c('long','lat','city')]
df_connect<-read.csv("Virtual_Connect.csv")  
df_connect<-df_connect %>%
  left_join(df_city,by=c('start'='city'))%>%
  left_join(df_city,by=c('end'='city'))

#(a) 直线连接地图-------------------------------------------------------------
ggplot() +
  geom_polygon(data=df_map, aes(x = long, y = lat,group=group),
               fill="white",colour="black",size=0.25)+
  geom_curve(data=df_connect,aes(x=long.x,y=lat.x,xend=long.y,yend=lat.y),
             size=0.75,colour="black",curvature = 0)+
  geom_point(data =df_connect,aes(x=long.y,y=lat.y),
             size=4,shape=21,fill="#F00000",colour="black",stroke=0.1)

#(b) 曲线连接地图-------------------------------------------------------------
ggplot() +
  geom_polygon(data=df_map, aes(x = long, y = lat,group=group),
               fill="white",colour="black",size=0.25)+
  geom_curve(data=df_connect,aes(x=long.x,y=lat.x,xend=long.y,yend=lat.y),
             size=0.75,colour="black",curvature = 0.1)+
  geom_point(data =df_connect,aes(x=long.y,y=lat.y),
             size=4,shape=21,fill="#F00000",colour="black",stroke=0.1)

#(c) 无箭头的流向地图-------------------------------------------------------------
ggplot() +
  geom_polygon(data=df_map, aes(x = long, y = lat,group=group),
               fill="white",colour="black",size=0.25)+
  geom_curve(data=df_connect,aes(x=long.x,y=lat.x,xend=long.y,yend=lat.y,
                                 colour=start,size=value),
             curvature = 0.1)+
  scale_size(range=c(0.5,1.5))+
  geom_point(data =df_connect,aes(x=long.y,y=lat.y),
             size=4,shape=21,fill="white",colour="black",stroke=0.5)

#(d) 带箭头的流向地图-------------------------------------------------------------
ggplot() +
  geom_polygon(data=df_map, aes(x = long, y = lat,group=group),
               fill="white",colour="black",size=0.25)+
  geom_curve(data=df_connect,aes(x=long.x,y=lat.x,xend=long.y,yend=lat.y,
                                 size=value,colour=start),
             arrow = arrow(length = unit(0.25, "cm")),curvature = 0.1)+
  scale_size(range=c(0.5,1.5))
  
  