
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)

dataProjected <- readOGR("Virtual_Map1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
watershedDF <- full_join(watershedPoints, dataProjected@data,by='id')

df_city<-read.csv("Virtual_City.csv")  

#(a) 数值映射到单个视觉通道（气泡大小）
ggplot()+
  geom_polygon(data=df_Map, aes(x=long, y=lat, group=group),fill='white',colour="black",size=0.25)+
  geom_point(data=df_city,aes(x=long, y=lat,size=orange),shape=21,fill='#EF5439',colour="black")+
  geom_text(data=df_city,aes(x=long, y=lat, label=city),vjust=2,colour="black",size=3)+
  scale_size(range=c(2,9),name='price')

#(b) 数值映射到两个视觉通道（气泡大小和颜色）
ggplot()+
  geom_polygon(data=df_Map, aes(x=long, y=lat, group=group),fill='white',colour="black",size=0.25)+
  geom_point(data=df_city,aes(x=long, y=lat,size=orange,fill=orange),shape=21,colour="black")+
  geom_text(data=df_city,aes(x=long, y=lat, label=city),vjust=2,colour="black",size=3)+
  scale_size(range=c(2,9),name='price')+
  scale_fill_gradientn(colours = brewer.pal(9,'YlOrRd'),name='price')