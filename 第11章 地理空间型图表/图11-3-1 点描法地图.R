
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)

dataProjected <- readOGR("Virtual_Map1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_Map <- full_join(watershedPoints, dataProjected@data, by = "id")


df_city<-read.csv("Virtual_City.csv")  

#(a) 散点（point）+文本（text）
ggplot()+
  geom_polygon(data=df_Map, aes(x=long, y=lat, group=group),fill='white',colour="black",size=0.25)+
  geom_point(data=df_city,aes(x=long, y=lat),shape=21,fill='red',colour="black",size=4)+
  geom_text(data=df_city,aes(x=long, y=lat, label=city),vjust=1.5,colour="black",size=3)

#(b) 标签（label）
ggplot()+
  geom_polygon(data=df_Map, aes(x=long, y=lat, group=group),fill='white',colour="black",size=0.25)+
  #geom_point(data=df_city,aes(x=long, y=lat),shape=21,fill='red',colour="black",size=4)+
  geom_label(data=df_city,aes(x=long, y=lat, label=city),fill='orange',colour="black",size=3)



