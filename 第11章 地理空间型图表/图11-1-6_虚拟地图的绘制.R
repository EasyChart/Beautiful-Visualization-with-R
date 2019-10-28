
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)

#(a) 陆地岛屿
dataProjected <- readOGR("Virtual_Map0.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_Map <- merge(watershedPoints, dataProjected@data, by = "id")
ggplot()+
  geom_polygon(data=df_Map, aes(x=long, y=lat, group=group,fill=type),colour="black",size=0.25)

#(b) 国家
dataProjected <- readOGR("Virtual_Map1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_Map <- merge(watershedPoints, dataProjected@data, by = "id")
ggplot()+
  geom_polygon(data=df_Map, aes(x=long, y=lat, group=group,fill=country),colour="black",size=0.25)
