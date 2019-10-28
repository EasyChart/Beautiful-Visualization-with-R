
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(RColorBrewer)
library(tmap)
library(tmaptools)

df_map <- readOGR("Virtual_Map1.shp")

tm_shape(df_map) +  
  tm_fill("country", palette="Set2") + 
  tm_borders("black", lwd = 1) +
  tm_scale_bar(position=c("left", "bottom")) + 
  tm_compass(type = "4star", position=c("left", "top")) + 
  tm_layout(inner.margins=c(0.12,0.03,0.08,0.03))+
  tm_legend(position = c("right", "bottom"))