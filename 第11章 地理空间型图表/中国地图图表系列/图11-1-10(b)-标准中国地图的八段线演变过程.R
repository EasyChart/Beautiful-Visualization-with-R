

#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(maptools)
library(ggplot2)
library(plyr)
library(grid)
library(RColorBrewer)

china_map <- readShapeLines("China_adm_shp/bou2_4m/bou2_4l.shp")      
x <- china_map@data         
xs <- data.frame(id=row.names(x),x)
china_map0<- fortify(china_map)
df_map1 <- join(china_map0, xs, type = "full")


china_map <- readShapePoly("China_adm_shp/bou2_4m/bou2_4p.shp")     
x <- china_map@data         
xs <- data.frame(id=row.names(x),x)
china_map0<- fortify(china_map)
df_map2 <- join(china_map0, xs, type = "full")

ggplot()+
  geom_polygon(data=df_map2, aes(x=long, y=lat, group=group),fill="#FAFAFA",  colour="black",size=0.25)+
  
  geom_path(data=df_map1, aes(x=long, y=lat, group=group),  colour="black",size=0.25)+
 
   theme_void()#中国地图
  
