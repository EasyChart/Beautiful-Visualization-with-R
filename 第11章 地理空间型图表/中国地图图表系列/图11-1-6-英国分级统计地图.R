
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggplot2)       #绘图函数
library(plyr)          #数据合并工具
library(maptools)      #地图素材导入 readShapePoly()函数
library(RColorBrewer)  #地图配色模板参考
library(scales)        #分割数据
library(sp)  


England_map1 <-readShapePoly("GBR_adm_shp/GBR_adm0.shp")  
AD1_1 <- England_map1@data
AD2_1 <- data.frame(id=rownames(AD1_1),AD1_1)
England_map1 <- fortify(England_map1)
England_map_data1 <- join(England_map1,AD2_1, type = "full")

England_map2 <-readShapePoly("GBR_adm_shp/GBR_adm2.shp")  
AD1_2 <- England_map2@data
AD2_2 <- data.frame(id=rownames(AD1_2),AD1_2)
England_map2 <- fortify(England_map2)
England_map_data2 <- join(England_map2,AD2_2, type = "full")

mydata<-data.frame(NAME_2=unique(England_map_data2$NAME_2),
                   value=round(runif(length(unique(England_map_data2$NAME_2)),0,10)))
England_map_data2<-join(England_map_data2,mydata,type="full")

ggplot() +
  geom_polygon(data=England_map_data2, aes(x = long, y = lat,group=group,fill=value),colour="grey60",size=0.1) +
  geom_path(data=England_map_data1, aes(x = long, y = lat,group=group),colour="black",size=0.25) +
  scale_fill_gradientn(colours=brewer.pal(9,"YlGnBu"))+
  theme( 
    legend.background =element_blank(),
    legend.position=c(0.13,0.20))
