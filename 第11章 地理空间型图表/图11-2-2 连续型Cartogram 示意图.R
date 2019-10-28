
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(cartogram) #提供cartogram()函数
library(rgeos) #提供gCentroid()函数

dataProjected <- readOGR("Virtual_Map1.shp")

df_city<-read.csv("Virtual_City.csv")  


dataProjected@data<-left_join(dataProjected@data,df_city[c('country','orange')],by = "country")

my_cartogram <- cartogram(dataProjected, 'orange') #新版函数：cartogram_cont()


carto_fortified <- fortify(my_cartogram,region='country')

carto_fortified <- carto_fortified %>% left_join(. , my_cartogram@data, by=c("id"="country")) 

df_centers <- cbind.data.frame(data.frame(gCentroid(my_cartogram, byid=TRUE), 
                                          id=my_cartogram@data$country))

#以orange 数值作为映射
ggplot() +
  geom_polygon(data = carto_fortified, aes(fill = orange, x = long, y = lat, group = group) , size=0.05, alpha=0.9, color="black") +
  geom_text(data=df_centers, aes(x=x, y=y, label=id), color="black", size=2, alpha=0.6) +
  scale_fill_gradientn(colours=rev(brewer.pal(7,"RdYlBu")), name="orange") 
