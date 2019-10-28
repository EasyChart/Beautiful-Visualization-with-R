
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(cartogram)
library(tidyverse)
library(broom)
library(geojsonio)
library(RColorBrewer)
library(rgeos)
library(Cairo)         #图片高清导出
library(RColorBrewer)  #该保重存储着一些高质量的地图配色模板可以参考
library(showtext)


China_Hexmap<-read.csv("ChinaMap.csv",stringsAsFactors=FALSE)

Province<-unique(China_Hexmap$Province)
N<-length(Province)
data.list<-list()  
for (i in 1:N){  
  temp<-China_Hexmap[China_Hexmap$Province==Province[i],]
  Sr <-Polygon(cbind(temp$x,temp$y))
  Srs <- Polygons(list(Sr), as.character(Province[i]))
  data.list[i]<-list(Srs)
}  

SpP <- SpatialPolygons(data.list, 1:N)

mydata<-data.frame(Province=Province,Value=round(runif(N,0,10)))

SpP_China_Hexmap<- SpatialPolygonsDataFrame(SpP,data = data.frame(
  row.names = row.names(SpP)))
SpP_China_Hexmap@data <-mydata
China_cartogram <- cartogram(SpP_China_Hexmap, 'Value')

carto_fortified <- tidy(China_cartogram, region = "Province")
carto_fortified = carto_fortified %>% left_join(. , China_cartogram@data, by=c("id"="Province")) 

centers <- cbind.data.frame(data.frame(gCentroid(China_cartogram, byid=TRUE), id=China_cartogram@data$Province))


plot(China_cartogram)

#cairo_pdf(file="变形China3.pdf",width=5.50,height=5.81)
#showtext.begin()
ggplot() +
  geom_polygon(data = carto_fortified, aes(fill = Value, x = long, y = lat, group = group) , size=0.05, alpha=0.9, color="black") +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="black", size=3, alpha=0.6) +
  scale_fill_gradientn(colours=rev(brewer.pal(7,"RdYlBu")), name="Value") +
  theme_void() +
  coord_map()

#showtext.end()
#dev.off()