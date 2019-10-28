

#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(grid)
library(RColorBrewer)

dataProjected <- readOGR("China_adm_shp/bou4_4m/BOUNT_poly.shp") 
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_China4 <- merge(watershedPoints, dataProjected@data, by = "id")

#mydata <- read.csv("geshengzhibiao.csv")  
#mydata为33 X 3的表格数据，列名分别为：NAME，province，Value
mydata<-data.frame(NAME99=unique(df_China4$NAME99),Value=runif(length(unique(df_China4$NAME99))))

Mainland_map<-join(df_China4,mydata,type="full") 

Mainland_map$class<-rep("Mainland",nrow(Mainland_map))


NanHai_Line <- read.csv("中国南海九段线.csv")   #中国南海九段线数据
colnames(NanHai_Line)<-c("long","lat","ID")

# 截图南海的方块部分的地理数据：经度为106.55~123.58，纬度为4.61~25.45
NanHai<-Mainland_map[Mainland_map$long>106.55 & Mainland_map$long<123.58,]
NanHai<- NanHai[NanHai$lat>4.61 & NanHai$lat<25.45,]

Min_long<-min(min(NanHai$long, na.rm = TRUE),min(NanHai_Line$long, na.rm = TRUE))
Max_long<-max(max(NanHai$long, na.rm = TRUE),max(NanHai_Line$long, na.rm = TRUE))

Min_lat<-min(min(NanHai$lat, na.rm = TRUE),min(NanHai_Line$lat, na.rm = TRUE))
Max_lat<-max(max(NanHai$lat, na.rm = TRUE),max(NanHai_Line$lat, na.rm = TRUE))

Width<-10
Height<-9
Long_Start<-124
lat_Start<-16
NanHai_Line$long<-(NanHai_Line$long-Min_long)/(Max_long-Min_long)*Width+Long_Start
NanHai_Line$lat<-(NanHai_Line$lat-Min_lat)/(Max_lat-Min_lat)*Height+lat_Start


NanHai$long<-(NanHai$long-Min_long)/(Max_long-Min_long)*Width+Long_Start
NanHai$lat<-(NanHai$lat-Min_lat)/(Max_lat-Min_lat)*Height+lat_Start
NanHai$class<-rep("NanHai",nrow(NanHai))

df_China4<-rbind(Mainland_map,NanHai)

#省级数据---------------------------------------------------------------------------------------------
dataProjected <- readOGR("China_adm_shp/bou2_4m/bou2_4p.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_China2 <- full_join(watershedPoints, dataProjected@data, by = "id")
df_China2$class<-rep("Mainland",nrow(df_China2))


NanHai_Province <-df_China2[df_China2$long>106.55 & df_China2$long<123.58,]
NanHai_Province<- NanHai_Province[NanHai_Province$lat>4.61 & NanHai_Province$lat<25.45,]
NanHai_Province$long<-(NanHai_Province$long-Min_long)/(Max_long-Min_long)*Width+Long_Start
NanHai_Province$lat<-(NanHai_Province$lat-Min_lat)/(Max_lat-Min_lat)*Height+lat_Start

NanHai_Province$class<-rep("NanHai",nrow(NanHai_Province))

df_China2<-rbind(df_China2,NanHai_Province)


ggplot()+
  geom_polygon(data=df_China4, aes(x=long, y=lat, group=interaction(class,group),fill=Value),colour="grey60",size=0.1)+ #中国地图

  geom_path(data=df_China2, aes(x=long, y=lat, group=interaction(class,group)), colour="black",size=0.5)+ #中国南海九段线
  
  geom_rect(aes(xmin=Long_Start, xmax=Long_Start+Width+0.3, ymin=lat_Start-0.3, ymax=lat_Start+Height),fill=NA,colour="black",size=0.25)+
  geom_line(data=NanHai_Line, aes(x=long, y=lat, group=ID), colour="black", size=1)+ #中国南海九段线
  
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,'Spectral')))(32))+
  coord_cartesian()+
  ylim(15,55)+
  theme(
    legend.position=c(0.15,0.2),
    legend.background = element_blank()
  )
