

#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)

dataProjected <- readOGR("China_adm_shp/bou2_4m/bou2_4p.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_China <- full_join(watershedPoints, dataProjected@data, by = "id")
df_China$class<-rep("Mainland",nrow(df_China))

mydata <- read.csv("Province_Data.csv")  
#mydata为33 X 3的表格数据，列名分别为：NAME，province，Value
df_China<-full_join(df_China,mydata,type="full") 


#---------------------df_Nanhai:Nanhai Region-----------------------------------
df_NanHaiLine <- read.csv("中国南海九段线.csv")  
colnames(df_NanHaiLine)<-c("long","lat","ID")

Width<-9
Height<-9
long_Start<-124
lat_Start<-16

df_Nanhai<-df_China[df_China$long>106.55 & df_China$long<123.58,]
df_Nanhai<-df_Nanhai[df_Nanhai$lat>4.61 & df_Nanhai$lat<25.45,]

min_long<-min(min(df_Nanhai$long, na.rm = TRUE),min(df_NanHaiLine$long))
min_lat<-min(min(df_Nanhai$lat, na.rm = TRUE),min(df_NanHaiLine$lat))
max_long<-max(min(df_Nanhai$long, na.rm = TRUE),max(df_NanHaiLine$long))
max_lat<-max(max(df_Nanhai$lat, na.rm = TRUE),max(df_NanHaiLine$lat))

df_Nanhai$long<-(df_Nanhai$long-min_long)/(max_long-min_long)*Width+long_Start
df_Nanhai$lat<-(df_Nanhai$lat-min_lat)/(max_lat-min_lat)*Height+lat_Start
df_Nanhai$class<-rep("NanHai",nrow(df_Nanhai))

df_China<-rbind(df_China,df_Nanhai)


df_NanHaiLine$long<-(df_NanHaiLine$long-min_long)/(max_long-min_long)*Width+long_Start
df_NanHaiLine$lat<-(df_NanHaiLine$lat-min_lat)/(max_lat-min_lat)*Height+lat_Start


ggplot()+
  geom_polygon(data=df_China, aes(x=long, y=lat, group=interaction(class,group)),fill="grey98",colour="black",size=0.25)+
  geom_rect(aes(xmin=124, xmax=125+Width, ymin=15, ymax=16+Height),fill=NA,colour="black")+
  geom_line(data=df_NanHaiLine, aes(x=long, y=lat, group=ID), colour="black", size=1)+ #中国南海九段线
  ylim(15,55)+
  theme_void()


#----------------------图9-1-14. 中国不同级别的统计地图------------------------------
ggplot()+
  geom_polygon(data=df_China, aes(x=long, y=lat, group=interaction(class,group),fill=Value),colour="black",size=0.25)+ 
  #中国地图，包括中国主体部分和长方形方块内的南海诸岛数据
  geom_rect(aes(xmin=long_Start, xmax=long_Start+Width+0.3, ymin=lat_Start-0.3, ymax=lat_Start+Height),fill=NA, colour="black",size=0.25)+
  #绘制长方形方框
  geom_line(data=df_NanHaiLine, aes(x=long, y=lat, group=ID), colour="black", size=1)+  
  #绘制长方形方框内的中国南海八段线 
  scale_fill_gradientn(colours = rev(brewer.pal(11,'Spectral')))+
  coord_cartesian()+
  ylim(15,55)+
  theme(
    legend.position=c(0.15,0.2),
    legend.background = element_blank()
  )

