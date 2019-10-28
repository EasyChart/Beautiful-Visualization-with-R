
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts


library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(geosphere)
library(Cairo)         #图片高清导出
library(RColorBrewer)  #地图配色模板参考
library(showtext)

dataProjected <- readOGR("China_adm_shp/bou2_4m/bou2_4p.shp") 
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_China2 <-  full_join(watershedPoints, dataProjected@data, by = "id")
df_China2$class<-rep("Mainland",nrow(df_China2))


df_NanHaiLine <- read.csv("中国南海九段线.csv")  
colnames(df_NanHaiLine)<-c("long","lat","ID")

Width<-9
Height<-9
long_Start<-124
lat_Start<-16

df_Nanhai<-df_China2[df_China2$long>106.55 & df_China2$long<123.58,]
df_Nanhai<-df_Nanhai[df_Nanhai$lat>4.61 & df_Nanhai$lat<25.45,]

min_long<-min(min(df_Nanhai$long, na.rm = TRUE),min(df_NanHaiLine$long))
min_lat<-min(min(df_Nanhai$lat, na.rm = TRUE),min(df_NanHaiLine$lat))
max_long<-max(min(df_Nanhai$long, na.rm = TRUE),max(df_NanHaiLine$long))
max_lat<-max(max(df_Nanhai$lat, na.rm = TRUE),max(df_NanHaiLine$lat))

df_Nanhai$long<-(df_Nanhai$long-min_long)/(max_long-min_long)*Width+long_Start
df_Nanhai$lat<-(df_Nanhai$lat-min_lat)/(max_lat-min_lat)*Height+lat_Start
df_Nanhai$class<-rep("NanHai",nrow(df_Nanhai))

df_China2<-rbind(df_China2,df_Nanhai)


df_NanHaiLine$long<-(df_NanHaiLine$long-min_long)/(max_long-min_long)*Width+long_Start
df_NanHaiLine$lat<-(df_NanHaiLine$lat-min_lat)/(max_lat-min_lat)*Height+lat_Start

#------------------------------------航班连接线的数据构造-------------------------------------------------
#flight_data<-read.table("国内航班数据.txt",header = TRUE)
#flight_data<-flight_data[1:300,]
# 
# for (j in 1:length(flight_data$Start_City)) {
#   inter <- data.frame(gcIntermediate(c(flight_data$Start_long[j], flight_data$Start_lat[j]),
#                                      c(flight_data$End_long[j], flight_data$End_lat[j]),
#                                      n=50, addStartEnd=TRUE))
#   inter$group<-j
#   if (j==1){
#     flight_line<-inter
#   }else{
#     flight_line<-rbind(flight_line,inter)
#   }
# 
# }
flight_line<- read.csv("flight_line.csv",header = TRUE,sep = ",",encoding="UTF-8")  

#cairo_pdf(file="连接线地图.pdf",width=5.50,height=5.81)
#showtext.begin()
#write.csv(flight_line, file = "flight_line.csv")
ggplot()+
  geom_polygon(data=df_China2, aes(x=long, y=lat, group=interaction(class,group)),fill="grey5",colour="grey30",size=0.1)+ 
  #中国地图，包括中国主体部分和长方形方块内的南海诸岛数据
  geom_rect(aes(xmin=long_Start, xmax=long_Start+Width+0.3, ymin=lat_Start-0.3, ymax=lat_Start+Height),fill=NA, colour="black",size=0.25)+
  #绘制长方形方框
  geom_line(data=df_NanHaiLine, aes(x=long, y=lat, group=ID), colour="black", size=1)+
  geom_line(data=flight_line,aes(x=lon, y=lat,group=group), colour="white",alpha=0.02,size=0.25)+
  theme_void()+
  theme(plot.background = element_rect(fill="grey20"))
#showtext.end()
#dev.off()

