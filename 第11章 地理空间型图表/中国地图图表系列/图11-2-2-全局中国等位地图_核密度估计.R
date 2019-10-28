
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(RColorBrewer)
library(classInt)
library(sm)
library(dplyr)
library(sp)

Num<-300
cycle<-data.frame(X=runif(Num,70,135),Y=runif(Num,10,55))


#------------------------------------省级地图图层-----------------------------------------------

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


ggplot() +
  geom_polygon(data=df_China2, aes(x = long, y = lat,group=interaction(class,group)),
               fill="grey98",colour="grey60",size=0.25) +
  geom_point(data=cycle,aes(x = X, y = Y),shape=21,fill="#F8766D",size=2)+
  geom_rect(aes(xmin=long_Start, xmax=long_Start+Width+0.3, ymin=lat_Start-0.3, ymax=lat_Start+Height),
            fill=NA,colour="black",size=0.25)+
  geom_line(data=df_NanHaiLine, aes(x=long, y=lat, group=ID), colour="black", size=1)+ #中国南海九段线
  coord_cartesian()+
  ylim(15,55)+
  theme_void()+
  theme(
    legend.position=c(0.85,0.2),
    legend.background=element_blank())

#---------------------------------二维核密度估计---------------------------------------------------------
cycle_dens<- sm.density(data.frame(cycle$X, cycle$Y), 
                        display= "image", ngrid=500,
                        ylim=c(10,60),xlim=c(60,140))

temp=SpatialPoints(expand.grid(x=cycle_dens$eval.points[,1], 
                               y=cycle_dens$eval.points[,2]))

temp = SpatialPixelsDataFrame(temp, data.frame(kde = array(cycle_dens$estimate, 
                                               length(cycle_dens$estimate))))
#------------------------------------------------------------------------------

Density_map<-data.frame(temp@coords,temp$kde)
min_z<-min(temp$kde)
max_z<-max(temp$kde)
breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/20)

ggplot(Density_map,aes(x=x,y=y,z=temp.kde))+
  geom_tile(aes(fill=temp.kde))+          #根据高度填充
  scale_fill_gradientn(colours=rev(brewer.pal(11,'Spectral')))+          
  stat_contour(aes(colour= ..level..),breaks=breaks_lines,color="black")


#---------------------------------二维核密度估计地图的计算---------------------------------------------
dataProjected <- readOGR("China_adm_shp/bou1_4m/bou1_4p.shp") 
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_China1 <- full_join(watershedPoints, dataProjected@data, by = "id")
df_China1<-df_China1[is.na(df_China1$long)==FALSE,]
df_China1<-df_China1[is.na(df_China1$lat)==FALSE,]


df_mianland<-df_China1[df_China1$AREA==954.943,]  # 中国大陆地区
Sr1 = Polygon(cbind(df_mianland$long,df_mianland$lat))
Srs1 = Polygons(list(Sr1), "1")
SpP = SpatialPolygons(list(Srs1), 1:1)
Mainland <- SpatialPolygonsDataFrame(SpP,
                                     data = data.frame(
                                      Names = "coords",
                                      row.names = row.names(SpP)))

df_taiwan<-df_China1[df_China1$AREA==3.171,] #中国台湾地区
Sr1 = Polygon(cbind(df_taiwan$long,df_taiwan$lat))
Srs1 = Polygons(list(Sr1), "1")
SpP = SpatialPolygons(list(Srs1), 1:1)
taiwan<- SpatialPolygonsDataFrame(SpP,
                                  data = data.frame(
                                  Names = "coords",
                                  row.names = row.names(SpP)))

df_hainan<-df_China1[df_China1$AREA==2.903,] #中国海南岛
Sr1 = Polygon(cbind(df_hainan$long,df_hainan$lat))
Srs1 = Polygons(list(Sr1), "1") 
SpP = SpatialPolygons(list(Srs1), 1:1)
hainan<- SpatialPolygonsDataFrame(SpP,
                                  data = data.frame(
                                  Names = "coords",
                                  row.names = row.names(SpP)))


sel_Mainland=!is.na(over(temp, Mainland))
sel_taiwan=!is.na(over(temp, taiwan))
sel_hainan=!is.na(over(temp, hainan))

clipped_grid_Mainland= temp[sel_Mainland[,1],]
clipped_grid_taiwan= temp[sel_taiwan[,1],]
clipped_grid_hainan= temp[sel_hainan[,1],]


tile_china<-cbind(clipped_grid_Mainland@coords,clipped_grid_Mainland@data)
tile_china<-rbind(tile_china,cbind(clipped_grid_taiwan@coords,clipped_grid_taiwan@data))
tile_china<-rbind(tile_china,cbind(clipped_grid_hainan@coords,clipped_grid_hainan@data))
colnames(tile_china)<-c("long","lat","kde")

max_z<-max(tile_china$kde)
min_z<-min(tile_china$kde)
breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/20)


# 截图南海的方块部分的地理数据：经度为106.55~123.58，纬度为4.61~25.45
tile_nanhai<-tile_china[tile_china$long>106.55 & tile_china$long<123.58,]
tile_nanhai<- tile_nanhai[tile_nanhai$lat>4.61 & tile_nanhai$lat<25.45,]
tile_nanhai$long<-(tile_nanhai$long-min_long)/(max_long-min_long)*Width+long_Start
tile_nanhai$lat<-(tile_nanhai$lat-min_lat)/(max_lat-min_lat)*Height+lat_Start


ggplot() +
  #添加核密度估计的图层
  geom_raster(data=tile_china,aes(x=long,y=lat,fill=kde))+
  geom_raster(data=tile_nanhai,aes(x=long,y=lat,fill=kde))+
  geom_contour(data=tile_china,aes(x=long,y=lat,z=kde),color="white",breaks=breaks_lines)+
  scale_fill_gradientn(colours=rev(brewer.pal(11,'Spectral')))+ 
  
  #添加省份的图层
  geom_polygon(data=df_China2, aes(x = long, y = lat,group=interaction(class,group)),
               fill=NA,colour="grey60",size=0.25) +
  geom_rect(aes(xmin=long_Start, xmax=long_Start+Width+0.3, ymin=lat_Start-0.3, ymax=lat_Start+Height),
            fill=NA,colour="black",size=0.25)+
  geom_line(data=df_NanHaiLine, aes(x=long, y=lat, group=ID), colour="black", size=1)+ #中国南海九段线
  coord_cartesian()+
  ylim(15,55)+
  theme_void()+
  theme(
    legend.position=c(0.2,0.2),
    legend.background=element_blank()
  )

