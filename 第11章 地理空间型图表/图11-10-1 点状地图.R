
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
#library(ggforce)

colormap<-colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

dataProjected <- readOGR("Virtual_Map0.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map <- full_join(watershedPoints, dataProjected@data, by = "id")

df_huouse<-read.csv("Virtual_huouse.csv")  

bins<-0.8
long_mar <-seq(105,135, 0.6)
lat_mar <- seq(30,60,bins)
grd<-expand.grid(long=long_mar, lat= lat_mar)

df_freq0 <-  as.data.frame(table(findInterval(df_huouse$long, long_mar,all.inside=TRUE),
                                findInterval(df_huouse$lat,lat_mar,all.inside=TRUE)))
df_freq0$long <- long_mar[df_freq0[,1]]
df_freq0$lat <- lat_mar[df_freq0[,2]]

df_freq0<-left_join(grd,df_freq0,by=c('long','lat'))
df_freq0[is.na(df_freq0$Freq),'Freq']<-0

ggplot(df_freq0, aes(long,lat,fill=Freq))+
   geom_tile(size=0.1) + 
   scale_fill_gradientn(colours=colormap)


freq_map<- SpatialPixelsDataFrame(SpatialPoints(df_freq0[c('long','lat')]),
                                 data.frame(value=df_freq0$Freq)) 

group<-1:length(dataProjected)
mypolys<-lapply(group,
                function(x) {
                  tmp = !is.na(over(freq_map, dataProjected[x,]));
                  clipped_grid= freq_map[tmp[,1],];
                  clipped_grid
                })

df_freq<-data.frame(long=numeric(0),lat=numeric(0),value=numeric(0))
for (i in group){
  df_freq<-rbind(df_freq,cbind(mypolys[[i]]@coords,mypolys[[i]]@data))
}

#(a) 离散点状地图
ggplot() +
  #geom_circle(data=df_freq,aes(x0=long,y0=lat,fill=value,r=bins/2),size=0.1)+
  #geom_polygon(data=df_map, aes(x=long, y=lat, group=group),fill='white',alpha=0.9,color=NA,size=1)+
  geom_point(data=df_freq,aes(x=long,y=lat),fill='black',size=2,shape=21,stroke=0.1)+
  geom_path(data=df_map, aes(x=long, y=lat, group=group),colour="black",size=1)

#(b) 统计直方的点状地图
ggplot() +
  #geom_circle(data=df_freq,aes(x0=long,y0=lat,fill=value,r=bins/2),size=0.1)+
  geom_point(data=df_freq,aes(x=long,y=lat,fill=value),size=3,shape=21,stroke=0.1)+
  scale_fill_gradientn(colours=colormap)#+ 
  #coord_fixed()
  #geom_path(data=df_map, aes(x=long, y=lat, group=group),colour="black",size=0.25)+