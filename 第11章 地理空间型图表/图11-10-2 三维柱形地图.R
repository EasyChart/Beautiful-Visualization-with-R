
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(plot3D)
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


grd<-expand.grid(long=long_mar, lat= lat_mar)

df_freq1<-left_join(grd,df_freq,by=c('long','lat'))


pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
z<-reshape2::dcast(df_freq1[c('long','lat','value')],long~lat)
rownames(z)<-z$lat



#(a) Spectral 多色渐变系
cols<-colorRampPalette(colormap)(20) 
hist3D(x=long_mar,y=lat_mar,z=as.matrix(z[,2:ncol(z)]),
       col = cols, border = "gray",space=0,alpha = 1,lwd=0.1,
       xlab = "long", ylab = "lat",zlab = "Count", clab="Count",zlim=c(0,15),
       ticktype = "detailed",bty = "f",box = TRUE,#cex.axis= 1e-09,
       theta = 35, phi = 20, d=3,
       colkey = list(length = 0.5, width = 1))

#(b) 单色渐变系
cols<-colorRampPalette(c("#F7FBFF", "red"))(20)    

hist3D(x=long_mar,y=lat_mar,z=as.matrix(z[,2:ncol(z)]),
       col = cols, border = "gray",space=0,alpha = 1,lwd=0.1,
       xlab = "long", ylab = "lat",zlab = "Count", clab="Count",zlim=c(0,15),
       ticktype = "detailed",bty = "f",box = TRUE,#cex.axis= 1e-09,
       theta = 35, phi = 20, d=3,
       colkey = list(length = 0.5, width = 1))
