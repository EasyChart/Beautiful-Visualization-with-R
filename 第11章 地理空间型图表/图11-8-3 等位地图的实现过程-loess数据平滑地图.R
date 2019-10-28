
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts


library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(reshape2)
#library(akima) #interp

colormap<-colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

dataProjected <- readOGR("Virtual_Map0.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map <- full_join(watershedPoints, dataProjected@data, by = "id")

df_huouse<-read.csv("Virtual_huouse.csv")  
set.seed(12345)
df_huouse<-df_huouse[sample(1:nrow(df_huouse),200),1:3]

#(a) 描点地图------------------------------------------------------------------------------
ggplot()+
  geom_polygon(data=df_map, aes(x=long, y=lat, group=group),
               fill='white',colour="black",size=0.25)+
  geom_point(data=df_huouse,aes(x=long, y=lat,fill=value),shape=21,size=2.5,alpha=1,stroke=0.1)+
  
  scale_fill_gradientn(colours=colormap,name='value')

#loess数据平滑-----------------------------------------------------------------------------
long_mar <-seq(105,135, 0.1)
lat_mar <- seq(30,60,0.05)

elev.loess <- loess(value ~ long * lat, df_huouse,span=0.3,
                    control = loess.control(surface = "direct"))
print(elev.loess)

# get fitted (interpolated) values
elev.loess <- loess(value ~ long * lat, df_huouse,span=0.3, 
                    control = loess.control(surface = "direct"))
elev.interp <- predict(elev.loess, expand.grid(long=long_mar,lat=lat_mar))
df_loessmap<-data.frame(matrix(elev.interp, nrow=length(long_mar),ncol=length(lat_mar)))
colnames(df_loessmap)<-lat_mar
df_loessmap$long<-long_mar
df_loessmap<-melt(df_loessmap,id.vars='long', variable.name ="lat",value.name = "value")
df_loessmap$lat<-as.numeric(as.character(df_loessmap$lat))

# Use the regression model output to interpolate the surface
Interp_map<- SpatialPixelsDataFrame(SpatialPoints(df_loessmap[c('long','lat')]),
                                    data.frame(value = df_loessmap$value))

#(a) 二维插值热力图-----------------------------------------------------------------
df_interp<-data.frame(Interp_map)
min_z<-min(df_interp$value)
max_z<-max(df_interp$value)
breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/20)

ggplot(df_interp,aes(x=long,y=lat,z=value))+
  geom_tile(aes(fill=value))+          #根据高度填充
  #geom_contour(data=df_interp,aes(x=long,y=lat,z=value),color="white",breaks=breaks_lines)+
  scale_fill_gradientn(colours=colormap)

#(b) 二维插值等位地图--------------------------------------------------------------------
group<-1:length(dataProjected)
mypolys<-lapply(group,
                function(x) {
                  tmp = !is.na(over(Interp_map, dataProjected[x,]));
                  clipped_grid= Interp_map[tmp[,1],];
                  clipped_grid
                })

df_interp<-data.frame(x=numeric(0),y=numeric(0),value=numeric(0))
for (i in group){
  df_interp<-rbind(df_interp,cbind(mypolys[[i]]@coords,mypolys[[i]]@data))
}

min_z<-min(df_interp$value)
max_z<-max(df_interp$value)
breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/10)

ggplot() +
  geom_raster(data=df_interp,aes(x=long,y=lat,fill=value))+
  
  geom_contour(data=df_interp,aes(x=long,y=lat,z=value),color="white",breaks=breaks_lines)+
  scale_fill_gradientn(colours=colormap)+ 
  
  geom_path(data=df_map, aes(x=long, y=lat, group=group),colour="black",size=0.25)+
  
  coord_cartesian()