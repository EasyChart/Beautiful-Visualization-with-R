
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(sm) #sm.density

colormap<-colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
  
dataProjected <- readOGR("Virtual_Map0.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map <- full_join(watershedPoints, dataProjected@data, by = "id")

df_huouse<-read.csv("Virtual_huouse.csv")  

ggplot()+
  geom_polygon(data=df_map, aes(x=long, y=lat, group=group),fill='white',colour="black",size=0.25)+
  geom_point(data=df_huouse,aes(x=long, y=lat),shape=19,colour="black",size=1,alpha=1)


cycle_dens<- sm.density(data.frame(df_huouse$long, df_huouse$lat),
                        display= "image", ngrid=500,
                        ylim=c(30,60),xlim=c(105,135))

Density_map<-SpatialPoints(expand.grid(x=cycle_dens$eval.points[,1], 
                               y=cycle_dens$eval.points[,2]))

Density_map<-SpatialPixelsDataFrame(Density_map, data.frame(kde = array(cycle_dens$estimate, 
                                                           length(cycle_dens$estimate))))

#(b) 二维核密度估计热力图-------------------------------------------------------------
df_density<-data.frame(Density_map)
# min_z<-min(df_density$Density_map.kde)
# max_z<-max(df_density$Density_map.kde)
# breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/20)
ggplot(df_density,aes(x=x,y=y,z=kde))+
  geom_tile(aes(fill=kde))+          #根据高度填充
  scale_fill_gradientn(colours=colormap)+
  #stat_contour(aes(colour= ..level..),breaks=breaks_lines,color="black")+
  labs(fill='kde',
       x='long',
       y='lat')

#(c) 二维核密度估计热力地图--------------------------------------------------------------
group<-1:length(dataProjected)
mypolys<-lapply(group,
                function(x) {
                  tmp = !is.na(over(Density_map, dataProjected[x,]));
                  clipped_grid= Density_map[tmp[,1],];
                  clipped_grid
                })

df_density<-data.frame(x=numeric(0),y=numeric(0),kde=numeric(0))
for (i in group){
  df_density<-rbind(df_density,cbind(mypolys[[i]]@coords,mypolys[[i]]@data))
}

min_z<-min(df_density$kde)
max_z<-max(df_density$kde)
breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/20)

ggplot() +
  geom_raster(data=df_density,aes(x=x,y=y,fill=kde))+
  geom_contour(data=df_density,aes(x=x,y=y,z=kde),color="white",breaks=breaks_lines)+
  scale_fill_gradientn(colours=colormap)+ 
  geom_path(data=df_map, aes(x=long, y=lat, group=group),colour="black",size=0.25)+
  coord_cartesian()