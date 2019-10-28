
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(ggalt)


dataProjected <- readOGR("Virtual_Map0.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map <- full_join(watershedPoints, dataProjected@data, by = "id")

df_huouse<-read.csv("Virtual_huouse.csv")  
set.seed(12345)
df_huouse<-df_huouse[sample(1:nrow(df_huouse),500),1:3]

long_steps<-0.05
long_mar <-seq(105,135, long_steps)
lat_mar <- seq(30,60,0.6)

elev.loess <- loess(value ~ long * lat, df_huouse,span=0.05,
                    control = loess.control(surface = "direct"))
print(elev.loess)

# get fitted (interpolated) values
elev.interp <- predict(elev.loess, expand.grid(long=long_mar,lat=lat_mar))
df_loessmap<-data.frame(matrix(elev.interp, nrow=length(long_mar),ncol=length(lat_mar)))
colnames(df_loessmap)<-lat_mar


df_loessmap$long<-long_mar
df_loessmap<-melt(df_loessmap,id.vars='long', variable.name ="lat",value.name = "value")
df_loessmap$lat<-as.numeric(as.character(df_loessmap$lat))
# Use the regression model output to interpolate the surface
Interp_map<- SpatialPixelsDataFrame(SpatialPoints(df_loessmap[c('long','lat')]),                                    data.frame(value = df_loessmap$value))

group<-1:length(dataProjected)
mypolys<-lapply(group,
                function(x) {
                  tmp = !is.na(over(Interp_map, dataProjected[x,]));
                  clipped_grid= Interp_map[tmp[,1],];
                  clipped_grid
                })


max_value<-max(df_loessmap$value)
scale<-10
threshold1<-0.15

  
p<-ggplot()+
  geom_polygon(data=df_map, aes(x=long, y=lat, group=group),fill='white',colour="NA",size=0.25)


for (i in group){
  df_region<-cbind(mypolys[[i]]@coords,mypolys[[i]]@data)
  df_region$height<-df_region$value/ max_value*scale
  
  df_region[df_region$height<threshold1,'height']<-0
  df_region$group<-i+runif(1,0,1)
  
  list_lat<- sort(unique(df_region$lat),decreasing=TRUE)
  
  for (j in list_lat){  
    df_temp<-df_region[df_region$lat==j,]
    
    if (nrow(df_temp)>2) {
      
      n<-1
      
      for (k in 1:(nrow(df_temp)-1)){
        if ((df_temp$long[k+1]-df_temp$long[k])>long_steps*1.1){
          df_temp$group[n:k]<-df_temp$group[n:k]+runif(1,0,1)/10
          n<-k+1
        }
      }
      p<-p+
        geom_ribbon(data=df_temp,aes(x=long,ymin=lat,ymax=lat+height,group=group),
                    fill="#02A3FE",alpha=0.8)+
        #geom_linerange(data=df_temp,aes(x=long,ymin=lat,ymax=lat+height,
        #                                color=value),size=0.3,alpha=1)+
        #geom_line(data=df_temp,aes(x=long,y=lat+height,group=group),colour="black",size=0.1)
        geom_xspline(data=df_temp,aes(x=long,y=lat+height,group=group),
                     spline_shape=-0.1,colour="black",size=0.8)
      }
    
  }
}
p
 
#p+geom_polygon(data=df_map, aes(x=long, y=lat, group=group),fill='NA',colour="black",size=0.25)
