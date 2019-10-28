
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)


dataProjected <- readOGR("Virtual_Map1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
watershedDF <- full_join(watershedPoints, dataProjected@data,by='id')
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_Map <- full_join(watershedPoints, dataProjected@data, by = "id")

df_city<-read.csv("Virtual_City.csv") 

selectCol<-c("orange","apple","banana","watermelon")
MaxH<-max(df_city[,selectCol])
Scale<-3
width<-1.1
df_city[,selectCol]<-df_city[,selectCol]/MaxH*Scale

df_city<-melt(df_city[c('lat','long','group','city',selectCol)],
              id.vars=c('lat','long','group','city'))
df_city<-transform(df_city,hjust1=ifelse(variable=='orange',-width, 
                                         ifelse(variable=='apple',-width/2,
                                         ifelse(variable=='banana',0,width/2))),
                          hjust2=ifelse(variable=='orange',-width/2, 
                                 ifelse(variable=='apple',0,
                                        ifelse(variable=='banana',width/2,width))))
  
Lengend_data<-data.frame(X=rep(132,5),Y=rep(32,5),index=seq(0,MaxH,MaxH/4))

#(a) 带柱形的地图------------------------------------------------------------------------------
ggplot() +
  geom_polygon(data=df_Map, aes(x = long, y = lat,group=group),
               fill="white",colour="black",size=0.25)+
  geom_rect(data = df_city, aes(xmin = long +hjust1, xmax = long+hjust2,
                                ymin = lat, ymax = lat + value ,
                                fill= variable),
            size =0.25, colour ="black", alpha = 1)+
  geom_text(data=df_city[!duplicated(df_city$city),],aes(x=long,y=lat-0.5,label=city),size=4)+
  labs(fill='type')+
  geom_rect(data = Lengend_data,aes(xmin = X , xmax = X+0.5 ,
                                   ymin = Y, ymax = Y+index / MaxH * Scale),
            size =0.25, colour ="black",fill = NA,alpha = 1)+
  geom_text(data = Lengend_data,aes(x=X+1.5,y=  Y+index / MaxH * Scale,
                                    label=index),size=3)


#(b) 带南丁格尔玫瑰图的地图--------------------------------------------------------------------

Map_Scale<-0.75
min_lat<-min(df_Map$lat) #30.11628
max_lat<-max(df_Map$lat) #59.94186
min_long<-min(df_Map$long) #105.2945
max_long<-max(df_Map$long) #134.8317

df_Map$x<-(df_Map$long-min_long)/(max_long-min_long)
df_Map$y<-(df_Map$lat-min_lat)/(max_lat-min_lat)*Map_Scale




df_city$x<-(df_city$long-min_long)/(max_long-min_long)
df_city$y<-(df_city$lat-min_lat)/(max_lat-min_lat)*Map_Scale

labels_x<-seq(110,135,10)
breaks_x<-(labels_x-min_long)/(max_long-min_long)

labels_y<-seq(30,60,10)
breaks_y<-(labels_y-min_lat)/(max_lat-min_lat)

r <- 0.05
scale <- r/max(sqrt(df_city$value))
df_city$start<-rep(c(0,90,180,270)/180*pi,each=nrow(df_city)/4)
df_city$end<-df_city$start+pi/2

ggplot() +
  geom_polygon(data=df_Map, aes(x = x, y = y,group=group),
               fill="white",colour="black",size=0.25)+
  geom_arc_bar(data=df_city,aes(x0=x,y0=y,r0=0,r=sqrt(value)*scale,
                   start=start,
                   end=end,
                   fill=variable),size=0.1)+
  geom_text(data=df_city[!duplicated(df_city$city),],aes(x=x,y=y,label=city),vjust=2.3,size=3)+
  labs(x='long',y='lat',fill='type')+
  scale_x_continuous(breaks=breaks_x,labels=labels_x)+
  scale_y_continuous(breaks=breaks_y,labels=labels_y)+
  coord_fixed()
