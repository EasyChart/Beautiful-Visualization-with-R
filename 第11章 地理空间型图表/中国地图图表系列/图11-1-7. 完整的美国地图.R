
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts


library(ggplot2)       #绘图函数
library(dplyr)          #数据合并工具
library(Cairo)         #图片高清导出
library(RColorBrewer)  #地图配色模板
library(scales)        #分割数据
library(showtext)
library(sp)  
library(mapproj) 
library(rgdal)   #提供readOGR()函数

#--------------------------USA_adm2.shp------------------------------------------------------

dataProjected <- readOGR("USA_adm_shp/USA_adm2.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map2 <- full_join(watershedPoints, dataProjected@data, by = "id")

df_map2<-df_map2[,c("lat","long","group","NAME_1","NAME_2")]


df_mainland<-subset(df_map2,NAME_1!='Alaska'& NAME_1!='Hawaii')
df_Hawaii<-subset(df_map2,NAME_1=="Hawaii")    
df_Alaska<-subset(df_map2,NAME_1=="Alaska")

df_Hawaii$long<-df_Hawaii$long+55
df_Hawaii$lat<-df_Hawaii$lat+3

df_Alaska$long<-(df_Alaska$long-min(df_Alaska$long))/(max(df_Alaska$long)-min(df_Alaska$long))*100-120
df_Alaska$lat<-(df_Alaska$lat-min(df_Alaska$lat))/(max(df_Alaska$lat)-min(df_Alaska$lat))*10+20

df_map2<-rbind(df_mainland,df_Hawaii,df_Alaska)

#---------------------------构造数据集-------------------------------------------------
mydata<-data.frame(NAME_2=unique(df_map2$NAME_2),
                   value=round(runif(length(unique(df_map2$NAME_2)),0,10)))

df_map2<-join(df_map2,mydata,type="full")


#--------------------------USA_adm1.shp------------------------------------------------------
dataProjected <- readOGR("USA_adm_shp/USA_adm1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map1<- full_join(watershedPoints, dataProjected@data, by = "id")
df_map1<-df_map1[,c("lat","long","group","NAME_1")]

df_mainland<-subset(df_map1,NAME_1!='Alaska'& NAME_1!='Hawaii')
df_Hawaii<-subset(df_map1,NAME_1=="Hawaii")    
df_Alaska<-subset(df_map1,NAME_1=="Alaska")

df_Hawaii$long<-df_Hawaii$long+55
df_Hawaii$lat<-df_Hawaii$lat+3

df_Alaska$long<-(df_Alaska$long-min(df_Alaska$long))/(max(df_Alaska$long)-min(df_Alaska$long))*100-120
df_Alaska$lat<-(df_Alaska$lat-min(df_Alaska$lat))/(max(df_Alaska$lat)-min(df_Alaska$lat))*10+20


df_map1<-rbind(df_mainland,df_Hawaii,df_Alaska)

df_map2<-df_map2[df_map2$long<=-50,]
df_map1<-df_map1[df_map1$long<=-50,]



ggplot() +
    geom_polygon(data=df_map2, aes(x = long, y = lat,group=group,fill = value),colour="gray",size=0.25) +
    geom_polygon(data=df_map1, aes(x = long, y = lat,group=group),fill =NA,colour="black",size=0.25) +
    scale_fill_distiller(palette="Spectral")+
    coord_map("albers",parameters=c(30,40))+
    theme_void()+
    theme( 
    legend.background =element_blank(),
    legend.position=c(0.92,0.4))
