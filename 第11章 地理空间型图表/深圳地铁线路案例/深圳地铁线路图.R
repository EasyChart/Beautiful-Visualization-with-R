

#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggplot2)
library(Cairo)
library(showtext)
library(ggmap)
library(dplyr)

mydata_station<- read.csv("深圳地铁线路图_Station.csv", header=T,sep=",")
mydata_Path<- read.csv("深圳地铁线路图_Path.csv", header=T)
mydata_Path$Subway_Num<-factor(mydata_Path$Subway_Num)
mydata_station$Subway_Num<-factor(mydata_station$Subway_Num)
#CairoPDF(file="广州_TrainLine",width=6.77,height=5)
#showtext.begin()

#图11-13-3 深圳市示意地铁线路图----------------------------------------------------
#m <- get_map("shenzhen",zoom=11,messaging = FALSE,maptype="toner-lite",source="google")
#ggmap(m)+
  ggplot()+
  geom_path (data=mydata_Path,aes(x=x,y=y,group=Subway_Num,colour=Subway_Num), size=1,linejoin = "bevel", lineend = "square")+
  geom_point(data=mydata_station,aes(x=x,y=y,group=Subway_Num,colour=Subway_Num),shape=21,size=3,fill="white")
#geom_text(aes(label=Name),size=2)


#图11-13-5 深圳市实际地铁线路图--------------------------------------------------------  
  
  ggplot()+
    geom_path (data=mydata_station,aes(x=long,y=lat,group=Subway_Num,colour=Subway_Num), size=1,linejoin = "bevel", lineend = "square")+
    geom_point(data=mydata_station,aes(x=long,y=lat,group=Subway_Num,colour=Subway_Num),shape=21,size=2.5,fill="white")+
    xlab("long")+
    ylab("lat")
#showtext.end()
#dev.off()


#-图11-13-8 房价分布散点地图----------------------------------------------------------------------------  

mydata_house<- read.csv("深圳二手房地址.csv", header=T,sep=",")

ggplot()+
  geom_point(data=mydata_house,aes(x=longitude,y=latitude,colour=unit_price),shape=19,size=1,alpha=0.8)+
  geom_path (data=mydata_station,aes(x=long,y=lat,group=Subway_Num), size=0.5,linejoin = "bevel", lineend = "square")+
  geom_point(data=mydata_station,aes(x=long,y=lat),shape=21,size=2,fill="white",color='black',stroke=0.1)+
  scale_color_distiller(palette = 'RdYlGn',direction=FALSE)+
  xlim(113.78,114.3)+
  xlab("long")+
  ylab("lat")

#图11-13-9 深圳市地铁线路房价分布图-----------------------------------------------------------------------------  

mydata_house[c('longitude','latitude')]<-mydata_house[c('longitude','latitude')]/180*pi
Num_house<-nrow(mydata_house)

for (i in 1:nrow(mydata_station)){
  temp<-mydata_house
  temp$long<-mydata_station[i,'long']/180*pi
  temp$lat<-mydata_station[i,'lat']/180*pi
  temp<-mutate(temp,d=acos(sin(lat)*sin(latitude)+cos(lat)*cos(latitude)*cos(long-longitude))*6371.004)%>%
        filter(d<=3)%>%
        summarise(avg=mean(unit_price))
  mydata_station$Unit_Price3[i]<-temp$avg
}

Price_max<-max(mydata_station$Unit_Price)
Price_min<-min(mydata_station$Unit_Price)

mydata_station$Unit_Price2<-cut(mydata_station$Unit_Price,breaks=c(0,30000,40000,50000,60000,70000,80000,90000,max(mydata_station$Unit_Price,na.rm=TRUE)),
                        labels=c(" <=30000","30000~40000","40000~50000","50000~60000","60000~70000","70000~80000","80000~90000"," >=90000"),order=TRUE)


#CairoPDF(file="Shenzhen_TrainLine",width=8.52*1.3,height=5.81*1.4)
#showtext.begin()
ggplot()+
  geom_path (data=mydata_Path,aes(x=x,y=y,group=Subway_Num,colour=Subway_Num),
             size=1,linejoin = "bevel", lineend = "square")+
  
  geom_point(data=mydata_station,aes(x=x,y=y,group=Subway_Num2,
                                     size=Unit_Price2,fill=Unit_Price2),shape=21)+
  guides(fill = guide_legend((title="二手房均价(平方米)")),
         size = guide_legend((title="二手房均价(平方米)")))+
  theme_void()+
  theme(legend.position = "right")
#showtext.end()
#dev.off()