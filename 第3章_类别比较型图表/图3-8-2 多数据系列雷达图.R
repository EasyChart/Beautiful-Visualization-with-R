
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)

#--------------------------------------------雷达图实现原理-------------------------------------------------------
#coord_radar-Reference:
#https://github.com/cardiomoon/ggplot2new/blob/4e50b7dcfee3246a169702f88f7dd46cbf933f4b/coord_radar.R

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{  theta <- match.arg(theta, c("x", "y"))
r <- if (theta == "x") 
  "y"
else "x"
ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
        direction = sign(direction),
        is_linear = function(coord) TRUE)}

#--------------------------------------------多数据系列-------------------------------------------------------
label_data<-data.frame(
  car=c("biology" , "english" ,"math" ,  "music" , "R-coding" ),
  id=c(1:5) ,
  v1=sample( 0:20,5, replace=T),
  v2=sample( 0:20,5, replace=T)
)

AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)-1],label_data[1,ncol(label_data)])
mydata<-rbind(label_data,AddRow)

myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  

mydata<-melt(mydata,id=c("car", "id"))

ggplot(data=mydata,aes(x=id, y=value,group=variable,fill=variable)) + 
  geom_polygon(colour="black",alpha=0.1)+
  geom_point(size=4,shape=21,color = 'black')+
  coord_radar()+
  #coord_polar() +
  scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
  theme_bw() +
  ylim(0,22)+
  theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle),
        axis.title=element_text(size=15,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        panel.grid.major = element_line(color="grey80"),
        axis.line = element_line(color="black"),
        axis.ticks =  element_line(color="black"))


