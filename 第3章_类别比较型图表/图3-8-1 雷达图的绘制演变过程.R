#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)

#--------------------------------------------雷达图实现原理-------------------------------------------------------
#Reference:https://github.com/cardiomoon/ggplot2new/blob/4e50b7dcfee3246a169702f88f7dd46cbf933f4b/coord_radar.R

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{  theta <- match.arg(theta, c("x", "y"))
r <- if (theta == "x") 
  "y"
else "x"
ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
        direction = sign(direction),
        is_linear = function(coord) TRUE)}


#----------------------------------------单数据系列--------------------------------------------------------------
label_data<-data.frame(car=c("Math" , "English" , "Biology" , "Music" , "R-Coding" ),
                    id=c(1:5) ,
                    value=c(12 , 2 ,14 ,20, 18))

AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)])
mydata<-rbind(label_data,AddRow)

myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  

ggplot() + 
  geom_polygon(data=mydata,aes(x=id, y=value),color = "black", fill=brewer.pal(7,"Set1")[1],alpha=0.1)+
  geom_point(data=mydata,aes(x=id, y=value),size=5,shape=21,color = 'black', fill=brewer.pal(7,"Set1")[1])+
  coord_polar() + #实现为图3-8-1(c) 的圆形雷达图
  ylim(0,22)+
  theme_light()+
  theme(axis.text.x=element_text(size = 11,colour="black"))

ggplot() + 
  geom_polygon(data=mydata,aes(x=id, y=value),color = "black", fill=brewer.pal(7,"Set1")[1],alpha=0.1)+
  geom_point(data=mydata,aes(x=id, y=value),size=5,shape=21,color = 'black', fill=brewer.pal(7,"Set1")[1])+
  coord_polar() + #实现为图3-8-1(c) 的圆形雷达图
  #coord_radar()+  #
  scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
  ylim(0,22)+
  theme_light()+
  theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle))

