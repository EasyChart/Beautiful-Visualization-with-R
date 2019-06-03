#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)

df<-read.csv("PolarBar_Data.csv", header = TRUE)

ggplot(df,aes(Date,Value))+
  geom_bar(stat = "identity", width = 10,colour="black",size=0.25,fill="#3BC8EB")+
  scale_x_continuous(name="Time(day)",breaks=seq(0,360,30))+
  scale_y_continuous(breaks=seq(0,160,20),limits=c(0,160),expand = expand_scale(add = 0))+
  theme_classic()


ggplot(df,aes(Date,Value))+
  geom_bar(stat = "identity", width = 10,colour="black",size=0.25,fill="#3BC8EB")+
  scale_x_continuous(breaks=seq(0,360,30))+
  scale_y_continuous(breaks=seq(0,115,30),limits=c(-30,115))+
  coord_polar(theta = "x",start=0) +
  theme_light()+
  theme( panel.background = element_blank(),
         panel.grid.major = element_line(colour = "grey80",size=.25),
         axis.text.y = element_text(size = 11,colour="black"),
         axis.line.y = element_line(size=0.25),
         axis.text.x=element_text(size = 11,colour="black"))

#------------------------------------------------------------
df<-read.csv("PolarArea_Data.csv", header = TRUE)

ggplot(df,aes(Date,Value))+
  geom_area(colour="black",size=0.25,fill="#FFA1B9")+
  scale_x_continuous(name="Time(day)",breaks=seq(0,360,60),expand = expand_scale(add = 0))+
  scale_y_continuous(breaks=seq(0,3500,500),limits=c(0,3500),expand = expand_scale(add = 0))+
  theme_classic()


ggplot(df,aes(Date,Value))+
  geom_area(colour=NA,size=0.25,fill="#FFA1B9")+
  geom_line(colour="black",size=0.25)+
  scale_x_continuous(name="Time(day)",breaks=seq(0,360,30))+
  scale_y_continuous(breaks=seq(0,3000,500),limits=c(0,3000))+
  coord_polar(theta = "x",start=0) +
  theme_light()+
  theme( panel.background = element_blank(),
         panel.grid.major = element_line(colour = "grey80",size=.25),
         axis.text.y = element_text(size = 10,colour="black"),
         axis.line.y = element_line(size=0.25),
         axis.text.x=element_text(size = 11,colour="black"))
