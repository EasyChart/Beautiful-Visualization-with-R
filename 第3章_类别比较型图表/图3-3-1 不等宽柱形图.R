
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(Cairo)
library(showtext)

mydata<-data.frame(Name=paste0("Project",1:5),Scale=c(35,30,20,10,5),ARPU=c(56,37,63,57,59))
mydata$xmin<-0
for (i in 2:5){
  mydata$xmin[i]<-sum(mydata$Scale[1:i-1])
}
#构造矩形X轴的终点（最大点）
for (i in 1:5){
  mydata$xmax[i]<-sum(mydata$Scale[1:i])
}
#构造数据标签的横坐标：
for (i in 1:5){
  mydata$label[i]<-sum(mydata$Scale[1:i])-mydata$Scale[i]/2
}

#CairoPDF(file="不等宽柱形图.pdf",width=4.89,height=5.53)
#showtext.begin()
#windowsFonts(myFont = windowsFont("微软雅黑"))
ggplot(mydata)+
  geom_rect(aes(xmin=xmin,xmax=xmax,ymin=0,ymax=ARPU,fill=Name),colour="black",size=0.25)+
  geom_text(aes(x=label,y=ARPU+3,label=ARPU),size=4,col="black")+
  geom_text(aes(x=label,y=-2.5,label=Name),size=4,col="black")+
  ylab("ARPU")+
  xlab("scale")+
  ylim(-5,80)+
  theme(panel.background=element_rect(fill="white",colour=NA),
        panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        text=element_text(size=15),
        plot.title=element_text(size=15,hjust=.5),#family="myfont",
        legend.position="none"
  )
#showtext.end()
#dev.off()
