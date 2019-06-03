#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
library(reshape2)


mydata<-read.csv("StackedArea_Data.csv",stringsAsFactors=FALSE) 
mydata$Date<-as.Date(mydata$Date)

#----------------------------图6-1-4堆积面积图.(a) 堆积面积图--------------------------------
mydata<-melt(mydata,id="Date")
ggplot(mydata, aes(x =Date, y = value,fill=variable) )+
  geom_area(position="stack",alpha=1)+ 
  geom_line(position="stack",size=0.25,color="black")+
  scale_x_date(date_labels = "%Y",date_breaks = "2 year")+
  xlab("Year")+ 
  ylab("Value")+
  theme( axis.title=element_text(size=10,face="plain",color="black"),
         axis.text = element_text(size=10,face="plain",color="black"),
         legend.position = "right",
         legend.background = element_blank()) 

#-----------------------------图6-1-4堆积面积图.  (b)百分比堆积面积图.----------------------------------
ggplot(mydata, aes(x =Date, y = value,fill=variable) )+
  geom_area(position="fill",alpha=1)+ 
  geom_line(position="fill",size=0.25,color="black")+
  scale_x_date(date_labels = "%Y",date_breaks = "2 year")+
  xlab("Year")+ 
  ylab("Value")+
  theme( axis.title=element_text(size=10,face="plain",color="black"),
         axis.text = element_text(size=10,face="plain",color="black"),
         legend.position = "right",
         legend.background = element_blank()) 

