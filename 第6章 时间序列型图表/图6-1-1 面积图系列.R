#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
library(reshape2)


#-------------------------图6-1-1 多数据系列图. (a)折线图-------------------------
mydata<-read.csv("Line_Data.csv",stringsAsFactors=FALSE) 
mydata$date<-as.Date(mydata$date)

mydata<-melt(mydata,id="date")
ggplot(mydata, aes(x =date, y = value,color=variable) )+
  #geom_area(fill="#FF6B5E",alpha=0.75)+ 
  geom_line(size=1)+
  scale_x_date(date_labels = "%Y",date_breaks = "2 year")+
  xlab("Year")+ 
  ylab("Value")+
  theme( axis.title=element_text(size=10,face="plain",color="black"),
         axis.text = element_text(size=10,face="plain",color="black"),
         legend.position = c(0.15,0.8),
         legend.background = element_blank()) 

#-------------------------图6-1-1 多数据系列图.(b)面积图.-------------------------
ggplot(mydata, aes(x =date, y = value,group=variable) )+
  geom_area(aes(fill=variable),alpha=0.5,position="identity")+ 
  geom_line(aes(color=variable),size=0.75)+#color="black",
  scale_x_date(date_labels = "%Y",date_breaks = "2 year")+
  xlab("Year")+ 
  ylab("Value")+
  theme( axis.title=element_text(size=10,face="plain",color="black"),
         axis.text = element_text(size=10,face="plain",color="black"),
         legend.position = c(0.15,0.8),
         legend.background = element_blank()) 


#--------------------------------------图6-1-2 填充面积折线图. (a)纯色填充-------------------
mydata<-read.csv("Area_Data.csv",stringsAsFactors=FALSE) 
mydata$date<-as.Date(mydata$date)
ggplot(mydata, aes(x =date, y = value) )+
  geom_area(fill="#FF6B5E",alpha=0.75)+ 
  geom_line(color="black",size=0.75)+
  scale_x_date(date_labels = "%Y",date_breaks = "2 year")+
  xlab("Year")+ 
  ylab("Value")+
  theme( axis.title=element_text(size=10,face="plain",color="black"),
         axis.text = element_text(size=10,face="plain",color="black")) 


#------------------------图6-1-2 填充面积折线图.(b)颜色映射填充.------------------------

x<-as.numeric(mydata$date)
newdata<-data.frame(spline(x,mydata$value,n=1000,method= "natural"))
newdata$date<-as.Date(newdata$x,origin = "1970-01-01")
ggplot(newdata, aes(x =date, y = y) )+ #geom_area(fill="#FF6B5E",alpha=0.75)
  geom_bar(aes(fill=y,colour=y),stat = "identity",alpha=1,width = 1)+ 
  geom_line(color="black",size=0.5)+
  scale_color_gradientn(colours=brewer.pal(9,'Reds'),name = "Value")+
  scale_x_date(date_labels = "%Y",date_breaks = "2 year")+
  xlab("Year")+ 
  ylab("Value")+
  guides(fill=FALSE)+
  theme( axis.title=element_text(size=10,face="plain",color="black"),
         axis.text = element_text(size=10,face="plain",color="black"),
         legend.position = c(0.12,0.75),
         legend.background = element_blank() )

#------------------------图6-1-3 夹层填充面积图. (a)单色------------------------------

mydata<-read.csv("Line_Data.csv",stringsAsFactors=FALSE) 
mydata$date<-as.Date(mydata$date)

mydata1<-mydata

mydata1$ymin<-apply(mydata1[,c(2,3)], 1, min)
mydata1$ymax<-apply(mydata1[,c(2,3)], 1, max)

ggplot(mydata1, aes(x =date))+
  geom_ribbon( aes(ymin=ymin, ymax=ymax),alpha=0.5,fill="white",color=NA)+
  #geom_area(aes(fill=variable),alpha=0.5,position="identity")+ 
  geom_line(aes(y=AMZN,color="#FF6B5E"),size=0.75)+#color="black",
  geom_line(aes(y=AAPL,color="#00B2F6"),size=0.75)+#color="black",
   scale_x_date(date_labels = "%Y",date_breaks = "2 year")+
   xlab("Year")+ 
   ylab("Value")+
   scale_colour_manual(name = "Variable", 
                       labels = c("AMZN", "AAPL"),
                       values = c("#FF6B5E", "#00B2F6"))+
   theme( axis.title=element_text(size=10,face="plain",color="black"),
         axis.text = element_text(size=10,face="plain",color="black"),
         legend.position = c(0.15,0.8),
         legend.background = element_blank()) 

#------------------------图6-1-3 夹层填充面积图.  (b)多色------------------------------
mydata1$ymin<-apply(mydata1[,c(2,3)], 1, min)
mydata1$ymax<-apply(mydata1[,c(2,3)], 1, max)

mydata1$ymin1<-mydata1$ymin
mydata1$ymin1[as.integer((mydata1$AAPL-mydata1$AMZN)>0)]=NA

mydata1$ymax1<-mydata1$ymax
mydata1$ymax1[as.integer((mydata1$AAPL-mydata1$AMZN)>0)==0]=NA

mydata1$ymin2<-mydata1$ymin
mydata1$ymin2[as.integer((mydata1$AAPL-mydata1$AMZN)<=0)==0]=NA

mydata1$ymax2<-mydata1$ymax
mydata1$ymax2[as.integer((mydata1$AAPL-mydata1$AMZN)<=0)==0]=NA


ggplot(mydata1, aes(x =date))+
  geom_ribbon( aes(ymin=ymin1, ymax=ymax1),alpha=0.5,fill="#FF6B5E",color=NA)+#,fill = AMZN > AAPL
  geom_ribbon( aes(ymin=ymin2, ymax=ymax2),alpha=0.5,fill="#00B2F6",color=NA)+#,fill = AMZN > AAPL
  #geom_area(aes(fill=variable),alpha=0.5,position="identity")+ 
  geom_line(aes(y=AMZN,color="#FF6B5E"),size=0.75)+#color="black",
  geom_line(aes(y=AAPL,color="#00B2F6"),size=0.75)+#color="black",
  scale_x_date(date_labels = "%Y",date_breaks = "2 year")+
  xlab("Year")+ 
  ylab("Value")+
  scale_colour_manual(name = "Variable", 
                      labels = c("AMZN", "AAPL"),
                      values = c("#FF6B5E", "#00B2F6"))+
  theme( axis.title=element_text(size=10,face="plain",color="black"),
         axis.text = element_text(size=10,face="plain",color="black"),
         legend.position = c(0.15,0.8),
         legend.background = element_blank()) 

#--------------------------------图6-1-3 夹层填充面积图.(c)颜色映射填充.------------------------------------------
library(ggridges)
ggplot(mydata1, aes(x=date)) +
  geom_ridgeline_gradient( aes(y=ymin, height = ymax-ymin,  fill = ymax-ymin)) +
  geom_line(aes(y=AMZN),color="black",size=0.75)+#color="black",
  geom_line(aes(y=AAPL),color="black",size=0.75)+#color="black",
  scale_fill_gradientn(colours= brewer.pal(9,'RdBu'),name = "Value")+
  theme(legend.position = c(0.15,0.8),
        legend.background = element_blank()) 
