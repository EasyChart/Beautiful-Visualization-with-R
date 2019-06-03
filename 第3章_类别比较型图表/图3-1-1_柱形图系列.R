
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)

#---------------------------单数剧系列柱形图----------------------------------------------------
mydata<-data.frame(Cut=c("Fair","Good","Very Good","Premium","Ideal"),
                    Price=c(4300,3800,3950,4700,3500))

#mydata$Cut <- factor(mydata$Cut, levels = mydata$Cut[order(mydata$Order)])
ggplot(data=mydata,aes(x=Cut,y=Price))+
  geom_bar(stat = "identity", 
           width = 0.8,colour="black",size=0.25,
           fill="#FC4E07",alpha=1)+
  ylim(0, 6000)+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black")
  )


#---------------------------双数剧系列柱形图----------------------------------------------------
library(reshape2)
mydata<-read.csv("MultiColumn_Data.csv",check.names=FALSE,
                 sep=",",na.strings="NA",
                 stringsAsFactors=FALSE)

mydata<-melt(mydata,id.vars='Catergory')

ggplot(data=mydata,aes(Catergory,value,fill=variable))+
  geom_bar(stat="identity",position=position_dodge(),
           color="black",width=0.7,size=0.25)+
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"))+
  ylim(0, 10)+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.background  =element_blank(),
    legend.position = c(0.88,0.88)
  )


#-------------------------------堆积柱形图-------------------------------------------------------
mydata<-read.csv("StackedColumn_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)


mydata<-melt(mydata,id.vars='Clarity')

ggplot(data=mydata,aes(variable,value,fill=Clarity))+
  geom_bar(stat="identity",position="stack", color="black", width=0.7,size=0.25)+
  scale_fill_manual(values=brewer.pal(9,"YlOrRd")[c(6:2)])+
  ylim(0, 15000)+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.background  =element_blank(),
    legend.position = c(0.85,0.82)
  )

#------------------------------百分比堆积柱形图-------------------------------------------------------

mydata<-read.csv("StackedColumn_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)

mydata<-melt(mydata,id.vars='Clarity')

ggplot(data=mydata,aes(variable,value,fill=Clarity))+
  geom_bar(stat="identity", position="fill",color="black", width=0.8,size=0.25)+
  scale_fill_manual(values=brewer.pal(9,"GnBu")[c(7:2)])+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.position = "right"
  )
