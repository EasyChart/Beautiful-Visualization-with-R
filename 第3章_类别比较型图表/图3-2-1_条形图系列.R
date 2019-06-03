
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)       
library(RColorBrewer)

#---------------------------单数剧系列条形图----------------------------------------------------
mydata<-read.csv("Stackedbar_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)

mydata$Country <- factor(mydata$Country, levels = mydata$Country[order(mydata$Pensions)])


ggplot(data=mydata,aes(Country,Pensions))+
  geom_bar(stat="identity", color="black", width=0.6,fill="#FC4E07",size=0.25) +#"#00AFBB"
  scale_fill_manual(values=brewer.pal(9,"YlOrRd")[c(6:2)])+
  coord_flip()+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.position = "right"# c(0.83,0.15)
  )

#---------------------------双数剧系列条形图----------------------------------------------------
library(reshape)
mydata<-read.csv("Stackedbar_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)

mydata<-mydata[,c(1,3,2)]
mydata$Country <- factor(mydata$Country, levels = mydata$Country[order(mydata$Pensions)])

mydata<-melt(mydata,id.vars='Country')

ggplot(data=mydata,aes(Country,value,fill=variable))+
  geom_bar(stat="identity", color="black", position=position_dodge(),width=0.7,size=0.25)+
  scale_fill_manual(values=c("#00AFBB", "#FC4E07", "#E7B800"))+
  coord_flip()+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.background  =element_blank(),
    legend.position = c(0.83,0.12)
  )


#-------------------------------堆积条形图-------------------------------------------------------
mydata<-read.csv("Stackedbar_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)


rowsum<-sort(rowSums(mydata[,2:ncol(mydata)]),index.return=TRUE)

mydata$Country <- factor(mydata$Country, levels = mydata$Country[order(rowsum$ix)])
mydata<-melt(mydata,id.vars='Country')

ggplot(data=mydata,aes(Country,value,fill=variable))+
  geom_bar(stat="identity",position="stack", color="black", width=0.65,size=0.25)+
  scale_fill_manual(values=brewer.pal(9,"YlOrRd")[c(6:2)])+
  ylim(0, 35)+
  coord_flip()+
  theme(
    #text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.position = "right"# c(0.83,0.15)
  )

#------------------------------百分比堆积柱形图-------------------------------------------------------
mydata<-read.csv("Stackedbar_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)

sum<-sort(rowSums(mydata[,2:ncol(mydata)]),index.return=TRUE)

mydata$Country <- factor(mydata$Country, levels = mydata$Country[order(sum$ix)])
mydata<-melt(mydata,id.vars='Country')

library(RColorBrewer)
ggplot(data=mydata,aes(Country,value,fill=variable))+
  geom_bar(stat="identity",position="fill", color="black", width=0.65,size=0.25)+
  scale_fill_manual(values=brewer.pal(9,"GnBu")[c(7:2)])+
  coord_flip()+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.position = "right"
  )


