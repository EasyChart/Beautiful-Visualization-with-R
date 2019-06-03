#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)  
library(reshape2)

#-----------------------------------图4-7-2 行分面的带填充的曲线图(a)-------------------------------------------
mydata0<-read.csv("Facting_Data.csv",stringsAsFactors=FALSE)

colnames(mydata0)<-c("X_Axis",seq(60,25,-5))
mydata<-melt(mydata0,id.vars = "X_Axis")

ggplot(mydata,aes(X_Axis,value,fill=variable))+
  geom_area(color="black",size=0.25)+
  facet_grid(variable~.)+
  theme(
    text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=10,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.position="none"
  )

#-----------------------------------图4-7-2 行分面的带填充的曲线图(b)-------------------------------------------
library(RColorBrewer)
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
mydata0<-read.csv("Facting_Data.csv",stringsAsFactors=FALSE)

N<-ncol(mydata0)-1

colnames(mydata0)<-c("X_Axis",seq(60,25,-5))

mydata<-data.frame(x=numeric(),y=numeric(),variable=character())

for (i in 1:N){
  newdata<-data.frame(spline(mydata0[,1],mydata0[,i+1],n=300,method= "natural"))
  newdata$variable<-colnames(mydata0)[i+1]
  mydata<-rbind(mydata,newdata)
}

mydata$variable<-factor(mydata$variable,levels=seq(60,25,-5))

ggplot(mydata,aes(x,y,group=variable))+
  geom_bar(aes(fill=y),color=NA,size=0.25,stat="identity")+
  geom_line(color="black",size=0.5)+
  scale_fill_gradientn(colours=colormap)+
  facet_grid(variable~.)+
  theme(
    text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=10,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.position="right"
  )



