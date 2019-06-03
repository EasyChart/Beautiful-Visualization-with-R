#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(scatterpie)
library(RColorBrewer)

crime <- read.csv("crimeRatesByState2005.tsv",header = TRUE, sep = "\t", stringsAsFactors = F)
radius <- sqrt(crime$population / pi)
Max_radius<-max(radius)
Bubble_Scale<-0.1
crime$radius <- Bubble_Scale * radius/Max_radius

mydata<-crime[,c(2,4,3,5:8)]  #数据集构造
Col_Mean<-apply(mydata,2,mean)
Col_Sort<-sort(Col_Mean,index.return=TRUE,decreasing = TRUE)
mydata<-mydata[,Col_Sort$ix]
x<-(mydata$murder-min(mydata$murder))/(max(mydata$murder)-min(mydata$murder))+0.00001
y<-(mydata$Robbery-min(mydata$Robbery))/(max(mydata$Robbery)-min(mydata$Robbery))+0.00001

xlabel<-seq(0,10,2)
xbreak<-(xlabel-min(mydata$murder))/(max(mydata$murder)-min(mydata$murder))+0.00001
ylabel<-seq(0,260,50)
ybreak<-(ylabel-min(mydata$Robbery))/(max(mydata$Robbery)-min(mydata$Robbery))+0.00001

mydata2<-data.frame(x,y,radius=crime$radius)
mydata2<-cbind(mydata2,mydata)

Legnd_label<-colnames(mydata2)[4:10]
colnames(mydata2)[4:10]<-LETTERS[1:7]

#---------------------------------------图7-1-3 散点复合饼图系列(a)-------------------------------------------------
ggplot() + 
  geom_scatterpie(aes(x=x, y=y,r=0.05), data=mydata2, cols=colnames(mydata2)[4:10],alpha=0.9,size=0.1) +
  scale_fill_manual(values=colorRampPalette(brewer.pal(7, "Set2"))(7),labels=Legnd_label)+
  #geom_scatterpie_legend(mydata2$radius, x=0.1, y=0.95, n=5,labeller=function(x) round((x* Max_radius/ Bubble_Scale)^2*pi))+
  #geom_scatterpie_legend(mydata2$radius, x=0.009758116, y=0.090868067, n=4,labeller=function(x) round((x* Max_radius/ Bubble_Scale)^2*pi))+
  scale_x_continuous(breaks=xbreak, labels=xlabel)+
  scale_y_continuous(breaks=ybreak, labels=ylabel)+
  xlab("murder")+
  ylab("Robbery")+
  coord_fixed()+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=15,face="plain",color="black"),
    legend.text = element_text(size=14,face="plain",color="black")
  )

#---------------------------------------图7-1-3 散点复合饼图系列(b)-------------------------------------------------
ggplot() + 
  geom_scatterpie(aes(x=x, y=y,r=radius), data=mydata2, cols=colnames(mydata2)[4:10],alpha=0.9,size=0.25) +
  scale_fill_manual(values=colorRampPalette(brewer.pal(7, "Set2"))(7),labels=Legnd_label)+
  geom_scatterpie_legend(mydata2$radius, x=0.1, y=0.95, n=5,
                         labeller=function(x) round((x* Max_radius/ Bubble_Scale)^2*pi))+
  #geom_scatterpie_legend(mydata2$radius, x=0.009758116, y=0.090868067, n=4,labeller=function(x) round((x* Max_radius/ Bubble_Scale)^2*pi))+
  scale_x_continuous(breaks=xbreak, labels=xlabel)+
  scale_y_continuous(breaks=ybreak, labels=ylabel)+
  xlab("murder")+
  ylab("Robbery")+
  coord_fixed()+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=15,face="plain",color="black"),
    legend.text = element_text(size=14,face="plain",color="black")
  )

#label<-seq(350000,35000000,10000000)
#x<-sqrt(label/pi)/Max_radius*Bubble_Scale
