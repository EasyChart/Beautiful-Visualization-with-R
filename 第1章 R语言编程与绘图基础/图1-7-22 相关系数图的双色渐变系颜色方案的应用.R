#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)  
library(RColorBrewer)  
library(reshape2) 

mat <- round(cor(mtcars), 1)
mydata <- melt(mat)  
colnames(mydata)<-c("Var1","Var2","value")

mydata$AbsValue<-abs(mydata$value)

#--------------------------------(b) 双色渐变系颜色方案---------------------------------------------------------
ggplot(mydata, aes(x= Var1 , y=Var2)) +
  geom_point(aes(size=AbsValue,fill = value), shape=21, colour="black") +
  scale_fill_gradientn(colours=c(brewer.pal(7,"Set1")[2],"white",brewer.pal(7,"Set1")[1]),na.value=NA)+
  scale_size_area(max_size=12, guide=FALSE) +
  theme(
    text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=13,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.position="right"
  )
#--------------------------------(a) R多色系颜色方案---------------------------------------------------------
mydata$Ceilingcound<-ceiling(mydata$value)

ggplot(mydata, aes(x= Var1 , y=Var2)) +
  geom_point(aes(size=AbsValue,fill  = factor(Ceilingcound)), shape=21, colour="black") +
  scale_fill_manual(values =c(brewer.pal(7,"Set1")[2],brewer.pal(7,"Set1")[1]),labels=c('Negative','Positive'),na.value=NA,name="factor")+
  scale_size_area(max_size=12, guide=FALSE) 
