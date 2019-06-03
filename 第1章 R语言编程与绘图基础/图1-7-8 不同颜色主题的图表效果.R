
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)


color_theme<-brewer.pal(7,"Set2")[c(1,2,4,5)]

mydata<-read.csv("Boxplot_Data.csv",stringsAsFactors=FALSE) 


ggplot(mydata, aes(Class, Value))+ 
  geom_boxplot(aes(fill = Class),size=0.25) +
  geom_jitter(width=0.2,size=1.)+
  #geom_dotplot(aes(fill = Class),binaxis = "y", stackdir = "center",dotsize = 0.4)+
  scale_fill_manual(values=c("#4F81BD","#C0504D","#9BBB59","#8064A2"))+
  theme_bw()+
  theme(legend.position="none")


ggplot(mydata, aes(Class, Value))+ 
  geom_boxplot(aes(fill = Class),size=0.25) +
  geom_jitter(width=0.2,size=1.)+
  #geom_dotplot(aes(fill = Class),binaxis = "y", stackdir = "center",dotsize = 0.4)+
  scale_fill_manual(values=c("#FF0000","#0000FF","#00FFFF","#FF00FF"))+
  theme_bw()+
  theme(legend.position="none")


ggplot(mydata, aes(Class, Value))+ 
  geom_boxplot(aes(fill = Class),size=0.25,outlier.color=NA) +
  geom_jitter(width=0.2,size=1.)+
  theme_bw()+
  theme(legend.position="none")

