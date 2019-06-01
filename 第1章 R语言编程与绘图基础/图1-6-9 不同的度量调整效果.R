
library(ggplot2)
library(RColorBrewer)

df<-read.csv("Facet_Data.csv", header = TRUE)

p1<-ggplot(df, aes(x=SOD,y=tau,size=age)) + 
  geom_point(shape=21,color="black",fill="#E53F2F",stroke=0.25,alpha=0.8)+
  scale_size(range = c(1, 8))

p2<-ggplot(df, aes(SOD,tau,fill=age,size=age)) + 
  geom_point(shape=21,colour="black",stroke=0.25,
             alpha=0.8)+
  scale_size(range = c(1, 8))+
  scale_fill_distiller(palette="Reds")

p3<-ggplot(df, aes(x=SOD,y=tau,fill=Class,shape=Class)) + 
  geom_point(size=3,colour="black",stroke=0.25)+
  scale_fill_manual(values=c("#36BED9","#FF0000","#FBAD01"))+
  scale_shape_manual(values=c(21,22,23))


p4<-ggplot(df, aes(SOD,tau,fill=Class,size=age)) + 
  geom_point(shape=21,colour="black",stroke=0.25,
             alpha=0.8) +
  scale_fill_manual(values=c("#36BED9","#FF0000","#FBAD01"))+
  scale_size(range = c(1, 8))


library(gridExtra) 
grid.arrange(p1,p2,p3,p4, ncol = 2, nrow =2)