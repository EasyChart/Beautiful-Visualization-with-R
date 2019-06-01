
library(ggplot2)


df<-read.csv("Facet_Data.csv", header = TRUE)

p1<-ggplot(df, aes(x=SOD,y=tau,size=age)) + 
  geom_point(shape=21,color="black",fill="#336A97",stroke=0.25)

p2<-ggplot(df, aes(SOD,tau,fill=age,size=age)) + 
  geom_point(shape=21,colour="black",stroke=0.25,
             alpha=0.8) 

p3<-ggplot(df, aes(x=SOD,y=tau,fill=Class)) + 
  geom_point(shape=21,size=3,colour="black",stroke=0.25)


p4<-ggplot(df, aes(SOD,tau,fill=Class,size=age)) + 
  geom_point(shape=21,colour="black",stroke=0.25,
             alpha=0.8) 


library(gridExtra) 
grid.arrange(p1,p2,p3,p4, ncol = 2, nrow =2)