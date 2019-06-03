#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

df<-read.csv("Facet_Data.csv", header = TRUE)

#-------------------------base----------------------------

plot(df$SOD, df$tau)#,pch=21,lty=0.25,col="grey10") 
hist(df$SOD,breaks =30,ylim=c(0,40),main  = "")
boxplot(SOD~Class,data=df,xlab="Class",ylab="SOD")


#----------------------------lattice---------------------------
library(lattice)
p1<-xyplot(SOD~tau,df,col="black")

p2<-histogram(~SOD,df,type="count",nint=30,col="white")


p3<-bwplot(SOD~Class,df,xlab="Class", 
           par.settings = canonical.theme(color = FALSE))

library(gridExtra) 
grid.arrange(p1,p2,p3, ncol = 3, nrow = 1)


#-------------------------ggplot2----------------------------
library(ggplot2)

p1<-ggplot(df, aes(x=SOD,y=tau)) + 
  geom_point() #shape=21,color="black",fill="red",size=3,stroke=0.1

p2<-ggplot(df, aes(SOD)) + 
  geom_histogram(bins=30,colour="black",fill="white")

p3<-ggplot(df, aes(x=Class,y=SOD)) + 
  geom_boxplot() 

library(gridExtra) 
grid.arrange(p1,p2,p3, ncol = 3, nrow = 1)
