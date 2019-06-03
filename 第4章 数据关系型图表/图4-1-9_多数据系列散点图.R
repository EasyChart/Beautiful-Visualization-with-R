#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)


mydata<-read.csv("HighDensity_Scatter_Data.csv",stringsAsFactors=FALSE)
mydata<-mydata[round(runif(300,0,10000)),]

kmeansResult<- kmeans(mydata, 2, nstart = 20)

mydata$cluster <- as.factor(kmeansResult$cluster)

ggplot(data = mydata, aes(x,y,fill=cluster,shape=cluster)) +
  geom_point(size=4,colour="black",alpha=0.7)+
  scale_shape_manual(values=c(21,23))+
  scale_fill_manual(values=c("#00AFBB",  "#FC4E07"))+
  labs(x = "Axis X",y="Axis Y")+
  scale_y_continuous(limits = c(-5, 10))+
  scale_x_continuous(limits = c(-5, 10))+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",color="black"),
    legend.background=element_blank(),
    legend.position=c(0.85,0.15)
  )

