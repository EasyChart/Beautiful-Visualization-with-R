
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)  

#-----------------------------(b) 带透明度设置的散点图------------------------------
mydata<-read.csv("HighDensity_Scatter_Data.csv",stringsAsFactors=FALSE)

ggplot(data = mydata, aes(x,y)) +
  geom_point( colour="black",alpha=0.1)+
  labs(x = "Axis X",y="Axis Y")+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.position="none"
  )

#-----------------------------------(c) kmeans聚类的散点图-----------------------------

kmeansResult<- kmeans(mydata, 2, nstart = 20)

mydata$cluster <- as.factor(kmeansResult$cluster)

ggplot(data = mydata, aes(x,y,color=cluster)) +
  geom_point( alpha=0.2)+
  scale_color_manual(values=c("#00AFBB",  "#FC4E07"))+
  labs(x = "Axis X",y="Axis Y")+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",color="black"),
    legend.background=element_blank(),
    legend.position=c(0.85,0.15)
  )

#---------------------------------(d) 带椭圆标定的聚类散点图--------------------------------------------------

ggplot(data = mydata, aes(x,y,color=cluster)) +
  geom_point (alpha=0.2)+
  # 绘制透明度为0.2 的散点图
  stat_ellipse(aes(x=x,y=y,fill= cluster), geom="polygon", level=0.95, alpha=0.2) +
  #绘制椭圆标定不同类别，如果省略该语句，则绘制图3-1-7(c)
  scale_color_manual(values=c("#00AFBB","#FC4E07")) +#使用不同颜色标定不同数据类别
  scale_fill_manual(values=c("#00AFBB","#FC4E07"))+  #使用不同颜色标定不同椭类别
  labs(x = "Axis X",y="Axis Y")+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",color="black"),
    legend.background=element_blank(),
    legend.position=c(0.85,0.15)
  )
