#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)

mydata<-read.csv("Scatter_Data.csv",stringsAsFactors=FALSE) 

ggplot(data = mydata, aes(x,y)) +
  geom_point(fill="black",colour="black",size=3,shape=21) +
  #geom_smooth(method="lm",se=TRUE,formula=y ~ splines::bs(x, 5),colour="red")+ #(h)
  #geom_smooth(method = 'gam',formula=y ~s(x))+   #(g)
  geom_smooth(method = 'loess',span=0.4,se=TRUE,colour="#00A5FF",fill="#00A5FF",alpha=0.2)+ #(f)
  scale_y_continuous(breaks = seq(0, 125, 25))+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",hjust=.5,color="black"),
    legend.position="none"
  )
