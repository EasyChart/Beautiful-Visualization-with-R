#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)

df<-read.csv("Facet_Data.csv", header = TRUE)

#------------------------------------------------------ÀëÉ¢ÐÍ------------------------------------------
library(RColorBrewer)

ggplot(df, aes(x=SOD,y=tau,fill=Class)) + 
  geom_point(shape=21,size=3,colour="black",stroke=0.25) +
  scale_fill_brewer(palette='Set1')+
  theme(
    text=element_text(size=14,color="black"),
    plot.title=element_text(size=14,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.8,0.15)
  )

library(viridis)
ggplot(df, aes(x=SOD,y=tau,fill=Class)) + 
  geom_point(shape=21,size=3,colour="black",stroke=0.25) +
  scale_fill_viridis(option = "plasma",discrete =TRUE)+
  theme(
    text=element_text(size=14,color="black"),
    plot.title=element_text(size=14,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.8,0.15)
  )


library(wesanderson)
ggplot(df, aes(x=SOD,y=tau,fill=Class)) + 
  geom_point(shape=21,size=3,colour="black",stroke=0.25) +
  scale_fill_manual(values=wes_palette("Darjeeling1")[c(1,3,5)])+
  theme(
    text=element_text(size=14,color="black"),
    plot.title=element_text(size=14,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.8,0.15)
  )


ggplot(df, aes(x=SOD,y=tau,fill=Class)) + 
  geom_point(shape=21,size=3,colour="black",stroke=0.25) +
  scale_fill_manual(values=c("#E7298A","#66A61E","#E6AB02"))+
  theme(
    legend.background = element_blank(),
    legend.position=c(0.8,0.15)
  )

#-------------------------------------------------Á¬ÐøÐÍ------------------------------------------------
ggplot(df, aes(x = tau, y = SOD, fill=age)) +
  geom_point(shape=21,size=4,colour="black",alpha=0.95) +
  scale_fill_distiller(palette="RdYlBu")+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.85,0.2)
  )


ggplot(df, aes(x = tau, y = SOD, fill=age)) +
  geom_point(shape=21,size=4,colour="black",alpha=0.95) +
  scale_fill_viridis(option = "viridis",discrete =FALSE)+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.85,0.2)
  )

ggplot(df, aes(x = tau, y = SOD, fill=age)) +
  geom_point(shape=21,size=4,colour="black",alpha=0.95) +
  scale_fill_gradient2(low="#00A08A",mid="white",high="#FF0000",midpoint = mean(df$age))+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.85,0.2)
  )

ggplot(df, aes(x = tau, y = SOD, fill=age)) +
  geom_point(shape=21,size=4,colour="black",alpha=0.95) +
  scale_fill_gradientn(colors= terrain.colors(10))+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.85,0.2)
  )
