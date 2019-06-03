#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
#library(RColorBrewer)
#library(reshape2)
df<-read.csv("MappingAnalysis_Data.csv", header = TRUE)

#--------------------------------------Size---------------------------
ggplot(data=df, aes(x=Time,y=value,group=variable)) + 
  geom_line()+
  geom_point(shape=21,size=4,colour="black",fill="white") +
  theme_classic()+
  theme(
    text=element_text(size=14,color="black"),
    plot.title=element_text(size=14,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.2,0.8)
  )

#--------------------------------------color---------------------------
ggplot(data=df, aes(x=Time,y=value,fill=variable)) + 
  geom_line()+
  geom_point(shape=21,size=4,colour="black") +
  scale_fill_manual(values=c("grey60","grey30","black","white"))+
  theme_classic()+
  theme(
    text=element_text(size=14,color="black"),
    plot.title=element_text(size=14,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.2,0.8)
  )
#--------------------------------------shape---------------------------
ggplot(data=df, aes(x=Time,y=value,shape=variable)) + 
  geom_line()+
  geom_point(size=4,colour="black",fill="grey60") +
  scale_shape_manual(values=c(21,22,23,24))+
  theme_classic()+
  theme(
    text=element_text(size=14,color="black"),
    plot.title=element_text(size=14,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.2,0.8)
  )

#--------------------------------------color---------------------------
ggplot(data=df, aes(x=Time,y=value,fill=variable)) + 
  geom_line()+
  geom_point(shape=21,size=4,colour="black") +
  scale_fill_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  theme_classic()+
  theme(
    text=element_text(size=14,color="black"),
    plot.title=element_text(size=14,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.2,0.8)
  )
#--------------------------------------color+shape---------------------------
ggplot(data=df, aes(x=Time,y=value,fill=variable,shape=variable)) + 
  geom_line()+
  geom_point(size=4,colour="black") +
  scale_fill_manual(values=c("grey60","grey30","black","white"))+
  scale_shape_manual(values=c(21,22,23,24))+
  theme_classic()+
  theme(
    text=element_text(size=14,color="black"),
    plot.title=element_text(size=14,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.2,0.8)
  )

#--------------------------------------color+shape---------------------------
ggplot(data=df, aes(x=Time,y=value,fill=variable,shape=variable)) + 
  geom_line()+
  geom_point(size=4,colour="black") +
  scale_fill_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  scale_shape_manual(values=c(21,22,23,24))+
  theme_classic()+
  theme(
    text=element_text(size=14,color="black"),
    plot.title=element_text(size=14,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.2,0.8)
  )
