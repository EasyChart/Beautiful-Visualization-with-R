#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)  
library(reshape2)
library(wesanderson)


Alz <-read.csv("Facet_Data.csv", header = T)

#-----------------------------------图8-2-1 列分面的散点图(a) 列分面的散点图--------------------------------------------------
ggplot(Alz, aes(x = tau, y = SOD, fill = Class)) +
  geom_point(size=3,shape=21,colour="black") +
  # stat_smooth(method = "loess")+
  facet_wrap( ~ Class) +
  theme(
    strip.text = element_text(size=13,face="plain",color="black"),
    text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=14,face="plain",color="black"),
    axis.text = element_text(size=11,face="plain",color="black"),
    legend.position="none"
  )

#-----------------------------------图8-2-1 列分面的散点图 (b) 列分面的带拟合曲线的散点图-------------------------------------------------------------------
ggplot(Alz, aes(x = tau, y = SOD,colour=Class, fill = Class)) +
  geom_point(size=2,shape=21,fill="black",colour="black",alpha=0.5) +
  stat_smooth(method = "loess")+
  facet_grid(. ~ Class) +
  theme(
    strip.text = element_text(size=13,face="plain",color="black"),
    text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=14,face="plain",color="black"),
    axis.text = element_text(size=11,face="plain",color="black"),
    legend.position="none"
  )

#---------------------------------------图8-2-2 列分面的气泡图(a) 列分面的气泡图-----------------------------------------------------------------------------------
ggplot(Alz, aes(x = tau, y = SOD, fill= Class, size = age)) +
  geom_point(shape=21,colour="black",alpha=0.7) +
  facet_wrap( ~ Class)+
  guides(fill = FALSE)+
  theme(
    strip.text = element_text(size=13,face="plain",color="black"),
    text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=14,face="plain",color="black"),
    axis.text = element_text(size=11,face="plain",color="black"),
    legend.position=c(0.935,0.13),
    legend.background = element_rect(fill=alpha("white",0))
  )

#----------------------------------------图8-2-2 列分面的气泡图 (b) 列分面的带颜色映射的气泡图----------------------------------------------------------------------------
ggplot(Alz, aes(x = tau, y = SOD, fill=age, size = age)) +
  geom_point(shape=21,colour="black",alpha=0.95) +
  
  scale_fill_gradient2(low="#00A08A",mid="white",high="#FF0000",midpoint = mean(Alz$age))+
  facet_wrap( ~ Class)+
  #guides(fill = FALSE)+
  theme(
    strip.text = element_text(size=13,face="plain",color="black"),
    text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=14,face="plain",color="black"),
    axis.text = element_text(size=11,face="plain",color="black"),
    #legend.position=c(0.935,0.13),
    legend.background = element_rect(fill=alpha("white",0))
  )

#-----------------------------------------图8-2-3 矩阵分面气泡图--------------------------------------------------------------------------
Alz$male<-as.character(Alz$male)
Alz$male[Alz$male=="0"] <- "Female"
Alz$male[Alz$male=="1"] <- "Male"
colnames(Alz)[colnames(Alz)=="male"]<-"Gender"
ggplot(Alz, aes(x = tau, y = SOD, fill= Class, size = age)) +
  geom_point(shape=21,colour="black",alpha=0.7) +
  facet_grid(Gender ~ Class)+
  guides(fill = FALSE)+
  theme(
    strip.text = element_text(size=13,face="plain",color="black"),
    text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=14,face="plain",color="black"),
    axis.text = element_text(size=11,face="plain",color="black"),
    legend.position=c(0.935,0.11),
    legend.background = element_rect(fill=alpha("white",0))
  )
