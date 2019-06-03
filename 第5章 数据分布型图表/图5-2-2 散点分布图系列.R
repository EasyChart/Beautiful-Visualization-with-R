#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
library(SuppDists) #提供rJohnson()函数

set.seed(141079)

# Generate sample data -------------------------------------------------------
#findParams函数参考：https://github.com/hadley/boxplots-paper

findParams <- function(mu, sigma, skew, kurt) {
  value <- .C("JohnsonMomentFitR", as.double(mu), as.double(sigma),
              as.double(skew), as.double(kurt - 3), gamma = double(1),
              delta = double(1), xi = double(1), lambda = double(1),
              type = integer(1), PACKAGE = "SuppDists")
  
  list(gamma = value$gamma, delta = value$delta,
       xi = value$xi, lambda = value$lambda,
       type = c("SN", "SL", "SU", "SB")[value$type])
}

# 均值为3，标准差为1的正态分布
n <- rnorm(100,3,1)
# Johnson分布的偏斜度2.2和峰度13
s <- rJohnson(100, findParams(3, 1, 2., 13.1))
# Johnson分布的偏斜度0和峰度20）
k <- rJohnson(100, findParams(3, 1, 2.2, 20))
# 两个峰的均值μ1，μ2分别为1.89和3.79，σ1 = σ2 =0.31
mm <- rnorm(100, rep(c(2, 4), each = 50) * sqrt(0.9), sqrt(0.1))

mydata <- data.frame(
  Class = factor(rep(c("n", "s", "k", "mm"), each = 100),
                 c("n", "s", "k", "mm")),
  Value = c(n, s, k, mm)
)

#----------------------------------------------------(a) 散点抖动图---------------------------------------------------------------------------------------
ggplot(mydata, aes(Class, Value))+
  geom_jitter(aes(fill = Class),position = position_jitter(0.3),shape=21, size = 2)+
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )

# #------------------------------------------------------(b) 蜂群图---------------------------------------------------------------------------------
 
library(ggbeeswarm) #library(beeswarm) biocLite(c("beeswarm","ggplot2"))
ggplot(mydata, aes(Class, Value))+
  geom_beeswarm(aes(fill = Class),shape=21,colour="black",size=2,cex=2)+
  scale_fill_manual(values= c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+ 
  xlab("Class")+
  ylab("Value")+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )

#----------------------------------------------------------(c)点阵图----------------------------------------------------------------------------------------
ggplot(mydata, aes(Class, Value))+
  geom_dotplot(aes(fill = Class),binaxis='y', stackdir='center', dotsize = 0.6)+
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )



#------------------------------------------------------(e) 带误差线散点与点阵组合图--------------------------------------------
ggplot(mydata, aes(Class, Value,fill = Class))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.6)+
  
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  geom_pointrange(stat="summary", fun.data="mean_sdl",fun.args = list(mult=1),
                  color = "black",size = 1.2)+
   geom_point(stat="summary", fun.y="mean",fun.args = list(mult=1),
                color = "white",size = 4)+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )

ggplot(mydata, aes(Class, Value,fill = Class))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.6)+

  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
               geom="pointrange", color = "black",size = 1.2)+
  stat_summary(fun.y="mean", fun.args = list(mult=1),
               geom="point", color = "white",size = 4)+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )

#------------------------------------------------------(d) 带误差线的散点与抖动图--------------------------------------------
ggplot(mydata, aes(Class, Value))+
  geom_jitter(aes(fill = Class),position = position_jitter(0.3),shape=21, size = 2,color="black")+
  
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
               geom="pointrange", color = "black",size = 1.2)+
  stat_summary(fun.y="mean", fun.args = list(mult=1),
               geom="point", color = "white",size = 4)+
  
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )


#------------------------------------------------------(f)带连接线的带误差线散点图--------------------------------------------

library(ggalt)
library(dplyr)
mydata2 <- mydata %>%
  group_by(Class) %>%
  summarise(sd = sd(Value),len = mean(Value))


ggplot(mydata2, aes(x = c(1:4), y = len, ymin = len-sd, ymax = len+sd))+
  geom_xspline(spline_shape = -0.5,size=1) +
  
  geom_errorbar(colour="black", width=0.2,size=1)+
  geom_point(aes(fill = Class),shape=21,size=5,stroke=1)+
  
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  scale_y_continuous(breaks=seq(0,7,2),lim=c(0,7.5))+
  xlab("Time")+
  ylab("Value")+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
