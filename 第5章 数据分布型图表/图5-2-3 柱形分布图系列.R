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

#--------------------------------------------------图5-2-3 柱形分布图系列。(a) 带误差线的柱形图------------------------------------------
ggplot(mydata, aes(Class, Value))+ 
  stat_summary(mapping=aes(fill = Class),fun.y=mean, fun.args = list(mult=1),geom='bar',colour="black",width=.7) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1),geom='errorbar', color='black',width=.2) +    
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  ylim(0,7.5)+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )

#---------------------------------------图5-2-3 柱形分布图系列(b) 带误差线柱形与抖动图----------------------------------------
ggplot(mydata, aes(Class, Value))+ 
  stat_summary(fun.y=mean, fun.args = list(mult=1),geom='bar',colour="black",fill="white",width=.7) +
  stat_summary(fun.data = mean_sdl,fun.args = list(mult=1), geom='errorbar', color='black',width=.2) +    
  geom_jitter(aes(fill = Class),position = position_jitter(0.2),shape=21, size = 2,alpha=0.9)+
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
