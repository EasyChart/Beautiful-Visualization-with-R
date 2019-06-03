#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

#---------------------------------------------Q-Q图---------------------------------------------------

set.seed(183)
x <- rnorm(250 , mean=10 , sd=1)
# # Compare the numbers sampled with rnorm() against normal distribution
qqnorm(x)
qqline(x)
abline(h = c(7:13)*1/2,col = "gray100",lty =1) # boe cpi target
abline(v = c(-3:3)*1/2,col = "gray100", lty = 1) # 2 year line
abline(h = c(7:13)*1,col = "gray100",lty =1) # boe cpi target
abline(v = c(-3:3),col = "gray100", lty = 1) # 2 year line
box(col="white")

#--------------------------------------------CircStats--P-P图---------------------------------------------------
library(CircStats)
pp.plot(x)
abline(h = c(0:10)*1/10,col = "gray100",lty =1) # boe cpi target
abline(v = c(0:10)*1/10,col = "gray100", lty = 1) # 2 year line
abline(h = c(0:10)*2/10,col = "gray100",lty =1) # boe cpi target
abline(v = c(0:10)*2/10,col = "gray100", lty = 1) # 2 year line
box(col="white")

#---------------------------------------------ggplot2-Q-Q图---------------------------------------------------
library(ggplot2)

df <-data.frame(x=rnorm(250 , mean=10 , sd=1))
ggplot(df, aes(sample = x))+
  geom_qq()  
  #geom_qq_line(fill = "#00AFBB",size=3)


#---------------------------------------------ggqqplot-Q-Q图---------------------------------------------------
library(ggpubr)
x <- rnorm(250 , mean=10 , sd=1)
ggqqplot(x,shape=21,fill="white",colour="black",
         add = "none",
         ggtheme = ggplot2::theme_grey())
