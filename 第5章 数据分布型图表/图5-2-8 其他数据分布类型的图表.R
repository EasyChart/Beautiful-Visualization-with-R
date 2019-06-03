#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

#Reference：https://github.com/hadley/boxplots-paper

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
# Johnson分布的偏斜度0和峰度20
k <- rJohnson(100, findParams(3, 1, 2.2, 20))
# 两个峰的均值μ1，μ2分别为1.89和3.79，σ1 = σ2 =0.31
mm <- rnorm(100, rep(c(2, 4), each = 50) * sqrt(0.9), sqrt(0.1))

mydata <- data.frame(
  Class = factor(rep(c("n", "s", "k", "mm"), each = 100),
                 c("n", "s", "k", "mm")),
  Value = c(n, s, k, mm)
)

#-----------------------------------------------(a) 瓶状图---------------------------------------------------------------------------
source("lvplot/boxplots-vase.r")
#pdf("images/four-vase.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
vase(split(mydata$Value, mydata$Class), bw = rep(0.1, 4))
axis(side = 2)
xlab("Class")
     
#-------------------------------------------(b)小提琴图---------------------------------------------------------------------------
ggplot(mydata, aes(Class, Value))+ 
  geom_violin(aes(fill = Class),trim = FALSE)+
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )

#-------------------------------------------(c)豆状图---------------------------------------------------------------------------
library(beanplot)
par(mar = c(2.1, 2.1, .1, .1))
beanplot(Value ~Class, data = mydata,col=c("white","black"),xlab ="Class",ylab ="value")


#--------------------------------------------(d) 海盗图-------------------------------------------------------------------------
library(yarrr)

pirateplot(formula =  mydata$Value~mydata$Class, data =mydata, 
           theme.o = 2,
           xlab = "Class", ylab = "Value", main = "",
           # Choose your color palette, or give common color vector
           #pal = color,#"google",
           #gl.col = gray(.8),
           # Set transparency of the elements:
           #bean.b.col="black",
           bean.f.col=brewer.pal(7,"Set2")[c(1,2,4,5)],
           bar.b.col="black",
           #line.o = 0.1,
           bar.o = .1,
           bean.o = .1,
           point.o = .9,
           # Shape of point
           #point.pch = 2,
           #Background color
           #back.col = "white",
           ylim=c(0,7.5),
           gl.col = "white", # gridlines
           gl.lwd = c(.5, 0)
) # turn off minor grid lines)
