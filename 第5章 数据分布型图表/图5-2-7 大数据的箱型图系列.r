#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
library(SuppDists) #提供rJohnson()函数


#Reference：https://github.com/hadley/boxplots-paper

source("lvplot/calculate.r")
source("lvplot/draw.r")
source("lvplot/lvplot.r")

set.seed(141079)

freq <- 10 ^ (2:5)
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
n <- rnorm(freq[1],3,1)
# Johnson分布的偏斜度2.2和峰度13
s <- rJohnson(freq[2], findParams(3, 1, 2, 13.1))
# Johnson分布的偏斜度0和峰度20
k <- rJohnson(freq[3], findParams(3, 1, 2.2, 20))
# 两个峰的均值μ1，μ2分别为1.89和3.79，σ1 = σ2 =0.31
mm <- rnorm(freq[4], rep(c(2, 4), each = 50) * sqrt(0.9), sqrt(0.1))

mydata <- data.frame(
  group = factor(rep(c("n", "s", "k", "mm"), freq),
                 c("n", "s", "k", "mm")),
  x = c(n, s, k, mm)
)


#pdf("images/letter-value2.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
LVboxplot(mydata$x ~ mydata$group, horizontal = FALSE,col="#F84800",alpha=0.95)
#dev.off()


