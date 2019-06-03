
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2) # load ggplot2
# setting seed for random numbers
set.seed(1111)

# random normal variables
var1<-rnorm(500,-5, 1.5)
var2<-rnorm(500, 1, 2)
var3<-rnorm(500, -1, 3)

# data frame with normal variables
distribs = data.frame(values = c(var1, var2, var3),type = gl(n = 3, k = 500))

# plot
ggplot(data = distribs, aes(x = values, group = type)) +
  geom_density(aes(fill = type), color = "black", alpha = 0.8,size=0.5)+
  theme(legend.position = c(0.9,0.8))
