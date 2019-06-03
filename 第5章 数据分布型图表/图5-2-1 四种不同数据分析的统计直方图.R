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

#--------------------------------------图5-2-1四种不同数据分析的分布类图表. (b)核密度估计曲线图.-----------------------------------------------------------
ggplot(mydata, aes(Value,fill=Class))+
  geom_density(alpha=1,bw=0.3,colour="black",size=0.25)+
  scale_fill_manual(values=brewer.pal(7,"Set2")[c(1,2,4,5)])+
  facet_grid(Class~.)+
  xlab("X")+
  ylab("Desnity")+
  theme_light()+
  theme(
    strip.text = element_text(size=15,color="black"),
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.position="none"
  )


#------------------------------------图5-2-1四种不同数据分析的分布类图表. (a) 统计直方图.-----------------------------------------------
library(reshape2)

type<-as.character(unique(mydata$Class))
step<-0.2
breaks<- seq(min(mydata$Value)-step,max(mydata$Value)+step,step)

mydata1<-data.frame(xvals=numeric(),yvals=numeric(),variable=character()) #创建空的Data.Frame
for (i in 1:length(type)){
  x <-mydata[mydata$Class==type[i],2] #rnorm(250 , mean=10 , sd=1)
  hg <- hist(x, breaks = breaks , plot = FALSE) # Make histogram data but do not plot
  dat <- data.frame(xvals=hg$mids, yvals=hg$counts,variable=rep(type[i],length(hg$mids)))
  mydata1 <- rbind(mydata1,dat[dat$yvals>0,])

}

mydata2<-data.frame(xvals=numeric(),value=numeric(),variable=character()) #创建空的Data.Frame
for (i in 1:nrow(mydata1)){
  N<-mydata1$yvals[i]
  temp<-data.frame(xvals=rep(mydata1$xvals[i],N),value=1:N,variable=rep(mydata1$variable[i],N))
  mydata2<-rbind(mydata2,temp)
}  

ggplot(mydata2, aes(x=xvals,y=value,fill=variable))+
    geom_point(shape=21,size=3,colour="black")+
    scale_fill_manual(values=brewer.pal(7,"Set2")[c(1,2,4,5)])+
    facet_grid(variable~.)+
    xlab("Bins")+
    ylab("Count")+
    theme_light()+
    theme(
      strip.text = element_text(size=15,color="black"),
      text=element_text(size=15,color="black"),
      plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
      legend.position="none"
    )
 
  
  
