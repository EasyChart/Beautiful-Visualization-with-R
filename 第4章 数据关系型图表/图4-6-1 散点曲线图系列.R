#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(ggalt)
#----------------------------------------图4-6-1 散点曲线图系列(a)-------------------------------------------------------
mydata<-read.csv("Line_Data.csv",header=T)

ggplot(mydata, aes(x, y) )+
  geom_xspline(spline_shape=-0.5, size=0.25)+
  geom_point(shape=21,size=4,color="black",fill="#F78179") +
  xlab("X-Axis")+
  ylab("Y-Axis")+
  ylim(0, 50)+
  theme_gray()+
  theme(
    text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=10,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black")
  )

#----------------------------------------图4-6-1 散点曲线图系列(d)-------------------------------------------------------
library(splines)
newdata <- data.frame(spline(mydata$x,mydata$y,n=300,method="hyman" ))

ggplot(newdata, aes(x, y) )+
  geom_line(size=0.5,color="black")+
  geom_area(fill="#F78179",alpha=0.9)+
  geom_point(data=mydata,aes(x,y),shape=21,size=3,color="black",fill="white") +
  xlab("X-Axis")+
  ylab("Y-Axis")+
  ylim(0, 50)+
  theme_gray()+
  theme(
    text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=12,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black")
  )

#The following rules apply for the methods called to interpolate:
#  Method	Description
#"fmm"	The spline used is that of Forsythe, Malcolm, and Moler.
#"periodic"	Periodic splines are used. The period is taken to be diff(range(x)), and it is expected that the y values corresponding to the extrema of x are equal.
#"natural"	Natural splines are used. The second derivative is zero at the endpoints.
#"monoH.FC"	The monotone piecewise cubic interpolation method of F.N. Fritsch and R.E. Carlson (with modifications by Butland and Brodlie) is used. (This is the Fortran routine DPCHIP from the slatec library.)
#"hyman"	Also gives a monotone spline: the "fmm" spline is initially calculated, and then the method of J.M. Hyman is used to force it to be monotone.
