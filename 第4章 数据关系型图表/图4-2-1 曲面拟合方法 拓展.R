#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(plot3D)
library(gridExtra)
library(reshape2)
library(RColorBrewer)

colormap<-colorRampPalette(rev(brewer.pal(11,'RdYlGn')))(100)

#--------------------------------------------------多项式拟合------------------------------------------
mydata <- read.csv("Surface_Data.csv", sep= ",", header=T)

#多项式拟合z=f(x,y)=a+bx+cy+dxx+eyy
x <- mydata$x
y <- mydata$y
z <- mydata$z
x2<-x*x
y2<-y*y
poly_z <- lm(z ~ x + y +x2+y2)
print(poly_z)

#设定为30X30的网格数据(x, y)，并根据拟合方程求其数值
N<-30
xmar <- seq(min(x),max(x),(max(x)-min(x))/N)
ymar <- seq(min(y),max(y),(max(y)-min(y))/N)
Grid_xy<-expand.grid(list(x=xmar,y=ymar))
Grid_xy$x2<-Grid_xy$x*Grid_xy$x
Grid_xy$y2<-Grid_xy$y*Grid_xy$y
Grid_z <- predict.lm(poly_z, newdata=Grid_xy)  


pred_z<-matrix(Grid_z, length(xmar), length(ymar))


persp3D (xmar, ymar, pred_z,
      theta = 150, phi = 40, d=3, 
      col = colormap, 
      scale = TRUE, border = "black", 
       bty = "f",box = TRUE,ticktype = "detailed",#nticks=5,
      ylab = "0-60 mph (sec)", 
      xlab = "Gax Mileage (mpg)",
      zlab="Power (KW)",
      clab="Power (KW)",
      zlim=c(20,180),
      add=TRUE,
      colkey = list(length = 0.5, width = 1))


fitpoints <- predict(poly_z) 

scatter3D(z = z, x = x, y = y, pch = 21, cex = 1, 
          theta = 150, phi = 40, d=3,ticktype = "detailed",
          col = colormap,
          surf = list(x = xmar, y = ymar, z = pred_z,border = "black",shade=0,ffit = fitpoints), # fit参数增加预测值与真实值之间的连线
          bty = "f", col.panel = NA,
          xlab = "0-60 mph (sec)", 
          ylab = "Gax Mileage (mpg)",
          zlab="Power (KW)",
          clab="Power (KW)",
          zlim=c(20,180),colkey = list(length = 0.5, width = 1))# col.panel = NA则panel透明

#-#--------------------------------------------------loess回归式拟合------------------------------------------
mydata <- read.csv("Surface_Data.csv", sep= ",", header=T)

x <- mydata$x
y <- mydata$y
#z <- mydata$z


elev.loess <- loess(z ~ x * y, mydata,span=0.95)
print(elev.loess)


xmar <- seq(min(x),max(x),(max(x)-min(x))/30)
ymar <- seq(min(y),max(y),(max(y)-min(y))/30)
# get fitted (interpolated) values
elev.interp <- predict(elev.loess, expand.grid(list(x=xmar,y=ymar)))

pred_z<-matrix(elev.interp, length(xmar),length(ymar))

# 显示曲面网格，网格边线颜色为洋红，显示box框线
persp3D (xmar, ymar, pred_z,
         theta = 150, phi = 40, d=3, 
         col = colormap, 
         scale = TRUE, border = "black", 
         bty = "f",box = TRUE,ticktype = "detailed",#nticks=5,
         xlab = "0-60 mph (sec)", 
         ylab = "Gax Mileage (mpg)",
         zlab="Power (KW)",
         zlim=c(20,180))


fitpoints <- predict(elev.loess ) 

scatter3D(z = z, x = x, y = y, pch = 21, cex = 1, 
                      theta = 150, phi = 40, d=3,ticktype = "detailed",
                      col = colormap,
                      surf = list(x = xmar, y = ymar, z = pred_z,border = "black",shade=0,ffit = fitpoints), # fit参数增加预测值与真实值之间的连线
                      bty = "f", col.panel = NA,
                      ylab = "0-60 mph (sec)", 
                      xlab = "Gax Mileage (mpg)",
                      zlab="Power (KW)",
                      zlim=c(20,180))# col.panel = NA则panel透明


