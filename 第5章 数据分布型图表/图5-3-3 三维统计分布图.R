#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(plot3D)

N<-300
x1 <- rnorm(mean=1.5, N)
y1 <- rnorm(mean=1.6, N)
x2 <- rnorm(mean=2.5, N)
y2 <- rnorm(mean=2.2, N)

data <- data.frame(x=c(x1,x2),y=c(y1,y2))


#图5-3-3 (a) 三维统计直方图
library(gplots) #提供hist2d()函数
df_hist<-hist2d(df$x,df$y, nbins=30)
pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
hist3D(x=df_hist$x,y=df_hist$y,z=df_hist$counts,
       col = colormap, border = "black",space=0,alpha = 1,lwd=0.1,
       xlab = "x", ylab = "y",zlab = "Count", clab="Count",
       ticktype = "detailed",bty = "f",box = TRUE,#cex.axis= 1e-09,
       theta = 65, phi = 20, d=3,
       colkey = list(length = 0.5, width = 1))


#图5-3-3 (b) 三维核密度估计图
library(MASS) #提供kde2d ()函数
df_density <- kde2d(df$x,df$y, n = 50, h = c(width.SJ(df$x), width.SJ(df$y)))
pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
persp3D (df_density$x, df_density$y, df_density$z,
         theta = 60, phi = 20, d=3,
         col = colormap, border = "black", lwd=0.1,
         bty = "f",box = TRUE,ticktype = "detailed",
         xlab = "x", ylab = "y",zlab = "desnity",clab="desnity",
         colkey = list(length = 0.5, width = 1))
