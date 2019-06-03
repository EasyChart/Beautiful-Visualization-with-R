#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(lattice)
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


df<-data.frame(matrix(Grid_z, length(xmar), length(ymar)))
colnames(df)<-xmar
df$x<-ymar
melt_df<-melt(df,id.vars='x', variable.name ="y",value.name = "z")

melt_df$y<-as.numeric(melt_df$y)
#trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1)) # Removes the border of the plot if you want

surface_plot1 <- wireframe(z ~ y*x, data=melt_df, 
                           xlab = "0-60 mph (sec)", 
                           ylab = "Gax Mileage (mpg)",
                           zlab="Power (KW)",
                           zlim=c(20,180),
                           drape = TRUE,
                           colorkey = TRUE,
                           scales = list(arrows=FALSE),
                           light.source = c(10,0,10),
                           col.regions = colorRampPalette(rev(brewer.pal(11,'RdYlGn')))(100),
                           screen = list(z = -60, x = -60)
)
surface_plot1


max_z<-max(melt_df$z)
min_z<-min(melt_df$z)
breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/10)
Contour1<-ggplot()+
  geom_raster(data=melt_df,aes(y=y,x=x,fill=z),interpolate=TRUE)+#根据高度填充
  geom_contour(data=melt_df,aes(y=y,x=x,z=z,colour= ..level..),breaks=breaks_lines,color="black")+#
  geom_point(data=mydata,aes(y,x,fill=z),shape=21,size=3)+
  scale_fill_gradientn(colours=colormap)+
  
  labs(x="0-60 mph (sec)",y="Gax Mileage (mpg)",fill="Power (KW)")+
  #xlim(min(x),max(x))+
  #ylim(min(y),max(y))+
  theme_light()+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position =c(0.82,0.25)
  )
Contour1

#-#--------------------------------------------------loess回归式拟合------------------------------------------
mydata <- read.csv("Surface_Data.csv", sep= ",", header=T)

x <- mydata$x
y <- mydata$y
#z <- mydata$z
xmar <- seq(min(x),max(x),(max(x)-min(x))/30)
ymar <- seq(min(y),max(y),(max(y)-min(y))/30)

elev.loess <- loess(z ~ x * y, mydata,span=0.95)
print(elev.loess)

# get fitted (interpolated) values
elev.interp <- predict(elev.loess, expand.grid(list(x=xmar,y=ymar)))
df<-data.frame(matrix(elev.interp, length(xmar),length(ymar)))
colnames(df)<-xmar
df$x<-ymar
melt_df<-melt(df,id.vars='x', variable.name ="y",value.name = "z")
melt_df$y<-as.numeric(melt_df$y)
#trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1)) # Removes the border of the plot if you want

surface_plot2 <- wireframe(z ~ y*x, data=melt_df, 
                          xlab = "0-60 mph (sec)", 
                          ylab = "Gax Mileage (mpg)",
                          zlab="Power (KW)",
                          #main = "orgpractices",
                          zlim=c(20,180),
                          drape = TRUE,
                          colorkey = TRUE,
                          scales = list(arrows=FALSE),##,cex=.5, tick.number = 5,  z = list(arrows=F), distance =c(1, 1, 1)),
                          light.source = c(10,0,10),
                          col.regions = colorRampPalette(rev(brewer.pal(11,'RdYlGn')))(100),
                          screen = list(z = -60, x = -60)
)
surface_plot2

max_z<-max(melt_df$z)
min_z<-min(melt_df$z)
breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/10)
Contour2<-ggplot()+
  geom_raster(data=melt_df,aes(y=y,x=x,fill=z),interpolate=TRUE)+#根据高度填充
  geom_contour(data=melt_df,aes(y=y,x=x,z=z,colour= ..level..),breaks=breaks_lines,color="black")+#
  geom_point(data=mydata,aes(y,x,fill=z),shape=21,size=3)+
  scale_fill_gradientn(colours=colormap)+
  labs(x="0-60 mph (sec)",y="Gax Mileage (mpg)",fill="Power (KW)")+
  #xlim(min(x),max(x))+
  #ylim(min(y),max(y))+
  theme_light()+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position =c(0.85,0.2)
  )
Contour2


#-----------------------------------------------图表组合1------------------------------------------------------------------------------------------
grid.arrange(surface_plot1,surface_plot2, ncol=2, clip=TRUE) # Clip removes the border of the plot

#-----------------------------------------------图表组合2----------------------------------------------------------------------------------
grid.arrange(Contour1,Contour2, ncol=2, clip=TRUE) # Clip removes the border of the plot
