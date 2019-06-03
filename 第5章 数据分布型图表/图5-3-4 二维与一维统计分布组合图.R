#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(ellipse)
library(gridExtra)
library(plyr)
library(RColorBrewer)

Colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

N<-300
x1 <- rnorm(mean=1.5, N)
y1 <- rnorm(mean=1.6, N)
x2 <- rnorm(mean=2.5, N)
y2 <- rnorm(mean=2.2, N)

data <- data.frame(x=c(x1,x2),y=c(y1,y2))


#-------------------------二维直方图+一维直方图------------------------------------------------------------
# 绘制上边的直方图，并将各种标注去除
hist_top <- ggplot()+
  geom_histogram(aes(data$x),colour='black',fill='#5E4FA2',binwidth = 0.3)+
  theme(panel.background=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_blank())
# 同样绘制右边的直方图
hist_right <- ggplot()+
  geom_histogram(aes(data$y),colour='black',fill='#5E4FA2',binwidth = 0.3)+
  theme(panel.background=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_blank())+
  coord_flip()

#ggplot(diamonds, aes(carat, price))
scatter<-ggplot(data, aes(x,y)) +
  stat_binhex(bins = 15,na.rm=TRUE,color="black")+#colour="black",
  scale_fill_gradientn(colours=Colormap)+#, trans="log"
  #geom_point(colour="white",size=1,shape=21) +
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position=c(0.10,0.80),
        legend.background=element_blank()
  )
# 最终的组合
grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4,1), heights=c(1,4))

#----------------------------二维核密度估计图+一维核密度估计图--------------------------
# 绘制上边的直方图，并将各种标注去除
hist_top <- ggplot(data, aes(x)) +
  geom_density(colour="black",fill='#5E4FA2',size=0.25)+
  theme_void()
# 同样绘制右边的直方图
hist_right <- ggplot(data, aes(y)) +
  geom_density(colour="black",fill='#5E4FA2',size=0.25)+
  theme_void()+
  coord_flip()

scatter<-ggplot(data, aes(x, y)) + 
  stat_density2d(geom ="polygon",aes(fill = ..level..),bins=30 )+#alpha=..level..,aes( fill=..level..), size=2, bins=10, geom="polygon") + 
  scale_fill_gradientn(colours=Colormap)+#, trans="log"
  #geom_point(size=1) +
  theme_minimal()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position=c(0.9,0.22),
        legend.background=element_blank()
  )
# 最终的组合
grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4,1), heights=c(1,4))
