#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
colormap<- rev(brewer.pal(11,'Spectral'))

# Create normally distributed data for plotting
x1 <- rnorm(mean=1.5, 5000)
y1 <- rnorm(mean=1.6, 5000)
x2 <- rnorm(mean=2.5, 5000)
y2 <- rnorm(mean=2.2, 5000)
x<-c(x1,x2)
y<-c(y1,y2)
df <- data.frame(x,y)

#--------------------------图5-3-1 不同类型的二维统计直方图------------------
ggplot(df, aes(x,y))+ 
  stat_bin2d(bins=40) + scale_fill_gradientn(colours=colormap)+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="right"
  )


ggplot(df, aes(x,y))+
  stat_binhex(bins=40) + scale_fill_gradientn(colours=colormap)+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="right"
  )




