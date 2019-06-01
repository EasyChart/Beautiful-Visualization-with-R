

#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

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


#------------------------------------图5-3-2 不同类型的二维核密度统计图-----------------
ggplot(df, aes(x,y))+
  stat_density_2d(geom ="raster",aes(fill = ..density..),contour = F)+# "polygon")+#geom_raster(aes(fill = density)) +
  scale_fill_gradientn(colours=colormap)+#, trans="log"scale_fill_gradientn(colours=c("#CEF5FF","#00B8E5","#005C72"),name = "Frequency",na.value=NA)+
  #scale_fill_gradientn(colours=c(brewer.pal(7,"Set2")[3],"white",brewer.pal(7,"Set2")[2]),na.value=NA)+
  #geom_contour(acolour = "white") +
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



#---------------------------
ggplot(df, aes(x, y)) + 
  stat_density2d(geom ="polygon",aes(fill = ..level..),bins=30 )+#alpha=..level..,aes( fill=..level..), size=2, bins=10, geom="polygon") + 
  #stat_density_2d(geom = "point", aes(size = ..density..), n = 20, contour = FALSE)
  scale_fill_gradientn(colours=colormap)+#scale_fill_gradient(low = "yellow", high = "red") +
  #scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  #geom_density2d( colour=NA,bins=30) +#
  #geom_point() +
  guides(alpha=FALSE) +
  xlim(-2,6)+
  ylim(-2,6)+
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
