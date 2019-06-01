
#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

#---------------------------------------图5-1-2核密度估计峰峦图--------------------------------------
library(ggplot2)
library(ggridges)
library(RColorBrewer)

ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..density..)) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.00,size = 0.3) + 
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,'Spectral')))(32))

###------------------------------自定义编写代码实现：图5-1-2核密度估计峰峦图-------------------------------
library(reshape2)

colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

dt<-lincoln_weather[,c("Month","Mean Temperature [F]")]
splitdata<-split(dt,dt$Month)
xmax<-max(dt$`Mean Temperature [F]`)*1.1
xmin<-min(dt$`Mean Temperature [F]`)*1.1


N<-length(splitdata)
labels_y<-names(splitdata)

mydata<-data.frame(x=numeric(),y=numeric(),variable=numeric()) #创建空的Data.Frame

for (i in 1:N){
  tempy<-density(splitdata[[i]][2]$`Mean Temperature [F]`,bw = 3.37,from=xmin, to=xmax)
  newdata<-data.frame(x=tempy$x,y=tempy$y)
  newdata$variable<-i
  mydata<-rbind(mydata,newdata)
}

Step<-max(mydata$y)*0.6
mydata$offest<--as.numeric(mydata$variable)*Step
mydata$V1_density_offest<-mydata$y+mydata$offest

p<-ggplot()
for (i in 1:N){
  p<-p+ geom_linerange(data=mydata[mydata$variable==i,],aes(x=x,ymin=offest,ymax=V1_density_offest,group=variable,color=y),size =1, alpha =1) +
        geom_line(data=mydata[mydata$variable==i,],aes(x=x, y=V1_density_offest),color="black",size=0.5)
}

p+scale_color_gradientn(colours=colormap,name="Density")+
  scale_y_continuous(breaks=seq(-Step,-Step*N,-Step),labels=labels_y)+
  xlab("Mean Temperature [F]")+
  ylab("Month")+
  theme_classic()+
  theme(
    panel.background=element_rect(fill="white",colour=NA),
    panel.grid.major.x = element_line(colour = "grey80",size=.25),
    panel.grid.major.y = element_line(colour = "grey60",size=.25),
    axis.line = element_blank(),
    text=element_text(size=15,colour = "black"),
    plot.title=element_text(size=15,hjust=.5),
    legend.position="right"
  )

