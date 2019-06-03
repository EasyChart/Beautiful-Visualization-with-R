#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)  

colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

mydata0<-read.csv("Facting_Data.csv",check.names =FALSE)

N<-ncol(mydata0)-1
labels_Y<-colnames(mydata0)[1:N+1]
colnames(mydata0)<-c("x",seq(1,N,1))
mydata<-data.frame(x=numeric(),y=numeric(),variable=character()) #创建空的Data.Frame

for (i in 1:N){
  newdata<-data.frame(spline(mydata0[,1],mydata0[,i+1],n=300,method= "natural"))
  newdata$variable<-colnames(mydata0)[i+1]
  mydata<-rbind(mydata,newdata)
}

Step<-5
mydata$offest<--as.numeric(mydata$variable)*Step
mydata$V1_density_offest<-mydata$y+mydata$offest

p<-ggplot()
for (i in 1:N){
  p<-p+ geom_linerange(data=mydata[mydata$variable==i,],aes(x=x,ymin=offest,ymax=V1_density_offest,group=variable,color=y),size =1, alpha =1) +
    geom_line(data=mydata[mydata$variable==i,],aes(x=x, y=V1_density_offest),color="black",size=0.5)
}
#ggplot() + 
#  geom_linerange(aes(x=x,ymin=offest,ymax=V1_density_offest,group=variable,color=y),mydata,size =1, alpha =1) +
p+scale_color_gradientn(colours=colormap)+
  #geom_line(aes(x, V1_density_offest,group=variable),mydata,color="black")+
  scale_y_continuous(breaks=seq(-Step*N,-Step,Step),labels=rev(labels_Y))+
  xlab("Time")+
  ylab("Class")+
  theme(
    panel.background=element_rect(fill="white",colour=NA),
    panel.grid.major.x = element_line(colour = "grey80",size=.25),
    panel.grid.major.y = element_line(colour = "grey60",size=.25),
    axis.line = element_blank(),
    text=element_text(size=13),
    plot.title=element_text(size=15,hjust=.5),
    legend.position="right"
  )

#------------------------------------------------------------------------------------------------------

ggplot() + 
  geom_ribbon(aes(x, ymin=offest,ymax=V1_density_offest, fill=variable),mydata, alpha=1,colour=NA)+
  geom_line(aes(x, V1_density_offest, color=variable,group=variable),mydata, color="black")+
  scale_y_continuous(breaks=seq(-40,-5,5),labels=rev(labels_Y))+
  theme_classic()+
  theme(
    panel.background=element_rect(fill="white",colour=NA),
    panel.grid.major.x = element_line(colour = "grey80",size=.25),
    panel.grid.major.y = element_line(colour = "grey60",size=.25),
    axis.line = element_blank(),
    text=element_text(size=15),
    plot.title=element_text(size=15,hjust=.5),#family="myfont",
    legend.position="none"
  )

