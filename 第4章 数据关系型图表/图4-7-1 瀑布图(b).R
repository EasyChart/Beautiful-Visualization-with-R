#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(plot3D)

library(RColorBrewer)

mydata0<-read.csv("Facting_Data.csv",check.names =FALSE)

N<-ncol(mydata0)-1

mydata<-data.frame(x=numeric(),y=numeric(),variable=character())

for (i in 1:N){
  newdata<-data.frame(spline(mydata0[,1],mydata0[,i+1],n=300,method= "natural"))
  newdata$variable<-colnames(mydata0)[i+1]
  mydata<-rbind(mydata,newdata)
}


mydata$variable<-as.numeric(mydata$variable)
group<-unique(mydata$variable)
M<-length(group)

#----------------------------------------------------------------------------------------------------------------
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')),alpha = TRUE)(32)
#colormap <- colorRampPalette(rev(brewer.pal(7,'RdYlGn')),alpha = TRUE)(32)
pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))

perspbox(z=as.vector(0),#add=TRUE,
         xlim=c(20,70),ylim=c(360,750),zlim=c(0,15),
         ticktype = "detailed",bty = "f",box = TRUE,colkey = FALSE,
         theta = -110, phi = 20, d=3)

for (i in 1:M){
  df0<-mydata[mydata$variable==group[i],]
  
  df<-cbind(df0,z0=rep(0,nrow(df0)))
  df<-df[df$y>0.05,]
  with(df,segments3D(x0=variable, y0=x,z0=z0,
                     x1=variable,y1=x, z1=y, colvar =y,
                     alpha=0.5,col=ramp.col(colormap,alpha = 0.9),lwd = 3,add=TRUE,colkey = FALSE))
  
  
  with(df0,lines3D(x=variable,y=x, z=y, 
                  lwd = 1.5,col="black",add=TRUE))
}

colkey (col=colormap,clim=range(mydata$y),clab = "Z Value", add=TRUE, length=0.5,side = 4)

