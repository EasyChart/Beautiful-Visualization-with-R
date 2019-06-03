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

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colormap <- rev(gg_color_hue(M))#brewer.pal(M,'RdYlGn')

pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))

perspbox(z=as.vector(0),#add=TRUE,
          xlim=c(20,70),ylim=c(360,750),zlim=c(0,15),
          ticktype = "detailed",bty = "f",box = TRUE,colkey = FALSE,
          theta = -110, phi = 20, d=3)

for (i in 1:M){
  df0<-mydata[mydata$variable==group[i],]
  Ndf<-nrow(df0)
  df<-rbind(df0,c(df0$x[1],df0$y[Ndf],df0$variable[Ndf]))
  with(df,polygon3D(x=variable,y=x, z=y, alpha=0.6,
                     col=colormap[i],lwd = 3,add=TRUE,colkey = FALSE))
  
  with(df0,lines3D(x=variable,y=x, z=y, 
                  lwd = 0.5,col="black",add=TRUE))
}

