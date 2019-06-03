
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
#library(scatterplot3d)
library(scales)
library(wesanderson)
library(RColorBrewer)
library(dplyr)
library(grid)

Alz<-read.csv("Facet_Data.csv", header = T)
df<-Alz[,c("Class","SOD","tau","age")]

#-------------------------------------------------------------ÈýÎ¬É¢µãÍ¼----------------------------------------------------
library(plot3D)
colors0 <- wes_palette(n=3, name="Darjeeling1")

df$Class_x<-as.numeric(df$Class)
colors <- colors0[df$Class_x]

pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(df, scatter3D(x = Class_x, y = SOD, z = tau, #bgvar = mag,
                       pch = 21, cex = 1.5,col="black",bg=colors,
                       xlab = "Class",
                       ylab = "SOD",
                       zlab = "tau",
                       labels=c(""),
                       ticktype = "detailed", bty = "f",box = TRUE,expand = 1,#cex.axis= 1e-09,
                       #panel.first = panelfirst,
                       theta = 30, phi = 20, d=5,
                       colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)


text3D(x = 1:3, y = rep(4, 3), z = rep(4.5, 3), labels = levels(df$Class), 
       add = TRUE, adj = 0, col = "black", bty = "g")

#colkey (col=colors0,clim=c(0,6),
#        at = c(1, 3, 5), labels = levels(df$Class),
#        clab = "Class", add=TRUE, width=1.5,length=0.2,side = 4)
legend("right",title =  "Species",legend=levels(df$Class),pch=21,
       cex=1,y.intersp=1,pt.bg = colors0,bg="white",bty="n")


#----------------------------------------------------ÈýÎ¬ÆøÅÝÍ¼------------------------------------------------------
Alz<-read.csv("Facet_Data.csv", header = T)
df<-Alz[,c("Class","SOD","tau","age")]

colors0 <- wes_palette(n=3, name="Darjeeling1")
df$Class_x<-as.numeric(df$Class)
colors <- colors0[df$Class_x]

pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(df, scatter3D(x = Class_x, y = SOD, z = tau, #bgvar = mag,
                   pch = 21, cex = rescale(df$age, c(.5, 3)),col="black",bg=colors,
                   xlab = "Class",
                   ylab = "SOD",
                   zlab = "tau",
                   labels=c(""),
                   ticktype = "detailed", bty = "f",box = TRUE,expand = 1,#cex.axis= 1e-09,
                   #panel.first = panelfirst,
                   theta = 30, phi = 20, d=5,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)


text3D(x = 1:3, y = rep(4, 3), z = rep(4.5, 3), labels = levels(df$Class), 
       add = TRUE, adj = 0, col = "black", bty = "g")
legend("right",title =  "Species",legend=levels(df$Class),pch=21,
       cex=1,y.intersp=1,pt.bg = colors0,bg="white",bty="n")

N<-5
breaks<-round(seq(min(df$age),max(df$age),length.out=5),3)


legend("topright",title =  "Age",legend=breaks,pch=21,
       pt.cex=rescale(breaks, c(.5, 3)),y.intersp=1,
       pt.bg = "white",bg="white",bty="n")

#colkey (col=colors0,clim=c(0,6),
#        at = c(1, 3, 5), labels = levels(df$Class),
#        clab = "Class", add=TRUE, width=1.5,length=0.2,side = 4)


#---------------------------------------------


colormap <- colorRampPalette(rev(brewer.pal(11,'RdYlGn')))(100)

index <- ceiling(((prc <- 0.7 * df$age/ diff(range(df$age))) - min(prc) + 0.3)*100)
for (i in seq(1,length(index)) ){
  prc[i]=colormap[index[i]]
}

pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(df, scatter3D(x = Class_x, y = SOD, z = tau, #bgvar = mag,
                   pch = 21, cex = rescale(df$age, c(.5, 3)),col="black",bg=prc,#colors,
                   xlab = "Class",
                   ylab = "SOD",
                   zlab = "tau",
                   labels=c(""),
                   ticktype = "detailed", bty = "f",box = TRUE,expand = 1,#cex.axis= 1e-09,
                   #panel.first = panelfirst,
                   theta = 30, phi = 20, d=5,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)


text3D(x = 1:3, y = rep(4, 3), z = rep(4.5, 3), labels = levels(df$Class), 
       add = TRUE, adj = 0, col = "black", bty = "g")
#colkey (col=colormap,clim=range(df$age),clab = "Age", add=TRUE, length=0.45,side = 4)

N<-5
breaks<-round(seq(min(df$age),max(df$age),length.out=5),3)

legend_index <- ceiling(((legend_prc <- 0.7 *breaks/ diff(range(breaks))) - min(legend_prc) + 0.3)*100)
for (i in seq(1,length(legend_index)) ){
  if (legend_index[i]>100){
    legend_index[i]<-100
  }
  legend_prc[i]=colormap[legend_index[i]]
}
legend("right",title =  "Age",legend=breaks,pch=21,
       pt.cex=rescale(breaks, c(.5, 3)),y.intersp=1,
       pt.bg = legend_prc,bg="white",bty="n")

