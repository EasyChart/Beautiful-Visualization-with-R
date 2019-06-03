
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(plot3D)
library(scales)
library(RColorBrewer)
library(fields) 

#---------------------------------------------------------------------------------------------
df<-read.csv("ThreeD_Scatter_Data.csv",header=T)

pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(df, scatter3D(x = mph, y = Gas_Mileage, z = Power, #bgvar = mag,
                   pch = 21, cex = 1.5,col="black",bg="#F57446",
                   xlab = "0-60 mph (sec)",
                   ylab = "Gas Mileage (mpg)",
                   zlab = "Power (kW)", 
                   zlim=c(40,180),
                   ticktype = "detailed",bty = "f",box = TRUE,
                   #panel.first = panelfirst,
                   theta = 60, phi = 20, d=3,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)

#---------------------------------------------------------------------------------------
colormap <- colorRampPalette(rev(brewer.pal(11,'RdYlGn')))(100)#

index <- ceiling(((prc <- 0.7 * df$Power/ diff(range(df$Power))) - min(prc) + 0.3)*100)
for (i in seq(1,length(index)) ){
  prc[i]=colormap[index[i]]
}
pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(df, scatter3D(x = mph, y = Gas_Mileage, z = Power, #bgvar = mag,
                       pch = 21, cex = 1.5,col="black",bg=prc,
                   xlab = "0-60 mph (sec)",
                   ylab = "Gas Mileage (mpg)",
                   zlab = "Power (kW)", 
                   zlim=c(40,180),
                       ticktype = "detailed",bty = "f",box = TRUE,
                       #panel.first = panelfirst,
                       theta = 60, phi = 20, d=3,
                       colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)
colkey (col=colormap,clim=range(df$Power),clab = "Power", add=TRUE, length=0.5,side = 4)

#----------------------------------------------------------------------------------------
index <- ceiling(((prc <- 0.7 * df$Weight/ diff(range(df$Weight))) - min(prc) + 0.3)*100)
for (i in seq(1,length(index)) ){
  prc[i]=colormap[index[i]]
}
pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(df, scatter3D(x = mph, y = Gas_Mileage, z = Power, #bgvar = mag,
                   pch = 21, cex = 1.5,col="black",bg=prc,
                   xlab = "0-60 mph (sec)",
                   ylab = "Gas Mileage (mpg)",
                   zlab = "Power (kW)", 
                   zlim=c(40,180),
                   ticktype = "detailed",bty = "f",box = TRUE,
                   #panel.first = panelfirst,
                   theta = 60, phi = 20, d=3,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)
colkey (col=colormap,clim=range(df$Weight),clab = "Weight", add=TRUE, length=0.5,side = 4)

#-----------------------------------------------------------------------------------------
with(df, scatter3D(x = mph, y = Gas_Mileage, z = Power, #bgvar = mag,
                   pch = 21, cex = rescale(df$Weight, c(.5, 5)),col="black",bg="#ED5E3C",
                   xlab = "0-60 mph (sec)",
                   ylab = "Gas Mileage (mpg)",
                   zlab = "Power (kW)", 
                   zlim=c(40,180),
                   ticktype = "detailed",bty = "f",box = TRUE,
                   #panel.first = panelfirst,
                   theta = 60, phi = 20, d=3,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)

breaks<-round(seq(500,2000,length.out=4),3)

legend("right",title =  "Weight",legend=breaks,pch=21,
       pt.cex=rescale(breaks, c(.5, 5)),y.intersp=1.6,cex=1,
       pt.bg = "#ED5E3C",bg="white",bty="n")

#-------------------------------------------------------------------------------------
index <- ceiling(((prc <- 0.7 * df$Weight/ diff(range(df$Weight))) - min(prc) + 0.3)*100)
for (i in seq(1,length(index)) ){
  prc[i]=colormap[index[i]]
}
pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(df, scatter3D(x = mph, y = Gas_Mileage, z = Power, #bgvar = mag,
                   pch = 21, cex = rescale(df$Weight, c(.5, 5)),col="black",bg=prc,
                   xlab = "0-60 mph (sec)",
                   ylab = "Gas Mileage (mpg)",
                   zlab = "Power (kW)", 
                   zlim=c(40,180),
                   ticktype = "detailed",bty = "f",box = TRUE,
                   theta = 60, phi = 20, d=3,
                   colkey = FALSE)
)
#colkey (col=colormap,clim=range(df$Weight),clab = "Weight", add=TRUE, length=0.5,side = 4)

breaks<-round(seq(500,2000,length.out=4),3)

legend_index <- ceiling(((legend_prc <- 0.7 *breaks/ diff(range(breaks))) - min(legend_prc) + 0.3)*100)
for (i in seq(1,length(legend_index)) ){
  legend_prc[i]=colormap[legend_index[i]]
}
legend("right",title =  "Weight",legend=breaks,pch=21,
       pt.cex=rescale(breaks, c(.5, 5)),y.intersp=1.6,
       pt.bg = legend_prc,bg="white",bty="n")


#-----------------------------------¶àÊý¾ÝÏµÁÐ--------------------------------
library(wesanderson)
pmar <- par(mar = c(5.1, 4.1, 4.1, 7.1))
colors0 <-  wes_palette(n=3, name="Darjeeling1")
colors <- colors0[as.numeric(iris$Species)]
with(iris, scatter3D(x = Sepal.Length, y = Sepal.Width, z = Petal.Length, #bgvar = mag,
                   pch = 21, cex = 1.5,col="black",bg=colors,
                   xlab = "longitude", ylab = "latitude",
                   zlab = "depth, km", 
                   ticktype = "detailed",bty = "f",box = TRUE,
                   #panel.first = panelfirst,
                   theta = 140, phi = 20, d=3,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)

legend("right",title =  "Species",legend=c("setosa", "versicolor", "virginica"),pch=21,
       cex=1,y.intersp=1,pt.bg = colors0,bg="white",bty="n")
