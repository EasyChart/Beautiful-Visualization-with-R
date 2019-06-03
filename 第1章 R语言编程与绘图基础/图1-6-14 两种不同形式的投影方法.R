#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(plot3D)

# Reference:https://github.com/coconn/cso002code_BrazilTradeOffsR

par(mfrow = c(1, 1))
panelfirst <- function(pmat) {
  zmin <- min(-quakes$depth)
  XY <- trans3D(quakes$long, quakes$lat,
                z = rep(zmin, nrow(quakes)), pmat = pmat)
  scatter2D(XY$x, XY$y, col = "black", pch = ".",
            cex = 2, add = TRUE, colkey = FALSE)
  xmin <- min(quakes$long)
  XY <- trans3D(x = rep(xmin, nrow(quakes)), y = quakes$lat,
                z = -quakes$depth, pmat = pmat)
  scatter2D(XY$x, XY$y, col = "black", pch = ".",
            cex = 2, add = TRUE, colkey = FALSE)
}


library(scales)
library(RColorBrewer)
library(fields) 
colormap <- colorRampPalette(rev(brewer.pal(11,'RdYlGn')))(100)#

index <- ceiling(((prc <- 0.7 * quakes$mag/ diff(range(quakes$mag))) - min(prc) + 0.3)*100)
for (i in seq(1,length(index)) ){
  prc[i]=colormap[index[i]]
}


pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(quakes, scatter3D(x = long, y = lat, z = -depth, #bgvar = mag,
                       pch = 21, cex = 1.5,col="black",bg=prc,
                       xlab = "longitude", ylab = "latitude",
                       zlab = "depth, km", 
                       ticktype = "detailed",#bty = "f",box = TRUE,
                       panel.first = panelfirst,
                       theta = 140, phi = 20, d=1.5,
                       colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)
colkey (col=colormap,clim=range(quakes$mag),clab = "Richter", add=TRUE, length=0.5,side = 4)


#--------------------------------------------------------------------
pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(quakes, scatter3D(x = long, y = lat, z = -depth, #bgvar = mag,
                       pch = 21, cex = 1.5,col="black",bg=prc,
                       xlab = "longitude", ylab = "latitude",
                       zlab = "depth, km", 
                       ticktype = "detailed",bty = "f",box = TRUE,
                       panel.first = panelfirst,
                       theta = 140, phi = 20, d=3,
                       colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)
colkey (col=colormap,clim=range(quakes$mag),clab = "Richter", add=TRUE, length=0.5,side = 4)

#--------------------------------------------------------------------
pmar <- par(mar = c(5.1, 4.1, 4.1, 6.1))
with(quakes, scatter3D(x = long, y = lat, z = -depth, #bgvar = mag,
                       pch = 21, cex = 1.5,col="black",bg=prc,
                       xlab = "longitude", ylab = "latitude",
                       zlab = "depth, km", 
                       ticktype = "detailed",#bty = "f",box = TRUE,
                       panel.first = panelfirst,
                       theta = 140, phi = 20, d=30,
                       colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)
colkey (col=colormap,clim=range(quakes$mag),clab = "Richter", add=TRUE, length=0.5,side = 4)
