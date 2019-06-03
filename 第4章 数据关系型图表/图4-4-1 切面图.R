#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(plot3D)
library(RColorBrewer)

x <- y <- z <- seq(-4, 4, by = 0.2)
M <- mesh(x, y, z)
R <- with (M, sqrt(x^2 + y^2 + z^2))
p <- sin(2*R) /(R+1e-3)

colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')),alpha = TRUE)(32)

slice3D(x, y, z, colvar = p, facets = FALSE,
        col = ramp.col(colormap,alpha = 0.9), 
        clab="p vlaue",
        xs = 0, ys = c(-4, 0, 4), zs = NULL, 
        ticktype = "detailed",bty = "f",box = TRUE,
        theta = -120, phi = 30, d=3,
        colkey = list(length = 0.5, width = 1, cex.clab = 1))

