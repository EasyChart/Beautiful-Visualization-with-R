#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

#------------------------------------图 7-7-3 星形图-----------------------------
library(graphics)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
data(diamonds)
dstar<- sample_n(diamonds, 100)
dstar$log10carat <- log10(dstar$carat)
dstar$log10price <- log10(dstar$price)
dstar<-dstar[order(dstar$cut,decreasing=T),]
#星形图
stars(dstar[,2:6], key.loc = c(-2, 10), scale = TRUE, 
      locations = NULL, len =1, radius = TRUE,
      full = TRUE, labels = NULL,draw.segments = TRUE,
      col.segments=palette(brewer.pal(7,"Set1"))[1:5])


#--------------------------------图 7-7-4 散点星形图----------------------------------------------

loc <- data.matrix(dstar[,11:12])
stars(dstar[,2:6], key.loc = c(-1, 3), scale = TRUE, 
      locations = loc, len =0.07, radius = TRUE,
      full = TRUE, labels = NULL,  draw.segments = TRUE,
      col.segments=palette(brewer.pal(7,"Set1"))[1:5],
      frame.plot=TRUE,axes = TRUE, 
      xlab="log10(carat)", ylab="log10price",
      xlim=c(-0.7,0.7))

