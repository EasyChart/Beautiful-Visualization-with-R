#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(VennDiagram)
library(RColorBrewer)
venn.diagram(list(B = 1:1800, A = 1571:2020,c=500:1100),fill = c(brewer.pal(7,"Set1")[1:3]),
             alpha = c(0.5, 0.5,0.5), cex = 2,
             cat.cex=3,cat.fontface = 4,lty =2, fontfamily =3, 
             resolution =300, filename = "trial2.tiff")
