#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(RColorBrewer)
library(pheatmap)
library(cowplot)
set.seed(12345)
df1<- data.frame(matrix(rnorm(100,10,3), ncol=10))
colnames(df1) <-LETTERS[1:10]
rownames(df1) <- letters[1:10]

df2<- data.frame(matrix(rnorm(100,15,5), ncol=10))
colnames(df2) <-LETTERS[1:10]
rownames(df2) <- letters[1:10]

Colormap <- colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)

breaks <- seq(min(unlist(c(df1, df2))), max(unlist(c(df1, df2))), length.out=100)

p1<- pheatmap(df1, color=Colormap, breaks=breaks,border_color="black",legend=TRUE)
p2 <- pheatmap(df2, color=Colormap, breaks=breaks,border_color="black",legend=TRUE)

plot_grid( p1$gtable, p2$gtable,align = 'vh',labels = c("A", "B"),ncol = 2)

