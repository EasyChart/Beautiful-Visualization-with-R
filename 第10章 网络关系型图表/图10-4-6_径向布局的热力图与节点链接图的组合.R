

#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts

#reference:
#http://wap.sciencenet.cn/blog-3406804-1154780.html?mobile=1
#https://jokergoo.github.io/circlize_book/book/graphics.html#links

library(circlize)
library(ComplexHeatmap)
library(RColorBrewer)

col <- colorRamp2(seq(-2,2,length.out=7),rev(brewer.pal(n = 7, name = "RdYlBu")))

set.seed(1234)
data <- matrix(rnorm(100 * 10), nrow = 10, ncol = 50)
factors <- rep(letters[1:2], times = c(10, 40))
data_list <- list(a = data[, factors == "a"], b = data[, factors == "b"])

df_link<-data.frame(from=c(0,3,5,9,8,3,5,2,7),
                    to  =c(1,8,9,15,19,25,30,32,38))

circlize_plot = function() {
  circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 5)
  circos.initialize(factors = factors, xlim = cbind(c(0, 0), table(factors)))


circos.track(ylim = c(0, 10), bg.border = NA,
             panel.fun = function(x, y) {
               sector.index = get.cell.meta.data("sector.index")
               d = data_list[[sector.index]]
               col_data = col(d)
               nr = nrow(d)
               nc = ncol(d)
               for (i in 1:nr) {
                circos.rect(1:nc - 1, rep(nr - i, nc), 1:nc, rep(nr - i + 1, nc),
                             border = 'black', col = col_data[i, ],size=0.1) }
               
               circos.text(CELL_META$xcenter,  CELL_META$cell.ylim[1] + uy(25, "mm"), 
                           CELL_META$sector.index)
               circos.axis(labels.cex = 1, major.at = seq(0.5, round(CELL_META$xlim[2])+0.5,1),
                           labels=seq(0, round(CELL_META$xlim[2]),1))
               
               circos.yaxis(labels.cex = 0.5,at = seq(0.5, round(CELL_META$ylim[2])+0.5,1),
                            labels=letters[1:10])
               })
for (i in 1:nrow(df_link)){
  circos.link("a", df_link$from[i]+0.5, "b", df_link$to[i]+0.5, h = 0.8)
}

circos.clear()
}


lgd_links = Legend(at = c(-2, -1, 0, 1, 2), col_fun = col, 
                   title_position = "topleft", title = "value")
circlize_plot()
w <- grobWidth(lgd_links)
h <- grobHeight(lgd_links)
vp <- viewport(x = unit(1, "npc") - unit(2, "mm"), 
               y = unit(4, "mm"), 
               width = w,
               height = h,
               just = c("right", "bottom"))
pushViewport(vp)
grid.draw(lgd_links)



