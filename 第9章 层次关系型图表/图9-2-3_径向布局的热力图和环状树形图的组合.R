
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#Referre: https://jokergoo.github.io/circlize_book/book/graphics.html#links

library(circlize)
library(RColorBrewer)
library(ComplexHeatmap)

col <- colorRamp2(seq(-2,2,length.out=7),rev(brewer.pal(n = 7, name = "RdYlBu")))

set.seed(1234)
data <- matrix(rnorm(100 * 10), nrow = 10, ncol = 100)
factors <- rep(letters[1:2], times = c(30, 70))
data_list <- list(a = data[, factors == "a"], b = data[, factors == "b"])
dend_list <- list(a = as.dendrogram(hclust(dist(t(data_list[["a"]])))),
                  b = as.dendrogram(hclust(dist(t(data_list[["b"]])))))

circlize_plot = function() {
  circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 5)
circos.initialize(factors = factors, xlim = cbind(c(0, 0), table(factors)))


circos.track(ylim = c(0, 10), bg.border = NA,
             panel.fun = function(x, y) {
               sector.index = get.cell.meta.data("sector.index")
               d = data_list[[sector.index]]
               dend = dend_list[[sector.index]]
               d2 = d[, order.dendrogram(dend)]
               col_data = col(d2)
               nr = nrow(d2)
               nc = ncol(d2)
               for (i in 1:nr) {
                 circos.rect(1:nc - 1, rep(nr - i, nc), 1:nc, rep(nr - i + 1, nc),
                             border = 'black', col = col_data[i, ],size=0.1) }
               
               circos.text(CELL_META$xcenter,  CELL_META$cell.ylim[1] + uy(25, "mm"), 
                           CELL_META$sector.index)
               circos.axis(labels.cex = 1, major.at = seq(0.5, round(CELL_META$xlim[2])+0.5,2),
                           labels=seq(0, round(CELL_META$xlim[2]),2))
               
               circos.yaxis(labels.cex = 0.5,at = seq(0.5, round(CELL_META$ylim[2])+0.5,1),
                            labels=letters[1:10])
               })

max_height <- max(sapply(dend_list, function(x) attr(x, "height")))
circos.track(ylim = c(0, max_height),
             bg.border = NA, track.height = 0.5,
             panel.fun = function(x, y) {
               sector.index = get.cell.meta.data("sector.index")
               dend = dend_list[[sector.index]]
               circos.dendrogram(dend, max_height = max_height)})
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

