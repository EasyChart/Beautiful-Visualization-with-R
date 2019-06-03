#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(circlize)
library(RColorBrewer)

set.seed(999)
mat<-matrix(sample(18, 18), 3, 6)
rownames(mat) <- paste0("S", 1:3)
colnames(mat) <- paste0("E", 1:6)
df<- data.frame(from = rep(rownames(mat), times = ncol(mat)),
                to = rep(colnames(mat), each = nrow(mat)),
                value = as.vector(mat),
                stringsAsFactors = FALSE)

chordDiagram(df,grid.col = brewer.pal(9,"Set1")[1:9],link.border="grey")
circos.clear()


chordDiagram(mat,grid.col = brewer.pal(9,"Set1")[1:9],link.border="grey")
circos.clear()


