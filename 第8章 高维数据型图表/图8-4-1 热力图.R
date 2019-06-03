#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

#--------------------------------------------图7-4-1 热力图 方法1---------------------------------------------------
library(ComplexHeatmap)
library(circlize)
library(dendextend)

df<-scale(mtcars) #使用scale方法来对数据进行标准化，以消除量纲对数据结构的影响
row_dend = hclust(dist(df))   # 根据行对数据进行系统性聚类
col_dend = hclust(dist(t(df)))  # 根据列对数据进行系统性聚类
mycol <-rev(brewer.pal(n = 7, name = "RdYlBu")) #构造颜色映射方案
Heatmap(df, col = mycol,name = "mtcars",rect_gp = gpar(col = "black"),
        column_dend_height = unit(4, "cm"), #设定列聚类显示部分的高度
        row_dend_width = unit(4, "cm"),    #设定行聚类显示部分的宽度
        cluster_rows = color_branches(row_dend, k = 4), #设定行聚类成4类
        cluster_columns = color_branches(col_dend, k = 2)) #设定列聚类成2类


#------------------------------------------图7-4-1 热力图 方法2-----------------------------------------------------
library(gplots)
colormap <- colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)

# order for rows
Rowv  <- mtcars %>% scale %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k = 4) %>% set("branches_lwd", 1.2) %>%
  ladderize

# Order for columns
Colv  <- mtcars %>% scale %>% t %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k = 2, value = c("orange", "blue")) %>%
  set("branches_lwd", 1.2) %>%
  ladderize

heatmap.2(scale(mtcars), scale = "none", col = colormap,
          Rowv = Rowv, Colv = Colv,
          trace = "none", density.info = "none")


#-----------------------------------图7-4-1 热力图：方法3------------------------------------------------
library(RColorBrewer)
library(pheatmap)
colormap <- colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)

breaks = seq(min(unlist(c(df))), max(unlist(c(df))), length.out=100)
pheatmap(df, color=colormap, breaks=breaks,border_color="black",
         cutree_col = 2,cutree_row = 4)
