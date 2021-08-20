
#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ComplexHeatmap )
library(circlize)
#数据来源：https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyUndirectedUnweighted.csv
df <-read.csv("AdjacencyUndirectedUnweighted.csv",row.names=1,header=TRUE,check.names = FALSE)

df <- df[which(rowSums(df,na.rm =TRUE)>=3), which(colSums(df,na.rm =TRUE)>=3)]
df[is.na(df)]<-0


col_fun <-colorRamp2(c(0,1), c("#440154","#FDE725"))

Heatmap(as.matrix(df), name = "mat", col = col_fun,
        cluster_columns = TRUE, show_row_dend = TRUE, rect_gp = gpar(col= "gray",lwd=0.05), 
        column_names_side = "bottom",column_names_gp = gpar(fontsize = 8),
        row_names_side = "right", row_names_gp = gpar(fontsize = 8),
        heatmap_legend_param = list(
          at = c(0,1),
          labels = c(0,1),
          title = "value",
          legend_height = unit(4, "cm")))
