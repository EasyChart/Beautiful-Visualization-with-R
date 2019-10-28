

#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

#------------------------------不同类型的树形图(a)------------------------------------
library(factoextra)

data(USArrests)
dd <- dist(scale(datasets::mtcars), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

fviz_dend(hc, k = 4, # 聚类的类别数目为4
          cex = 0.8, # 数据标签的字体大小
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = FALSE, # 数据标签也根据颜色设定
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect = TRUE, # 使用不同颜色的矩形框标定类别
          rect_fill = TRUE)

#-----------------------------图4-10-2(b)树形图----------------------
fviz_dend(hc, k = 4, cex = 0.8, horiz = TRUE, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),# k_colors = "jco", 
          color_labels_by_k = FALSE, # 数据标签也根据颜色设定
           rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect = TRUE,rect_fill = TRUE)

#-----------------------------图4-10-2(c)环形树形图----------------------
par(mar = rep(300,4))
fviz_dend(hc, cex = 0.8, k = 4, 
          color_labels_by_k = FALSE, # 数据标签也根据颜色设定
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          type = "circular",#phylogenic",
          #labels_track_height = 0.1,
          repel = TRUE,
          rect_lty = 0.5)

#-----------------------------图4-10-2(d)树形图----------------------------
par(mar = rep(300,4))
fviz_dend(hc, cex = 1, k = 4, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          type = "phylogenic",
          color_labels_by_k = FALSE, # 数据标签也根据颜色设定
          labels_track_height = 0.1,
          repel = TRUE,
          rect_lty = 0.5)

#-----------------------------图4-10-2(c)环形树形图---------------------------------------------------------

library(dendextend)
library(circlize)

dd <- dist(scale(datasets::mtcars), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
dend <- as.dendrogram(hc)
# modify the dendrogram to have some colors in the branches and labels
dend <- dend %>% 
  color_branches(k=4,col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))# %>% 
  #color_labels(k=4,col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), color_labels_by_k = TRUE)

# plot the radial plot
par(mar = rep(0,4))
#circlize_dendrogram(dend, dend_track_height = 0.8) 
circlize_dendrogram(dend, labels_track_height = 0.6, dend_track_height = 0.3,size=3) 
