
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#Referre: ggraph教程
#http://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/make/ggraph/ggraph.html
#https://www.data-imaginist.com/2017/ggraph-introduction-layouts/

library(ggraph)
library(igraph)

graph = graph_from_data_frame(flare$edges, vertices = flare$vertices)

# (a) 节点链接图的正交布局法:dendrogram
ggraph(graph, layout = 'dendrogram') + 
  geom_edge_diagonal()+
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  ) 


#(b)节点链接图的径向布局法: circular dendrogram
ggraph(graph,layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() + 
  geom_node_point(aes(filter = leaf)) + 
  coord_fixed()+
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  ) 
 


#(g) 圆填充图: circular tree map
ggraph(graph, layout = 'circlepack', weight = 'size') + 
  geom_node_circle( size = 1, n = 50) + #aes(fill = depth),
  coord_fixed()+
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  ) 


#(h) 矩形树状图: rectangular tree map
ggraph(graph, layout = 'treemap', weight = 'size') + 
  geom_node_tile(fill = "white", size = 0.25)+
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  )

#(e) 冰柱图:icicle
ggraph(graph, layout = 'partition') + 
  geom_node_tile(aes(y = -y,size = -depth))+#, fill = depth
  scale_size(range=c(0.1,1))+
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  ) 


# (f) 旭日图: sunburst (circular icicle)
ggraph(graph, layout = 'partition', circular = TRUE) +
  geom_node_arc_bar(size = 1) +#aes(fill = depth)
  coord_fixed()+
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  ) 

#(c) 正交布局的树形图
dendrogram <- as.dendrogram(hclust(dist(iris[, 1:4])))
ggraph(dendrogram, 'dendrogram') + 
  geom_edge_elbow(edge_width=1)+
  theme_minimal() +
  theme(
    legend.position="none",
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks =element_blank(),
    axis.text =element_blank(),
    axis.title = element_blank()
    #plot.margin=unit(c(0,0,0,0), "null"),
    #panel.spacing=unit(c(0,0,0,0), "null")
  ) 


#(d) 径向布局的树形图
ggraph(dendrogram, 'dendrogram', circular = TRUE) + 
  geom_edge_elbow(edge_width=1) + 
  coord_fixed()+
  theme_minimal() +
  theme(
    legend.position="none",
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks =element_blank(),
    axis.text =element_blank(),
    axis.title = element_blank()
    #plot.margin=unit(c(0,0,0,0), "null"),
    #panel.spacing=unit(c(0,0,0,0), "null")
  ) 