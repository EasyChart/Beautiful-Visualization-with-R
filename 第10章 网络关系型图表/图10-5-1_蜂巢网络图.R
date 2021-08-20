

#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts

#reference：
#http://www.hiveplot.net/
#https://www.data-imaginist.com/tags/ggraph/
#http://www.sthda.com/english/articles/33-social-network-analysis/135-network-visualization-essentials-in-r/


library(ggraph)
library(igraph)
library(dplyr)
library(wesanderson)

graph <- graph_from_data_frame(highschool)

V(graph)$friends <- degree(graph, mode = 'in')
V(graph)$friends <- ifelse(V(graph)$friends < 5, 'Few', 
                           ifelse(V(graph)$friends >= 15, 'Many', 'Medium'))
V(graph)$count <- degree(graph, mode = 'in')


mycolor <- wes_palette("Darjeeling1", length(unique((V(graph)$friends))), type = "continuous")

ggraph(graph, 'hive', axis = 'friends',sort.by ='count') + 
  geom_edge_hive(colour = 'black', edge_alpha = 0.3) + 
  geom_axis_hive(color='black', size = 1, label = TRUE) + 
  geom_node_point(aes(size=count,fill=friends),shape=21,colour = 'black',stroke=0.2, alpha = 0.95)+
  scale_size_continuous(range=c(0.5,8)) +
  scale_fill_manual(values=mycolor)+
  guides(fill='none')+
  coord_fixed()+
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks =element_blank(),
    axis.text =element_blank(),
    axis.title = element_blank()
  ) 


ggraph(graph, 'hive', axis = 'friends',sort.by ='count') + 
  geom_edge_hive(aes(colour = factor(year)), edge_alpha = 0.5) + 
  geom_axis_hive(color='black', size = 1, label = TRUE) + 
  geom_node_point(aes(size=count),fill='gray80',shape=21,colour = 'black',stroke=0.2, alpha = 0.95)+
  scale_size_continuous(range=c(0.5,8)) +
  scale_edge_colour_manual(values=mycolor[c(1,3)])+
  coord_fixed()+
  theme_minimal() +
  theme(
    legend.position="right",
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks =element_blank(),
    axis.text =element_blank(),
    axis.title = element_blank()) 
