

#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)
data(flare)

edges<-flare$edges %>% filter(to %in% from) %>% droplevels()
vertices <- flare$vertices %>% filter(name %in% c(edges$from, edges$to)) %>% droplevels()
vertices$size<-runif(nrow(vertices))

mygraph <- graph_from_data_frame( edges, vertices=vertices )

colormap <- colorRampPalette(rev(brewer.pal(9,'YlGnBu')))(32)
ggraph(mygraph, layout = 'circlepack', weight="size" ) + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_text( aes(label=shortName, filter=leaf, fill=depth, size=size)) +
  theme_void() + 
  theme(legend.position="FALSE") + 
  scale_fill_gradientn(colors=colormap)
  #scale_fill_viridis()


#-------------------------------------------小数据集案例---------------------------------------------
library(ggraph)
library(igraph)
library(dplyr)

df <- data.frame(group=c("root", "root", "a","a","b","b","b"),    
                 subitem=c("a", "b", "x","y","z","u","v"), 
                 size=c(0, 0, 6,2,3,2,5))

# create a dataframe with the vertices' attributes
vertices <- df %>% 
  distinct(subitem, size) %>% 
  add_row(subitem = "root", size = 0)

graph <- graph_from_data_frame(df, vertices = vertices)

ggraph(graph, layout = "circlepack", weight = 'size') + 
  geom_node_circle(aes(fill =depth)) +
  # adding geom_text to see which circle is which node 
  geom_text(aes(x = x, y = y, label = paste(name, "size=", size))) +
  coord_fixed()