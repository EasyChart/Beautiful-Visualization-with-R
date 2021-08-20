
#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts
library(tidyverse)
library(circlize)
library(viridis)
library(igraph)
library(ggraph)
library(colormap)
library(ggplot2)
library(reshape2)
library(wesanderson)

dataUU <-read.csv("AdjacencyUndirectedUnweighted.csv",header=TRUE,check.names = FALSE)

# Transform the adjacency matrix in a long format
connect <- dataUU %>%
  gather(key="to", value="value", -1) %>%
  mutate(to = gsub("\\.", " ",to)) %>%
  na.omit()
# Number of connection per person
c( as.character(connect$from), as.character(connect$to)) %>%
  as.tibble() %>%
  group_by(value) %>%
  summarize(n=n()) -> vertices

colnames(vertices) <- c("name", "n")


# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = vertices, directed = FALSE )



# Find community
com <- walktrap.community(mygraph)

#Reorder dataset and make the graph
vertices <- vertices %>%
  mutate( group = com$membership) %>%
  filter( group<10) %>%
  mutate(group=as.numeric(factor(group,
                                 levels=sort(summary (as.factor(group)),index.return=TRUE,decreasing = T)$ix,
                                 order=TRUE)))%>%
  arrange(group,desc(n)) %>%
  mutate(name=factor(name, name))



# keep only this people in edges
connect <- connect %>%
  filter(from %in% vertices$name) %>%
  filter(to %in% vertices$name)%>%
  left_join(vertices,by=c('from'='name'))

# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = vertices, directed = FALSE )

mycolor <- wes_palette("Darjeeling1", max(vertices$group), type = "continuous")

# Make the graph
ggraph(mygraph, layout="linear") + 
  #geom_edge_arc(aes(x=y, y=x, xend=yend, yend=xend, alpha = ..index..,color = node.class))+
  geom_edge_arc(aes(edge_colour=as.factor(group)), edge_alpha=0.5, edge_width=0.3, fold=TRUE) +
  geom_node_point(aes(size=n, fill=as.factor(group)), shape=21,color='black',alpha=0.9,stroke=0.1) +
  scale_size_continuous(range=c(0.5,10)) +
  scale_fill_manual(values=mycolor) +
  scale_edge_colour_manual(values=mycolor) +
  #geom_node_text(aes(label=name), angle=0, hjust=1, nudge_y = -1.5, size=2) +
  #coord_flip()+
  geom_node_text(aes(label=name,color=as.factor(group)), angle=90, hjust=1, nudge_y = -1.5, size=2) +
  scale_color_manual(values=mycolor) +
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2))+
  
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



