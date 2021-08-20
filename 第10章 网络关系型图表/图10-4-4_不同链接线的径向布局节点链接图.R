
#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts

#reference:https://www.data-imaginist.com/2017/ggraph-introduction-layouts/

library(tidyverse)
library(circlize)
library(viridis)
library(igraph)
library(ggraph)
library(colormap)
library(ggplot2)
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

  mutate(group=as.numeric(factor(group,
                                 levels=sort(summary (as.factor(group)),index.return=TRUE,decreasing = T)$ix,
                                 order=TRUE)))%>%
  filter( group<10) %>%
  arrange(group,desc(n)) %>%
  mutate(name=factor(name, name))

# keep only this people in edges
connect <- connect %>%
  filter(from %in% vertices$name) %>%
  filter(to %in% vertices$name) %>%
  left_join(vertices,by=c('from'='name'))

# Add label angle
number_of_bar<-nrow(vertices)
vertices$id = seq(1, nrow(vertices))
angle= 360 * (vertices$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
vertices$hjust<-ifelse(angle>180, 1, 0)
vertices$angle<-ifelse(angle>180, 90-angle+180, 90-angle)

# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = vertices, directed = FALSE )

mycolor <- wes_palette("Darjeeling1", max(vertices$group), type = "continuous")

#(b)曲线链接
ggraph(mygraph,layout = 'linear', circular = TRUE) +
  geom_edge_arc(aes(edge_colour=as.factor(group)), edge_alpha=0.5, edge_width=0.3) +
  geom_node_point(aes(size=n, fill=as.factor(group)), shape=21,color='black',alpha=0.9) +
  scale_size_continuous(range=c(0.5,10)) +
  scale_fill_manual(values=mycolor) +
  geom_node_text(aes(x = x*1.06, y=y*1.06, label=name, angle=angle,hjust=hjust,
                     color=as.factor(group)),size=3) +
  scale_color_manual(values=mycolor) +
  scale_edge_color_manual(values=mycolor) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))+
  coord_fixed()+
  theme_minimal() +
  theme(
    legend.position="none",
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks =element_blank(),
    axis.text =element_blank(),
    axis.title = element_blank(),
    plot.margin=unit(c(0,0,0,0), "null"),
    panel.spacing=unit(c(0,0,0,0), "null")
  ) 

# (a) 直线链接
ggraph(mygraph,layout = 'linear', circular = TRUE) +
  geom_edge_link(aes(edge_colour=as.factor(group)),  edge_alpha=0.5, edge_width=0.3) +
  geom_node_point(aes(size=n, fill=as.factor(group)), shape=21,color='black',alpha=0.9) +
  scale_size_continuous(range=c(0.5,10)) +
  scale_fill_manual(values=mycolor) +
  geom_node_text(aes(x = x*1.05, y=y*1.05, label=name, angle=angle,hjust=hjust,
                     color=as.factor(group)),size=3) +
  scale_color_manual(values=mycolor) +
  scale_edge_color_manual(values=mycolor) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))+
  coord_fixed()+
  theme_minimal() +
  theme(
    legend.position="none",
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks =element_blank(),
    axis.text =element_blank(),
    axis.title = element_blank(),
    plot.margin=unit(c(0,0,0,0), "null"),
    panel.spacing=unit(c(0,0,0,0), "null")
  ) 
