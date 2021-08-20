#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts

library(tidyverse)
library(igraph)
library(ggraph)
library(colormap)
library(wesanderson)
library(reshape2)

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
  filter(to %in% vertices$name)%>%
 left_join(vertices,by=c('from'='name'))
 
# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = vertices, directed = FALSE )

mycolor <- wes_palette("Darjeeling1", max(vertices$group), type = "continuous")
mycolor <- sample(mycolor, length(mycolor))


#-----------------------------------MDS(多尺度分析布局)-------------------------------------------
ggraph(mygraph,layout='mds') + 
  geom_edge_link(edge_colour="black", edge_alpha=0.2, edge_width=0.3) +
  geom_node_point(aes(size=n, fill=as.factor(group)), shape=21,color='black',alpha=0.9) +
  scale_size_continuous(range=c(0.5,10)) +
  scale_fill_manual(values=mycolor) +
  geom_node_text(aes(label=ifelse(n>20, as.character(name), "")), size=3, color="black") +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))+
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

#------------------------------nicely(简单决策树simpe decision tree)---------------------------
ggraph(mygraph,layout='nicely') + 
  geom_edge_link(edge_colour="black", edge_alpha=0.2, edge_width=0.3) +
  geom_node_point(aes(size=n, fill=as.factor(group)), shape=21,color='black',alpha=0.9) +
  scale_size_continuous(range=c(0.5,10)) +
  scale_fill_manual(values=mycolor) +
  geom_node_text(aes(label=ifelse(n>20, as.character(name), "")), size=3, color="black") +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))+
  theme_minimal() +
  theme(
    legend.position="none",
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks =element_blank(),
    axis.text =element_blank(),
    axis.title = element_blank())


#----------------------------------fr(Fruchterman-Reingold 算法的力导向布局)-----------------------
ggraph(mygraph,layout='fr') + 
  geom_edge_link(edge_colour="black", edge_alpha=0.2, edge_width=0.3) +
  geom_node_point(aes(size=n, fill=as.factor(group)), shape=21,color='black',alpha=0.9) +
  scale_size_continuous(range=c(0.5,10)) +
  scale_fill_manual(values=mycolor) +
  geom_node_text(aes(label=ifelse(n>20, as.character(name), "")), size=3, color="black") +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))+
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

#----------------------------------------randomly(随机布局)------------------------------------------
ggraph(mygraph,layout='randomly') + 
  geom_edge_link(edge_colour="black", edge_alpha=0.2, edge_width=0.3) +
  geom_node_point(aes(size=n, fill=as.factor(group)), shape=21,color='black',alpha=0.9) +
  scale_size_continuous(range=c(0.5,10)) +
  scale_fill_manual(values=mycolor) +
  geom_node_text(aes(label=ifelse(n>20, as.character(name), "")), size=3, color="black") +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))+
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

#----------------------------------------drl(drl导向布局)---------------------------------------------
ggraph(mygraph,layout='drl') + 
  geom_edge_link(edge_colour="black", edge_alpha=0.2, edge_width=0.3) +
  geom_node_point(aes(size=n, fill=as.factor(group)), shape=21,color='black',alpha=0.9) +
  scale_size_continuous(range=c(0.5,10)) +
  scale_fill_manual(values=mycolor) +
  geom_node_text(aes(label=ifelse(n>20, as.character(name), "")), size=3, color="black") +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))+
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
