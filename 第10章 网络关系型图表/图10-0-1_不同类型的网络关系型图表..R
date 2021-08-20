

#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggraph)
library(igraph)

#(a) 热力图-------------------------------------------------------------------------------------------
library(viridis)
library(ggplot2)
library(reshape2)
df <- read.csv("AdjacencyDirectedWeighted.csv",header=TRUE,stringsAsFactors = FALSE)
df_sum<-apply(df[,2:ncol(df)],2,sum)+apply(df[,2:ncol(df)],1,sum)
order<-sort(df_sum,index.return=TRUE,decreasing =TRUE)


df_melt <- melt(df,id.vars = 'Region')
colnames(df_melt)<-c("from","to","value")

df_melt$to<-gsub("\\.", " ",df_melt$to)

df_melt$to<-factor(df_melt$to,levels=df$Region[order$ix],order=TRUE)


ggplot(df_melt, aes(x = from, y = to, fill = value,label=value)) +
  geom_tile(colour="black",size=1) +
  #geom_text(size=3,colour="white")+
  coord_equal()+
  scale_fill_gradientn(colors=c('white','black'))+
  xlab('FROM')+
  ylab('TO')+
  #scale_fill_viridis(discrete=FALSE)+
  theme(
    axis.text.x = element_text(angle=90,hjust=1,colour='black'),
    axis.text.y = element_text(angle=0,hjust=1,colour='black')
  )

#(b)桑基图------------------------------------------------------------------------------------------
library(ggalluvial)
library(ggplot2)
data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           label = response)) +
  geom_flow(alpha = 0.7) +
  geom_stratum(alpha = 1,fill = 'black',color = "white") +
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x =element_text(color="black",size=12),
        axis.title.x = element_blank(),
        axis.text.y =element_blank(),
        axis.line = element_blank(),
        axis.ticks =element_blank() )


#(c)和弦图-------------------------------------------------------------------------------------------
library(circlize)
set.seed(999)
mat<-matrix(sample(18, 18), 3, 6)
rownames(mat) <- paste0("S", 1:3)
colnames(mat) <- paste0("E", 1:6)
df<- data.frame(from = rep(rownames(mat), times = ncol(mat)),
                to = rep(colnames(mat), each = nrow(mat)),
                value = as.vector(mat),
                stringsAsFactors = FALSE)
chordDiagram(df,grid.col = 'black',
             link.border="grey", 
             transparency = 0.35,
             directional = 1,
             #direction.type = c("arrows", "diffHeight"),
             diffHeight = -0.04,
             annotationTrack = "grid",
             annotationTrackHeight = c(0.05, 0.1),
             link.arr.type = "big.arrow",
             link.sort = TRUE,
             link.largest.ontop = TRUE)

circos.clear()


#(d) 节点链接图---------------------------------------------------------------------------------------
library(dplyr)
# read edges and nodes
nodes = read.csv("dolphin_nodes.csv")
edges = read.csv("dolphin_edges.csv")
n = nrow(nodes)
m = nrow(edges)

# mutate edges and nodes
edge_type = sample(c("love", "friendship"), m, replace = TRUE)
edge_weight = runif(m, 1, 10)
edges = mutate(edges, type = edge_type, weight = edge_weight)
nodes = mutate(nodes, id = 1:n) %>% select(id, everything())

# degree of nodes (number of ties for each dolphin)
tb = tibble(v = c(1:n, edges$x, edges$y))
d = count(tb, v)$n - 1
nodes = mutate(nodes, degree = d)

# create graph from data frames
mygraph = graph_from_data_frame(edges, nodes,directed = FALSE)

ggraph(mygraph, layout = "fr") + 
  geom_edge_link(edge_colour = "grey") + 
  geom_node_point(aes(size = degree), colour = "black") +
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



#(h)蜂箱图---------------------------------------------------------------------------------------
flareGr <- graph_from_data_frame(flare$imports)
# Add some metadata to divide nodes by
V(flareGr)$type <- 'Both'
V(flareGr)$type[degree(flareGr, mode = 'in') == 0] <- 'Source'
V(flareGr)$type[degree(flareGr, mode = 'out') == 0] <- 'Sink'
analyticsNodes <- grep('flare.analytics', V(flareGr)$name)
E(flareGr)$type <- 'Other'
E(flareGr)[inc(analyticsNodes)]$type <- 'Analytics'
ggraph(flareGr, 'hive', axis = 'type') +
  geom_edge_hive(colour = 'black', edge_alpha = 0.4) +
  geom_axis_hive(colour = 'black',size=1) +
  #geom_edge_hive(aes(colour = type), edge_alpha = 0.1) +
  #geom_axis_hive(aes(colour = type)) +
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


#(i)边绑定图---------------------------------------------------------------------------------------
library(ggraph)
library(igraph)
library(tidyverse)

# create a data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
hierarchy=rbind(d1, d2)

# create a dataframe with connection between leaves (individuals)
all_leaves=paste("subgroup", seq(1,100), sep="_")
connect=rbind( data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
connect$value=runif(nrow(connect))

# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) , 
  value = runif(111)
) 
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = hierarchy$from[ match( vertices$name, hierarchy$to ) ]


# Create a graph object
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )

# The connection object must refer to the ids of the leaves:
from = match( connect$from, vertices$name)
to = match( connect$to, vertices$name)

# Basic graph
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), 
                   alpha=0.2, colour="black", width=0.9, tension=1)+ 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
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

