
#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts


library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)

d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=sort(sample(rep(d1$to, each=100),100,replace=FALSE)), 
              to=sample(paste("subgroup", seq(1,100), sep="_"),100,replace=FALSE))

d2<-d2 %>% 
  mutate(order2=as.numeric(factor(from,
                                  levels=unique(from)[sort(summary (as.factor(from)),index.return=TRUE,decreasing = T)$ix],
                                  order=TRUE)))%>% 
  arrange(order2)

edges=rbind(d1[,1:2], d2[,1:2])


vertices_name<-unique(c(as.character(edges$from), as.character(edges$to)))
vertices_value <- runif(length(vertices_name))
names(vertices_value)<-vertices_name

d2<-d2%>% 
  left_join(data.frame(name = vertices_name, value =vertices_value) ,by = c("to" = "name")) %>% 
  arrange(order2,desc(value))

edges=rbind(d1[,1:2], d2[,1:2])

list_unique<-unique(c(as.character(edges$from), as.character(edges$to)))
vertices = data.frame(
  name = list_unique, 
  value = vertices_value[list_unique]
) 

vertices$group = edges$from[ match( vertices$name, edges$to ) ]


vertices$id<-NA
myleaves<-which(is.na( match(vertices$name, edges$from) ))
nleaves<-length(myleaves)
vertices$id[ myleaves ]<- seq(1:nleaves)
vertices$angle<-90 - 360 * vertices$id / nleaves

vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

# create a dataframe with connection between leaves (individuals)
all_leaves=paste("subgroup", seq(1,100), sep="_")
connect=rbind( data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), 
               data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), 
               data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), 
               data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
connect$value=runif(nrow(connect))

mygraph <- graph_from_data_frame( edges, vertices=vertices )

from = match( connect$from, vertices$name)
to = match( connect$to, vertices$name)

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, fill=group, size=value),
                  alpha=1,shape=21, stroke=0.2) +# 
  geom_conn_bundle(data = get_con(from = from, to = to), aes(edge_colour=group),
                   tension=1,alpha=1,  edge_width=0.5) +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=3, alpha=1) +
  scale_size_continuous( range = c(0.1,5) ) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_edge_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_fill_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.3, 1.3))+
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


#png("my figure.png", height = 480, width=480)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, color=group, size=value),
                 alpha=0.8) +#shape=21, stroke=0.2, 
  geom_conn_bundle(data = get_con(from = from, to = to), aes(edge_colour=group),
                   tension=1,alpha=0.3,  edge_width=0.9) +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=3, alpha=1) +
  scale_size_continuous( range = c(0.1,10) ) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_edge_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_fill_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.3, 1.3))+
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


ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, width=0.9, aes(colour=..index..)) +
  scale_edge_colour_distiller(palette = "RdPu") +
  
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=3, alpha=1) +
  
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) +
  
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))+
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
