
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#Reference: https://rpubs.com/rushgeo/flowcharts1

library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer) 
set.seed(1)
d0=data.frame(from="origin", to=paste("group0", seq(1,3), sep=""))
d1=data.frame(from=rep(d0$to, c(5,3,2)), to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=sort(sample(rep(d1$to, each=100),100,replace=FALSE)), 
              to=sample(paste("subgroup", seq(1,100), sep="_"),100,replace=FALSE))

group_sum<-summary (as.factor(d2$from))
d1$group_sum<-group_sum[d1$to]
d1<-d1 %>% 
  mutate(order1=as.numeric(factor(from,
                                  levels=unique(from)[sort(summary (as.factor(from)),index.return=TRUE,decreasing = T)$ix],
                                  order=TRUE)))%>% 
  arrange(order1, desc(group_sum))

d2<-left_join(d2,d1,by = c("from" = "to"))
d2<-d2 %>% 
  mutate(order2=as.numeric(factor(from,
                                  levels=unique(from)[sort(summary (as.factor(from)),index.return=TRUE,decreasing = F)$ix],
                                  order=TRUE)))%>% 
  arrange(order1, desc(group_sum),desc(order2))

edges<-rbind(d0,d1[,1:2], d2[,1:2])

vertices_name<-unique(c(as.character(edges$from), as.character(edges$to)))
vertices<-data.frame(name = vertices_name, value =runif(length(vertices_name)))
rownames(vertices)<-vertices_name

d2<-d2%>% 
  left_join(vertices ,by = c("to" = "name")) %>% 
  arrange(order1, desc(group_sum),order2,desc(value))

edges=rbind(d0,d1[,1:2], d2[,1:2])

list_unique<-unique(c(as.character(edges$from), as.character(edges$to)))
vertices = data.frame(
  name = list_unique, 
  value = vertices[list_unique,'value']
) 
vertices$group<-edges$from[ match( vertices$name, edges$to ) ]

vertices$id<-NA
myleaves<-which(is.na( match(vertices$name, edges$from) ))
nleaves<-length(myleaves)
vertices$id<-90
vertices$id[myleaves]<-0
vertices$angle<-90 - vertices$id 

mygraph <- graph_from_data_frame( edges, vertices = vertices, directed = TRUE )


ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal(aes(colour=..index..)) +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x, y=y,  angle = angle,  label=name,color=group),
                 hjust=1.25,size=2.7, alpha=1) +
  
  geom_node_point(aes( x = x, y=y, fill=group, size=value), 
                  shape=21,stroke=0.2,color='black',alpha=0.9) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_fill_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,7) ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))+
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")) 
