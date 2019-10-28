

#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#Reference: https://rpubs.com/rushgeo/flowcharts1


library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer) 
set.seed(1)

#构造节点连接数据edges
d11=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d21=data.frame(from=sort(sample(rep(d11$to, each=100),100,replace=FALSE)), 
              to=sample(paste("subgroup", seq(1,100), sep="_"),100,replace=FALSE))
edges<-rbind(d11[,1:2], d21[,1:2])

#构造节点属性数据vertices
vertices_name<-unique(c(as.character(edges$from), as.character(edges$to)))
vertices<-data.frame(name = vertices_name, value =runif(length(vertices_name)))
rownames(vertices)<-vertices_name


#根据最外层的节点的分组总数，对节点作降序处理
d2<-d21 %>% 
  mutate(order2=as.numeric(factor(from,
                                  levels=unique(from)[sort(summary (as.factor(from)),index.return=TRUE,decreasing = T)$ix],
                                  order=TRUE)))%>% 
  arrange(order2)
#根据最外层的节点的属性数值,对节点作降序处理，
d2<-d2%>% 
  left_join(vertices ,by = c("to" = "name")) %>% 
  arrange(order2,desc(value))

#重新构造节点连接数据edges
edges<-rbind(d11[,1:2], d2[,1:2])

#重新构造节点属性数据vertices
list_unique<-unique(c(as.character(edges$from), as.character(edges$to)))
vertices = data.frame(
  name = list_unique, 
  value = vertices[list_unique,'value']
  ) 

#节点属性数据vertices添加列叶节点的分组group和标签旋转角度angle
vertices$group<-edges$from[match(vertices$name, edges$to)]
vertices$id<-NA
myleaves<-which(is.na( match(vertices$name, edges$from) ))
nleaves<-length(myleaves)
vertices$id<-nleaves/360*90
vertices$id[ myleaves]<-seq(1:nleaves)
vertices$angle<-90 - 360 * vertices$id / nleaves
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)


mygraph <- graph_from_data_frame( edges, vertices = vertices, directed = TRUE )

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(aes(colour=..index..)) +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.25, y=y*1.25,  angle = angle,  label=name,color=group),
                 size=2.7, alpha=1) +
  
  geom_node_point(aes( x = x*1.07, y=y*1.07, fill=group, size=value), 
                  shape=21,stroke=0.2,color='black',alpha=1) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_fill_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,7) ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))+
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  )