
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#Reference: https://rpubs.com/rushgeo/flowcharts1

library(ggraph)
library(igraph) 
library(tidyverse)
library(colormap)
library(RColorBrewer) 
set.seed(1)

#构造数据集
d11=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d21=data.frame(from=sort(sample(rep(d11$to, each=100),100,replace=FALSE)), 
              to=sample(paste("subgroup", seq(1,100), sep="_"),100,replace=FALSE))

edges=rbind(d11, d21)


vertices_name<-unique(c(as.character(edges$from), as.character(edges$to)))
vertices_value <- runif(length(vertices_name))
names(vertices_value)<-vertices_name

vertices = data.frame(
  name = vertices_name,
  value = vertices_value
) 

vertices$group = edges$from[ match( vertices$name, edges$to ) ]

vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id=nleaves/360*90
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves

# 计算标签的旋转方向
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

mygraph <- graph_from_data_frame( edges, vertices=vertices )


mycolor <- colormap(colormap = colormaps$viridis, nshades = 6, format = "hex", alpha = 1, reverse = FALSE)[sample(c(1:6), 10, replace=TRUE)]

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(aes(colour=..index..)) +#colour="grey"
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.25, y=y*1.25,  angle = angle,  label=name,color=group),#90-360 * (1:114-0.5) /nleaves),
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
    plot.margin=unit(c(0,0,0,0),"cm"))
  