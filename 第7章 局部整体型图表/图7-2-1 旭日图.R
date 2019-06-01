

#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts


library(ggraph)
library(igraph)
library(RColorBrewer) 

colormap <- colorRampPalette(rev(brewer.pal(5,'Reds')))(5)


graph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)


#---------------------------------------图7-2-2 冰柱图-------------------------------------------------
ggraph(graph, layout ='partition') + 
  geom_node_tile(aes(fill = as.factor(depth)), size = 0.25)+
  geom_node_text( aes(label=shortName,filter =(depth==0)),size=6, angle=0,colour="white") +
  geom_node_text( aes(label=shortName,filter =(depth==1 )),size=3.5, angle=90,colour="white") +
  geom_node_text( aes(label=shortName,filter =(depth<=3 & depth>1 &  size <10)),size=1.5, angle=90) +
  scale_fill_manual(values= colormap)+
  #scale_fill_viridis()+
  scale_y_reverse()+
  theme_void()+
  theme(legend.position = "none")

#--------------------------------------图7-2-3 旭日图----------------------------------------------------
ggraph(graph, layout ='partition', circular = TRUE) + 
  geom_node_arc_bar(aes(fill = as.factor(depth)), size = 0.25)+
  geom_node_text( aes(label=shortName,filter =(depth==0)),size=6, angle=0,colour="white") +
  geom_node_text( aes(label=shortName,filter =(depth==1 )),size=3.5, angle=0,colour="white") +
  geom_node_text( aes(label=shortName,filter =(depth<=3 & depth>1 &  size <10)),size=2, angle=0) +
  coord_fixed()+
  scale_fill_manual(values= colormap)+
  #scale_fill_viridis(discrete = TRUE)+
  theme_void()+
  theme(legend.position = "none")
