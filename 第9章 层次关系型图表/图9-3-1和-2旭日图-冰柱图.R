
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggraph)
library(igraph)
library(RColorBrewer) 
library(dplyr)

df<-read.csv('旭日图.csv',header=TRUE,stringsAsFactors=FALSE)

#--------------------------------分割角度均等平分----------------------------------------
edges<- data.frame(rbind(
               cbind(rep('origin',4),unique(as.character(df$Season))),
               as.matrix(df[!duplicated(df[c('Season','Month')]),1:2]),
               as.matrix(df[!duplicated(df[c('Month','Week')]),2:3])))
colnames(edges)<-c('from','to')

vertices0<-data.frame(name=unique(c(as.character(edges$from), as.character(edges$to))))
df_leaf<-df[,c('Week','Value','label')]
df_leaf$angle<-90-(1:nrow(df_leaf))/nrow(df_leaf)*360
df_leaf$angle<-ifelse(df_leaf$angle< -90, df_leaf$angle+180, df_leaf$angle)

vertices<-left_join(vertices0,df_leaf,by=c('name'='Week'))

graph <- graph_from_data_frame(edges, vertices = vertices)

#图9-3-1 旭日图(a) 分割角度均等平分
ggraph(graph, layout ='partition', circular = TRUE) +
  geom_node_arc_bar(aes(filter =(depth<=2 & depth>0 )),fill='#FEE5D9',color='black',size=0.1)+#,size=size
  geom_node_arc_bar(aes(filter =(depth==3 ),fill = Value),size=0.1)+
  geom_node_text( aes(filter =(depth<=2 & depth>0 ),label=name,size=-depth),angle=0,colour="black") +
  geom_node_text(aes(label=label,angle=angle),size=2,colour="black") +
  scale_size(range=c(3,5))+
  coord_fixed()+
  scale_fill_distiller(palette='Reds')+
  guides(size="none")+
  theme_void()

#图9-3-2 冰柱图(a) 分割角度均等平分
ggraph(graph, layout ='partition') + 
  geom_node_tile(aes(filter =(depth<=2)),fill='#FEE5D9',color='black',size=0.1)+#,size=size
  geom_node_tile(aes(filter =(depth==3 ),fill = Value),size=0.1)+
  
  geom_node_text(aes(filter =(depth<=2 & depth>0),label=name,size=-depth),angle=90,colour="black") +
  geom_node_text(aes(label=label),size=2,angle=90,colour="black")+
  scale_size(range=c(3,4))+
  scale_y_reverse()+
  scale_fill_distiller(palette="Reds",na.value = "#FFF2EC")+
  guides(size="none")+
  theme_void()

#-------------------------------分割角度与某个数值成比例----------------------------------
fake_circle<-c()
for (i in 1:nrow(df)){
  fake_circle<-append(fake_circle,rep(df$Week[i],round(10*df$Value[i])))
}

edges<- data.frame(rbind(
  cbind(rep('origin',4),unique(as.character(df$Season))),
  as.matrix(df[!duplicated(df[c('Season','Month')]),1:2]),
  as.matrix(df[!duplicated(df[c('Month','Week')]),2:3]),
  cbind(fake_circle,as.character(1:length(fake_circle)))
  ))

colnames(edges)<-c('from','to') 

vertices0<-data.frame(name=unique(c(as.character(edges$from), as.character(edges$to))))
df_leaf<-df[,c('Week','Value','label')]
df_leaf$angle<-90-(cumsum(df_leaf$Value)-df_leaf$Value/2)/sum(df_leaf$Value)*360
df_leaf$angle<-ifelse(df_leaf$angle< -90, df_leaf$angle+180, df_leaf$angle)
            
vertices<-left_join(vertices0,df_leaf,by=c('name'='Week'))


df_color<- data.frame(rbind(
  as.matrix(df[!duplicated(df[c('Season','Season')]),c(1,1)]),
  as.matrix(df[!duplicated(df[c('Season','Month')]),c(1,2)]),
  as.matrix(df[!duplicated(df[c('Season','Week')]),c(1,3)])
))

colnames(df_color)<-c('Season','name') 

vertices<-left_join(vertices,df_color,by='name')

graph <- graph_from_data_frame(edges, vertices = vertices)

#图9-3-1 旭日图(b) 分割角度与某个数值成比例
ggraph(graph, layout ='partition', circular = TRUE) + 
  geom_node_arc_bar(aes(filter =(depth<=3 & depth>0 ),fill = Season),size=0.1)+
  geom_node_text(aes(filter =(depth<=2 & depth>0),label=name,size=-depth),angle=0,colour="black") +
  geom_node_text(aes(label=label,angle=angle),size=2,colour="black")+
  scale_size(range=c(3,4))+
  coord_fixed()+
  scale_fill_brewer(palette="Reds",direction=-1)+
  guides(size="none",fill="none")+
  theme_void()

#图9-3-2 冰柱图(b) 分割角度与某个数值成比例
ggraph(graph, layout ='partition')+ 
  geom_node_tile(aes(filter =(depth<=3 ),fill = Season),size = 0.1)+
  geom_node_text(aes(filter =(depth<=2 & depth>0),label=name,size=-depth),angle=90,colour="black") +
  geom_node_text(aes(label=label),size=2,angle=90,colour="black")+
  scale_size(range=c(3,4))+
  scale_y_reverse()+
  scale_fill_brewer(palette="Reds",na.value = "#FFF2EC")+
  guides(size="none",fill="none")+
  theme_void()

