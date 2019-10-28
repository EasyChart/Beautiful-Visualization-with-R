

#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggalluvial)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(reshape2)
df <- read.csv("AdjacencyDirectedWeighted.csv", header=TRUE,stringsAsFactors = FALSE,check.names = FALSE)

df_melt<-melt(df,id.vars = 'Region')
colnames(df_melt)<-c('from','to','weight')

df_melt$to<-as.character(df_melt$to)
df_melt$group<-seq(1,nrow(df_melt))
df_melt<-melt(df_melt,id.vars = c('weight','group'),value.name ='Region',factorsAsStrings=FALSE)

#--------------------------------------排序:纵向桑基图-----------------------------------------------
df_sum<-apply(df[,2:ncol(df)],2,sum)
order<-sort(df_sum,index.return=TRUE,decreasing =TRUE)

df_melt$Region<-factor(df_melt$Region,levels=df$Region[order$ix])
df_melt$variable<-factor(df_melt$variable,levels=c('from','to'))


mycolor <- colorRampPalette(brewer.pal(9,'YlGnBu'))(13)

ggplot(df_melt,
       aes(x = variable,weight = weight,
           stratum = Region, alluvium = group,
           fill = Region, label = Region)) +
  geom_flow(alpha = 0.7,width=0.25,color = "darkgray") +
  geom_stratum(alpha =1,width=0.25) +
  geom_text(stat = "stratum", size = 3.5,angle=0) +
  scale_fill_manual(values= mycolor)+   #values=mycolor)+
  theme_test()+
  theme(legend.position = "none",
        axis.text.y =element_blank(),
        axis.line = element_blank(),
        axis.ticks =element_blank() )


#--------------------------------------排序：横向桑基图-----------------------------------------------
f_sum<-apply(df[,2:ncol(df)],2,sum)
order<-sort(df_sum,index.return=TRUE,decreasing =FALSE)

df_melt$Region<-factor(df_melt$Region,levels=df$Region[order$ix])
df_melt$variable<-factor(df_melt$variable,levels=c('from','to'))

ggplot(df_melt,
       aes(x = variable,weight = weight,
           stratum = Region, alluvium = group,
           fill = Region, label = Region)) +
  geom_flow(alpha = 0.7,width=0.25,color = "darkgray") +
  geom_stratum(alpha =1,width=0.25) +
  geom_text(stat = "stratum", size = 3.5,angle=0) +
  scale_fill_manual(values= mycolor)+   #values=mycolor)+
  
  coord_flip() +
  theme_test()+
  theme(legend.position = "none",
        #axis.text.y =element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank(),
        axis.line = element_blank(),
        axis.ticks =element_blank() )