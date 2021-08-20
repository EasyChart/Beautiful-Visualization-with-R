
#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts


library(RColorBrewer)
library(ggplot2)
library(reshape2)
#数据来源：https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv
df <- read.csv("AdjacencyDirectedWeighted.csv",header=TRUE,stringsAsFactors = FALSE)

df_sum<-apply(df[,2:ncol(df)],2,sum)#+apply(df[,2:ncol(df)],1,sum)
order<-sort(df_sum,index.return=TRUE,decreasing =FALSE)

df_melt <- melt(df,id.vars = 'Region')
colnames(df_melt)<-c("from","to","value")

df_melt$to<-gsub("\\.", " ",df_melt$to)

df_melt$to<-factor(df_melt$to,levels=df$Region[order$ix],order=TRUE)


ggplot(df_melt, aes(x = from, y = to, fill = value,label=value)) +
  geom_tile(colour="black") +
  #geom_text(size=3,colour="white")+
  coord_equal()+
  scale_fill_gradientn(colors=brewer.pal(9,'YlGnBu'))+
  xlab('FROM')+
  ylab('TO')+
  #scale_fill_viridis(discrete=FALSE)+
  theme(
    axis.text.x = element_text(angle=90,hjust=1,colour='black'),
    axis.text.y = element_text(angle=0,hjust=1,colour='black')
  )


ggplot(df_melt, aes(x = from, y = to, fill = value,label=value)) +
  geom_tile(colour="black") +
  scale_fill_gradientn(colors=brewer.pal(9,'YlGnBu'))+
  xlab('FROM')+
  ylab('TO')+
  coord_equal()+
  theme(
    axis.text.x = element_text(angle=90,hjust=1,colour='black'),
    axis.text.y = element_text(angle=0,hjust=1,colour='black')
  )

