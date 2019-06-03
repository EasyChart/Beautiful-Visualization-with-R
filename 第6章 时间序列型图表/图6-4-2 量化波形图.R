#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(reshape2)
library(ggTimeSeries)

df<-read.csv("StreamGraph_Data.csv",header=TRUE)
df_series<-df[,2:ncol(df)]
Col_Max<-apply(df_series,2,max)
Col_Sort<-sort(Col_Max,index.return=TRUE,decreasing = TRUE)

mydata<-melt(df,id="time")

mydata$variable<-factor(mydata$variable,levels=colnames(df_series)[Col_Sort$ix])
ggplot(mydata, aes(x = time, y = value, group = variable, fill = variable)) +
  stat_steamgraph(colour="black",size=0.25)+ 
  xlab('Time') + 
  ylab('') + 
  theme_light()
