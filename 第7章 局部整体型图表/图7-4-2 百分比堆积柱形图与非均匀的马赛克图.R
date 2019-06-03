#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
library(reshape2)  #提供melt()函数
library(plyr)      #提供ddply()函数,join()函数

df <- data.frame(segment = c("A", "B", "C","D"),
                      Alpha = c(2400	,1200,	600	,250), 
                      Beta = c(1000	,900,	600,	250),
                      Gamma = c(400,	600	,400,	250), 
                      Delta = c(200,	300	,400,	250))

melt_df<-melt(df,id="segment")

segpct<-rowSums(df[,2:ncol(df)])


for (i in 1:nrow(df)){
  for (j in 2:ncol(df)){
    df[i,j]<-df[i,j]/segpct[i]*100  #将数字转换成百分比
  }
}

segpct<-segpct/sum(segpct)*100
df$xmax <- cumsum(segpct)
df$xmin <- (df$xmax - segpct)

dfm <- melt(df, id = c("segment", "xmin", "xmax"),value.name="percentage")
colnames(dfm)[ncol(dfm)]<-"percentage"

#ddply()函数使用自定义统计函数，对data.frame分组计算
dfm1 <- ddply(dfm, .(segment), transform, ymax = cumsum(percentage))
dfm1 <- ddply(dfm1, .(segment), transform,ymin = ymax - percentage)
dfm1$xtext <- with(dfm1, xmin + (xmax - xmin)/2)
dfm1$ytext <- with(dfm1, ymin + (ymax - ymin)/2)

#join()函数，连接两个表格data.frame
dfm2<-join(melt_df, dfm1, by = c("segment", "variable"), type = "left", match = "all")

ggplot()+
  geom_rect(aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = variable),dfm2,colour = "black") + 
  geom_text(aes(x = xtext, y = ytext,  label = value),dfm2 ,size = 4)+
  geom_text(aes(x = xtext, y = 103, label = paste("Seg ", segment)),dfm2 ,size = 4)+
  geom_text(aes(x = 102, y = seq(12.5,100,25), label = c("Alpha","Beta","Gamma","Delta")), size = 4,hjust = 0)+
  scale_x_continuous(breaks=seq(0,100,25),limits=c(0,110))+
  theme(panel.background=element_rect(fill="white",colour=NA),
        panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        text=element_text(size=15),
        legend.position="none")



#-----------------------------------------------Method 2-----------------------------------
library(vcd)
table<-xtabs(value ~variable+segment, melt_df)
mosaic( ~segment+variable,table,shade=TRUE,legend=TRUE,color=TRUE)
