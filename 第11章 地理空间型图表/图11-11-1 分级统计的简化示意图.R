
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts


library(geofacet)
library(ggplot2)
library(RColorBrewer)  
library(Cairo)         
library(showtext)

library(dplyr)
set.seed(1234)


#(a) 中国简化六边形地图--------------------------------------------------------------------------------------------

df_hexmap<-read.csv("ChinaMap.csv",stringsAsFactors=FALSE)

df_province<-data.frame(Province=unique(df_hexmap$Province),
                        value=round(runif(34,0,250)))

df_province$group<-cut(df_province$value,breaks=c(0,50,100,150,200,250),labels=c('0~50','50~100','100~150','150~200','200~250'),order=TRUE,include.lowest = TRUE)

df<-left_join(df_hexmap,df_province)

ggplot()+
  geom_polygon(data=df, aes(x=x, y=y, group=Province, fill=group),colour="black",size=0.25)+  
  geom_text(data=df, aes(x=Centerx, y=Centery-0.01, group=Province,label=Province),size=3)+
  scale_fill_manual(values=rev(brewer.pal(5,'Spectral')))


#图(b)中国简化圆圈地图------------------------------------------------------------------------------------

library(ggplot2)
df_point<-read.csv("China_Grid.csv",stringsAsFactors=TRUE)

df<-left_join(df_point,df_province,by=c('code'='Province'))

ggplot(data= df,aes(x=col,y=row))+
  geom_point(fill='white',size=15,shape=21,colour="black")+
  geom_point(aes(fill=value,size=value),shape=21,colour="black")+
  geom_text(aes(label=code),size=3)+
  scale_size(range=c(1,12))+
  scale_fill_gradientn(colors=rev(brewer.pal(11,'Spectral')))+
  guides(fill=guide_legend(title="Value"),size=guide_legend(title="Value"))+ #reverse=TRUE,      
  xlim(0.5,7.5)+
  scale_y_reverse(limits =c(8.5,0.5))
