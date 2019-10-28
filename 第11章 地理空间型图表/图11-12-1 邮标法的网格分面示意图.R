
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)  
library(dplyr)
set.seed(1234)
df_hexmap<-read.csv("ChinaMap.csv",stringsAsFactors=FALSE)


df_province<-data.frame(Province=unique(df_hexmap$Province),
                        '2001'=round(runif(34,0,250)),
                        '2002'=round(runif(34,0,250)),
                        '2003'=round(runif(34,0,250)),
                        '2004'=round(runif(34,0,250)),
                        check.names=FALSE)


df<-left_join(df_hexmap,df_province)

df_melt<-melt(df,id.vars = c('id', 'x','y', 'Province', 'Centerx','Centery'))
df_melt$group<-cut(df_melt$value,breaks=c(0,50,100,150,200,250),labels=c('0~50','50~100','100~150','150~200','200~250'),order=TRUE,include.lowest = TRUE)


#showtext.begin()

ggplot(data=df_melt)+
  geom_polygon(aes(x=x,y=y,group=Province,fill=group),colour="grey30",size=0.1)+
  geom_text(aes(x=Centerx,y=Centery,label=Province),size=2)+
  scale_fill_manual(values=rev(brewer.pal(5,'Spectral')))+
  facet_wrap(~variable)+
  theme_void()+
  theme( strip.text = element_text(size=12),
         legend.position = "right")  

#showtext.end()
#dev.off()
