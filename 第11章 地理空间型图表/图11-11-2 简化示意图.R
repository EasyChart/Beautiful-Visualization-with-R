
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts
library(ggplot2)
df_point<-read.csv("China_Grid.csv",stringsAsFactors=TRUE)

#图11-11-2(a)矩形
ggplot(data= df_point,aes(x=col,y=row))+
  geom_tile(colour="black",size=0.1,fill="white")+
  geom_text(aes(label=code),size=3)+
  xlim(0.5,7.5)+
  scale_y_reverse(limits =c(8.5,0.5))


#图11-11-2(b)圆圈
ggplot(data= df_point,aes(x=col,y=row))+
  geom_point(shape=21,colour="black",size=14,fill="white")+
  geom_text(aes(label=code),size=3)+
  xlim(0.5,7.5)+
  scale_y_reverse(limits =c(8.5,0.5))

##图11-11-2(c)六角形
df_hexmap<-read.csv("ChinaMap.csv",stringsAsFactors=FALSE)
ggplot()+
  geom_polygon(data= df_hexmap, aes(x=x, y=y, group=Province), fill="white", colour="black",size=0.25)+
  geom_text(data= df_hexmap, aes(x=Centerx, y=Centery-0.01, group=Province,label=Province),size=2)