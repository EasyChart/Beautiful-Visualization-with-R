#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)

N<-20
df1 <- data.frame(x=sort(rnorm(N)),y=sort(rnorm(N)))
df2 <- data.frame(x=df1$x+0.1*rnorm(N),y=df1$y+0.1*rnorm(N))


# 所有图层共享数据源和美学映射参数
ggplot(df1,aes(x,y,colour=x+y))+
  geom_line(size=1)+
  geom_point(shape=16,size=5)+
  guides(color=guide_colorbar(title="Point\nLine"))

#所有图层仅共享数据源
ggplot(df1,aes(x,y))+
  geom_line(aes(colour=x+y),size=1)+    
  geom_point(aes(fill=x+y),color="black",shape=21,
             size=5)+
  scale_fill_distiller(name="Point",palette="YlOrRd")+
  scale_color_distiller(name="Line",palette="Blues")


#各图层对象均使用独立的数据源与美学映射参数
ggplot()+
  geom_line(aes(x,y,colour=x+y),df1,size=1)+    
  geom_point(aes(x,y,fill=x+y),df2,color="black",
             shape=21, size=5)+
  scale_fill_distiller(name="Point",palette="YlOrRd")+
  scale_color_distiller(name="Line",palette="Blues")
