
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts


library(ggplot2)
library(ggforce)
#图11-3-3(a) 水平双气泡(两个数据系列)的绘图数据


#(a) 水平双气泡（两个数据系列）
df<-data.frame(r=c(4,2),
               start=c(-pi/2, pi/2),
               end=c(pi/2,3*pi/2),
               group=c('a','b'))
ggplot(df)+
  geom_arc_bar(aes(x0=0,y0=0,r0=0,r=r,
                   start=start,
                   end=end,
                   fill=group))+
  xlim(-4,4)+
  ylim(-4,4)+
  coord_fixed()


#(b) 竖直双气泡（两个数据系列）
df<-data.frame(r=c(4,2),
               start=c(0,pi),
               end=c(pi,2*pi),
               group=c('a','b'))
ggplot(df)+
  geom_arc_bar(aes(x0=0,y0=0,r0=0,r=r,
                   start=start,
                   end=end,
                   fill=group))+
  xlim(-4,4)+
  ylim(-4,4)+
  coord_fixed()

#(c) 南丁格尔玫瑰图（4 个数据系列）
df3<-data.frame(r=c(4,3,1,2), start=c(0,90,180,270)/180*pi, end=(c(0,90,180,270)+90)/180*pi, group=c('a','b','c','d'))
ggplot(df3)+
  geom_arc_bar(aes(x0=0,y0=0,r0=0,r=r, start=start, end=end, fill=group))+
  xlim(-4,4)+
  ylim(-4,4)+
  coord_fixed()
