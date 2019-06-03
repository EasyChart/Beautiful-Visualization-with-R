
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
library(ggrepel)
attach(mtcars)

 #-------------------------------(c) 带数据标签的气泡图-------------------------------------------------------------

 ggplot(data=mtcars, aes(x=wt,y=mpg))+
   geom_point(aes(size=disp,fill=disp),shape=21,colour="black",alpha=0.8)+
   scale_fill_gradient2(low="#377EB8",high="#E41A1C",midpoint = mean(mtcars$disp))+
   geom_text_repel(label = disp )+
   scale_size_area(max_size=12)+
   guides(size = guide_legend((title="Value")),
          fill = guide_legend((title="Value")))+
   theme(
     legend.text=element_text(size=10,face="plain",color="black"),
     axis.title=element_text(size=10,face="plain",color="black"),
     axis.text = element_text(size=10,face="plain",color="black"),
     legend.position = "right"
   )


#--------------------------(d) 方块状的气泡图--------------------------------------------------
ggplot(mtcars, aes(wt,mpg))+
  geom_point(aes(size=disp,fill=disp),shape=22,colour="black",alpha=0.8)+
  scale_fill_gradient2(low=brewer.pal(7,"Set1")[2],high=brewer.pal(7,"Set1")[1],
                       midpoint = mean(mtcars$disp))+
  scale_size_area(max_size=12)+
  guides(fill = guide_legend((title="Value")),
         size =  guide_legend((title="Value")))+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",color="black")#,
    #legend.position=c(0.9,0.05)
  )
