
#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts


#------------------------------------------------图7-3-2 树状图(a)----------------------------------------------------------------------
library(treemap)
library(RColorBrewer)
proglangs <- read.csv("Treemap_Data.csv ")
treemap(proglangs,
        index=c("parent", "id"),
        vSize="value",
        vColor="parent",
        type="index",
        border.lwds=c(2,0.1),
        fontcolor.labels=c('white',"grey10"),
        title="",
        align.labels = list(c("center", "center"), c("right", "bottom")),
        fontsize.labels=c(12, 9),
        palette="Set1")

#-------------------------------------------------图7-3-2 树状图(b)--------------------------------------------------------------------------------

library(ggplot2) 
library(treemapify) #devtools::install_github("wilkox/treemapify")
proglangs <- read.csv("Treemap_Data.csv")

treeMapCoordinates <- treemapify(proglangs,
                                 area = "value",
                                 fill="parent",
                                 group = "parent",
                                 label='id')


Class_Label1<-aggregate(cbind(xmin,ymin)~group,treeMapCoordinates,min)
Class_Label2<-aggregate(cbind(xmax,ymax)~group,treeMapCoordinates,max)
Class_Label<-cbind(Class_Label1,Class_Label2[c('xmax','ymax')])

treeMapCoordinates$Area<-(treeMapCoordinates$xmax-treeMapCoordinates$xmin)*(treeMapCoordinates$ymax-treeMapCoordinates$ymin)
#treeMapCoordinates$label<-treeMapCoordinates$id
treeMapCoordinates$label[treeMapCoordinates$Area<=50]<-""

ggplot(treeMapCoordinates)+
  geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=group),colour="black")+
  geom_text(aes(x=xmin+(xmax-xmin)/2,y=ymin+(ymax-ymin)/4,label=label,size=Area))+
  scale_size(range=c(2,5))+
  geom_label(aes(x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=group),data=Class_Label,size=5,fill="white",alpha=0.5)+
  theme_void()+
  theme(legend.position = "none")

#print(treeMapPlot)
