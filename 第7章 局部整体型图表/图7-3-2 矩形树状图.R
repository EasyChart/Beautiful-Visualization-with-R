#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

#------------------------------------------------图7-3-2 树状图(a)----------------------------------------------------------------------
library(treemap)
library(RColorBrewer)
proglangs <- read.csv("Treemap_Data.csv ")
treemap(proglangs,
        index=c("parent", "id"),
        vSize="value",
        vColor="parent",
        fontcolor.labels="black",
        title="",
        palette="Set1")

#-------------------------------------------------图7-3-2 树状图(b)--------------------------------------------------------------------------------

library(ggplot2) 
library(treemapify) #devtools::install_github("wilkox/treemapify")
proglangs <- read.csv("Treemap_Data.csv")

treeMapCoordinates <- treemapify(proglangs,
                                 area = "value",
                                 subgroup = "parent")


Class_Label1<-aggregate(cbind(xmin,ymin)~parent,treeMapCoordinates,min)
Class_Label2<-aggregate(cbind(xmax,ymax)~parent,treeMapCoordinates,max)
Class_Label<-cbind(Class_Label1,Class_Label2)

treeMapCoordinates$Area<-(treeMapCoordinates$xmax-treeMapCoordinates$xmin)*(treeMapCoordinates$ymax-treeMapCoordinates$ymin)
treeMapCoordinates$label<-treeMapCoordinates$id
treeMapCoordinates$label[treeMapCoordinates$Area<=0.005]<-""

ggplot(treeMapCoordinates)+
  geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=parent),colour="black")+
  geom_text(aes(x=(xmax+xmin)/2,y=ymin+(ymax-ymin)/3,label=label,size=Area))+
  scale_size(range=c(2.5,4))+
  geom_label(aes(x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=parent),data=Class_Label,size=5,fill="white",alpha=0.5)+
  theme_void()+
  theme(legend.position = "none")
  
#print(treeMapPlot)
