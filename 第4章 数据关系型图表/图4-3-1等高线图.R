#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library( directlabels)
library(RColorBrewer)

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

z<-as.matrix(read.table("等高线.txt",header=TRUE))
colnames(z)<-seq(1,ncol(z),by=1)
max_z<-max(z)
min_z<-min(z)
breaks_lines<-seq(min(z),max(z),by=(max_z-min_z)/10)
map<-melt(z)
colnames(map)<-c("Var1","Var2","value")
head(map)
Contour<-ggplot(map,aes(x=Var1,y=Var2,z=value))+
          geom_tile(aes(fill=value))+#根据高度填充
          scale_fill_gradientn(colours=colormap)+
          geom_contour(aes(colour= ..level..),breaks=breaks_lines,color="black")+#
          labs(x="X-Axis",y="Y-Axis",fill="Z-Value")+
         theme(
           axis.title=element_text(size=15,face="plain",color="black"),
           axis.text = element_text(size=13,face="plain",color="black"),
           legend.title=element_text(size=13,face="plain",color="black"),
           legend.text = element_text(size=11,face="plain",color="black"),
           legend.background = element_blank(),
           legend.position =c(0.15,0.2)
  )
Contour

direct.label(Contour, list("bottom.pieces", cex=0.8, #"far.from.others.borders",
                  fontface="plain", fontfamily="serif", colour='black'))

