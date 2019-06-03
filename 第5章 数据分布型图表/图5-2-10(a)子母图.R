#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(ggpubr)

theme<-theme_minimal()+theme(
  axis.title=element_text(size=14,face="plain",color="black"),
  axis.text = element_text(size=12,face="plain",color="black"),
  legend.text= element_text(size=12,face="plain",color="black"),
  legend.title=element_text(size=12,face="plain",color="black"),
  legend.background=element_rect(fill=NA,colour=NA)
)

ggscatterhist(
  iris, x = "Sepal.Length", y = "Sepal.Width",  #iris
  shape=21,color ="black",fill= "Species", size =3.5, alpha = 1,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.plot =  "boxplot",
  margin.params = list(fill = "Species", color = "black", size = 0.2),
  legend = c(0.82,0.15),
  ggtheme = theme)
