#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(GGally)
library(RColorBrewer) 

#library(devtools)
#install_github("ggobi/ggally")
#-------------------------------图7-3-1 矩阵散点图(a)单数据系列 ----------------------------------------------

lowerFn <- function(data, mapping, method = "loess", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(size=1)+#colour = "blue") +
    geom_smooth(method = method, color = "red", ...)+
    theme(panel.background = element_rect(fill = "white", colour = "grey20"))
  p
}

diagFn <- function(data, mapping, method = "loess", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_histogram(colour = "black",size=0.1)+#) 
    #geom_smooth(method = method, color = "red", ...)+
    theme(panel.background = element_rect(fill = "white", colour = "grey20"))
  p
}

ggpairs(df, 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap(diagFn)),#"barDiag", colour = "black")),
  upper = list(continuous = wrap("cor", size = 4,color="black", alignPercent = 0.9)))+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect( colour = "black", fill = NA,size=0.25),
                    axis.title=element_text(size=8,face="plain",color="grey30"),
                    axis.text = element_text(size=8,face="plain",color="grey30"),
                    strip.background = element_blank())


#---------------------------------图7-3-1 矩阵散点图(b)多数据系列 -------------------------

library(wesanderson)
ggpairs_theme <- theme_bw()+theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect( colour = "black", fill = NA,size=0.25),
                axis.title=element_text(size=8,face="plain",color="grey30"),
                axis.text = element_text(size=8,face="plain",color="grey30"),
                strip.background = element_blank())


ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1"))+
  scale_color_manual(values=wes_palette(n=3, name="Darjeeling1"))
#ggplot<-function(...) ggplot2::ggplot(...) + scale_colour_brewer(palette="Set1")
unlockBinding("ggplot",parent.env(asNamespace("GGally")))
assign("ggplot",ggplot,parent.env(asNamespace("GGally")))

ggpairs(iris, columns =1:4, mapping = ggplot2::aes(fill = Species,colour=Species),
  lower=list(continuous = wrap("points",size=1,shape=21)),#,colour="black"
  diag = list(continuous = wrap("densityDiag",alpha=0.5,colour="black",size=0.25)),
  upper= list(continuous = wrap("cor",size = 3, alignPercent = 0.9)))+
  ggpairs_theme

