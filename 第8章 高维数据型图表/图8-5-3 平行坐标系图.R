#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(GGally)

dlarge <-read.csv("Parallel_Coordinates_Data.csv", header=TRUE,check.names=FALSE)


dlarge<-transform(dlarge, Class=ifelse(reading> 523, "Class1", "Class2"))

ggparcoord(data = dlarge, columns = 1:6, mapping=aes(color=Class),#groupColumn = 5, order = "anyClass",
           groupColumn=7,#"cut",
           showPoints = FALSE, boxplot = FALSE,#title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.7)+#,,splineFactor =TRUE
  scale_x_discrete(position = "top")+
  scale_colour_manual(values=c("#90C539","#45BFFC" ))+
  xlab("")+
  theme_minimal()+
  theme(
    #strip.text = element_text(size=13,face="plain",color="black"),
    #text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    panel.grid.major.y=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.x=element_line(size=1),
    legend.text  = element_text(size=11,face="plain",color="black"),
    legend.title =element_blank(),
    #legend.position = "right",
    axis.line.y=element_line(size=1,colour="grey70"),
    axis.ticks.y=element_line(size=1,colour="grey70"),
    legend.position=c(0.82,0.1)
    #legend.background = element_rect(fill=alpha("white",0))
  )
