#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)  
library(RColorBrewer)  
library(reshape2) 

#------------------------------------------(a)热力图 --------------------------------------------------------
data("mtcars")
mat <- round(cor(mtcars), 1)
mydata <- melt(mat)  
colnames(mydata)<-c("Var1","Var2","value")
ggplot(mydata, aes(x = Var1, y = Var2, fill = value,label=value)) +  
  geom_tile(colour="black") +
  geom_text(size=3,colour="white")+
  coord_equal()+
  scale_fill_gradientn(colours=c(brewer.pal(7,"Set1")[2],"white",brewer.pal(7,"Set1")[1]),na.value=NA)+
  theme(panel.background=element_rect(fill="white",colour=NA),
        panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        text=element_text(size=15),
        plot.title=element_text(size=15,family="myfont",hjust=.5)
  )


#------------------------------------------(b) 气泡图 --------------------------------------------------------

mydata$AbsValue<-abs(mydata$value)


ggplot(mydata, aes(x= Var1 , y=Var2)) +
  geom_point(aes(size=AbsValue,fill = value), shape=21, colour="black") +
  scale_fill_gradientn(colours=c(brewer.pal(7,"Set1")[2],"white",brewer.pal(7,"Set1")[1]),na.value=NA)+
  scale_size_area(max_size=12, guide=FALSE) +
  theme(
    text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=13,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.position="right"
  )

#------------------------------------------(d) 椭圆图-------------------------------------------------------
library(corrplot)
library(matlab)

color<-colorRampPalette(c(brewer.pal(7,"Set1")[2],"white",brewer.pal(7,"Set1")[1]))(100)

corrplot(mat, method="ellipse",order ="alphabet",pch.col = "black",col=color)

#------------------------------------------ (e)气泡标签图-------------------------------------------------------
corrplot.mixed(mat,order ="alphabet",pch.col = "black",bg = "grey80", lower.col = color, upper.col = color)
