#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)

data(diamonds)
dsmall <- sample_n(diamonds, 500)
mydata <- data.frame(dsmall[1:100,2:6])

-------------------------------------------------------------
for (i in 1:ncol(mydata))
{
  if (sum(as.integer(class(mydata[,i])=="factor")))
  {
    levels(mydata[,i]) <- seq(0,length(levels(mydata[,i]))-1,1)
    mydata[,i]<-as.numeric(mydata[,i])
  }
  temp<-mydata[,i]
  Dmin<-min(temp)
  Dmax<-max(temp)
  mydata[,i]<-(temp-Dmin)/(Dmax-Dmin)
}


mydata<-mydata[order(mydata$cut,decreasing=T),] #按照第4列降序排序

mydata$category<-as.integer(seq(1,nrow(mydata),1))

Meltdata <- melt(mydata, id.vars="category")

ggplot(Meltdata, aes(variable,value,fill=variable)) + 
  geom_bar(stat="identity",colour="black",size=0.25,width=1.0)+
  scale_fill_manual(values=brewer.pal(7,"Set1")[1:5])+
  facet_wrap(~category)+
      #theme_classic()+
      theme(#legend.position = "none",
            panel.background = element_rect(fill = "grey95"),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.line =element_blank(),
            axis.ticks=element_blank(),
            strip.background = element_blank(), 
            strip.text = element_text(color="white"),
            strip.placement = "outside",
            #plot.margin = margin(2, 2, 2, 2, "cm"),
            plot.background = element_rect(
              fill = "white",
              colour = "white",
              size = 1)
            )
