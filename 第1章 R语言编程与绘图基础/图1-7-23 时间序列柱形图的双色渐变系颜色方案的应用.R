
#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)

mydata<-read.csv("Column_Data.csv",stringsAsFactors=FALSE)
mydata$Date<-as.Date(mydata$Date)

ggplot(data = mydata, aes(x = Date, y = temperature,fill = temperature)) +
  geom_bar(stat = "identity", width = 2)+
  scale_fill_gradient2("Temperature",low=brewer.pal(7,"Set1")[2],mid="grey90",high=brewer.pal(7,"Set1")[1],midpoint=0)+
  scale_y_continuous(name="Temperature", limits=c(-10, 30))+
  theme(
    panel.background=element_rect(fill="white",colour="black"),
    panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
    panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),

    axis.title=element_text(size=15),
    axis.text.x = element_text(color="black",size=12),
    axis.text.y = element_text(color="black",size=12),
        
    legend.text=element_text(size=10),
    legend.title=element_text(color="black",size=12),
    legend.title.align = 0.5,
    legend.position=c(0.15,0.75))
