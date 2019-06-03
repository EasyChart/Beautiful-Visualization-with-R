
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(viridis)

df<-read.csv("PloarRange_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)
df$date<-as.Date(df$date)

myAngle <-seq(-20,-340,length.out = 12)

ggplot(df, aes(date,
               ymin = min.temperaturec,
               ymax = max.temperaturec,
               color = mean.temperaturec)) + 
  geom_linerange(size = 1.3, alpha = 0.75) +
  scale_color_viridis("Temperature", option = "D") +
  scale_x_date(labels = date_format("%m"), breaks = date_breaks("month")) + 
  ylim(-10, 35) + 
  coord_polar() + 
  theme_light() +
  theme( panel.background = element_blank(),
         panel.grid.major = element_line(colour = "grey80",size=.25),
         axis.text.y = element_text(size = 12,colour="black"),
         axis.line.y = element_line(size=0.25),
         axis.text.x=element_text(size = 13,colour="black",angle = myAngle))

