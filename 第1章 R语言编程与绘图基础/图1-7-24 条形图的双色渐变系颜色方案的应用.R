
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
library(Cairo)
library(showtext)


mydata<- read.csv("Bar_Data.csv",stringsAsFactors=FALSE)

mydata$Team<-as.character(mydata$Team)


mydata<-transform(mydata, label1=ifelse(Difference>=0, Team, NA),
                  label2=ifelse(Difference>0, NA,Team))


mydata$Team <- factor(mydata$Team, levels = mydata$Team[order(mydata$Difference)])

#CairoPDF(file="图1-8-15(a)_条形图.pdf",width=5.28,height=5.47) 
#showtext.begin()
ggplot(data = mydata, aes(x = Team, y = Difference,fill = Difference)) +
  geom_bar(stat = "identity", width = 0.8,colour="black",size=0.25)+
  scale_fill_gradient2(low=brewer.pal(7,"Set1")[2],mid="grey90",high=brewer.pal(7,"Set1")[1],midpoint=0)+
  geom_text(aes(y = 0,     label=label2),size=3,hjust=-0.1)+ #添加负值部分的数据标签
  geom_text(aes(y = -0.001,label=label1),size=3,hjust= 1.1)+ #添加正值部分的数据标签
  coord_flip() +   #坐标轴翻转
  ylim(-5,5)+
  theme_minimal() + #图表主题设定
  theme(
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line(colour = "grey80",size=.25),
        panel.grid.minor.x = element_line(colour = "grey80",size=.25),
        plot.title=element_text(size=15,hjust=.5),
        axis.text.x = element_text(face="plain", color="black",
                                   size=11, angle=0),
        axis.text.y = element_blank(),
        legend.position="right",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))
#showtext.end()
#dev.off()
