#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
df <- data.frame(item=rep(LETTERS[1:10], 5), 
                 score=rep(letters[1:5], each=10), 
                 value=rep((1:5), each=10) + rnorm(50, 0, .5))


myAng <-seq(-20,-340,length.out =10)
ggplot(data=df,aes(item,value,fill=score))+
  geom_bar(stat="identity", color="black", position=position_dodge(),width=0.7,size=0.25)+
  coord_polar(theta = "x",start=0) +
  ylim(c(-3,6))+
  scale_fill_brewer(palette="YlGnBu")+
  theme_light()+
  theme( panel.background = element_blank(),
         panel.grid.major = element_line(colour = "grey80",size=.25),
         axis.text.y = element_text(size = 12,colour="black"),
         axis.line.y = element_line(size=0.25),
         axis.text.x=element_text(size = 13,colour="black",angle = myAng))
