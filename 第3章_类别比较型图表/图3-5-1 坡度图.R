
#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(scales)
library(reshape)
#--------------------------------------(a)两年份对比---------------------------------------------------------------

df <- read.csv("Slopecharts_Data1.csv")
colnames(df) <- c("continent", "1952", "1957")
left_label <- paste(df$continent, round(df$`1952`),sep=", ")
right_label <- paste(df$continent, round(df$`1957`),sep=", ")
df$class <- ifelse((df$`1957` - df$`1952`) < 0, "red", "green")

p <- ggplot(df) + 
  geom_segment(aes(x=1, xend=2, y=`1952`, yend=`1957`, col=class), size=.75, show.legend=F) +  #连接线
  geom_vline(xintercept=1, linetype="solid", size=.1) + # 1952年的垂直直线
  geom_vline(xintercept=2, linetype="solid", size=.1) + # 1957年的垂直直线
  geom_point(aes(x=1, y=`1952`), size=3,shape=21,fill="grey80",color="black") + # 1952年的数据点
  geom_point(aes(x=2, y=`1957`), size=3,shape=21,fill="grey80",color="black") + # 1957年的数据点
  scale_color_manual(labels = c("Up", "Down"), values = c("green"="#A6D854","red"="#FC4E07")) +  
  xlim(.5, 2.5) 

# 添加文本信息
p <- p + geom_text(label=left_label, y=df$`1952`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
p <- p + geom_text(label=right_label, y=df$`1957`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
p <- p + geom_text(label="1952", x=1, y=1.02*(max(df$`1952`, df$`1957`)), hjust=1.2, size=5)   
p <- p + geom_text(label="1957", x=2, y=1.02*(max(df$`1952`, df$`1957`)), hjust=-0.1, size=5) 

p<-p+theme_void()
p

#-------------------------------------(b)多年份对比---------------------------------------------------------------------------------
library(ggalt)

df <- read.csv("Slopecharts_Data2.csv")
colnames(df) <- c("continent", 2007:2013)


df2<-melt(df, id="continent")

df2$value<-as.numeric(df2$value)
df2$variable<-as.numeric(df2$variable)

left_label<-paste(df2$continent,  round(df2$value),sep=", ")
right_label<-paste(df2$continent, round(df2$value),sep=", ")

left_point<-df2$value
right_point<-df2$value
class<-df2$variable
  
for (i in 1:nrow(df2))
{
  if (df2$variable[i]!=1)
  {
    left_label[i]<-""
    left_point[i]<-NaN
  }
  if (df2$variable[i]!=7)
  {
    right_label[i]<-""
    right_point[i]<-NaN
  }
  
  if (df[df$continent==df2$continent[i],2]>df[df$continent==df2$continent[i],8])
  {
    class[i]<-"green"
  }
  else
  {
    class[i]<-"red"
  }
  
}

p <- ggplot(df2) + 
  geom_xspline(aes(x=variable, y=value,group=continent, colour=class),size=.75) + 
  geom_vline(xintercept=1, linetype="solid", size=.1) + 
  geom_vline(xintercept=7, linetype="solid", size=.1) +
  geom_point(aes(x=variable, y=left_point), size=3,shape=21,fill="grey80",color="black") + 
  geom_point(aes(x=variable, y=right_point), size=3,shape=21,fill="grey80",color="black") + 
  scale_color_manual(labels = c("Up", "Down"), values = c("green"="#FC4E07",  "red"="#A6D854")) +  
  xlim(-4, 12) 

p <- p + geom_text(label=left_label, y=df2$value, x=rep(1, NROW(df2)), hjust=1.1, size=3.5)
p <- p + geom_text(label=right_label, y=df2$value, x=rep(7, NROW(df2)), hjust=-0.1, size=3.5)
p <- p + geom_text(label="2007", x=1, y=1.02*(max(df2$value)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="2013", x=7, y=1.02*(max(df2$value)), hjust=-0.1, size=5)  # title

p<-p+theme_void()+
  theme(legend.position = "none")
p
