#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts

#Reference：https://github.com/hadley/boxplots-paper

# #--------------------------------------------------数据的准备与导入------------------------------------------------------------------
library(RColorBrewer)
library(ggplot2)

mydata<-read.csv("Norm.csv",stringsAsFactors=FALSE,header=TRUE) #以3为均值，1为标准差的正态分布

mydata$Class<-rep("Class",nrow(mydata))
colnames(mydata)<-c("Value","Class")

color<-brewer.pal(7,"Set2")[c(1,2,4,5)]

#----------------------------------------------------(a)散点抖动图-------------------------------------------------------------
p <- ggplot(mydata, aes(Class, Value))+
  geom_jitter(fill =color[4],position = position_jitter(0.2),shape=21, size = 3)+
  scale_y_continuous(breaks=seq(0,6,1))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        axis.title.x = element_blank(),
        legend.position="none"
  )
p

#-----------------------------------------------------(b) 蜂巢图-----------------------------------------------------------------
library(beeswarm)

class<-mydata$Class
value<-mydata$Value
beeswarm<-beeswarm(value~class, data = mydata,method = 'swarm')[, c(1, 2, 6)]

colnames(beeswarm) <- c("x", "y","Class")

ggplot(beeswarm, aes(x,y)) +
  geom_point(fill = color[4],shape=21,colour="black",size=3.5)+
  scale_fill_manual(values= c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+ 
  scale_y_continuous(breaks=seq(0,6,1))+
  xlab("Class")+
  ylab("Value")+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )


#----------------------------------------------(c)点状图--------------------------------------------------------------------------------
p <- ggplot(mydata, aes(Class, Value))+
  geom_dotplot(fill =color[4],binaxis='y', stackdir='center', dotsize = 0.8)+
  scale_y_continuous(breaks=seq(0,6,1))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p

#----------------------------------------------(d)统计直方图--------------------------------------------------------------------

ggplot(mydata, aes(Value,  fill=Value))+ 
  geom_histogram(alpha=1,fill=color[4],colour="black",size=0.25)+#binwidth = 1,
  coord_flip()+
  theme_classic()+
  scale_x_continuous(breaks=seq(0,6,1))+
  theme(
    panel.background  = element_rect(color="black"),
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.position=c(0.8,0.8),
    legend.background = element_blank()
  )
#----------------------------------------------(e)核密度估计图--------------------------------------------------------------------
ggplot(mydata, aes(Value,  fill=Value))+ 
  geom_density(alpha=1,colour="black",size=0.25,fill=color[4])+
  coord_flip()+
  theme_classic()+
  scale_x_continuous(breaks=seq(0,6,1))+
  theme(
    panel.background  = element_rect(color="black"),
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.position=c(0.8,0.8),
    legend.background = element_blank()
  )



#-------------------------------------------(f)带误差线的散点图-------------------------------------------------
p <- ggplot(mydata, aes(Class, Value))+
  geom_dotplot(fill="white",binaxis='y', stackdir='center', dotsize = 0.8)+
  stat_summary(fill = color[4],fun.data="mean_sdl", fun.args = list(mult=1),
               geom="pointrange", color = "black",size =2 ,shape=21)+
  
  scale_y_continuous(breaks=seq(0,6,1))+
  ylab("Value")+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p

#-----------------------------------------------(g)带误差线的柱形图----------------------------------------------------

p <- ggplot(mydata, aes(Class, Value))+ 
  stat_summary(fill =color[4],fun.y=mean, geom='bar',colour="black",width=.7,size=0.5) +
  stat_summary(fun.data = mean_sdl, geom='errorbar', color='black',width=.2,size=0.5) + 
  geom_jitter(fill ="white",position = position_jitter(0.2),shape=21, size = 2,alpha=0.9)+
  scale_y_continuous(breaks=seq(0,6,1))+
  ylab("Value")+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p

#------------------------------------------------(h) 梯度图--------------------------------------------------
library(denstrip)
#pdf("images/four-denstrip2.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
plot(c(0.5, 1.5), range(mydata$Value), type = "n", axes = F, xlab = "Class", ylab = "Value")
rect(-10, -10, 10, 10, col = "white")
denstrip(mydata$Value, at = 1,mticks=mean(mydata$Value), hor = F, width = 0.75, bw = 0.2, colmax=brewer.pal(7,"Set2")[c(5)],
         colmin="white",mlen=1.1,mcol="black")
box()
axis(2)
axis(1, at = 1, labels = "Class")


#------------------------------------------------(i) 箱型图--------------------------------------------------
p <- ggplot(mydata, aes(Class, Value))+ 
  geom_boxplot(fill =color[4],notch = FALSE) +
  theme_classic()+
  scale_y_continuous(breaks=seq(0,6,1))+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p

#------------------------------------------------(j) 带凹槽的箱型图--------------------------------------------------
p <- ggplot(mydata, aes(Class, Value))+ 
  geom_boxplot(fill =color[4],notch = TRUE) +
  theme_classic()+
  scale_y_continuous(breaks=seq(0,6,1))+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p

#---------------------------------------------------------(k)瓶状图----------------------------------------------------
source("boxplots-vase.r")
par(mar = c(2.1, 2.1, .1, .1))
vase(split(mydata$Value, mydata$Class),  bw = 0.15)
axis(side = 2)
xlab("Class")


#------------------------------------------------------ (l)豆状图----------------------------------------------------
library(beanplot)
par(mar = c(2.1, 2.1, 2.1, 2.1))
beanplot(Value ~Class, data = mydata,col=color[4],xlab ="Class",ylab ="value")#,ylim =c(-3,3))


#------------------------------------------------------(m)小提琴图------------------------------------------------

p <- ggplot(mydata, aes(Class, Value))+ 
  geom_violin(fill =color[4],trim = FALSE)+
  geom_boxplot(width = 0.2)+
  scale_y_continuous(breaks=seq(0,6,1))+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p

#----------------------------------------------------(n)海盗图--------------------------------------------------------------------
library(yarrr)
head(pirates)
pirateplot(formula =  mydata$Value~mydata$Class, data =mydata, 
           theme.o = 2,
           xlab = "", ylab = "Value", main = "",
           # Choose your color palette, or give common color vector
           pal = color[1],#"google",
           #gl.col = color[4],
           # Set transparency of the elements:
           #bean.b.col="black",
           bean.f.col=color[4],
           bar.b.col="black",
           line.o = 0.9,
           bar.o = .4,
           bean.o = .1,
           point.o = .9,
           # Shape of point
           #point.pch = 2,
           #Background color
           #back.col = "white",
           ylim=c(0,6),
           gl.col = "white", # gridlines
           gl.lwd = c(.5, 0)
)

