#http://datavizcatalogue.com/blog/box-plot-variations/
library(ggplot2)
library(grid)
library(RColorBrewer)

#setwd("C:/Users/Jie Zhang/Desktop/R语言_Code/R-数据分布/")
color<-brewer.pal(7,"Set2")[c(1,2,4,5)]

#mydata<-read.csv("boxplot.csv",stringsAsFactors=FALSE) 
#------------------------------------------------------------
library(Hmisc)
library(SuppDists)
library(UsingR)
library(beanplot)
library(hdrcde)
library(denstrip)
library(SuppDists)

set.seed(141079)

# Generate sample data -------------------------------------------------------

findParams <- function(mu, sigma, skew, kurt) {
  value <- .C("JohnsonMomentFitR", as.double(mu), as.double(sigma),
              as.double(skew), as.double(kurt - 3), gamma = double(1),
              delta = double(1), xi = double(1), lambda = double(1),
              type = integer(1), PACKAGE = "SuppDists")
  
  list(gamma = value$gamma, delta = value$delta,
       xi = value$xi, lambda = value$lambda,
       type = c("SN", "SL", "SU", "SB")[value$type])
}

# normal
n <- rnorm(100)
# right-skew
s <- rJohnson(100, findParams(0, 1, 2.2, 13.1))
# leptikurtic
k <- rJohnson(100, findParams(0, 1, 0, 20))
# mixture
mm <- rnorm(100, rep(c(-1, 1), each = 50) * sqrt(0.9), sqrt(0.1))

mydata <- data.frame(
  Class = factor(rep(c("n", "s", "k", "mm"), each = 100),
                 c("n", "s", "k", "mm")),
  Value = c(n, s, k, mm)
)
#-------------------------------------------------------------

#mydata<-read.csv("Plotbox_Data.csv",stringsAsFactors=FALSE) 
#mydata<-melt(mydata)
colnames(mydata)<-c("Class", "Value")
p <- ggplot(mydata, aes(Class, Value))+ 
  geom_violin(aes(fill = Class),trim = FALSE)+
  geom_boxplot(width = 0.2)+
  # stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
  #              geom="pointrange", color = "black")+
  scale_fill_manual(values=color)+
  #scale_y_continuous(breaks=seq(0,0.5,0.1))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p
# #-------------------------------------------------------beeswarm---------------------------------------------------------------------------------
# # library(vioplot)
# # A1<-mydata[mydata$Class=="A1",1]
# # B1<-mydata[mydata$Class=="B1",1]
# # C1<-mydata[mydata$Class=="C1",1]
# # D1<-mydata[mydata$Class=="D1",1]
# # vioplot(A1,B1,C1,D1,
# #         col =c(brewer.pal(7,"Set2")[c(1)],brewer.pal(7,"Set2")[c(2)],brewer.pal(7,"Set2")[c(4)],brewer.pal(7,"Set2")[c(5)]),
# #         names=c("A1","B1","C1","D1"))
# #-------------------------------
#http://www.cbs.dtu.dk/~eklund/beeswarm/
#http://www.cnblogs.com/nxld/p/6065574.html 
library(beeswarm)
#biocLite(c("beeswarm","ggplot2"))
class<-mydata$Class
value<-mydata$Value
beeswarm<-beeswarm(value~class, data = mydata,method = 'swarm')[, c(1, 2, 6)]
        # main = 'Default')
colnames(beeswarm) <- c("x", "y","Class")
ggplot(beeswarm, aes(x,y)) +
  geom_point(aes(fill = Class),shape=21,colour="black",size=2)+
  scale_fill_manual(values= c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+ 
  scale_y_continuous(breaks=seq(0,0.5,0.1))+
  scale_x_continuous(breaks = c(1:4),labels = c("A1", "B1","C1", "D1"))+
  xlab("Class")+
  ylab("Value")+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )

#print(beeswarm.plot)
#----------------------------------------------dotplot----------------------------------------------------------------------------------------
p <- ggplot(mydata, aes(Class, Value))+
  geom_dotplot(aes(fill = Class),binaxis='y', stackdir='center', dotsize = 0.8)+
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1)],brewer.pal(7,"Set2")[c(2)],brewer.pal(7,"Set2")[c(4)],brewer.pal(7,"Set2")[c(5)]))+

  #geom_jitter(position = position_jitter(0.2),shape=17, size = 0)+
  #stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
  #             geom="pointrange", color = "black",size = 2.5)+
  scale_y_continuous(breaks=seq(0,0.5,0.1))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p
#----------------------------------------------------jitter-----------------------------------------------------------------------------------------
p <- ggplot(mydata, aes(Class, Value))+
  #geom_dotplot(aes(fill = Class),binaxis='y', stackdir='center', dotsize = 0.7)+
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1)],brewer.pal(7,"Set2")[c(2)],brewer.pal(7,"Set2")[c(4)],brewer.pal(7,"Set2")[c(5)]))+
  
  geom_jitter(aes(fill = Class),position = position_jitter(0.2),shape=21, size = 2)+
  #stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
  #             geom="pointrange", color = "black",size = 2.5)+
  scale_y_continuous(breaks=seq(0,0.5,0.1))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p

#------------------------------------------------------dotplot + stad line------------------------------
  p <- ggplot(mydata, aes(Class, Value))+
  geom_dotplot(aes(fill = Class),binaxis='y', stackdir='center', dotsize = 0.8)+
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1)],brewer.pal(7,"Set2")[c(2)],brewer.pal(7,"Set2")[c(4)],brewer.pal(7,"Set2")[c(5)]))+
  scale_color_manual(values=c(brewer.pal(7,"Set2")[c(1)],brewer.pal(7,"Set2")[c(2)],brewer.pal(7,"Set2")[c(4)],brewer.pal(7,"Set2")[c(5)]))+
  
  geom_jitter(aes(fill = Class,colour= Class),position = position_jitter(0),shape=4, size = 0)+
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
               geom="pointrange", color = "black",size = 1.2)+
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
               geom="point", color = "white",size = 4)+
  scale_y_continuous(breaks=seq(0,0.5,0.1))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p 
#------------------------------------------------boxplot--------------------------------
p <- ggplot(mydata, aes(Class, Value))
p <-p+ geom_boxplot(aes(fill = Class),notch = TRUE) +
    #geom_dotplot(binaxis = "y", stackdir = "center",dotsize = 0.4)+
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1)],brewer.pal(7,"Set2")[c(2)],brewer.pal(7,"Set2")[c(4)],brewer.pal(7,"Set2")[c(5)]))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p 


#------------------------------------------------boxplot with different width--------------------------------
mydata<-read.csv("boxplot2.csv",stringsAsFactors=FALSE) 

# #Creating data 
# names=c(rep("A", 20) , rep("B", 8) , rep("C", 30), rep("D", 80))
# value=c( sample(2:5, 20 , replace=T) , sample(4:10, 8 , replace=T), 
#          sample(1:7, 30 , replace=T), sample(3:8, 80 , replace=T) )
# data=data.frame(names,value)


# Calculate proportion of each level
proportion<-table(mydata$Class)/nrow(mydata)

#Draw the boxplot, with the width proportionnal to the occurence !
#boxplot(mydata$Value ~ mydata$Class , width=proportion,col=c(brewer.pal(7,"Set2")[c(1,2,4,5)]), 
#        ylim=c(0,0.5),
#        xlab=("Class"),
#        ylab=("Value") )#, col=c("orange" , "seagreen"))

p <- ggplot(mydata, aes(Class, Value))+
  geom_boxplot(aes(fill = Class), varwidth = TRUE) +
  #geom_dotplot(binaxis = "y", stackdir = "center",dotsize = 0.4)+
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1)],brewer.pal(7,"Set2")[c(2)],brewer.pal(7,"Set2")[c(4)],brewer.pal(7,"Set2")[c(5)]))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        #panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        #text=element_text(size=15),
        #plot.title=element_text(size=15,family="myfont",hjust=.5),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )
p 