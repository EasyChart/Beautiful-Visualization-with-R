#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)  
library(reshape2)


df<-read.csv("StreamGraph_Data.csv",header=TRUE)

#------------------------------------------------图6-4-3 时间序列峰峦图(a)-----------------------------------
x<-seq(1:nrow(df))
label<-letters[1:ncol(df)]#colnames(y2)<-
colnames(df)<-c(seq(1,ncol(df),1))

# base plot
dfData2<-as.data.frame(cbind(x,df))

Order<-sort(colSums(dfData2[,2:ncol(dfData2)]),index.return=TRUE,decreasing = TRUE) 
label2<-label[Order$ix]
mydata<-melt(dfData2,id="x")
#levels(mydata$variable)[as.integer( colnames(Order))]
mydata$variable <- factor(mydata$variable, levels = levels(mydata$variable)[Order$ix])

N<-ncol(df)
Step<-1500
mydata$offest<--as.numeric(mydata$variable)*Step# adapt the 0.2 value as you need

mydata$V1_density_offest<-mydata$value+mydata$offest


ggplot(mydata, aes(x, V1_density_offest, color=variable)) + 
  #geom_linerange(aes(x, ymin=offest,ymax=V1_density_offest, color==variable),size =1.5, alpha =1) 
  geom_ribbon(aes(x, ymin=offest,ymax=V1_density_offest, fill=variable),alpha=1,colour=NA)+
  geom_line(aes(group=variable),color="black")+
  scale_y_continuous(breaks=seq(-Step,-Step*N,-Step),labels=label2)+
  #scale_fill_manual(values =COLS)+
  xlab("Time")+
  ylab("Class")+
  theme_classic()+
  theme(
    panel.background=element_rect(fill="white",colour=NA),
    panel.grid.major.x = element_line(colour = "grey80",size=.25),
    panel.grid.major.y = element_line(colour = "grey60",size=.25),
    axis.line = element_blank(),
    text=element_text(size=15),
    plot.title=element_text(size=15,hjust=.5),#family="myfont",
    legend.position="none"
  )

#----------------------------------------------------图6-4-3 时间序列峰峦图(b)----------------------------------
library(RColorBrewer)
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

label<-letters[1:ncol(df)]#colnames(y2)<-
colnames(df)<-c(seq(1,ncol(df),1))

# base plot
dfData2<-as.data.frame(cbind(x,df))

Order<-sort(colSums(dfData2[,2:ncol(dfData2)]),index.return=TRUE,decreasing = TRUE) 
label2<-label[Order$ix]

dfData2<-dfData2[c(1,Order$ix+1)]

mydata<-melt(dfData2,id="x")
colnames(dfData2)<-c("x",c(seq(ncol(y2),1,-1)))


N<-ncol(df)
Step<-1500
mydata$offest<--as.numeric(mydata$variable)*Step# adapt the 0.2 value as you need

mydata$V1_density_offest<-mydata$value+mydata$offest


ggplot(mydata, aes(x, V1_density_offest,group=variable)) + 
  geom_linerange(aes(x, ymin=offest,ymax=V1_density_offest, color=value),size =1.5, alpha =1) +
  scale_color_gradientn(colours=colormap)+
  #geom_ribbon(aes(x, ymin=offest,ymax=V1_density_offest, fill=variable),alpha=1,colour=NA)+
   geom_line(aes(group=variable),color="black")+
   scale_y_continuous(breaks=seq(-Step,-Step*N,-Step),labels=label2)+
  xlab("Time")+
  ylab("Class")+
  theme_classic()+
  theme(
    panel.background=element_rect(fill="white",colour=NA),
    panel.grid.major.x = element_line(colour = "grey80",size=.25),
    panel.grid.major.y = element_line(colour = "grey60",size=.25),
    axis.line = element_blank(),
    text=element_text(size=13),
    plot.title=element_text(size=15,hjust=.5),#family="myfont",
    legend.position="right"
  )

