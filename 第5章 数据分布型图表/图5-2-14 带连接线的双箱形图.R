
#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggplot2)

#---------------------------------------------Method1：图5-2-14 带连接线的双箱型图--------------------------------------------------------
library(ggpubr) 

set.seed(141079)
data <- data.frame(BAI2013 = rnorm(60),
                   class = rep(rep(letters[1:3], each=10),2),
                   treatment = rep(c("elevated","ambient"),each=30),
                   index=rep(seq(1,30),2)) 

palette<-c(brewer.pal(7,"Set2")[c(1,2,4,5)])

ggpaired(data, x = "treatment", y = "BAI2013",
         fill = "treatment", palette = palette, 
         line.color = "grey50", line.size = 0.15, point.size = 1.5,width=0.6,
         facet.by = "class", short.panel.labs = FALSE)+
  stat_compare_means(paired = TRUE)+
  theme_minimal()+
  theme(strip.background = element_rect(fill="grey90"),
        strip.text = element_text(size=13,face="plain",color="black"),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=11,face="plain",color="black"),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid=element_blank(),
        legend.position="none",
        legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black"))


#-------------------------------------------Method2：图5-2-14 带连接线的双箱型图--------------------------------------------------
library(RColorBrewer)
library(reshape2)
library(ggforce)
library(dplyr)


set.seed(141079)
df_point <- data.frame(BAI2013 = rnorm(60),
                   class = rep(rep(letters[1:3], each=10),2),
                   treatment = rep(c("elevated","ambient"),each=30),
                   index=rep(seq(1,30),2)) 


type<-as.character(unique(df_point$class))

df_bezier<-data.frame(matrix(ncol = 4, nrow = 0))
colnames(df_bezier)<-c("index","treatment","class","value")

for (i in 1:length(type)){
  data0<-df_point[df_point$class==type[i],]
  
  data1<-split(data0,data0$treatment)
  
  data2<-data.frame(ambient=data1$ambient[,1],
                    elevated=data1$elevated[,1],
                    index=data1$ambient[,4])
  
  colnames(data2)<-c(1,2,"index")
  data2$'1.3'<-data2$'1'
  data2$'1.7'<-data2$'2'
  
  data3<-melt(data2,id="index",variable.name ="treatment")
  data3$treatment<-as.numeric((as.character(data3$treatment)))
  
  data4<-arrange(data3,index,treatment) 
  data4$class<-type[i]
  
  df_bezier<-rbind(df_bezier,data4)
  
}

  
ggplot()+
    geom_boxplot(data=df_point,aes(x = factor(treatment), y = BAI2013,fill=factor(treatment)),
                 width=0.35,position = position_dodge(0),size=0.5,outlier.size = 0) + 
    geom_point(data=df_point,aes(x = factor(treatment), y = BAI2013,fill=factor(treatment)),
               shape=21,colour="black",size=2)+
    #geom_line(data=df_point,aes(x = factor(treatment), y = BAI2013,group=index), 
    #           size=0.25,colour="grey20")+
    geom_bezier(data=df_bezier,aes(x= treatment, y = value, group = index,linetype = 'cubic'),
                size=0.25,colour="grey20") +
  
    scale_fill_manual(values=brewer.pal(7,"Set2")[c(5,2)])+
  
    facet_grid(.~class)+
    
    labs(x="treatment",y="Value")+
    theme_minimal()+
    theme(strip.background = element_rect(fill="grey90"),
        strip.text = element_text(size=15,face="plain",color="black"),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=11,face="plain",color="black"),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid.minor=element_blank(),
        legend.position="none",
        legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black"))

