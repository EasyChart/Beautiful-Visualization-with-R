#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
library(ggstance) #devtools::install_github("lionel-/ggstance")

color<-brewer.pal(7,"Set2")[c(1,2,4,5)]

set.seed(141079)
data <- data.frame(BAI2013 = rnorm(300), class = rep(letters[1:3], 100),
                   treatment = rep(c("elevated","ambient"),150))
data<-transform(data,dist_cat_n=as.numeric(class), scat_adj=ifelse(treatment == "ambient",-0.2,0.2))

#--------------------------------------------------图5-2-13 水平显示的箱型图(a)-----------------------------------------------
ggplot(data, aes(class,BAI2013))+
  geom_boxplot(aes(fill=factor(treatment)),
               size=0.5,outlier.size = 1,
               position = position_dodge(0.8)) +  
  guides(fill=guide_legend(title="treatment"))+
  theme_minimal()+
  coord_flip()+
  theme(axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=11,face="plain",color="black"),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid.minor=element_blank(),
        legend.position="right",
        legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black"))

#----------------------------------------------------图5-2-13 水平显示的箱型图(b)-----------------------------------------------
ggplot(data, aes(BAI2013,class))+
  geom_boxploth(aes(fill=factor(treatment)),
               size=0.5,outlier.size = 1,
               position =position_dodgev(0.8)) +  
  guides(fill=guide_legend(title="treatment"))+
  theme_minimal()+
  theme(axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=11,face="plain",color="black"),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid.minor=element_blank(),
        legend.position="right",
        legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black"))




