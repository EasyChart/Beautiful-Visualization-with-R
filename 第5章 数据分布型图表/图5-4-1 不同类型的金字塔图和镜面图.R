#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
#-----------------------------------------------(a1)-------------------------------------------
df<-read.csv("Population_Pyramid_Data.csv",header=TRUE)
df[df$gender == "female",]$pop<--df[df$gender == "female",]$pop
df$age<-factor(df$age,levels=df$age[seq(1,nrow(df)/2,1)])

ggplot(data = df, aes(x =age , y = pop, fill = gender)) +
  geom_bar(stat = "identity",position = "identity",color="black",size=0.25) +
  
  scale_y_continuous(labels = abs, limits = c(-400, 400), breaks = seq(-400, 400, 100)) +
 
  coord_flip() +
  theme_light()+
  theme(
    #axis.text.x = element_text(angle=60, hjust=1),
    panel.grid.minor=element_blank(),
    #text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.text=element_text(size=12,face="plain",color="black"),
    legend.background=element_blank(),
    legend.position = c(0.9,0.88)
  )
#------------------------------------(a2)------------------------------------


ggplot(data = df, aes(x =age , y = pop, fill = gender)) +
  geom_bar(stat = "identity",position = "identity",color="black",size=0.25) +
  
  scale_y_continuous(labels = abs, limits = c(-400, 400), breaks = seq(-400, 400, 100)) +
  theme_light()+
  theme(
    axis.text.x = element_text(angle=60, hjust=1),
    panel.grid.minor=element_blank(),
    #text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.text=element_text(size=12,face="plain",color="black"),
    legend.background=element_blank(),
    legend.position = c(0.9,0.88)
  )


#-------------------------------------------(b1)--------------------------------------------------------
#df<-read.csv("Population_Pyramid_Data.csv",header=TRUE)

#df[df$gender == "female",]$pop<--df[df$gender == "female",]$pop
df$age_x<-rep(seq(0, 100,5),2)


ggplot(data = df, aes(x =age_x , y = pop, fill = gender)) +

  geom_area(stat = "identity", position = "identity",color="black",size=0.25) +

  scale_fill_manual(values=c("#36BED9","#FBAD01"))+
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-400, 400), breaks = seq(-400, 400, 100)) +
  scale_x_continuous(breaks = seq(0, 100, 5),labels=df$age[seq(1,nrow(df)/2,1)])+
  theme_light()+
  theme(
    panel.grid.minor=element_blank(),
    #text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.text=element_text(size=12,face="plain",color="black"),
    legend.background=element_blank(),
    legend.position = c(0.9,0.88)
  )


#-------------------------------------------(b2)------------------------------------------------------------------

ggplot(data = df, aes(x =age_x , y = pop, fill = gender)) +
  
  geom_area(stat = "identity", position = "identity",color="black",size=0.25) +
  
  scale_fill_manual(values=c("#36BED9","#FBAD01"))+
  #coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-400, 400), breaks = seq(-400, 400, 100)) +
  scale_x_continuous(breaks = seq(0, 100, 5),labels=df$age[seq(1,nrow(df)/2,1)])+
  theme_light()+
  theme(
    axis.text.x = element_text(angle=60, hjust=1),
    panel.grid.minor=element_blank(),
    #text=element_text(size=15,face="plain",color="black"),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.text=element_text(size=12,face="plain",color="black"),
    legend.background=element_blank(),
    legend.position = c(0.9,0.88)
  )
