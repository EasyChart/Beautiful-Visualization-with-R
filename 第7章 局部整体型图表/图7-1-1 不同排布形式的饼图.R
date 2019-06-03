#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts


library(RColorBrewer)  
library(dplyr)
library(graphics)

#-------------------------图7-1-1 饼图(a)------------------------------------------
df <- data.frame(value = c(24.20,30.90,12.50,12.30,8.10,12.10), 
                 group = c('LVS','SJM','MCE','Galaxy','MGM','Wynn'))
df <-arrange(df,value)

labs <- paste0(df$group," \n(", round(df$value/sum(df$value)*100,2), "%)")

pie(df$value,labels=labs, init.angle=90,col =  brewer.pal(nrow(df), "Reds"),
    border="black")



#-------------------------图7-1-1 饼图(b)------------------------------------------
df <- data.frame(value = c(24.20,75.90,12.50,12.30,8.10,12.10), 
                 group = c('LVS','SJM','MCE','Galaxy','MGM','Wynn'))
df <-arrange(df,desc(value))
df$color<-rev(brewer.pal(nrow(df), "Blues"))#Oranges

df<-df[c(2:nrow(df),1),]
labs <- paste0(df$group," \n(", round(df$value/sum(df$value)*100,2), "%)")

pie(df$value,labels=labs, init.angle=90,col = df$color,
    border="black")

n<-6
gg_color_hue <- function(n) {hues = seq(15, 375, length = n + 1); hcl(h = hues, l = 65, c = 100)[1:n]} 
pie(df$value,labels=labs, init.angle=90,col =gg_color_hue(n),
    border="black")

pie(df$value,labels=labs, init.angle=90,col ="#3182BD",
    border="black")

#-------------------------图7-1-1 饼图(c)------------------------------------------
df <- data.frame(value = c(24.20,75.90,12.50,12.30,8.10,12.10), 
                 group = c('LVS','SJM','MCE','Galaxy','MGM','Wynn'))
df <-arrange(df,value)

labs <- paste0(df$group," \n(", round(df$value/sum(df$value)*100,2), "%)")

pie(df$value,labels=labs, init.angle=90,col =  brewer.pal(nrow(df), "Reds"),
    border="black")


#-------------------------图7-1-1 饼图(d)------------------------------------------
df <- data.frame(value = c(24.20,30.90,12.50,12.30,8.10,12.10), 
                 group = c('LVS','SJM','MCE','Galaxy','MGM','Wynn'))
df <-arrange(df,desc(value))
df$color<-rev(brewer.pal(nrow(df), "Oranges"))

df<-df[c(2:nrow(df),1),]

labs <- paste0(df$group," \n(", round(df$value/sum(df$value)*100,2), "%)")

pie(df$value,labels=labs, init.angle=90,col = df$color,
    border="black")

