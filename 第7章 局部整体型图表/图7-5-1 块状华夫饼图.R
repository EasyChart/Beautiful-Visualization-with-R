
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#------------------------------------------------图7-5-1 块状华夫饼图(a)-------------------------------------------------------
library(ggplot2)
library(RColorBrewer)  

library(reshape2)
nrows <- 10
categ_table <- round(table(mpg$class ) * ((nrows*nrows)/(length(mpg$class))))
sort_table<-sort(categ_table,index.return=TRUE,decreasing = FALSE)
Order<-sort(as.data.frame(categ_table)$Freq,index.return=TRUE,decreasing = FALSE)

df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category<-factor(rep(names(sort_table),sort_table), levels=names(sort_table))


Color<-brewer.pal(length(sort_table), "Set2")

ggplot(df, aes(x = y, y = x, fill = category)) + 
  geom_tile(color = "white", size = 0.25) +
  #geom_point(color = "black",shape=21,size=6) + 
  coord_fixed(ratio = 1)+
  scale_x_continuous(trans = 'reverse') +#expand = c(0, 0),
  scale_y_continuous(trans = 'reverse') +#expand = c(0, 0),
  scale_fill_manual(name = "Category", 
                    #labels = names(sort_table),
                    values = Color)+
  theme(#panel.border = element_rect(fill=NA,size = 2),
    panel.background  = element_blank(),
    plot.title = element_text(size = rel(1.2)),
    #axis.text = element_blank(),
    #axis.title = element_blank(),
    #axis.ticks = element_blank(),
    # legend.title = element_blank(),
    legend.position = "right")

#------------------------------------------------------------------------------------------
library(ggforce)
ggplot(df, aes(x0 = y, y0 = x, fill = category,r=0.5)) + 
  geom_circle(color = "black", size = 0.25) +
  #geom_point(color = "black",shape=21,size=6) + 
  coord_fixed(ratio = 1)+
  scale_x_continuous(trans = 'reverse') +#expand = c(0, 0),
  scale_y_continuous(trans = 'reverse') +#expand = c(0, 0),
  scale_fill_manual(name = "Category", 
                    #labels = names(sort_table),
                    values = Color)+
  theme(#panel.border = element_rect(fill=NA,size = 2),
    panel.background  = element_blank(),
    plot.title = element_text(size = rel(1.2)),
    #axis.text = element_blank(),
    #axis.title = element_blank(),
    #axis.ticks = element_blank(),
    # legend.title = element_blank(),
    legend.position = "right")
#------------------------------------------------图7-5-1 块状华夫饼图(b)------------------------------------------
library(dplyr)
nrows <- 10
ndeep <- 10
unit<-100
df <- expand.grid(y = 1:nrows, x = 1:nrows)

categ_table <- as.data.frame(table(mpg$class) * (nrows*nrows))
colnames(categ_table)<-c("names","vals")
categ_table<-arrange(categ_table,desc(vals))
categ_table$vals<-categ_table$vals /unit


tb4waffles <- expand.grid(y = 1:ndeep,x = seq_len(ceiling(sum(categ_table$vals) / ndeep)))
regionvec <- as.character(rep(categ_table$names, categ_table$vals))
tb4waffles<-tb4waffles[1:length(regionvec),]


tb4waffles$names <- factor(regionvec,levels=categ_table$names)

Color<-brewer.pal(nrow(categ_table), "Set2")


ggplot(tb4waffles, aes(x = x, y = y, fill = names)) + 
  #geom_tile(color = "white") + # 
  geom_point(color = "black",shape=21,size=5) + # 
  scale_fill_manual(name = "Category", 
                    values = Color)+
  xlab("1 square = 100")+
  ylab("")+
  coord_fixed(ratio = 1)+
  theme(#panel.border = element_rect(fill=NA,size = 2),
         panel.background  = element_blank(),
        plot.title = element_text(size = rel(1.2)),
        #axis.text = element_blank(),
        #axis.title = element_blank(),
        #axis.ticks = element_blank(),
        # legend.title = element_blank(),
        legend.position = "right")

#------------------------------------------------------------------------------------------
library(ggforce)

ggplot(tb4waffles, aes(x0 = x, y0 = y, fill = names,r=0.5)) + 
  #geom_tile(color = "white") + # 
  geom_circle(color = "black",size=0.25) + # 
  scale_fill_manual(name = "Category", 
                    values = Color)+
  xlab("1 square = 100")+
  ylab("")+
  coord_fixed(ratio = 1)+
  theme(#panel.border = element_rect(fill=NA,size = 2),
    panel.background  = element_blank(),
    plot.title = element_text(size = rel(1.2)),
    #axis.text = element_blank(),
    #axis.title = element_blank(),
    #axis.ticks = element_blank(),
    # legend.title = element_blank(),
    legend.position = "right")