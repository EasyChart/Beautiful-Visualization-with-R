#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)
library(ggalt) # ggalt 的下载语句：devtools::install_github("hrbrmstr/ggalt")
library(reshape2)
colormap <- colorRampPalette(rev(brewer.pal(11,'RdYlBu')))(15)

df<-as.data.frame(matrix(cumsum(rnorm(250 * 15)), ncol = 15))
colnames(df) <- paste("series", LETTERS[1:15])
df$x<-rownames(df)

dfData<-melt(df,id='x')
ggplot(dfData, aes(x =as.numeric(x), y = value) )+
  geom_horizon(colour=NA,size=0.25,bandwidth=10)+
  facet_wrap(~variable, ncol = 1,strip.position = "left")+
  scale_fill_manual(values=colormap)+
  xlab('Time') + 
  ylab('') + 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.y = element_text(hjust=0, angle=180,size=10),
        axis.text.y=element_blank(),
        panel.grid = element_blank(),
        panel.spacing.y=unit(-0.05, "lines"),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y =element_blank(),
        axis.ticks.y = element_blank())
