#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggtern)
library(grid)
library(RColorBrewer)

#---------------------------------------(a) 三元相散点图-----------------------------------------------------
data(Fragments)
arrangement = list()
for(base in c('ilr')){
  y = ggtern(Fragments,aes(Qm,Qp,M)) +
    theme_showarrows()+
    geom_point(size=3) + 
    ggtitle(sprintf("Basis: %s",base)) +
    limit_tern(.5,1,.5)
  arrangement[[length(arrangement) + 1]] = y
}
grid.arrange(grobs = arrangement,nrow=1)


#----------------------------------(a) -三元相等高线图-----------------------------------------------------
data(Fragments)
arrangement = list()
for(base in c('ilr')){
  x = ggtern(Fragments,aes(Qm,Qp,M)) +
    stat_density_tern(geom='polygon',
                      aes(fill=..level..),
                      base=base,  
                      colour='grey50') + 
    theme_showarrows()+
    geom_point() + 
    ggtitle(sprintf("Basis: %s",base)) +
    scale_fill_gradientn(colours=c(brewer.pal(7,"Set1")[2],"white",brewer.pal(7,"Set1")[1]),na.value=NA)+
    limit_tern(.5,1,.5)
  arrangement[[length(arrangement) + 1]] = x
}
grid.arrange(grobs = arrangement,nrow=1)



