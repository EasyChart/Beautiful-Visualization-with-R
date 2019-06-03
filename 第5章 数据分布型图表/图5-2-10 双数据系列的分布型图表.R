#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
#-------------------------------------------------(a)和(b) 多数据系列的箱型图--------------------------------------------------------
set.seed(141079)
data <- data.frame(BAI2013 = rnorm(300),
                 class = rep(letters[1:3], 100),
                 treatment = rep(c("elevated","ambient"),150)) 


#(a)多数据系列的箱型图
ggplot(data, aes(x = class, y = BAI2013))+
  geom_boxplot(outlier.size = 1, aes(fill=factor(treatment)),
               position = position_dodge(0.8),size=0.5) +  
  guides(fill=guide_legend(title="treatment"))+
  theme_minimal()+
  theme(axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=11,face="plain",color="black"),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid.minor=element_blank(),
        legend.position="right",
        legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black"))

#(b) 带抖动散点的多数据系列箱型图


data<-transform(data,dist_cat_n=as.numeric(class),
                scat_adj=ifelse(treatment == "ambient",-0.2,0.2))

ggplot(data, aes(x =class, y = BAI2013))+
    geom_boxplot(outlier.size = 0, aes(fill=factor(treatment)),
                 position = position_dodge(0.8),size=0.4) + 
    geom_jitter(aes(scat_adj+dist_cat_n, BAI2013,fill = factor(treatment)),
                position=position_jitter(width=0.1,height=0),
                alpha=1,
                shape=21, size = 1.5)+
  guides(fill=guide_legend(title="treatment"))+
  theme_minimal()+
  theme(axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=11,face="plain",color="black"),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid.minor=element_blank(),
        legend.position="right",
        legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black"))


#-------------------------------------------------(c)多数据系列的小提琴图SplitViolin------------------------------------------------
#Reference:    
# https://stackoverflow.com/a/45614547
# https://gist.github.com/Karel-Kroeze/746685f5613e01ba820a31e57f87ec87

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL){
                             # Original function by Jan Gleixner (@jan-glx)
                             # Adjustments by Wouter van der Bijl (@Axeman)
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1,'group']
                             newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
                               quantiles <- create_quantile_segment_frame(data, draw_quantiles, split = TRUE, grp = grp)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           }
)

create_quantile_segment_frame <- function (data, draw_quantiles, split = FALSE, grp = NULL) {
  dens <- cumsum(data$density)/sum(data$density)
  ecdf <- stats::approxfun(dens, data$y)
  ys <- ecdf(draw_quantiles)
  violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
  violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)
  violin.xs <- (stats::approxfun(data$y, data$x))(ys)
  if (grp %% 2 == 0) {
    data.frame(x = ggplot2:::interleave(violin.xs, violin.xmaxvs), 
               y = rep(ys, each = 2), group = rep(ys, each = 2)) 
  } else {
    data.frame(x = ggplot2:::interleave(violin.xminvs, violin.xs), 
               y = rep(ys, each = 2), group = rep(ys, each = 2)) 
  }
}

geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

data<-transform(data,dist_cat_n=as.numeric(class),
                scat_adj=ifelse(treatment == "ambient",-0.15,0.15))
#data$scat_adj[data$treatment == "ambient"] <- -0.15
#data$scat_adj[data$treatment == "elevated"] <- 0.15

ggplot(data, aes(x = class, y = BAI2013,fill=factor(treatment)))+
  geom_split_violin(draw_quantiles = 0.5,trim = FALSE)+
  geom_jitter(aes(scat_adj+dist_cat_n, BAI2013,fill = factor(treatment)),
              position=position_jitter(width=0.1,height=0),
              alpha=1,
              shape=21, size = 1)+
  guides(fill=guide_legend(title="treatment"))+
  theme_minimal()+
  theme(axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=11,face="plain",color="black"),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid.minor=element_blank(),
        legend.position="right",
        legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black"))

#----------------------------------------------------(d)  多数据系列的豆状图-----------------------------
library(beanplot)
par(mai=c(0.5,0.5,0.25,1.2))
beanplot(BAI2013 ~treatment*class, data,col = list("#FF6B5E", "#00C3C2"),
         side = "both",xlab ="Class",ylab ="value")
legend(x=3.7,y=1.5 ,xpd=TRUE,bty="n",c("ambient", "elevated"),
       fill = c("#FF6B5E", "#00C3C2"),title="treatment")

