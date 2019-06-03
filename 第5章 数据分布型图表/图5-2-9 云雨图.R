#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(grid)
library(RColorBrewer)
library(dplyr)
library(SuppDists) #提供rJohnson()函数

# somewhat hackish solution to:
# https://twitter.com/EamonCaddigan/status/646759751242620928
# based mostly on copy/pasting from ggplot2 geom_violin source:
# https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

color<-brewer.pal(7,"Set2")[c(1,2,4,5)]


geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}


GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x)) #利用transform函数为数据框mydata增加数据
            
            newdata <- rbind(plyr::arrange(transform(data, x = xmaxv), -y),plyr::arrange(transform(data, x = xminv), y))
            newdata_Polygon <- rbind(newdata, newdata[1,])
            newdata_Polygon$colour<-NA
            
            newdata_Path <- plyr::arrange(transform(data, x = xmaxv), -y)
            
            ggplot2:::ggname("geom_flat_violin", grobTree(
              GeomPolygon$draw_panel(newdata_Polygon, panel_scales, coord),
              GeomPath$draw_panel(newdata_Path, panel_scales, coord))
            )
            },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

# "%||%" <- getFromNamespace("%||%", "ggplot2")
# "%>%"  <- getFromNamespace("%>%", "magrittr")

set.seed(141079)

# Generate sample data -------------------------------------------------------
#findParams函数参考：https://github.com/hadley/boxplots-paper

findParams <- function(mu, sigma, skew, kurt) {
  value <- .C("JohnsonMomentFitR", as.double(mu), as.double(sigma),
              as.double(skew), as.double(kurt - 3), gamma = double(1),
              delta = double(1), xi = double(1), lambda = double(1),
              type = integer(1), PACKAGE = "SuppDists")
  
  list(gamma = value$gamma, delta = value$delta,
       xi = value$xi, lambda = value$lambda,
       type = c("SN", "SL", "SU", "SB")[value$type])
}

# 均值为3，标准差为1的正态分布
n <- rnorm(100,3,1)
# Johnson分布的偏斜度2.2和峰度13
s <- rJohnson(100, findParams(3, 1, 2., 13.1))
# Johnson分布的偏斜度0和峰度20）
k <- rJohnson(100, findParams(3, 1, 2.2, 20))
# 两个峰的均值μ1，μ2分别为1.89和3.79，σ1 = σ2 =0.31
mm <- rnorm(100, rep(c(2, 4), each = 50) * sqrt(0.9), sqrt(0.1))

mydata <- data.frame(
  Class = factor(rep(c("n", "s", "k", "mm"), each = 100),
                 c("n", "s", "k", "mm")),
  Value = c(n, s, k, mm)+3
)
#-------------------------------------------------------------

colnames(mydata)<-c("Class", "Value")

d <- group_by(mydata, Class) %>%
  summarize(mean = mean(Value),
            sd = sd(Value))

ggplot(mydata, aes(Class, Value, fill=Class))  +
  geom_flat_violin(position=position_nudge(x=.2)) +
  geom_jitter(aes(color=Class), width=.1) +
  geom_pointrange(aes(y=mean, ymin=mean-sd, ymax=mean+sd),
                  data=d, size=1, position=position_nudge(x=.2)) +
  coord_flip() + 
  theme_bw() +
  theme( axis.text = element_text(size=13),
         axis.title =  element_text(size=15),
         legend.position="none")

ggplot(mydata, aes(x=Class, y=Value))  +
  geom_flat_violin(aes(fill=Class),position=position_nudge(x=.25),color="black") +
  geom_jitter(aes(color=Class), width=0.1) +
  geom_boxplot(width=.1,position=position_nudge(x=0.25),fill="white",size=0.5) +
  coord_flip() + 
  theme_bw() +
  theme( axis.text = element_text(size=13),
         axis.title =  element_text(size=15),
         legend.position="none")
