
#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts


library(ggplot2)
library(RColorBrewer)
library(scales)

x <- rnorm(250 , mean=10 , sd=1) #(a2) 服从正态分布的原始数据

#x <-sample(1:20, 250, replace = TRUE) #(a1) 服从均匀分布的原始数据

step<-0.2
breaks<- seq(min(x)-step,max(x)+step,step)#"sturges"

hg <- hist(x, breaks = breaks , plot = FALSE) # Make histogram data but do not plot

bins <- length(hg$counts) # How many bin categories are needed?
yvals <- numeric(0)                  # A blank variable to fill in
xvals <- numeric(0) 
for(i in 1:bins) {                  # Start a loop
  yvals <- c(yvals, hg$counts[i]:0)  # Work out the y-values
  xvals <- c(xvals, rep(hg$mids[i], hg$counts[i]+1))  # Work out x-values
}    # End the loop
                                                   # End the loop
dat <- data.frame(xvals, yvals)  # Make data frame of x, y variables
dat <- dat[yvals > 0, ]          # Knock out any zero y-values

colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

#------------------------------------块状tile---------------------------------------
ggplot(dat, aes(x=xvals,y=yvals,fill=yvals))+
  geom_tile(colour="black")+
  scale_fill_gradientn(colours=colormap)+
  ylim (0, max(yvals)*1.3)+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.9,0.75)
  )

#-----------------------------------圆圈point-----------------------------------------
ggplot(dat, aes(x=xvals,y=yvals,fill=yvals))+
  geom_point(colour="black",shape=21,size=4)+
  scale_fill_gradientn(colours=colormap)+
  ylim (0, max(yvals)*1.3)+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.9,0.75)
  )
