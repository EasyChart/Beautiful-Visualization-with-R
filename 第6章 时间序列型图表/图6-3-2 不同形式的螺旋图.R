#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(dplyr)
library(ggplot2)
library(readxl)
library(RColorBrewer)

colormap <- colorRampPalette(brewer.pal(9,'YlGnBu'))(9)

dat <- read_excel("SpiralChart_Data.xlsx")

dat$time <-  with(dat, as.POSIXct(paste(Date, Time), tz="GMT"))  #把日期转换成POSIXct格式
dat$hour <-  as.numeric(dat$time) %% (24*60*60) / 3600 #时刻
dat$day <- as.Date(dat$time)   #把天数转换成日期型
dat$datt<-as.numeric(strftime(dat$day , "%d"))  #把天数转换成数值型
dat$datt<-dat$datt-min(dat$datt)


dat$Value <- as.numeric(dat$Value)
dat$Value2<-dat$Value/max(dat$Value)

N<-24
width<-0.5

#---------------------------------------图6-3-2 不同形式的螺旋图。(a) 螺旋柱形图--------------------------------

bars <- dat %>% 
  mutate(hour.group = cut(hour, breaks=seq(0,24,width), labels=seq(0,23.75,width),include.lowest=TRUE), 
         hour.group = as.numeric(as.character(hour.group))) %>%
  group_by(datt, hour.group) %>%
  summarise(meanTT = mean(Value2))  %>%
  mutate(value=meanTT*max(dat$Value),
         xmin=  hour.group,
         xmax = hour.group + width,
         ymin = datt*N + hour.group,
         ymax = datt*N + hour.group + meanTT*N*1.1)

poly <- bars %>%
  rowwise() %>%
  do(with(., data_frame(day=datt,
                        date=day,
                        hour=hour.group,
                        value=value,
                        x = c(xmin, xmax, xmax, xmin),
                        y = c(ymin ,
                              ymin + width,
                              ymax + width,
                              ymax ))))

ggplot(poly, aes(x, y, group = interaction(hour, day),fill=value)) +
  geom_polygon(colour="black",size=0.25) +
  scale_fill_gradientn(colours=colormap)+

  ylab("Date")+
  xlab("")+
  theme_bw()+
  coord_polar() +
  
  scale_y_continuous(limits=c(-N/2, max(poly$y)), 
                       breaks=seq(N,max(poly$y),N),
                       labels=unique(dat$day) )+
  scale_x_continuous(limits=c(0,N), breaks=seq(0,N-1,1), minor_breaks=0:N,
                     labels=paste0(rep(c(12,1:11),1), rep(c("AM","PM"),each=12))) +
  theme( panel.background = element_blank(),
         panel.border =  element_rect(fill=NA,colour = "grey80",size=.25),
         panel.grid.major.y  = element_line(colour = "grey80",size=.25),#,linetype ="dotted" ),
         panel.grid.major.x  = element_line(colour = "grey80",size=.25),#,linetype ="dotted" ),
         panel.grid.minor.y  = element_blank(),
         panel.grid.minor.x  = element_blank(),
         axis.line = element_line(colour = "grey80",size=.25),
         # panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
         axis.text.y = element_text(size = 10,colour="grey50"),#,hjust=0,vjust=1),
         axis.line.y = element_line(size=0.25)
  )
 

#----------------------------------------图6-3-2 不同形式的螺旋图。(b) 螺旋热力图---------------------------------------------

bars <- dat %>% 
  mutate(hour.group = cut(hour, breaks=seq(0,24,width), labels=seq(0,23.75,width),include.lowest=TRUE), #
         hour.group = as.numeric(as.character(hour.group))) %>%
  group_by(datt, hour.group) %>%
  summarise(meanTT = mean(Value2))  %>%
  mutate(value=meanTT*max(dat$Value),
         xmin=  hour.group,
         xmax = hour.group + width,
         ymin = datt*N + hour.group,
         ymax = datt*N + hour.group + 24)

poly <- bars %>%
  rowwise() %>%
  do(with(., data_frame(day=datt,
                        date=day,
                        hour=hour.group,
                        value=value,
                        x = c(xmin, xmax, xmax, xmin),
                        y = c(ymin ,
                              ymin + width,
                              ymax + width,
                              ymax ))))


ggplot(poly, aes(x, y, group = interaction(hour, day),fill=value)) +
  geom_polygon(colour="black",size=0.25) +
  
  coord_polar() +
  # ylim(-20, max(poly$y)) +
  #viridis::scale_fill_viridis(discrete = TRUE, option = 'C')# +
  scale_x_continuous(limits=c(0,N), breaks=seq(0,N-1,1), minor_breaks=0:N,
                     labels=paste0(rep(c(12,1:11),1), rep(c("AM","PM"),each=12))) +
  scale_y_continuous(limits=c(-N/2, max(poly$y)), 
                     breaks=seq(N,max(poly$y),N),
                     labels=unique(dat$day) )+
  
  #scale_fill_gradient2(low="green", mid="yellow", high="red", midpoint=mean(bars$meanTT)) +
  scale_fill_gradientn(colours=colormap)+
  ylab("Date")+
  xlab(NA)+
  theme_bw()+
  theme( panel.background = element_blank(),
         panel.border =  element_rect(fill=NA,colour = "grey80",size=.25),
         panel.grid.major.y  = element_line(colour = "grey80",size=.25),#,linetype ="dotted" ),
         panel.grid.major.x  = element_line(colour = "grey80",size=.25),#,linetype ="dotted" ),
         panel.grid.minor.y  = element_blank(),
         panel.grid.minor.x  = element_blank(),
         axis.line = element_line(colour = "grey80",size=.25),
         # panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
         axis.text.y = element_text(size = 10,colour="grey50"),#,hjust=0,vjust=1),
         axis.line.y = element_line(size=0.25)
  )


#-----------------------------------图6-3-3 不同形式的螺旋图和雷达图.(a) 径向热力图.------------------------------------

bars <- dat %>% 
  mutate(hour.group = cut(hour, breaks=seq(0,24,width), labels=seq(0,23.75,width),include.lowest=TRUE), #
         hour.group = as.numeric(as.character(hour.group))) %>%
  group_by(datt, hour.group) %>%
  summarise(meanTT = mean(Value2))  %>%
  # mutate(value=meanTT*max(dat$TravelTime),
  #        xmin=  hour.group,
  #        xmax = hour.group + width,
  #        ymin = datt*N ,
  #        ymax = datt*N  + 24)
  # 
     mutate(value=meanTT*max(dat$Value),
             xmin=  hour.group,
             xmax = hour.group + width,
             ymin = datt*N,
             ymax = datt*N + meanTT*N*1.1)

poly <- bars %>%
  rowwise() %>%
  do(with(., data_frame(day=datt,
                        date=day,
                        hour=hour.group,
                        value=value,
                        x = c(xmin, xmax, xmax, xmin),
                        y = c(ymin ,
                              ymin ,
                              ymax + width,
                              ymax + width ))))


ggplot(poly, aes(x, y, group = interaction(hour, day),fill=value)) +
  geom_polygon(colour="black",size=0.25) +
  
  coord_polar() +
  # ylim(-20, max(poly$y)) +
  #viridis::scale_fill_viridis(discrete = TRUE, option = 'C')# +
  scale_x_continuous(limits=c(0,N), breaks=seq(0,N-1,1), minor_breaks=0:N,
                     labels=paste0(rep(c(12,1:11),1), rep(c("AM","PM"),each=12))) +
  scale_y_continuous(limits=c(-N/2, max(poly$y)*1.2), 
                     breaks=seq(N,max(poly$y)*1.2,N),
                     labels=unique(dat$day))+
  
  #scale_fill_gradient2(low="green", mid="yellow", high="red", midpoint=mean(bars$meanTT)) +
  scale_fill_gradientn(colours=colormap)+
  #scale_fill_brewer(palette="Blues")+
  ylab("Date")+
  xlab(NA)+
  theme_bw()+
  theme( panel.background = element_blank(),
         panel.border =  element_rect(fill=NA,colour = "grey80",size=.25),
         panel.grid.major.y  = element_line(colour = "grey80",size=.25),#,linetype ="dotted" ),
         panel.grid.major.x  = element_line(colour = "grey80",size=.25),#,linetype ="dotted" ),
         panel.grid.minor.y  = element_blank(),
         panel.grid.minor.x  = element_blank(),
         axis.line = element_line(colour = "grey80",size=.25),
         # panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
         axis.text.y = element_text(size = 10,colour="grey50"),#,hjust=0,vjust=1),
         axis.line.y = element_line(size=0.25)
  )


#--------------------------------------图6-3-3 不同形式的螺旋图和雷达图. (b) 螺旋柱形图. -----------------------------------------------

bars <- dat %>% 
  mutate(hour.group = cut(hour, breaks=seq(0,24,width), labels=seq(0,23.75,width),include.lowest=TRUE), #
         hour.group = as.numeric(as.character(hour.group))) %>%
  group_by(datt, hour.group) %>%
  summarise(meanTT = mean(Value2))  %>%
  mutate(value=meanTT*max(dat$Value),
         xmin=  hour.group,
         xmax = hour.group + width,
         ymin = datt*N ,
         ymax = datt*N  + 24)

poly <- bars %>%
  rowwise() %>%
  do(with(., data_frame(day=datt,
                        date=day,
                        hour=hour.group,
                        value=value,
                        x = c(xmin, xmax, xmax, xmin),
                        y = c(ymin ,
                              ymin ,
                              ymax + width,
                              ymax + width ))))


ggplot(poly, aes(x, y, group = interaction(hour, day),fill=value)) +
  geom_polygon(colour="black",size=0.25) +
  
  coord_polar() +
  # ylim(-20, max(poly$y)) +
  #viridis::scale_fill_viridis(discrete = TRUE, option = 'C')# +
  scale_x_continuous(limits=c(0,N), breaks=seq(0,N-1,1), minor_breaks=0:N,
                     labels=paste0(rep(c(12,1:11),1), rep(c("AM","PM"),each=12))) +
  scale_y_continuous(limits=c(-N/2, max(poly$y)), 
                     breaks=seq(N,max(poly$y),N),
                     labels=unique(dat$day) )+
  
  #scale_fill_gradient2(low="green", mid="yellow", high="red", midpoint=mean(bars$meanTT)) +
  scale_fill_gradientn(colours=colormap)+
  #scale_fill_brewer(palette="Blues")+
  ylab("Date")+
  xlab(NA)+
  theme_bw()+
  theme( panel.background = element_blank(),
         panel.border =  element_rect(fill=NA,colour = "grey80",size=.25),
         panel.grid.major.y  = element_line(colour = "grey80",size=.25),#,linetype ="dotted" ),
         panel.grid.major.x  = element_line(colour = "grey80",size=.25),#,linetype ="dotted" ),
         panel.grid.minor.y  = element_blank(),
         panel.grid.minor.x  = element_blank(),
         axis.line = element_line(colour = "grey80",size=.25),
         # panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
         axis.text.y = element_text(size = 10,colour="grey50"),#,hjust=0,vjust=1),
         axis.line.y = element_line(size=0.25)
  )
