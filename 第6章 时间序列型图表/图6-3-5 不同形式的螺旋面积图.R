

#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggplot2)
library(data.table)
library(RColorBrewer)


set.seed(1)
dtData <- data.table(
  date = seq(as.Date("1/01/2014", "%d/%m/%Y"),as.Date("31/12/2017", "%d/%m/%Y"),"days"),
  ValueCol = runif(1461))
dtData[, ValueCol := ValueCol + (strftime(date,"%u") %in% c(6,7) * runif(1) * 0.75), .I]
dtData[, ValueCol := ValueCol + (abs(as.numeric(strftime(date,"%m")) - 6.5)) * runif(1) * 0.75, .I]


dtData$Year<- as.integer(strftime(dtData$date, '%Y'))   #年份

dtData$DateNum<-as.numeric(dtData$date)-as.numeric(as.Date(paste(as.character(strftime(dtData$date, "%Y")),"-01-01", sep = "")))


Step<-5
dtData$Asst<-rep(Step,nrow(dtData))

YearRange<-unique(dtData$Year)
for(i in 1:length(YearRange)){
  dtData$Asst[dtData$Year==YearRange[i]]<-seq(i*Step, (i+1)*Step, length.out = length(dtData$Asst[dtData$Year==YearRange[i]]))
}
circlelabel<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
circlemonth<-seq(15,345,length=12)
circlebj<-rep(c(-circlemonth[1:3],rev(circlemonth[1:3])),2)

#-------------------------------------图6-3-5不同形式的螺旋面积图. (b)颜色映射填充----------------------------------------
Height<-4.8
dtData$Valueht<-(dtData$ValueCol-min(dtData$ValueCol))/(max(dtData$ValueCol)-min(dtData$ValueCol))*Height
       
ggplot()+
 geom_linerange(data=dtData,aes(x=DateNum,ymin=Asst-5,ymax=Asst+Valueht-5,color=ValueCol),size =1)+
  geom_line(data=dtData,aes(x=DateNum,y=Asst+Valueht-5,group=Year),size =0.25,color="black")+
  geom_line(data=dtData,aes(x=DateNum,y=Asst-5,group=Year),size =0.25,color="grey20")+
  
  coord_polar(theta="x",start=0)+
  scale_x_continuous(breaks=c(1,31,59,90,120,151,181,212,243,273,304,334))+
  scale_y_continuous(limits=c(-5,28),breaks=c(2.5,7.5,12.5,17.5),labels=c("2014","2015","2016","2017"))+
  scale_color_gradientn(colours=rev(brewer.pal(11,'Spectral')))+
  geom_text(data=NULL,aes(x=circlemonth,y=28,label=circlelabel,
                          angle=circlebj),size=4,color="grey50")+#,hjust=0.5,vjust=.5，family="myfont",
  ylab("Year")+
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


#----------------------------------------图6-3-5不同形式的螺旋面积图.(a)纯色填充-----------------------------------------
ggplot()+
  geom_ribbon(data=dtData,aes(x=DateNum,ymin=Asst-5,ymax=Asst+Valueht-5,group=Year,fill=Year),
              size =0.75,fill="#FF8B49")+
  geom_line(data=dtData,aes(x=DateNum,y=Asst+Valueht-5,group=Year),size =0.25,color="black")+
  geom_line(data=dtData,aes(x=DateNum,y=Asst-5,group=Year),size =0.25,color="grey20")+
  
  coord_polar(theta="x",start=0)+
  xlim(1,355)+
  scale_x_continuous(breaks=c(1,31,59,90,120,151,181,212,243,273,304,334))+
  scale_y_continuous(limits=c(-5,28),breaks=c(2.5,7.5,12.5,17.5),labels=c("2014","2015","2016","2017"))+
  geom_text(data=NULL,aes(x=circlemonth,y=28,label=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                          angle=circlebj),size=4,color="grey50")+
  ylab("Year")+
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