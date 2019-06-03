
#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

library(ggplot2)
library(data.table) #提供data.table()函数
library(ggTimeSeries)
library(RColorBrewer)
set.seed(1234)
dat <- data.table(
  date = seq(as.Date("1/01/2014", "%d/%m/%Y"),as.Date("31/12/2017", "%d/%m/%Y"),"days"),
  ValueCol = runif(1461)
)
dat[, ValueCol := ValueCol + (strftime(date,"%u") %in% c(6,7) * runif(1) * 0.75), .I]
dat[, ValueCol := ValueCol + (abs(as.numeric(strftime(date,"%m")) - 6.5)) * runif(1) * 0.75, .I]

dat$Year<- as.integer(strftime(dat$date, '%Y'))   #年份
dat$month <- as.integer(strftime(dat$date, '%m')) #月份
dat$week<- as.integer(strftime(dat$date, '%W'))   #周数

MonthLabels <- dat[,list(meanWkofYr = mean(week)), by = c('month') ]
MonthLabels$month <-month.abb[MonthLabels$month]

ggplot(data=dat,aes(date=date,fill=ValueCol))+
  stat_calendar_heatmap()+
  scale_fill_gradientn(colours= rev(brewer.pal(11,'Spectral')))+ 
  facet_wrap(~Year, ncol = 1,strip.position = "right")+
  scale_y_continuous(breaks=seq(7, 1, -1),labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
  scale_x_continuous(breaks = MonthLabels[,meanWkofYr], labels = MonthLabels[, month],expand = c(0, 0)) +
  xlab(NULL)+ 
  ylab(NULL)+
  theme( panel.background = element_blank(),
         panel.border = element_rect(colour="grey60",fill=NA),
         strip.background = element_blank(),
         strip.text = element_text(size=13,face="plain",color="black"),
         axis.line=element_line(colour="black",size=0.25),
         axis.title=element_text(size=10,face="plain",color="black"),
         axis.text = element_text(size=10,face="plain",color="black"))

#---------------------------------------------------------
library(dplyr)
dat17 <- filter(dat,Year==2017)[,c(1,2)]

dat17$month <- as.integer(strftime(dat17$date, '%m'))  #月份
dat17$monthf<-factor(dat17$month,levels=as.character(1:12),
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
dat17$weekday<-as.integer(strftime(dat17$date, '%u'))#周数
dat17$weekdayf<-factor(dat17$weekday,levels=(1:7),
                       labels=(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
dat17$yearmonth<- strftime(dat17$date, '%m%Y')   #月份
dat17$yearmonthf<-factor(dat17$yearmonth)
dat17$week<- as.integer(strftime(dat17$date, '%W'))#周数

dat17<-dat17 %>% group_by(monthf)%>%mutate(monthweek=1+week-min(week))

dat17$day<-strftime(dat17$date, "%d")

ggplot(dat17, aes(weekdayf, monthweek, fill=ValueCol)) + 
  geom_tile(colour = "white") + 
  scale_fill_gradientn(colours=rev(brewer.pal(11,'Spectral')))+
  geom_text(aes(label=day),size=3)+
  facet_wrap(~monthf ,nrow=3) +
  scale_y_reverse()+
  xlab("Day") + ylab("Week of the month") +
  theme(strip.text = element_text(size=11,face="plain",color="black"))
