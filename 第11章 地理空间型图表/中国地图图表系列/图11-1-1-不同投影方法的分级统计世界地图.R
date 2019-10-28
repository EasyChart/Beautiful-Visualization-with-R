
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#reference:http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot


library(maps)
library(ggplot2)
library(RColorBrewer)

color1<-brewer.pal(9,"YlOrRd")[c(3,4,5,6,7,8,9)]
color2<-brewer.pal(9,"Greens")[c(4,6)]
color<-c(rev(color2),color1)


mydata<-read.csv("Country_Data.csv",stringsAsFactors=FALSE) 
names(mydata)[1]<-c("Country" ,"Scale" ,"million","fan"  )
mydata$million<-mydata$Scale/1000000


mydata$fan<-cut(mydata$million,
                breaks=c(min(mydata$million,na.rm=TRUE),
                         0,300,600,900,1200,1500,1800,2100,2400,
                         max(mydata$million,na.rm=TRUE)),
                labels=c(" <=0","0~300","300~600","600~900","900~1200","1200~1500",
                         "1500~1800","1800~2100","2100~2400"," >=2400"),
                          order=TRUE)

world_map <- map_data("world")

#--------------------------------------mercator--------------------------------------
# CairoPDF(file="wordmap.pdf",width=12,height=6)
# showtext.begin()
ggplot()+
  geom_map(data=mydata,aes(map_id=Country,fill=fan),map=world_map)+
  geom_path(data=world_map,aes(x=long,y=lat,group=group),colour="black",size=.2)+ 
  coord_map("mercator",xlim=c(-180,180), ylim=c(-90, 90))+
  scale_y_continuous(breaks=(-3:3)*30) +
  scale_x_continuous(breaks=(-6:6)*30) +
  scale_fill_manual(name="million dollars",values=color,na.value="grey75")+
  guides(fill=guide_legend(reverse=TRUE)) +
  theme_minimal()+
  theme(
    #panel.background=element_rect(fill="white",colour=NA),
    #plot.background = element_rect(fill="white",colour=NA),
    #panel.grid.major = element_line(colour = "grey60",size=.25),
    #panel.grid.minor = element_line(colour = "grey60",size=.25),
    text=element_text(size=15)#
    #axis.text=element_blank(),
    #axis.title=element_blank(),
    #axis.ticks=element_blank()
    #plot.title=element_text(size=15,family="myfont",hjust=.5)
    # plot.caption=element_text(size=15,family="myfont",hjust=0),
    #plot.margin = unit(c(ifelse(i<=4,2,0),0,ifelse(i>=4,2,0),0),"lines")
    #legend.position="none"
  )
# showtext.end()
# dev.off()
#-----------------------------------albers-----------------------------------------------

ggplot()+
  geom_map(data=mydata,aes(map_id=Country,fill=fan),map=world_map)+
  geom_path(data=world_map,aes(x=long,y=lat,group=group),colour="black",size=.2)+ 
  coord_map("albers", parameters = c(0, 0))+
  scale_y_continuous(breaks=(-3:3)*30) +
  scale_fill_manual(name="million($)",values=color,na.value="grey80")+
  guides(fill=guide_legend(reverse=TRUE))+
  theme_minimal()+
  theme(axis.text.x=element_blank())


#------------------------------------Azimuthal ----------------------------------------
ggplot()+
  geom_map(data=mydata,aes(map_id=Country,fill=fan),map=world_map)+
  geom_path(data=world_map,aes(x=long,y=lat,group=group),colour="black",size=.2)+ 
  coord_map("azequalarea", orientation = c(0, 30, 0))+
  scale_y_continuous(breaks=(-3:3)*30) +
  scale_x_continuous(breaks=(-6:6)*30) +
  scale_fill_manual(name="million($)",values=color,na.value="grey80")+
  guides(fill=guide_legend(reverse=TRUE))+
  theme_minimal()+
  theme(axis.text=element_blank())