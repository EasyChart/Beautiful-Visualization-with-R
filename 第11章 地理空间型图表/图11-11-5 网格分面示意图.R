

#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(geofacet)
library(ggplot2)
library(reshape2)
library(plyr)

#(b)中国---------------------------------------------------------------------
Griddata<-read.csv("China_Grid.csv",stringsAsFactors=TRUE)
sdata<-read.csv("Province_data.csv",stringsAsFactors=TRUE)
colnames(sdata)<-c("code","value")
mydata<-join(x=Griddata,y=sdata,by=c("code"))

ggplot(mydata, aes(x=value,fill=code)) +
  geom_density(alpha=1,colour="black",size=0.25)+
  facet_geo(~ code,grid=Griddata)+
  theme_light()+
  theme(legend.position = "none",
        panel.grid.minor =element_blank())

#cairo_pdf(file="ChinaGrid6.pdf",width=6.52,height=6)
#showtext.begin()
ggplot(mydata, aes(x=value,fill=code)) +
  geom_density(alpha=1,colour="black",size=0.25)+
  facet_geo(~ code,grid=Griddata)+
  theme_light()+
  theme(panel.background=element_blank(),
        panel.border =element_blank(),
        legend.position = "none",
        panel.grid =element_blank(),
        strip.placement = "bottom",
        strip.background=element_blank(),
        strip.text=element_text(colour="black"))
#showtext.end()
#dev.off()





#Province_data.csv的构造------------------------------------------------------------
for (i in 1 :nrow(mydata))
{
  x<- rnorm(mean=runif(1)*10, 100)
  if (i==1){
    sdata<-as.data.frame(x)
    colnames(sdata)[i]<-as.character(mydata[i,4])
  }
  else{
    sdata<-cbind(sdata,x)
    colnames(sdata)[i]<-as.character(mydata[i,4])
  }
  
}
sdata<-melt(sdata)
colnames(sdata)<-c("code","value")
write.csv(sdata,"Province_data.csv",row.names = FALSE)

#-(a) 美国------------------------------------------------------------------------
mydata <- us_state_grid1
mydata$col[mydata$code == "WI"] <- 7
grid_preview(my_grid)

for (i in 1 :nrow(mydata))
{
  x<- rnorm(mean=runif(1)*10, 100)
  if (i==1){
    sdata<-as.data.frame(x)
    colnames(sdata)[i]<-as.character(mydata[i,4])
  }
  else{
    sdata<-cbind(sdata,x)
    colnames(sdata)[i]<-as.character(mydata[i,4])
  }
}
sdata<-melt(sdata)
colnames(sdata)<-c("name","value")

mydata_new<-join(x=mydata,y=sdata,by=c("name"))

#cairo_pdf(file="USAGrid5.pdf",width=6.52,height=6)
#showtext.begin()
ggplot(mydata_new, aes(x=value,fill=code)) +
  geom_density(alpha=1,colour="black",size=0.25)+
  facet_geo(~ code,grid=mydata)+#, scales='free'
  theme_light()+
  ylim(0,0.6)+
  theme(legend.position = "none",
        panel.grid.minor =element_blank())
#showtext.end()
#dev.off()
