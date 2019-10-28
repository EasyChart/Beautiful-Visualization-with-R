
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(dplyr)
library(graphics)
library(tcltk)
library(pracma)
library(ggplot2)
library(RColorBrewer)
library(reshape2)

plot.data<-read.table("China_Gridded_Population/ChinaGrid90.txt",header = FALSE)

colnames(plot.data)<-seq(70,70+(ncol(plot.data)-1)*0.0416666666667,0.0416666666667)
rownames(plot.data)<-seq(16+(nrow(plot.data)-1)*0.0416666666667,16,-0.0416666666667)

plot.data[[1633]]<-0
N<-15
max <-351135.5

plotting.threshold1 <- 0.005
plotting.threshold2 <- 0.0001

plot.length <- nrow(plot.data)

plot.data3<-data.frame(x=0,y=0,height=0,group=0)
plot.data4<-data.frame(x=0,y=0,height=0,group=0)

groupid1<-1
groupid2<-1

record_index<-1


# Plot each line
for (i in seq(1,plot.length,N)) {
  # Grabs a row of data
  yVals <- as.vector(as.numeric(plot.data[i,])) #* scaleFactor
  xVals <- as.numeric(colnames(plot.data))      #c(0:(length(yVals) - 1))
  yVals.smooth =  savgol(yVals, 3, forder=4)
  
  if (i==1){
    plot.data2<-data.frame(t(yVals.smooth))
    colnames(plot.data2)<-colnames(plot.data)
    rownames(plot.data2)[i]<- rownames(plot.data)[i]
  } else{
    record_index<-record_index+1
    plot.data2<-rbind(plot.data2,yVals.smooth)
    rownames(plot.data2)[record_index]<- rownames(plot.data)[i]
  }
  

  j <- 2 # Skip padding
  while (j <= (length(yVals.smooth) - 2)) {

    if ((yVals.smooth[j])/ max> plotting.threshold1 | (yVals.smooth[j+1])/max > plotting.threshold1) {
     # segments(xVals[j], yVals.smooth[j] + plottingHeight, xVals[j+1], yVals.smooth[j+1] + plottingHeight, col="#000000", lwd=1.5)
      plot.data3<-rbind(plot.data3,c(xVals[j],rownames(plot.data)[i],yVals.smooth[j],groupid1))
    } else { 
      
      groupid1<-groupid1+1
      
    } # Do nothing
    
    
    if ((yVals.smooth[j])> plotting.threshold2) {
      # segments(xVals[j], yVals.smooth[j] + plottingHeight, xVals[j+1], yVals.smooth[j+1] + plottingHeight, col="#000000", lwd=1.5)
      plot.data4<-rbind(plot.data4,c(xVals[j],rownames(plot.data)[i],yVals.smooth[j],groupid2))
    } else{
      groupid2<-groupid2+1
    }
    
  
    j <- j + 1
  }
 # plottingHeight <- plottingHeight - gap

}

plot.data3$x<-as.numeric(plot.data3$x)
plot.data3$y<-as.numeric(plot.data3$y)
plot.data3$height<-as.numeric(plot.data3$height)

plot.data4$x<-as.numeric(plot.data4$x)
plot.data4$y<-as.numeric(plot.data4$y)
plot.data4$height<-as.numeric(plot.data4$height)


colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)


#------------------------------------------------------------------------------------------------------
dataProjected <- readOGR("China_adm_shp/bou1_4m/bou1_4p.shp") 
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_China1 <- full_join(watershedPoints, dataProjected@data, by = "id")
taiwan_data<-df_China1[df_China1$AREA==3.171,]

scale<-20


Color="#00AAFF"
p<-ggplot() +
  #geom_path(data=china_map_data0,  aes(x = long, y = lat,group=group),colour="black",size=0.25)+
  geom_polygon(data=df_China1,  aes(x = long, y = lat,group=group),fill="grey95",size=0.25)+
  geom_line(data=plot.data4,aes(x=x,y=y+height/ max*scale,group=group),colour=Color,size=0.1)+
  #geom_line(data=mydata,aes(x=variable,y=y+value/ max*scale,group=y),colour="grey90",size=0.1)+
  geom_path(data=taiwan_data,  aes(x = long, y = lat,group=group),colour=Color,size=0.25)#+
  #geom_ribbon(data=plot.data3,aes(x=x,ymin=y,ymax=y-height*scale,group=group),fill="white")


Rowname<-unique(plot.data3$group)
for (i in 2:length(Rowname)){
  Inputdata<-plot.data3[plot.data3$group==Rowname[i],]
  p<-p+ geom_ribbon(data=Inputdata,aes(x=x,ymin=y,ymax=y+height/ max*scale,group=group),fill="white")+
        #geom_linerange(data=Inputdata,aes(x=x,ymin=y,ymax=y+height/ max*scale,group=group,color=height),size=0.15)+#,alpha=0
        geom_line(data=Inputdata,aes(x=x,y=y+height/ max*scale,group=group),colour=Color,size=0.1)
    
}
  
# #geom_linerange(data=mydata,aes(x=y,ymin=variable,ymax=variable-value*scale,color=value),alpha=0.6)+
p<-p+scale_color_gradientn(colours=colormap,name="Density")+
  # scale_y_reverse()+
  xlim(70,70+(ncol(plot.data)-1)*0.0416666666667)+
  ylim(16,16+(nrow(plot.data)-1)*0.0416666666667)+
  theme(
    #panel.grid.major = element_line(colour = "grey90",size=.25,linetype ="solid" ),
    #panel.grid.minor = element_line(colour = "grey90",size=.25,linetype ="solid" ),
    plot.background=element_rect(fill="grey100",colour=NA),
    panel.background=element_rect(fill="grey100",colour=NA),
    text=element_text(size=15),
    line=element_blank(),
    axis.text=element_blank(),
    axis.title=element_blank(),
    plot.title=element_text(size=15),
    plot.caption=element_text(size=10,hjust=0),
    legend.position=c(0.9,0.25),
    legend.text=element_text(size=10),
    legend.title=element_text(size=12),
    #legend.justification="left",
    #legend.text.align=1,
    legend.background=element_blank()
  )
p
