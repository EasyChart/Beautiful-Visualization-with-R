

#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(grid)
library(RColorBrewer)
library(Cairo)         #图片高清导出
library(showtext)

dataProjected <- readOGR("China_adm_shp/bou4_4m/BOUNT_poly.shp") 
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_China4 <- merge(watershedPoints, dataProjected@data, by = "id")
df_China4$province<-substr(as.character(df_China4$ADCODE99),1,2)
mydata<-data.frame(NAME99=unique(df_China4$NAME99),
                   value=round(runif(length(unique(df_China4$NAME99)),0,10)))
df_China<-join(df_China4,mydata,type="full") 


ProCode <- read.csv("PcCode.csv")
ProCode$ID<-as.character(ProCode$ID)

for (i in 1:nrow(ProCode)){
  df_China$province<- gsub(pattern = ProCode$ID[i], 
                           replacement =ProCode$PcCode[i], 
                           x = df_China$province)
}

df_China$province<-factor(df_China$province,levels=as.character(ProCode$PcCode))


# ggplot() +
#   geom_polygon(data=China_map[China_map$province==0,], aes(x = long, y = lat,group=group,fill=value),
#                colour="grey50",size=0.1) +
#   #geom_polygon(data=china_map_data2,aes(x = long, y = lat,group=group),fill=NA,colour="black",size=0.25) +
#   scale_fill_gradientn(colours=brewer.pal(9,"Spectral")) +
#   theme( 
#     legend.background =element_blank(),
#     legend.position=c(0.1,0.2))
#df_China<-subset(China_map,province!="0")

df_China<-df_China[df_China$NAME99!='南沙群岛' & 
                     df_China$NAME99!='西沙群岛' & 
                     df_China$NAME99!='中沙群岛' & 
                     df_China$NAME99!='中沙群岛的岛礁及其海域', ]

cairo_pdf(file="China_Provincemap.pdf",width=8.42,height=7)
showtext_begin()
ggplot(df_China, aes(x = long, y = lat,group=group,fill=value)) +
  geom_polygon(colour="black",size=0.1) +
  scale_fill_gradientn(colours=brewer.pal(9,"Spectral")) +
  facet_wrap(~province, ncol = 7,scales="free",strip.position = "top")+
  coord_cartesian()+
  theme_void()+
  theme(strip.background = element_rect(color="grey60",size=0.25),
        panel.background = element_rect(fill=NA,color="grey60",size=0.25))
showtext_end()
dev.off()
