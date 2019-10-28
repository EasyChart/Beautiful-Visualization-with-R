
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts


library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#---------------------------------方法1：rgdal::readOGR()-------------------------------------
dataProjected <- readOGR("Virtual_Map1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
watershedDF <- full_join(watershedPoints, dataProjected@data,by='id')
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_Map <- full_join(watershedPoints, dataProjected@data, by = "id")

df_city<-read.csv("Virtual_City.csv")  

df <- left_join(df_Map, df_city[c('country','orange')], by = "country")

#(a) 单色渐变系颜色主题
ggplot()+
  geom_polygon(data=df, aes(x=long, y=lat, group=group,fill=orange),colour="black",size=0.25)+
  geom_text(data=df_city,aes(x=long, y=lat, label=country),colour="black",size=3)+
  scale_fill_gradientn(colours = rev(brewer.pal(9,'YlGnBu')))+
  theme(legend.position = c(0.9,0.2),
        legend.background = element_blank())


#(b) 双色渐变系颜色主题
ggplot()+
  geom_polygon(data=df, aes(x=long, y=lat, group=group,fill=orange),colour="black",size=0.25)+
  geom_text(data=df_city,aes(x=long, y=lat, label=country),colour="black",size=3)+
  scale_fill_gradient2(low="#00A08A",mid="white",high="#FF0000",
                       midpoint = mean(df_city$orange))+
  theme(legend.position = c(0.9,0.2),
        legend.background = element_blank())


#---------------------------------------方法2：sf::st_read()----------------------------------------------
library(sf)  #提供st_read()函数
library(ggplot2)
library(dplyr)

nz<-st_read("Virtual_Map1.shp")
df_city<-read.csv("Virtual_City.csv")  
df_map<-left_join(nz,df_city[c('country','orange')],by = "country")

#(b) 双色渐变系颜色主题
ggplot() + 
  geom_sf(data = df_map, aes(fill = orange))+
  coord_sf(datum = st_crs("+proj=longlat +datum=WGS84"))+
  scale_fill_gradient2(low="#00A08A",mid="white",high="#FF0000",
                      midpoint = mean(df_city$orange))

