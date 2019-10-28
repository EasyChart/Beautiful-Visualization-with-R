
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts


library(rgdal)   #提供readOGR()函数
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(reshape2)

dataProjected <- readOGR("Virtual_Map1.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map <- full_join(watershedPoints, dataProjected@data, by = "id")

df_city<-read.csv("Virtual_City.csv") 

df <- left_join(df_map[c('country','long','lat','group')], df_city[c('country','orange','apple','banana','watermelon')], by = "country")
df_melt<-melt(df,id.vars = c('country', 'group','long','lat'))


#双色渐变系颜色主题
ggplot()+
  geom_polygon(data=df_melt, aes(x=long, y=lat, group=group,fill=value),colour="black",size=0.25)+
  geom_text(data=df_city,aes(x=long, y=lat, label=country),colour="black",size=3)+
  scale_fill_gradient2(low="#00A08A",mid="white",high="#FF0000",
                       midpoint = mean(df_city$orange))+
  facet_wrap(~variable)+
  theme(strip.text = element_text(size=12))
