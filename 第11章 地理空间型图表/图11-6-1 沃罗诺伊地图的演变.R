
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#Reference:
#https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
#https://www.codesd.com/item/fill-the-voronoi-polygons-with-ggplot.html

library(ggplot2)
library(sp)
library(deldir) #提供deldir()函数
library(raster) #提供intersect()函数

dataProjected <- readOGR("Virtual_Map0.shp")
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected)
df_map <- full_join(watershedPoints, dataProjected@data, by = "id")

df_city<-read.csv("Virtual_City.csv")  

dati<-data.frame(x=df_city$long,y=df_city$lat,z=df_city$orange,city=df_city$city)
vor_pts <- SpatialPointsDataFrame(cbind(dati$x,dati$y),dati, match.ID=TRUE)

SPointsDF_to_voronoi_SPolysDF <- function(sp) {
  # tile.list extracts the polygon data from the deldir computation
  vor_desc <- tile.list(deldir(sp@coords[,1], sp@coords[,2],rw=c(105,135,30,60)))
  lapply(1:(length(vor_desc)), function(i) {
    # tile.list gets us the points for the polygons but we
    # still have to close them, hence the need for the rbind
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1,])
    # now we can make the Polygon(s)
    Polygons(list(Polygon(tmp)), ID=i)
  }) -> vor_polygons
  # hopefully the caller passed in good metadata!
  sp_dat <- sp@data
  # this way the IDs _should_ match up w/the data & voronoi polys
  rownames(sp_dat) <- sapply(slot(SpatialPolygons(vor_polygons),'polygons'),slot, 'ID')
  SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons),data=sp_dat)
}


vor <- SPointsDF_to_voronoi_SPolysDF(vor_pts)

#(a) 沃罗诺伊图------------------------------------------------------------------------

vor@data$id <- rownames(vor@data)
df_vor <- fortify(vor)
df_vor <- full_join(df_vor, vor@data,by='id')

ggplot() +
  geom_polygon(data=df_map, aes(x = long, y = lat,group=group),
               fill="white",colour="black",size=0.25) +
  geom_polygon(data=df_vor, aes(x = long, y = lat,group=group,fill=city), 
           color="black", size=1,alpha=0.4)+
  geom_point(data = dati, aes(x, y),size=4,shape=21,color="black",fill="red",stroke=0.1)

#(b) 沃罗诺伊地图-----------------------------------------------------------------------
group<-1:length(vor)

mypolys<-lapply(group,
                function(x) {
                  tmp = intersect(dataProjected, vor[x,]);
                  df_pi= fortify(tmp)[c('long','lat','group')]
                  df_pi$city=as.character(vor[x,]@data$city[1])
                  df_pi$group=as.numeric(df_pi$group)+runif(1,0,100)
                  df_pi
                })

df_vor<-data.frame(long=numeric(0),lat=numeric(0),group=numeric(0),city=character(0))
for (i in group ){
  df_vor<-rbind(df_vor,mypolys[[i]])
}

ggplot() +
  geom_polygon(data=df_vor,aes(x = long, y = lat,group=group,fill=city), 
           color="black", size=0.5,alpha=0.6)+
  geom_point(data = dati, aes(x, y),size=4,shape=21,color="black",fill="red",stroke=0.1)
