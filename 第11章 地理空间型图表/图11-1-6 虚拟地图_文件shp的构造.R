
#EasyCharts团队出品，
#如需使用与深入学习，请联系微信：EasyCharts

#reference:
#https://cloud.tencent.com/developer/article/1103887

library(maptools)
df_Map <- read.csv("Virtual_Map1.csv",stringsAsFactors=FALSE)  
group<-as.character(unique(df_Map$group))
#group<-unique(df_Map$group)
mypolys = lapply(group,
                 function(x) {
                   tmp = df_Map[df_Map$group==x,]
                   tmp$X = as.numeric(tmp$long);
                   tmp$Y = as.numeric(tmp$lat);
                   tmp
                 })


names(mypolys) = group
myPolygons = lapply(group,
                    function(x) {
                      tmp = mypolys[[x]];
                      Polygons(list(Polygon(cbind(tmp$X, tmp$Y))), x)
                    })

mySpn = SpatialPolygons(myPolygons)

df_group<-df_Map[!duplicated(df_Map$group),]

myshpdata = SpatialPolygonsDataFrame(mySpn,
                                     data = data.frame(
                                      country = df_group$country,
                                      #type = df_group$type,
                                      row.names = row.names(mySpn)))

writePolyShape(x = myshpdata, fn = "Virtual_Map1")