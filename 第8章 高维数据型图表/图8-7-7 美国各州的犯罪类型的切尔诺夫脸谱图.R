
#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts

library(aplpack)
library(RColorBrewer)
crime <- read.csv("Faces_Data.csv")
crime_filled <- cbind(crime[,1:6], rep(0, length(crime$state)), crime[,7:8])
crime_filled<-crime_filled[order(crime_filled$murder,decreasing=T),] 
faces(crime_filled[,2:8],
      col.face = colorRampPalette(brewer.pal(9, "Reds"))(20), 
      col.hair= colorRampPalette(brewer.pal(9, "Blues"))(20),
      labels= crime_filled$state,
      cex=1)
