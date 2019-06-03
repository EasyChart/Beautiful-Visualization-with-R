#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

#-----------------------------------------------------------Method 1-----------------------------------------------------------
library(ggplot2)
library(factoextra)
library(FactoMineR)
df <- iris[c(1, 2, 3, 4)]
iris.pca<- PCA(df, graph = FALSE)
fviz_pca_ind(iris.pca,
             geom.ind = "point", # show points only (nbut not "text")
             pointsize =3,pointshape = 21,fill.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups",
             title="")+
  theme_grey() +
  theme(
  text=element_text(size=12,face="plain",color="black"),
  axis.title=element_text(size=11,face="plain",color="black"),
  axis.text = element_text(size=10,face="plain",color="black"),
  legend.title = element_text(size=11,face="plain",color="black"),
  legend.text = element_text(size=11,face="plain",color="black"),
  legend.background = element_blank(),
  legend.position=c(0.88,0.15)
)

#----------------------------------------------------
set.seed(1)
num_rows_sample <- 5000

train        <- read.csv("Tsne_Data.csv")
train_sample <- train[sample(1:nrow(train), size = num_rows_sample),]
features     <- train_sample[,c(-1, -95)]
features.pca<- PCA(features, graph = FALSE)

fviz_pca_ind(features.pca,
             geom.ind = "point", # show points only (nbut not "text")
             pointsize =3,pointshape = 21,fill.ind = train_sample$target, # color by groups
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups",
             title="")+
  theme_grey() +
  theme(
    text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=11,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title = element_text(size=11,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position="right"
  )

#-----------------------------------------------------------Method 2-----------------------------------------------------------
library(ggfortify)
autoplot(prcomp(df), data = iris, 
         shape=21,colour ="black",fill= 'Species', size=3,
         frame = TRUE,frame.type = 'norm', frame.colour = 'Species')
