#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(Rtsne) # Load package
library(ggplot2)
library(RColorBrewer)

iris_unique <- unique(iris) # Remove duplicates
set.seed(42) # Sets seed for reproducibility
tsne_out <- Rtsne(as.matrix(iris_unique[,1:4])) # Run TSNE

mydata<-data.frame(tsne_out$Y,iris_unique$Species)
colnames(mydata)<-c("t_DistributedY1","t_DistributedY2","Group")

ggplot(data=mydata,aes(t_DistributedY1,t_DistributedY2,fill=Group))+
  geom_point(size=4,colour="black",alpha=0.7,shape=21)+
  scale_fill_manual(values=c("#00AFBB",  "#FC4E07","#E7B800","#2E9FDF"))+
  theme(
    text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=11,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title = element_text(size=11,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.position=c(0.83,0.15)
  )
  

#----------------------------------------------------
set.seed(1)
num_rows_sample <- 5000

train        <- read.csv("Tsne_Data.csv")
train_sample <- train[sample(1:nrow(train), size = num_rows_sample),]
features     <- train_sample[,c(-1, -95)]

tsne <- Rtsne(as.matrix(features), check_duplicates = FALSE, pca = TRUE,
              perplexity=30, theta=0.5, dims=2)

embedding <- as.data.frame(tsne$Y)

embedding$Class<-train_sample$target

ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
  geom_point(size=1.25) +
  #scale_colour_manual(values=palette(brewer.pal(8,"Set1")))
  guides(colour = guide_legend(override.aes = list(size=4))) +
  xlab("t_DistributedY1") + ylab("t_DistributedY2") +
  #ggtitle("t-SNE 2D Embedding of Products Data") +
  #theme_light(base_size=20) +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        #axis.text.x      = element_blank(),
        #axis.text.y      = element_blank(),
        #axis.ticks       = element_blank(),
        axis.line        = element_blank(),
        panel.border     = element_blank())

