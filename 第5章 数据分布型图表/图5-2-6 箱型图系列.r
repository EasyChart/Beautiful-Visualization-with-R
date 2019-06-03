#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(RColorBrewer)

freq <- 10 ^ ((1:4))
df <- data.frame(
  group = rep(letters[seq_along(freq)], freq),
  x = rnorm(sum(freq),3,1)
)

ggplot(df, aes(group,x))+
  geom_boxplot(aes(fill = group),notch = TRUE, varwidth = TRUE) +
  scale_fill_manual(values=c(brewer.pal(7,"Set2")[c(1,2,4,5)]))+
  theme_classic()+
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
        axis.line=element_line(colour="black",size=0.25),
        axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        legend.position="none"
  )

