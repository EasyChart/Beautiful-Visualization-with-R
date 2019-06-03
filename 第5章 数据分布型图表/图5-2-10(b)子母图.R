#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggplot2)
library(grid)
#library(Cairo)         #图片高清导出

library(showtext)
p1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width, fill = Species)) +
  geom_point(size = 4,shape=21,color="black") +
  scale_fill_manual(values= c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme_minimal() +
  xlim(4, 10) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

p2 <- ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
  geom_boxplot() +
  scale_fill_manual(values= c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme_bw() +
  ggtitle("Submian: Box plot") +
  theme(plot.background = element_blank(),
        panel.background= element_blank(),
        panel.grid.minor= element_blank(),
        panel.grid.major.y= element_blank(),
         axis.title = element_blank(),
        axis.text = element_text(size = 10,colour="black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

#cairo_pdf(file="子母图.pdf",width=6.56,height=5.09)
#showtext.begin()
subvp <- viewport(x = 0.78, y = 0.38, width = 0.4, height = 0.5)
p1
print(p2, vp = subvp)
#showtext.end()
#dev.off()


