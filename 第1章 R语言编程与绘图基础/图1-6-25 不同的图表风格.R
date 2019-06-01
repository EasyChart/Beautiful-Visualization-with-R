
library(ggplot2)

library(wesanderson)

ggplot(iris,aes(Sepal.Length, Petal.Length, fill= Species))+
  geom_point(size=3.5,shape=21,colour="black") +
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1"))+
  theme_light()


#----------------------------Python---------------------------
ggplot(iris,aes(Sepal.Length, Petal.Length, fill= Species))+
  geom_point(size=3.5,shape=21,colour="black") +
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1"))+
  theme_minimal()