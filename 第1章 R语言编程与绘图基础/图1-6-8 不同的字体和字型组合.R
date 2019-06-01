library(ggplot2)
library(Cairo)
library(showtext)


df <- expand.grid(x = seq(0,1,length.out = 4), y= seq(0,1,length.out = 3))
df$fontface <-rep(c("plain", "bold", "italic", "bold.italic"),3)
df$family<-rep(c("sans", "times",  "mono"),each=4)

df$label<-paste(df$family,"\n ",df$fontface)


#CairoPDF(file="×ÖÌåÍ¼.pdf",width=4.67,height=4.36)
#showtext.begin()

ggplot(df, aes(x, y)) +
  geom_text(aes(label = label, fontface = fontface,family=family),size = 4) +
  xlim(-0.2,1.3)+
  ylim(-0.2,1.2)

#showtext_end()
#dev.off()