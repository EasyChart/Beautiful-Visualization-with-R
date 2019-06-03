#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(tm)
library(wordcloud)

Paper1<-paste(scan("Paper1.txt", what = character(0),sep = ""), collapse = " ")  
Paper2<-paste(scan("Paper2.txt", what = character(0),sep = ""), collapse = " ")  

tmpText<- data.frame(c(Paper1, Paper2),row.names=c("Text1","Text2"))

df_title <- data.frame(doc_id=row.names(tmpText),
                       text=tmpText$c.Paper1..Paper2.)

ds <- DataframeSource(df_title)
corp = Corpus(ds)
corp = tm_map(corp,removePunctuation)
corp = tm_map(corp,PlainTextDocument)
corp = tm_map(corp,removeNumbers)
corp = tm_map(corp, function(x){removeWords(x,stopwords())})

term.matrix <- TermDocumentMatrix(corp)
term.matrix <- as.matrix(term.matrix)
colnames(term.matrix) <- c("Paper1","paper2")

#------------------------------------------------------------------------------------------------------
comparison.cloud(term.matrix, max.words=300, random.order=FALSE, rot.per=.15, c(4,0.4), title.size=1.4)

comparison.cloud(term.matrix,max.words=300,random.order=FALSE,colors=c("#00B2FF", "red"))
commonality.cloud(term.matrix,max.words=100,random.order=FALSE,color="#E7298A")


# comparison cloud
comparison.cloud(term.matrix, random.order=FALSE, 
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
                 title.size=1.5, max.words=500)

#------------------------------------------------------------------------------------------------------
df<-data.frame(term.matrix)
#Colors<-colorRampPalette(rev(brewer.pal(9,'RdBu')))(length(df$Paper1>10))
wordcloud(row.names(df) , df$Paper1 , min.freq=10,col=brewer.pal(8, "Dark2"), rot.per=0.3 )
