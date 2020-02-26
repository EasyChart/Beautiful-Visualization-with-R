
#EasyShu团队出品，更多文章请关注微信公众号【EasyShu】
#如有问题修正与深入学习，可联系微信：EasyCharts

library(tm)
library(wordcloud)

Paper1<-paste(scan("Paper1.txt", what = character(0),sep = ""), collapse = " ") #读入TXT 文档1
Paper2<-paste(scan("Paper2.txt", what = character(0),sep = ""), collapse = " ") #读入TXT 文档2
tmpText<- data.frame(c(Paper1, Paper2),row.names=c("Text1","Text2"))
df_title <- data.frame(doc_id=row.names(tmpText),
                       text=tmpText$c.Paper1..Paper2.)
ds <- DataframeSource(df_title)
#创建一个数据框格式的数据源，首列是文档id(doc_id),第二列是文档内容
corp <- VCorpus(ds)
#加载文档集中的文本并生成语料库文件
corp<- tm_map(corp,removePunctuation) #清除语料库内的标点符号
corp <- tm_map(corp,PlainTextDocument) #转换为纯文本
corp <- tm_map(corp,removeNumbers) #清除数字符号
corp <- tm_map(corp, function(x){removeWords(x,stopwords())}) #过滤停止词库
term.matrix <- TermDocumentMatrix(corp)
#利用TermDocumentMatrix()函数将处理后的语料库进行断字处理，生成词频权重矩阵

term.matrix <- as.matrix(term.matrix) #频率
colnames(term.matrix) <- c("Paper1","paper2")
df<-data.frame(term.matrix)
write.csv(df,'term_matrix.csv') #导出两篇文章的频率分析结果

#---------------------------------------导入数据------------------------------------------
df<-read.csv('term_matrix.csv',header=TRUE,row.names=1)

#-------------------------------------单篇文章数据的展示-----------------------------------------------------------------
#Colors<-colorRampPalette(rev(brewer.pal(9,'RdBu')))(length(df$Paper1>10))
wordcloud(row.names(df) , df$Paper1 , min.freq=10,col=brewer.pal(8, "Dark2"), rot.per=0.3 )


#----------------------------------------两篇文章数据的对比-------------------------------------------------------------
comparison.cloud(df, max.words=300, random.order=FALSE, rot.per=.15, c(4,0.4), title.size=1.4)

comparison.cloud(df,max.words=300,random.order=FALSE,colors=c("#00B2FF", "red"))
commonality.cloud(df,max.words=100,random.order=FALSE,color="#E7298A")


# comparison cloud
comparison.cloud(df, random.order=FALSE, 
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
                 title.size=1.5, max.words=500)
