library(tm)
library(slam)
install.packages("topicmodels")
library(topicmodels)


x <- readLines("G:/Data Science/ExcelR/LM Portal/Assignments/Text Mining/tweets.txt")
mydata.corpus <- Corpus(VectorSource(x))
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
my_stopwords <- c(stopwords('english'),"the","due","from","and","so","on","is","of","this","was","be","done","by","not","all","with","him","you","we","are","a","no","only","so","many","other","to","but","why","yet","they","each","we","in","my","an","who","than","their","now","up","just","whether","or","not","make","lot","nothing","another","every","True")
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
mydata.corpus <- tm_map(mydata.corpus, removeNumbers)

#build a term document matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
mydata.dtm3

dim(mydata.dtm3)

#converting to document term matrix
dtm <- as.DocumentTermMatrix(mydata.dtm3)
rowTotals <- apply(dtm , 1 , sum)
dtm.new <- dtm[rowTotals>0,]
lda <- LDA(dtm.new, 13)
term <- terms(lda, 10)
term

tops <- terms(lda)
tb <- table(names(tops), unlist(tops))
tb <- as.data.frame.matrix(tb)

cls <- hclust(dist(tb), method = 'ward.D2') #ward is absolut distance
par(family = "HiraKakuProN-W3")
plot(cls)
