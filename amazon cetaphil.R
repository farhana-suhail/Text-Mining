install.packages("rvest")
install.packages("XML")
install.packages("magrittr")
library(rvest)
library(XML)
library(magrittr)
# Amazon Reviews #############################
aurl <- "https://www.amazon.com/product-reviews/B07GC74LL5/ref=cm_cr_arp_d_paging_btm_next_2?pageNumber=2"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"cetaphil.txt",row.names = F)
getwd()

#read file
cetaphil <- read.delim("G:/Data Science/ExcelR/LM Portal/Assignments/Text Mining/cetaphil.txt", header = T)
str(cetaphil)

#Build Corpus
library(tm)
corpus <- iconv(cetaphil$x, "ASCII", "UTF-8", sub = "")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('ny','\022','know','now','made','done','well','need','us','one','also','within','yr','also','\n','thought','ive','much','anyone','isnt','use','used','doesnt','get','will','however','usually','dont','made','using','another','\022','can','bought','reallyly')) #remove our own words(common) from text
cleanset <- tm_map(cleanset, gsub, pattern = 'really', replacement = 'real') #to replace similar like words

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

#Barplot
w <- rowSums(tdm)
w
y <- subset(w, w>10)
y
barplot(y, las = 2, col = rainbow(50))

#word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
?set.seed
set.seed(222)
wordcloud(words = names(w), 
          freq = w, 
          max.words = 150,
          random.order = FALSE,
          min.freq = 2,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(1.5,0.5),
          rot.per = 0.5)

install.packages("wordcloud2")
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word','freq')
wordcloud2(w, size = 0.8, shape = 'circle', rotateRatio = 0.5, minSize = 2)

#letterCloud(w, word = 'T', size=2)


#sentiment analysis
install.packages("syuzhet")
install.packages("lubridate")

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#read file
cetaphil <- read.delim("G:/Data Science/ExcelR/LM Portal/Assignments/Text Mining/cetaphil.txt", header = T)
reviews <- iconv(cetaphil$x, "ASCII", "UTF-8", sub = "")

#obtain sentiment analysis
s <- get_nrc_sentiment(reviews)
head(s)
reviews[3]
get_nrc_sentiment('error')

#barplot
barplot(colSums(s), las=2, col = rainbow(10), ylab = 'Count', main = 'sentiment scores for Cetaphil Cream Reviews' )

