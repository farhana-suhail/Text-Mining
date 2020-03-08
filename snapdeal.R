library(rvest)
library(XML)
library(magrittr)

# Snapdeal reviews 
surl_1 <- "https://www.snapdeal.com/product/johnsons-baby-lotion-500-ml/700866186/reviews?"
surl_2 <- "page=2&sortBy=RECENCY#defRevPDP"
snapdeal_reviews <- NULL
for (i in 1:30){
  surl <- read_html(as.character(paste(surl_1,surl_2,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"johnson.txt",row.names = FALSE)
getwd()

#read file
johnson <- read.delim("G:/Data Science/ExcelR/LM Portal/Assignments/Text Mining/johnson.txt", header = T)
str(johnson)

#Build Corpus
library(tm)
corpus <- iconv(johnson$x, "ASCII", "UTF-8", sub = "")
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

cleanset <- tm_map(cleanset, removeWords, c('ny','know','now','made','done','well','need','us','one','also','within','yr','also')) #remove our own words(common) from text
#cleanset <- tm_map(cleanset, gsub, pattern = 'stocks', replacement = 'stock') #to replace similar like words

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

#Barplot
w <- rowSums(tdm)
w
#y <- subset(w, w>30)
barplot(w, las = 2, col = rainbow(50))

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
          scale = c(1.7,0.6),
          rot.per = 0.5)

install.packages("wordcloud2")
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word','freq')
wordcloud2(w, size = 0.3, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

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
johnson <- read.delim("G:/Data Science/ExcelR/LM Portal/Assignments/Text Mining/johnson.txt", header = T)
reviews <- iconv(johnson$x, "ASCII", "UTF-8", sub = "")

#obtain sentiment analysis
s <- get_nrc_sentiment(reviews)
head(s)
reviews[1]
get_nrc_sentiment('expire')

#barplot
barplot(colSums(s), las=2, col = rainbow(10), ylab = 'Count', main = 'sentiment scores for Johnson Reviews' )
