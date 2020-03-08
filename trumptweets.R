install.packages("twitteR")
library(twitteR)

install.packages("ROAuth")
library(ROAuth)

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

cred <- OAuthFactory$new(consumerKey='NLRFL5EbpEKnrIOY4Jb458hXh',
                         consumerSecret='OcaPeNWgjheF2UP09ukVOZ90xTBX7GN7sYJ8pQ7KIEEJBIMwFE',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
save(cred, file = "twitter authentication.Rdata")
load("twitter authentication.Rdata")

setup_twitter_oauth("NLRFL5EbpEKnrIOY4Jb458hXh",
                    "OcaPeNWgjheF2UP09ukVOZ90xTBX7GN7sYJ8pQ7KIEEJBIMwFE",
                    "50555492-uLBOXmWauzhPWH3zosHAGvgcBiLWdvmpZSTJyMcXi",
                    "EqlfLCROpqTSNxQTOnLyKhnO4FIXCh7XpVNkyTTBNYbx4")

#Tweets <- userTimeline('narendramodi', n = 1000)
#TweetsDF <- twListToDF(Tweets)

tweets <- userTimeline('realDonaldTrump', n = 1000)
tweetsDF <- twListToDF(tweets)
write.csv(tweetsDF,"tweetstrump.csv")
getwd()

#Read file
trump <- read.csv("G:/Data Science/ExcelR/LM Portal/Assignments/Text Mining/tweetstrump.csv", header = T)
str(trump)

#Build Corpus
library(tm)
corpus <- iconv(trump$text, "ASCII", "UTF-8", sub = "")
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

cleanset <- tm_map(cleanset, removeWords, c('ny','know','now','made','done','well','need','us')) #remove our own words(common) from text
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
y <- subset(w, w>=3)
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
          scale = c(1.7,0.6),
          rot.per = 0.5)

install.packages("wordcloud2")
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word','freq')
wordcloud2(w, size = 0.3, shape = 'star', rotateRatio = 0.5, minSize = 1)

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
trump <- read.csv("G:/Data Science/ExcelR/LM Portal/Assignments/Text Mining/tweetstrump.csv", header = T)
tweets <- iconv(trump$text, "ASCII", "UTF-8", sub = "")

#obtain sentiment analysis
s <- get_nrc_sentiment(tweets)
head(s)
tweets[3]
get_nrc_sentiment('defeat')

#barplot
barplot(colSums(s), las=2, col = rainbow(10), ylab = 'Count', main = 'sentiment scores for Trump tweets' )
