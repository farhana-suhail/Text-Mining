rm(list = ls()) #removes work history

install.packages("rJava")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RWeka")
install.packages("textir")
install.packages("igraph")
install.packages("qdap")
install.packages("maptpx")

library(rJava)
library(tm)
library(SnowballC)
library(wordcloud)
library(RWeka)
library(textir)
library(igraph)
library(qdap)
library(maptpx)

pos = scan(file.choose(),what="character",comment.char = ";")
neg = scan(file.choose(),what="character",comment.char = ";")

x = readLines(file.choose())  #read data from "amazon.txt"
length(x)    #check length
x1 = Corpus(VectorSource(x))   #constructs a source for a vector as input

x1 = tm_map(x1, stripWhitespace) #removes white space
x1 = tm_map(x1, tolower) #converts to lowercase
x1 = tm_map(x1, removePunctuation) #removes punctuation marks
x1 = tm_map(x1, removeNumbers) #removes numbers in the document
x1 = tm_map(x1, removeWords, c(stopwords('english')))
x1 = tm_map(x1, PlainTextDocument)

ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 3, max = 3))
tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram, 
                                              tolower = TRUE,
                                              removePunctuation = TRUE,
                                              stripWhitespace = TRUE,
                                              removeNumbers = TRUE,
                                              stopwords = TRUE,
                                              stemDocument = TRUE 
                                              )) #patience, takes a minute

#build term- document with different weights
tdm1 = TermDocumentMatrix(x1, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), stopwords = TRUE))  #replace i by I
tdm0 = TermDocumentMatrix(x1)  #gives regular TF weighing, Above gave TFIDF weighing
dim(tdmi)  #check its basic characteristics
a0 = NULL
for (i1 in 1:ncol(tdm0)) {
  if (sum(tdm0[,i1])==0) {a0 = c(a0,i1)
  }}
length(a0)
if(length(a0)>0) { tdm01 = tdm0[,-a0]} else {tdm01=tdm0};
inspect(tdm01[1:5,1:10])
dtm0 = t(tdm01)


makewordc()
