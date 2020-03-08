library(tm)
library(slam)
library(topicmodels)

x <- readLines("G:/Data Science/ExcelR/LM Portal/Assignments/modispeech.txt")
mydata.corpus <- Corpus(VectorSource(x))
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
inspect(mydata.corpus[1:5])
my_stopwords <- c(stopwords('english'), "I","This","then","went","now","Here","got","one","such","many",
                  "Not","even","also","And","give","When","time","make","To","new","need","needs","get","rid","can","whole","well","One",
                  "never","on","know","Each","types","like","much","make","reach","today","At","first","Then","Such","Truly","forms","The","going","Even","always",
                  "some","A","st","come","see","will","Make","given","For","where","every","Which","visit","just","All","take","last","needed")
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
mydata.corpus <- tm_map(mydata.corpus, removeNumbers)
inspect(mydata.corpus[1:10])
mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)
inspect(mydata.corpus[1:10])

#Build Term Document Matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
mydata.dtm3

dim(mydata.dtm3)

dtm <- as.DocumentTermMatrix(mydata.dtm3)
rowTotals <- apply(dtm, 1, sum)
dtm.new <- dtm[rowTotals>0,]
lda <- LDA(dtm.new,10) #find 10 topics
term <- terms(lda, 5) #first 5 terms of every topic
term

tops <- terms(lda)
tb <- table(names(tops), unlist(tops))
tb <- as.data.frame.matrix(tb)

cls <- hclust(dist(tb), method = 'ward.D2')
par(family = "HiraKaKuProN-w3")
plot(cls)

##structured Data Extraction##
library(rJava)
install.packages("NLP")
library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/",type = "source")
bio <- readLines("G:/Data Science/ExcelR/LM Portal/Assignments/modispeech.txt")
bio <- paste(bio, collapse = "")
#warnings()
bio <- as.String(bio)
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
bio_annotations <- annotate(bio, list(sent_ann, word_ann))
class(bio_annotations)
head(bio_annotations)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)
library(dplyr)
sents(bio_doc)%>%head(2)
words(bio_doc)%>%head(10)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
bio_annotations <- annotate(bio, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

entities <- function(doc, kind) {
  s <- doc$content
  a <- annotation(doc)
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}
entities(bio_doc, kind = "person")
entities(bio_doc, kind = "location")
entities(bio_doc, kind = "organization")


############ Predictions #############

install.packages("RTextTools")
library(RTextTools)
setRepositories()
setRepositories(ind = c(1:6, 8))
ap <- available.packages()

