########################
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)
library(fpc)


getwd()
setwd("/Users/michal/Politechnika/Projekt/text/")
getwd()
wd<-"/Users/michal/Politechnika/Projekt/text/"
dir(wd)
docs <- Corpus(DirSource(wd))
docs
writeLines(as.character(docs[[1]]))
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers) 
writeLines(as.character(docs[[1]]))
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("–", " ", docs[[j]])
  docs[[j]] <- gsub("’", " ", docs[[j]])
  docs[[j]] <- gsub("“", " ", docs[[j]])
  docs[[j]] <- gsub("‘", " ", docs[[j]])
  docs[[j]] <- gsub(")", " ", docs[[j]])
  docs[[j]] <- gsub("”", " ", docs[[j]])
}
writeLines(as.character(docs[[1]]))
docs <- tm_map(docs, tolower)
writeLines(as.character(docs[[1]]))
length(stopwords("english"))
stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("English"))
writeLines(as.character(docs[[1]]))

StW<-read.table("/Users/michal/Politechnika/Projekt/stopwords.txt") 
StW
StWW<-as.character(StW$V1)
StWW

docs <- tm_map(docs, removeWords, StWW)
writeLines(as.character(docs[[1]]))
docs <- tm_map(docs, stripWhitespace)
writeLines(as.character(docs[[1]]))

library(SnowballC)
stemDocument("modelling", language = "english") 
stemDocument("modeller", language = "english") 
stemDocument("models", language = "english")


for (j in seq(docs)) { 
  docs[[j]]<-stemDocument(docs[[j]], language = "english")
}
writeLines(as.character(docs[[1]]))

dtm <- DocumentTermMatrix(docs) 
dtm

dtm <- DocumentTermMatrix(docs)

filenames <- list.files(getwd(),pattern="*.txt") 
filenames <-c(filenames)
filenames 
rownames(dtm) 
rownames(dtm)<-filenames



tdm <- t(dtm)
tdm <- TermDocumentMatrix(docs) 
tdm




dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),bounds = list(global = c(2,Inf)))) 
filenames <- list.files(getwd(),pattern="*.txt")

filenames <-c(filenames) 
rownames(dtmr )<-filenames

dtmr1 = removeSparseTerms(dtmr, 0.70)
filenames <- list.files(getwd(),pattern="*.txt") 
filenames <-c(filenames)
rownames(dtmr1 )<-filenames

dtmr

dtmr1

m0 <- as.matrix(dtm)

write.csv(m0, file="/Users/michal/Politechnika/Projekt/cluster/DocumentTermMatrix.csv")
m1 <- as.matrix(dtmr)
write.csv(m1, file="/Users/michal/Politechnika/Projekt/cluster/DocumentTermMatrix_1.csv")
m2 <- as.matrix(dtmr1)
write.csv(m2, file="/Users/michal/Politechnika/Projekt/cluster/SparseDocumentTermMatrix.csv")



######################## Clustering of Topics ######################## 
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)
library(fpc)


MyData <-read.csv("/Users/michal/Politechnika/Projekt/cluster/feat.csv",
                  header = TRUE, #are there column names in 1st row?
                  sep = ";", #USE COMA 
                  strip.white = TRUE, #strip out extra white space in strings.
                  fill = TRUE, #fill in rows that have unequal numbers of columns
                  comment.char = "#", #character used for comments that should not be read in
                  stringsAsFactors = FALSE #Another control for deciding whether characters should be converted to factor
)
dtm1 = as.data.frame.matrix(MyData)
dtm1
dtm1 [1:5, ] #1 - docs 2 - term
asd<-dtm1[, 1:3] # select [1:5, 1:2] [ 'all' ,1:2]
#dtm<-dtm1[,-1]
#dtm
asd
#rownames(dtm)
#rownames(dtm)<-asd




filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
filenames
#rownames(dtm)
#rownames(dtm)<-filenames
#filenames


#rownames(asd)
#rownames(asd)<-filenames


rownames(asd) <- c("Facebook", "Instagram", "LinkedIn", "TikTok", "Twitter")
################## EUCLIDIAN ###################### K-MEANS
d1 <- dist(asd, method="euclidian")
kfit <- kmeans(d1, 3) 
kfit 
clusplot(as.matrix(d1), kfit$cluster, color=T, shade=T, labels=2, lines=0, main = "Features")

#plot(MyData$Stripes - MyData$Seeds, ylab = "YYY", xlab = "XXX", main = "Data")

