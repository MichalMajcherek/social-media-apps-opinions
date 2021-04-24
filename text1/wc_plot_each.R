getwd()
library(tm) 
setwd("/Users/michal/Politechnika/Projekt/text1")
getwd() 
wd<-"/Users/michal/Politechnika/Projekt/text1" 
dir(wd)
library(tm)
docs <- Corpus(DirSource(wd))
docs
writeLines(as.character(docs[[1]]))
dtm <- DocumentTermMatrix(docs) #11599 words dtm, 7379 terms
dtm

#pre-processing
docs <- tm_map(docs,removePunctuation) #usuwa znaki interpunkcyjne 
docs <- tm_map(docs, removeNumbers) #usuwa liczby
#docs <- tm_map(docs, PlainTextDocument) # writeLines(as.character(docs[[1]]))
for (j in seq(docs)) 
{ # usuwa znaki charakterystyczna dla maili 
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("–", " ", docs[[j]])
  docs[[j]] <- gsub("’", " ", docs[[j]]) 
  docs[[j]] <- gsub("“", " ", docs[[j]]) 
  #docs[[j]] <- gsub("...", " ", docs[[j]])  #error occurs
  docs[[j]] <- gsub("‘", " ", docs[[j]]) 
  docs[[j]] <- gsub(")", " ", docs[[j]]) 
  docs[[j]] <- gsub("”", " ", docs[[j]]) 
}
#docs <- tm_map(docs, PlainTextDocument) 
writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, tolower) #capital to lower
#docs <- tm_map(docs, PlainTextDocument) 
writeLines(as.character(docs[[1]]))

length(stopwords("english"))
stopwords("english")

docs <- tm_map(docs, removeWords, stopwords("English")) #usuwa stopwords 
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

#stopwords
StW<-read.table("/Users/michal/Politechnika/Projekt/stopwords.txt") 
StW
StWW<-as.character(StW$V1)
StWW
docs <- tm_map(docs, removeWords, StWW)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, stripWhitespace) #usuwa puste przestrzenie 
#docs <- tm_map(docs, PlainTextDocument) 
writeLines(as.character(docs[[1]]))

for (j in seq(docs)) { 
  docs[[j]]<-stemDocument(docs[[j]], language = "english")
}
#docs <- tm_map(docs, PlainTextDocument) 
writeLines(as.character(docs[[1]]))

# ile słów po pre-processing
dtm <- DocumentTermMatrix(docs) #3299 terms
dtm


############################################################


dtm <- DocumentTermMatrix(docs) #macierz 1.5
dtm
inspect(dtm[1:1, 1:5]) #pokazuje wszystkie słowa w którym sa dokumencie i ile razy

#nadaje nazwy dokumentom, wczesniej było tylko character(0) 
filenames <- list.files(getwd(),pattern="*.csv")
filenames <-c(filenames)
filenames # checking the filenames length
rownames(dtm) # checking the number of rows in dtm 
rownames(dtm)<-filenames
#inspect(dtm[1:5, 1:5])

tdm <- t(dtm) #transpozycja macierzy dtm tdm
#inspect(dtm[8:10, 11:15]) #sprawdzanie obu macierzy 
#inspect(tdm[11:15,8:10])


############################################################

freq <- colSums(as.matrix(dtm)) #zliczamy ile kolumn i mamy ilosc słów 
length(freq) 

ord <- order(freq,decreasing=TRUE) 
freq[head(ord)] #najczęstsze słowa 
freq[tail(ord)] #najrzadsze slowa

#zatrzymaliśmy słowa ktore wystepuja co najmniej w dwoch dokumentach #zatrzymalismy slowa o dlugosci 4-20 liter
dtmr <-DocumentTermMatrix(docs, control=list(bounds = list(global = c(2,Inf)))) 
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20)))

dtmr
filenames <- list.files(getwd(),pattern="*.csv") 
filenames <-c(filenames)
rownames(dtmr )<-filenames

#usuwa nieistotne slowa
dtmr1 = removeSparseTerms(dtmr, 0.70) 
filenames <- list.files(getwd(),pattern="*.csv") 
filenames <-c(filenames)
rownames(dtmr1 )<-filenames

dtmr
dtmr1

freqr <- colSums(as.matrix(dtmr)) #wystepowanie w slow w dok (ich ilosc) length(freqr)
freq <- sort(freqr, decreasing=TRUE)
head(freq, 14)
tail(freq, 14) #najrzadziej wystepujace slowa i ile razy sie pojawiaja

findFreqTerms(dtmr,lowfreq=5) #slowa ktore pojawiaja sie nie mniej niz 5 razy


######################### plot #########################
freqr <- colSums(as.matrix(dtmr)) #Zipf's distribution
length(freqr)
freq <- sort(freqr, decreasing=TRUE)
mk<-min(head(freq, 30))
mk
wf=data.frame(word=names(freq),freq=freq)
library(ggplot2)
p <- ggplot(subset(wf, freq>mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

library(wordcloud) 
#set.seed(42) 
#wordcloud(names(freq),freq)

#set.seed(142)
#wordcloud(names(freq), freq, max.words=10)
#wordcloud(names(freq), freq, min.freq=20,colors=brewer.pal(6, "Dark2"))

set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, random.order = FALSE, colors=dark2)
