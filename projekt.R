#install.packages("tm")
#install.packages("SnowballC")
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

############################################################
getwd()
library(tm) 
setwd("/Users/michal/Politechnika/Projekt/text")
getwd() 
wd<-"/Users/michal/Politechnika/Projekt/text" 
dir(wd)
library(tm)
docs <- Corpus(DirSource(wd))
docs
writeLines(as.character(docs[[1]]))
dtm <- DocumentTermMatrix(docs) #11599 words dtm, 7379 terms
dtm

#pre-processing
docs <- tm_map(docs,removePunctuation) 
docs <- tm_map(docs, removeNumbers) 
#docs <- tm_map(docs, PlainTextDocument) # writeLines(as.character(docs[[1]]))
for (j in seq(docs)) 
  { 
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

docs <- tm_map(docs, removeWords, stopwords("English")) 
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

docs <- tm_map(docs, stripWhitespace) 
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
inspect(dtm[1:5, 1:5]) 

#nadaje nazwy dokumentom, wczesniej było tylko character(0) 
filenames <- list.files(getwd(),pattern="*.csv")
filenames <-c(filenames)
filenames # checking the filenames length
rownames(dtm) # checking the number of rows in dtm 
rownames(dtm)<-filenames
#inspect(dtm[1:5, 1:5])

tdm <- t(dtm) #DTM ->TDM
#inspect(dtm[8:10, 11:15])  
#inspect(tdm[11:15,8:10])


############################################################

freq <- colSums(as.matrix(dtm)) #zliczamy ile kolumn i mamy ilosc słów 
length(freq) 

ord <- order(freq,decreasing=TRUE) 
freq[head(ord)] #najczęstsze słowa 
freq[tail(ord)] #najrzadsze slowa


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


######################### wordcloud #########################

library(wordcloud) 
#set.seed(42) 
#wordcloud(names(freq),freq)

#set.seed(142)
#wordcloud(names(freq), freq, max.words=10)
#wordcloud(names(freq), freq, min.freq=20,colors=brewer.pal(6, "Dark2"))

set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, random.order = FALSE, colors=dark2)


######################### clustering ######################
#hierarchical

#install.packages("igraph") 
library("tm") 
library("igraph")


#tworzenie tf_idf
tdm<- t(dtm) # t(dtm) – transpose matrix DTM into TDM 
tf <- as.matrix(tdm)
idf <- log(ncol(tf) / (rowSums(tf != 0)))
tf[1:5,1:5]
idf[1:5]
idf1 <- diag(idf)
idf1[1:5,1:5]
tf_idf <- crossprod(tf, idf1)
tf_idf
colnames(tf_idf) <- rownames(tf)
colnames(tf_idf)
tf_idf


########## EXCEL ######## 

m <- as.matrix(dtmr)
write.csv(m, file="/Users/michal/Politechnika/Projekt/excel/DocumentTermMatrix.csv")
m1 <- as.matrix(dtmr1)
write.csv(m1, file="/Users/michal/Politechnika/Projekt/excel/SparseDocumentTermMatrix.csv")

MyData <-read.csv("/Users/michal/Politechnika/Projekt/excel/DocumentTermMatrix.csv",
                  header = TRUE, #are there column names in 1st row?
                  sep = ",", #what separates rows?
                  strip.white = TRUE, #strip out extra white space in strings.
                  fill = TRUE, #fill in rows that have unequal numbers of columns
                  comment.char = "#", #character used for comments that should not be read in
                  stringsAsFactors = FALSE #Another control for deciding whether characters should be converted to factor 
                  )

MyData

#cosine similarity

dtm
mm_s = as.matrix(tf_idf) 
mm<-as.matrix(mm_s[1:5,]) 
cosineSim <- function(x){ #cosinus between docs
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
  }
cs <- cosineSim(mm)
cs 
write.csv(as.matrix(cs),file="/Users/michal/Politechnika/Projekt/excel/DocumentCosine.csv")

min_cos<-0.02 #tworzymy macierz, w której cos < 0.02 jest 0
cs[cs < min_cos] <- 0 
cs <- round(cs,3) 

write.csv(as.matrix(cs),file="/Users/michal/Politechnika/Projekt/excel/DocumentAdjacencyMatrix.csv") 
cs

#filenames <- list.files(getwd(),pattern="*.csv") 
#filenames <-c(filenames[1:5])
#filenames
#cs

#rownames(mm1)<-filenames 
#cs<-as.matrix(mm1)
#cs

dat<-read.csv("/Users/michal/Politechnika/Projekt/excel/DocumentAdjacencyMatrix.csv", header = TRUE,
              sep = ",",
              colClasses = NA,
              na.string = "NA",
              skip = 0,
              strip.white = TRUE,
              fill = TRUE, comment.char = "#", stringsAsFactors = FALSE )
mm1 = as.data.frame.matrix(dat) 
mm1=mm1[,-1]

filenames <- list.files(getwd(),pattern="*.txt") 
filenames <-c(filenames[1:5])
filenames
cs

rownames(mm1)<-filenames 
cs<-as.matrix(mm1)
cs

mm1

library(igraph) 
g=graph.adjacency(cs,mode="undirected",weighted=TRUE)
g
list.vertex.attributes(g)
list.vertex.attributes(g)

list.edge.attributes(g) 
V(g)$name 
E(g)$weight

filenames <- list.files(getwd(),pattern="*.csv") 
filenames <-c(filenames)
filenames
rownames(dtm) 
rownames(dtm)<-filenames

d1 <- dist(cs, method="euclidian")
# make the clustering
fit <- hclust(d=d1, method="complete") 
fit
plot.new()
plot(fit, hang=-1, cex=0.5)

#group
groups <- cutree(fit, k=3) # "k" defines the number of clusters you are using 
rect.hclust(fit, k=3, border="red")



############################################################ SENTIMENT ###############################



########## Facebook #################
###Corpus Preprocessing
#Start for preprocessing
#Loading (already installed) packages
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
#Setting working directory
setwd("/Users/michal/Politechnika/Projekt/OpinionMining")
wd<-"/Users/michal/Politechnika/Projekt/OpinionMining"
dir(wd)
stopwords<-"/Users/michal/Politechnika/Projekt/stopwords.txt"
dir(stopwords)
#Read CSV Brand Database


Com<-read.csv("/Users/michal/Politechnika/Projekt/OpinionMining/facebook.csv", #!!!!!!!!!!CHANGE!!!!!!!!!
              header = TRUE,
              sep = "\t",
              strip.white = TRUE,
              fill = TRUE, comment.char = "#", 
              stringsAsFactors = FALSE
)

head(Com)
#Com
#Matrix from Brand Database
ComDF = as.data.frame.matrix(Com)
Comments1 <- ComDF [,1]
Comments1
Comments <- VCorpus(VectorSource(Comments1)) 
Comments
writeLines(as.character(Comments [[1]]))
#Get only comments column
getTransformations()
#Remove Punctuation and Numbers
Comments <- tm_map(Comments , tolower) 
writeLines(as.character(Comments [[1]]))
Comments <- tm_map(Comments ,removePunctuation)

writeLines(as.character(Comments [[1]]))
Comments <- tm_map(Comments , removeNumbers) 
writeLines(as.character(Comments [[1]]))
#Comments <- tm_map(Comments , PlainTextDocument) 
#writeLines(as.character(Comments [[1]]))

#Remove special characters
for (j in seq(Comments)) { # usuwa znaki charakterystyczna dla maili 
  Comments[[j]] <- gsub("/", " ", Comments[[j]])
  Comments[[j]] <- gsub("@", " ", Comments[[j]])
  Comments[[j]] <- gsub("–", " ", Comments[[j]])
  Comments[[j]] <- gsub("’", " ", Comments[[j]])
  Comments[[j]] <- gsub("“", " ", Comments[[j]])
  #Comments[[j]] <- gsub("...", " ", Comments[[j]])
  Comments[[j]] <- gsub("‘", " ", Comments[[j]])
  Comments[[j]] <- gsub(")", " ", Comments[[j]])
  Comments[[j]] <- gsub("”", " ", Comments[[j]])
}

#Comments <- tm_map(Comments , PlainTextDocument) 
writeLines(as.character(Comments [[1]]))

#Remove english stopwords
Comments <- tm_map(Comments , removeWords, stopwords("English"))
#Comments <- tm_map(Comments , PlainTextDocument) 
writeLines(as.character(Comments [[1]]))
#stopwords("English")

#Remove Special Stopwords
StW <- read.table("/Users/michal/Politechnika/Projekt/stopwords.txt") 
StWW <- as.character(StW$V1)
StWW

Comments <- tm_map(Comments , removeWords, StWW)
Comments <- tm_map(Comments , PlainTextDocument) #bez tego nie działa DTM COMMENTS
writeLines(as.character(Comments [[1]]))

#Remove withespace
for (j in seq(Comments)) {
  Comments [[j]]<-stemDocument(Comments [[j]], language = "english") }
Comments
writeLines(as.character(Comments [[1]]))

dtm <- DocumentTermMatrix(Comments)
#inspect(dtm[1:1,10:100])

m <- as.matrix(dtm)
write.csv(m, file="/Users/michal/Politechnika/Projekt/excel2/DocumentTermMatrix.csv") #!!!!!!!!!!CHANGE!!!!!!!!!
dtm
library(tm)
library(topicmodels)
library(lsa)
library(scatterplot3d)
library(ggplot2)

#check the presence of rows with a zero's sum
raw.sum=apply(dtm,1,FUN=sum) #sum by raw for each raw of the table
raw.sum

#number of rows with a zero's sum
mmm<-nrow(dtm[raw.sum==0,])
mmm

# if mmm=0, only create dtm2 and NN (number of rows in DTM)
# if mmm>0, delete the rows with zero's sum form corpus

if (mmm==0) {
  dtm2<-dtm
  NN<-nrow(dtm2)
  NN
} else {
  dtm2<-dtm[raw.sum!=0,]
  NN<-nrow(dtm2)
}
 NN
 
 

######### wordcloud ###############
 freq <- colSums(as.matrix(dtm)) #zliczamy ile kolumn i mamy ilosc słów
 length(freq) 
 library(wordcloud)
 set.seed(142)
 dark2 <- brewer.pal(6, "Dark2")
 wordcloud(names(freq), freq, max.words=100, rot.per=0.2, random.order=FALSE, colors=dark2)

 
####################### LDA Topic Modelling ######################
 burnin <- 4000
 iter <- 2000
 thin <- 500
 seed <-list(2003,5,63,100001,765)
 nstart <- 5
 best <- TRUE
 k <- 4

ldaOut <-LDA(dtm2, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

ldaOut.terms <- as.matrix(terms(ldaOut,5)) 
ldaOut.terms

ldaOut.topics <- as.matrix(topics(ldaOut)) 
ldaOut.topics



####################### Rows numbering ######################
nrow(ldaOut.topics) #komentarze mają konkretny numer i przydzielony numer tematu 
Comment<-seq(1, NN, by=1)
Comment
wf=data.frame(Comment=Comment, Topics=ldaOut.topics)
wf



####################### Builing SubCorpus of Topic 1 ######################
topic1<-wf[wf[2] == 1,] #find Topic 1
topic1$Comment
#number of comments with Topic 1 
kk1<-nrow(topic1)
kk1

kk<-nrow(dtm2)
kk
list1<-c()
i=1

while(i<=kk) {
  if (wf[i,2]==1) {     #find Topic 1
    list1<-c(list1,i)
    }
  i=i+1 
}
list1 

wf1=NULL

for (i in 1:kk) {
  for (j in 1:kk1) {
    if (i==list1[j]){
      c <- data.frame(file=as.character(wf[list1[j],1]),document=as.character(Comments[[i]]))
      wf1=rbind(wf1,c)
    }
  }
}
wf1

wf1$document[1]

####################### Corpus Creating ######################
Topic_1_docs <- Corpus(VectorSource(as.character(wf1$document)))
writeLines(as.character(Topic_1_docs[[1]])) 


####################### Writing in CSV File_ with Comments of Topic 1######################
mycorpus_dataframe <- data.frame(text=wf1$document, stringsAsFactors=F)
mycorpus_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Topic1_comments.csv', row.names=FALSE)




####################### Builing SubCorpus of Topic 2 ######################

topic2<-wf[wf[2] == 2,] #find Topic 2
topic2$Comment
#number of comments with Topic 2 
kk2<-nrow(topic2)
kk2

kk<-nrow(dtm2)
kk
list2<-c()
i=1

while(i<=kk) {
  if (wf[i,2]==2) {     #find Topic 2
    list2<-c(list2,i)
  }
  i=i+1 
}
list2 

wf2=NULL

for (i in 1:kk) {
  for (j in 1:kk2) {
    if (i==list2[j]){
      c <- data.frame(file=as.character(wf[list2[j],1]),document=as.character(Comments[[i]]))
      wf2=rbind(wf2,c)
    }
  }
}
wf2

wf2$document[1]

####################### Corpus Creating ######################
Topic_2_docs <- Corpus(VectorSource(as.character(wf2$document)))
writeLines(as.character(Topic_2_docs[[1]])) #ERROR ERROR ERROR ERROR


####################### Writing in CSV File_ with Comments of Topic 2 ######################
mycorpus_dataframe <- data.frame(text=wf2$document, stringsAsFactors=F)
mycorpus_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Topic2_comments.csv', row.names=FALSE)





####################### Builing SubCorpus of Topic 3 ######################

topic3<-wf[wf[2] == 3,] #find Topic 3
topic3$Comment
#number of comments with Topic 3 
kk3<-nrow(topic3)
kk3

kk<-nrow(dtm2)
kk
list3<-c()
i=1

while(i<=kk) {
  if (wf[i,2]==3) {     #find Topic 3
    list3<-c(list3,i)
  }
  i=i+1 
}
list3 

wf3=NULL

for (i in 1:kk) {
  for (j in 1:kk3) {
    if (i==list3[j]){
      c <- data.frame(file=as.character(wf[list3[j],1]),document=as.character(Comments[[i]]))
      wf3=rbind(wf3,c)
    }
  }
}
wf3

wf3$document[1]

####################### Corpus Creating ######################
Topic_3_docs <- Corpus(VectorSource(as.character(wf3$document)))
writeLines(as.character(Topic_3_docs[[1]])) 


####################### Writing in CSV File_ with Comments of Topic 3 ######################
mycorpus_dataframe <- data.frame(text=wf3$document, stringsAsFactors=F)
mycorpus_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Topic3_comments.csv', row.names=FALSE)




####################### Builing SubCorpus of Topic 4 ######################

topic4<-wf[wf[2] == 4,] #find Topic 4
topic4$Comment
#number of comments with Topic 4 
kk4<-nrow(topic4)
kk4

kk<-nrow(dtm2)
kk
list4<-c()
i=1

while(i<=kk) {
  if (wf[i,2]==4) {     #find Topic 4
    list4<-c(list4,i)
  }
  i=i+1 
}
list4 

wf4=NULL

for (i in 1:kk) {
  for (j in 1:kk4) {
    if (i==list4[j]){
      c <- data.frame(file=as.character(wf[list4[j],1]),document=as.character(Comments[[i]]))
      wf4=rbind(wf4,c)
    }
  }
}
wf4

wf4$document[1]

####################### Corpus Creating ######################
Topic_4_docs <- Corpus(VectorSource(as.character(wf4$document)))
writeLines(as.character(Topic_4_docs[[1]]))


####################### Writing in CSV File_ with Comments of Topic 4 ######################
mycorpus_dataframe <- data.frame(text=wf4$document, stringsAsFactors=F)
mycorpus_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Topic4_comments.csv', row.names=FALSE)


#________________________________________________________________________

#____________________SENTIMENT ANALYSIS__________________________________








###############################################################################################

###################################### SENTIMENT_1st OPTION1 ##############
library("plyr")
library("stringr")

neg=scan("/Users/michal/Politechnika/Projekt/Lexicon/negative-words.txt", what="character", comment.char =";" )
pos=scan("/Users/michal/Politechnika/Projekt/Lexicon/positive-words2.txt", what="character" ,comment.char =";" )

#__________Initialization of the Sentiment analysis Procedure_______________
score.sentiment = function(docs, pos.words, neg.words, .progress='none')
  
{
  
scores = laply(docs_s, function(docs, pos.words, neg.words) 
  {
    
    word.list = str_split(docs, '\\s+')
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms 
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum(): 
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress ) 
  
  scores.df = data.frame(score=scores, text=docs) 
  return(scores.df)
  
  }

####################### Topic 1 Sentiment Scoring ####################### 
# COPY IT   
result=c()
docs<-Topic_1_docs #replace it from 2 to 5 Topic
m1=c()
for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  print(docs_s)
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text) 
  #print(newRow1)
  m1<- rbind(m1,newRow1)
  #print(m1)
}
m1


#______________________Statistics______________________
summary(m1$Score)

####################### Topic 1___Histograms ######################## 

#______________________Histogram_1_____________________
hist(m1$Score,
     main="Histogram for the Sentiment by Topic: Application", 
     xlab="Scores",
     ylab="Number of of Opinions",
     border="blue",
     col="grey",
)

#______________________Histogram_2_____________________
hist(m1$Score,
     main="Histogram for the Sentiment by Topic: Application", 
     xlab="Scores",
     ylab="Probability",
     border="blue",
     col="grey",
     prob = TRUE
)
lines(density(m1$Score))
m11<-as.matrix(m1)
m11
write.csv(m11, file="/Users/michal/Politechnika/Projekt/excel2/Sent_1.csv")

####################### Division of opinions ######################### 

#______________________Topic 1_____________________
#______________________Positive - score >=1_____________________

pos1<-m1[m1$Score>=1,]
pos1$Documents
pos1$Score
length(pos1$Score)

#______________________Neutral - score <1 and >=0_____________________

neu1<-m1[(m1$Score<1)&(m1$Score>=0),]
neu1$Documents
neu1$Score
length(neu1$Score)

#______________________Negative - score <0_____________________

neg1<-m1[m1$Score<0,]
neg1$Score
length(neg1$Score)
neg1$Documents


#____________Topic 1: List of Positive, Neutral and Negative opinions_______

pos_docs_1 <- Corpus(VectorSource(pos1$Documents)) 
pos_docs_1
neu_docs_1 <- Corpus(VectorSource(neu1$Documents)) 
neu_docs_1
neg_docs_1 <- Corpus(VectorSource(neg1$Documents)) 
neg_docs_1

#___________Writing in CSV File_ with Positive Comments of Topic 1 _________

pos_docs_1_dataframe <- data.frame(text=pos1$Documents, stringsAsFactors=F) 
pos_docs_1_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Pos_Topic 1_Comments.csv', row.names=FALSE)

#___________Writing in CSV File_ with Negative Comments of Topic 1 _________

neg_docs_1_dataframe <- data.frame(text=neg1$Documents, stringsAsFactors=F) 
neg_docs_1_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Neg_Topic 1_Comments.csv', row.names=FALSE)

#___________Writing in CSV File_ with Neutral Comments of Topic 1 __________

neu_docs_1_dataframe <- data.frame(text=neu1$Documents, stringsAsFactors=F) 
neu_docs_1_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Neu_Topic 1_Comments.csv', row.names=FALSE)


####################### OPTION2 ####################### 

library(syuzhet)
mycorpus_dataframe1<- data.frame(text=Comments1, stringsAsFactors=F) 
mycorpus_dataframe1

usableText=str_replace_all(mycorpus_dataframe1$text,"[^[:graph:]]", " ")

d<-get_nrc_sentiment(usableText)
head(d)
d$anger

t(d)
td<-data.frame(t(d))
td[,5]

td_new <- data.frame(rowSums(td))
td_new

names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new) 
rownames(td_new) <- NULL
td_new
td_new2<-td_new[1:10,]
td_new2

#Visualisation
library("ggplot2")
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("Opinion sentiments")


###############################################################################################

####################### Topic 2 Sentiment Scoring ####################### 
# COPY IT   
result=c()
docs<-Topic_2_docs #replace it from 2 to 5 Topic
m2=c()
for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  print(docs_s)
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text) 
  #print(newRow1)
  m2<- rbind(m2,newRow1)
  #print(m1)
}
m2


#______________________Statistics______________________
summary(m2$Score)

####################### Topic 2___Histograms ######################## 

#______________________Histogram_1_____________________
hist(m2$Score,
     main="Histogram for the Sentiment by Topic: Community", 
     xlab="Scores",
     ylab="Number of of Opinions",
     border="blue",
     col="grey",
)

#______________________Histogram_2_____________________
hist(m2$Score,
     main="Histogram for the Sentiment by Topic: Community", 
     xlab="Scores",
     ylab="Probability",
     border="blue",
     col="grey",
     prob = TRUE
)
lines(density(m2$Score))
m22<-as.matrix(m2)
m22
write.csv(m22, file="/Users/michal/Politechnika/Projekt/excel2/Sent_2.csv")

####################### Division of opinions ######################### 

#______________________Topic 2_____________________
#______________________Positive - score >=1_____________________

pos2<-m2[m2$Score>=1,]
pos2$Documents
pos2$Score
length(pos2$Score)

#______________________Neutral - score <1 and >=0_____________________

neu2<-m2[(m2$Score<1)&(m2$Score>=0),]
neu2$Documents
neu2$Score
length(neu2$Score)

#______________________Negative - score <0_____________________

neg2<-m2[m2$Score<0,]
neg2$Score
length(neg2$Score)
neg2$Documents


#____________Topic 2: List of Positive, Neutral and Negative opinions_______

pos_docs_2 <- Corpus(VectorSource(pos2$Documents)) 
pos_docs_2
neu_docs_2 <- Corpus(VectorSource(neu2$Documents)) 
neu_docs_2
neg_docs_2 <- Corpus(VectorSource(neg2$Documents)) 
neg_docs_2

#___________Writing in CSV File_ with Positive Comments of Topic 2 _________

pos_docs_2_dataframe <- data.frame(text=pos2$Documents, stringsAsFactors=F) 
pos_docs_2_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Pos_Topic 2_Comments.csv', row.names=FALSE)

#___________Writing in CSV File_ with Negative Comments of Topic 2 _________

neg_docs_2_dataframe <- data.frame(text=neg2$Documents, stringsAsFactors=F) 
neg_docs_2_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Neg_Topic 2_Comments.csv', row.names=FALSE)

#___________Writing in CSV File_ with Neutral Comments of Topic 2 __________

neu_docs_2_dataframe <- data.frame(text=neu2$Documents, stringsAsFactors=F) 
neu_docs_2_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Neu_Topic 2_Comments.csv', row.names=FALSE)



###############################################################################################


####################### Topic 3 Sentiment Scoring ####################### 
# COPY IT   
result=c()
docs<-Topic_3_docs #replace it from 2 to 5 Topic
m3=c()
for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  print(docs_s)
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text) 
  #print(newRow1)
  m3<- rbind(m3,newRow1)
  #print(m1)
}
m3


#______________________Statistics______________________
summary(m3$Score)

####################### Topic 3___Histograms ######################## 

#______________________Histogram_1_____________________
hist(m3$Score,
     main="Histogram for the Sentiment by Topic: User experience", 
     xlab="Scores",
     ylab="Number of of Opinions",
     border="blue",
     col="grey",
)

#______________________Histogram_2_____________________
hist(m3$Score,
     main="Histogram for the Sentiment by Topic User experience", 
     xlab="Scores",
     ylab="Probability",
     border="blue",
     col="grey",
     prob = TRUE
)
lines(density(m3$Score))
m33<-as.matrix(m3)
m33
write.csv(m33, file="/Users/michal/Politechnika/Projekt/excel2/Sent_3.csv")

####################### Division of opinions ######################### 

#______________________Topic 3_____________________
#______________________Positive - score >=1_____________________

pos3<-m3[m3$Score>=1,]
pos3$Documents
pos3$Score
length(pos3$Score)

#______________________Neutral - score <1 and >=0_____________________

neu3<-m2[(m3$Score<1)&(m3$Score>=0),]
neu3$Documents
neu3$Score
length(neu3$Score)

#______________________Negative - score <0_____________________

neg3<-m3[m3$Score<0,]
neg3$Score
length(neg3$Score)
neg3$Documents


#____________Topic 3: List of Positive, Neutral and Negative opinions_______

pos_docs_3 <- Corpus(VectorSource(pos3$Documents)) 
pos_docs_3
neu_docs_3 <- Corpus(VectorSource(neu3$Documents)) 
neu_docs_3
neg_docs_3 <- Corpus(VectorSource(neg3$Documents)) 
neg_docs_3

#___________Writing in CSV File_ with Positive Comments of Topic 3 _________

pos_docs_3_dataframe <- data.frame(text=pos3$Documents, stringsAsFactors=F) 
pos_docs_3_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Pos_Topic 3_Comments.csv', row.names=FALSE)

#___________Writing in CSV File_ with Negative Comments of Topic 3 _________

neg_docs_3_dataframe <- data.frame(text=neg3$Documents, stringsAsFactors=F) 
neg_docs_3_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Neg_Topic 3_Comments.csv', row.names=FALSE)

#___________Writing in CSV File_ with Neutral Comments of Topic 3 __________

neu_docs_3_dataframe <- data.frame(text=neu3$Documents, stringsAsFactors=F) 
neu_docs_3_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Neu_Topic 3_Comments.csv', row.names=FALSE)


###############################################################################################
####################### Topic 4 Sentiment Scoring ####################### 
# COPY IT   
result=c()
docs<-Topic_4_docs #replace it from 2 to 5 Topic
m4=c()
for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  print(docs_s)
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text) 
  #print(newRow1)
  m4<- rbind(m4,newRow1)
  #print(m1)
}
m4


#______________________Statistics______________________
summary(m4$Score)

####################### Topic 4___Histograms ######################## 

#______________________Histogram_1_____________________
hist(m4$Score,
     main="Histogram for the Sentiment by Topic: Features", 
     xlab="Scores",
     ylab="Number of of Opinions",
     border="blue",
     col="grey",
)

#______________________Histogram_2_____________________
hist(m4$Score,
     main="Histogram for the Sentiment by Topic: Features", 
     xlab="Scores",
     ylab="Probability",
     border="blue",
     col="grey",
     prob = TRUE
)
lines(density(m4$Score))
m44<-as.matrix(m4)
m44
write.csv(m44, file="/Users/michal/Politechnika/Projekt/excel2/Sent_4.csv")

####################### Division of opinions ######################### 

#______________________Topic 4_____________________
#______________________Positive - score >=1_____________________

pos4<-m4[m4$Score>=1,]
pos4$Documents
pos4$Score
length(pos4$Score)

#______________________Neutral - score <1 and >=0_____________________

neu4<-m4[(m4$Score<1)&(m4$Score>=0),]
neu4$Documents
neu4$Score
length(neu4$Score)

#______________________Negative - score <0_____________________

neg4<-m4[m4$Score<0,]
neg4$Score
length(neg4$Score)
neg4$Documents


#____________Topic 4: List of Positive, Neutral and Negative opinions_______

pos_docs_4 <- Corpus(VectorSource(pos4$Documents)) 
pos_docs_4
neu_docs_4 <- Corpus(VectorSource(neu4$Documents)) 
neu_docs_4
neg_docs_4 <- Corpus(VectorSource(neg4$Documents)) 
neg_docs_4

#___________Writing in CSV File_ with Positive Comments of Topic 4 _________

pos_docs_4_dataframe <- data.frame(text=pos4$Documents, stringsAsFactors=F) 
pos_docs_4_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Pos_Topic 4_Comments.csv', row.names=FALSE)

#___________Writing in CSV File_ with Negative Comments of Topic 4 _________

neg_docs_4_dataframe <- data.frame(text=neg4$Documents, stringsAsFactors=F) 
neg_docs_4_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Neg_Topic 4_Comments.csv', row.names=FALSE)

#___________Writing in CSV File_ with Neutral Comments of Topic 4 __________

neu_docs_4_dataframe <- data.frame(text=neu4$Documents, stringsAsFactors=F) 
neu_docs_4_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2/Neu_Topic 4_Comments.csv', row.names=FALSE)



###############################################################################################





