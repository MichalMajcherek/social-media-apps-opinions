############################################################ SENTIMENT ###############################



########## TikTok #################
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
#Setting working directory
setwd("/Users/michal/Politechnika/Projekt/OpinionMining4") #!!!!!!!!!!CHANGE!!!!!!!!!
wd<-"/Users/michal/Politechnika/Projekt/OpinionMining4"#!!!!!!!!!!CHANGE!!!!!!!!!
dir(wd)
stopwords<-"/Users/michal/Politechnika/Projekt/stopwords.txt"
dir(stopwords)
#Read CSV Brand Database


Com<-read.csv("/Users/michal/Politechnika/Projekt/OpinionMining4/tiktok.csv", #!!!!!!!!!!CHANGE!!!!!!!!!
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

#Remove special characters
for (j in seq(Comments)) { # usuwa znaki charakterystyczna dla maili 
  Comments[[j]] <- gsub("/", " ", Comments[[j]])
  Comments[[j]] <- gsub("@", " ", Comments[[j]])
  Comments[[j]] <- gsub("–", " ", Comments[[j]])
  Comments[[j]] <- gsub("’", " ", Comments[[j]])
  Comments[[j]] <- gsub("“", " ", Comments[[j]])
  Comments[[j]] <- gsub("‘", " ", Comments[[j]])
  Comments[[j]] <- gsub(")", " ", Comments[[j]])
  Comments[[j]] <- gsub("”", " ", Comments[[j]])
}

writeLines(as.character(Comments [[1]]))

#Remove english stopwords
Comments <- tm_map(Comments , removeWords, stopwords("English"))

writeLines(as.character(Comments [[1]]))


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
write.csv(m, file="/Users/michal/Politechnika/Projekt/excel2_tik/DocumentTermMatrix.csv") #!!!!!!!!!!CHANGE!!!!!!!!!
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
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Topic1_comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!




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
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Topic2_comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!





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
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Topic3_comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!




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
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Topic4_comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!


#________________________________________________________________________

#____________________SENTIMENT ANALYSIS__________________________________








###############################################################################################

###################################### SENTIMENT_1st OPTION1 ##############
library("plyr")
library("stringr")

neg=scan("/Users/michal/Politechnika/Projekt/Lexicon/negative-words.txt", what="character", comment.char =";" )
pos=scan("/Users/michal/Politechnika/Projekt/Lexicon/positive-words.txt", what="character" ,comment.char =";" )

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
write.csv(m11, file="/Users/michal/Politechnika/Projekt/excel2_tik/Sent_1.csv") #!!!!!!!!!!CHANGE!!!!!!!!!

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
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Pos_Topic 1_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!

#___________Writing in CSV File_ with Negative Comments of Topic 1 _________

neg_docs_1_dataframe <- data.frame(text=neg1$Documents, stringsAsFactors=F) 
neg_docs_1_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Neg_Topic 1_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!

#___________Writing in CSV File_ with Neutral Comments of Topic 1 __________

neu_docs_1_dataframe <- data.frame(text=neu1$Documents, stringsAsFactors=F) 
neu_docs_1_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Neu_Topic 1_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!


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
write.csv(m22, file="/Users/michal/Politechnika/Projekt/excel2_tik/Sent_2.csv") #!!!!!!!!!!CHANGE!!!!!!!!!

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
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Pos_Topic 2_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!

#___________Writing in CSV File_ with Negative Comments of Topic 2 _________

neg_docs_2_dataframe <- data.frame(text=neg2$Documents, stringsAsFactors=F) 
neg_docs_2_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Neg_Topic 2_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!

#___________Writing in CSV File_ with Neutral Comments of Topic 2 __________

neu_docs_2_dataframe <- data.frame(text=neu2$Documents, stringsAsFactors=F) 
neu_docs_2_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Neu_Topic 2_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!



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
     main="Histogram for the Sentiment by Topic: User experience", 
     xlab="Scores",
     ylab="Probability",
     border="blue",
     col="grey",
     prob = TRUE
)
lines(density(m3$Score))
m33<-as.matrix(m3)
m33
write.csv(m33, file="/Users/michal/Politechnika/Projekt/excel2_tik/Sent_3.csv") #!!!!!!!!!!CHANGE!!!!!!!!!

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
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Pos_Topic 3_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!

#___________Writing in CSV File_ with Negative Comments of Topic 3 _________

neg_docs_3_dataframe <- data.frame(text=neg3$Documents, stringsAsFactors=F) 
neg_docs_3_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Neg_Topic 3_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!

#___________Writing in CSV File_ with Neutral Comments of Topic 3 __________

neu_docs_3_dataframe <- data.frame(text=neu3$Documents, stringsAsFactors=F) 
neu_docs_3_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Neu_Topic 3_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!


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
write.csv(m44, file="/Users/michal/Politechnika/Projekt/excel2_tik/Sent_4.csv") #!!!!!!!!!!CHANGE!!!!!!!!!

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
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Pos_Topic 4_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!

#___________Writing in CSV File_ with Negative Comments of Topic 4 _________

neg_docs_4_dataframe <- data.frame(text=neg4$Documents, stringsAsFactors=F) 
neg_docs_4_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Neg_Topic 4_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!

#___________Writing in CSV File_ with Neutral Comments of Topic 4 __________

neu_docs_4_dataframe <- data.frame(text=neu4$Documents, stringsAsFactors=F) 
neu_docs_4_dataframe
write.csv(mycorpus_dataframe,'/Users/michal/Politechnika/Projekt/excel2_tik/Neu_Topic 4_Comments.csv', row.names=FALSE) #!!!!!!!!!!CHANGE!!!!!!!!!




###############################################################################################





