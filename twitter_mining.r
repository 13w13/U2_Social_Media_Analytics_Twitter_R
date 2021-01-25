library(ROAuth)
library(RCurl)

#rm(list=ls())

# plotting and pipes - tidyverse!
# install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
# text mining library
#install.packages("tidytext")
library(tidytext)
#install.packages("maps")
library(maps)
#install.packages("ggmap")
library(ggmap)


library(rtweet)
library(twitteR)
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="C:\\Users\\antoi\\Desktop\\TBS\\UE2\\Social Media Analytics\\cours3\\test\\cacert.pem")

my.key <- "VjKpUNyCAQVnjvgyuPdOrp7Vs"
my.secret <- "LXv3XKqxBMEYSHUKAOAMKH8ASW7fmqrefkjn9CBimVDNO2mzsB"
access_token <- "1321220647624888322-M8mObeu1ANUAi7HOXqfo5ZSk9voOQ7"
access_secret <- "u9El5vMwXMMk32VGxhyQuCDfbe08aagaHUNOIbSCyRDM6"

t <- create_token("FirstApp6934",
                  my.key,
                  my.secret,
                  access_token,
                  access_secret 
                  )


setup_twitter_oauth(my.key, my.secret, access_token, access_secret)


library(rtweet)
#twitter_token <- create_token(app = "FirstApp6934", 
#                              consumer_key = my.key, 
#                              consumer_secret = my.secret,
#                              set_renv = FALSE)


#search_tweets("rstats", token = twitter_token)

bigdata <- searchTwitter("#macron", n=150)

bigdata

?search_tweets

?search_tweets2

#Macrondata <- se

Macrondata <- search_tweets("Macron OR MACRON OR EMMANUEL MACRON OR Emmanuel Macron OR French President", n=3000, token=t)

View(Macrondata)

#if is not working, go to file windows and remove http_ouath and then accept the connexion on chrome (twitter app)


?search_tweets

#Macrondata<-searchTwitter("Macron OR MACRON OR EMMANUEL MACRON OR Emmanuel 
                          #Macron OR French President OR Président Français",
                          #n=100)

#View(Macrondata)

head(Macrondata, n = 2)

length(unique(Macrondata$location))
#where the followers of emmanuel macron come from 

Macrondata$location2<-iconv(Macrondata$location,
                            to = "ASCII", sub="")

Macrondata$location2[Macrondata$location2==""] <- NA
Macrondata$location2[Macrondata$location2==", "] <- NA

Macrondata %>%count(location2, sort=TRUE) %>%
  mutate(location2=reorder(location2,n)) %>%
  na.omit()%>% top_n(10)%>%ggplot(aes(x=location2,y=n))+
  geom_bar(stat="identity")+geom_col()+coord_flip() +
  labs(x = "Location", y = "Count",
       title = "Twitter users - unique locations ")+
  theme_light()

#library("ggplot2")
## plot time series of tweets
ts_plot(Macrondata, "hours")+
  ggplot2::theme_minimal()+
  ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))+
  ggplot2::labs(x=NULL,y=NULL,
                title="Frequency of Macron Twitter statuses",
                subtitle="Twitter status counts 1-hour intervals",
                caption="\nSource: Data collected from Twitter's API"
  )

library("tm")
library("stringi")
library("stringr")

usableText <- iconv(Macrondata$text, to = "ASCII", sub="")

Macrondata_corpus<-Corpus(VectorSource(usableText))

Macrondata_corpus<-tm_map(Macrondata_corpus,
                          tolower)
Macrondata_corpus<-tm_map(Macrondata_corpus,
                          removePunctuation)
Macrondata_corpus<-tm_map(Macrondata_corpus,
                          removeNumbers)
Macrondata_corpus<-tm_map(Macrondata_corpus,
                          function(x)removeWords(x,
                                                 stopwords("en")))

Macrondata_corpus<-tm_map(Macrondata_corpus,
                          function(x)removeWords(x,
                                                 stopwords("french")))
Macrondata_corpus<-tm_map(Macrondata_corpus,
                          function(x)removeWords(x,
                                                 stopwords("italian")))
Macrondata_corpus<-tm_map(Macrondata_corpus,
                          function(x)removeWords(x,
                                                 stopwords("spanish")))

Macrondata_corpus<-tm_map(Macrondata_corpus,
           function(x)removeWords(x,
                                  c("prsident", "franais", "emmanuel", "french", "macron", "france")))

library("wordcloud")
text_corpus <- tm_map(Macrondata_corpus,
                      content_transformer(function(x)
                        iconv(x,to='ASCII',sub='byte')))
# The document-term matrix
Macrondata.tdm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(Macrondata.tdm)
m[1:5,1:10]

v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 5)

barplot(d[1:20,]$freq, las = 3,
        names.arg = d[1:20,]$word,col ="lightblue",
        main ="Most frequent words",
        ylab = "Word frequencies") 

findFreqTerms(Macrondata.tdm, lowfreq=50)[1:10]

wordcloud(words = d$word, freq = d$freq, min.freq = 40,
          max.words=100, random.order=FALSE,
          colors=brewer.pal(4,"Dark2"))

#How to proceed with different language words ?
#Translate our tweets into our langage. First before 
#any transformations

Macrondata.tdm<-removeSparseTerms(Macrondata.tdm,
                                  sparse=0.95)

Macrondata.df <- as.data.frame(as.matrix(Macrondata.tdm))

View(Macrondata.df)

Macrondata.df.scale <- scale(Macrondata.df)


#A word is know seen as a vector 
#We are going to compute the distance between 2 words
#Hierarichical clustering - dendogram  

Macrondata.dist <- dist(Macrondata.df.scale,
                        method = "euclidean")

Macrondata.fit<-hclust(Macrondata.dist, method="ward.D2")

plot(Macrondata.fit, main="Cluster-Macron") 


#choose 5 clusters to (hope) have the good numbers of numbers 
#per group 
groups <- cutree(Macrondata.fit, k=5) 
plot(Macrondata.fit, main="Cluster-Macron")
rect.hclust(Macrondata.fit, k=5, border="red")



#we want to know if there is a kind of links within the htags 
#behavior of followers
#relationship between followers tweets 
#a kind of path, a kind of map, a kind of topics

tags<-function(x) toupper(grep("#",strsplit(x,
                                            " +")[[1]],value=TRUE))

l <- nrow(Macrondata)
taglist <- vector(mode = "list", l)

texts <- vector(mode = "character", length = l)


#extract all the tweet paragraphs

for (i in 1:l) texts[i] <- Macrondata$text[i]
texts <- iconv(texts, to = "ASCII", sub="")


# ... and populate it - extract only the # text
j<-0
for(i in 1:l){
  if(is.na(str_match(texts[i],"#"))[1,1]==FALSE){
    j<-j+1
    taglist[[j]]<-str_squish(removePunctuation(tags(ifelse(is.na(str_match(texts[i], "[\n]")[1,1])==TRUE,texts[i],gsub("[\n]"," ",texts[i])))))
  }
}
alltags <- NULL
for (i in 1:l) alltags<-union(alltags,taglist[[i]])

library(igraph)
hash.graph <- graph.empty(directed = T)
# Populate it with nodes
hash.graph <- hash.graph + vertices(alltags)


for (tags in taglist){
  if (length(tags)>1){ #2 hastags appearing in the same tweet
    for (pair in combn(length(tags),2,simplify=FALSE,
                       FUN=function(x) sort(tags[x]))){
      if (pair[1]!=pair[2]) {
        if (hash.graph[pair[1],pair[2]]==0)
          hash.graph<-hash.graph+edge(pair[1],
                                      pair[2])
      }
    }
  }
}

V(hash.graph)$color <- "black"
E(hash.graph)$color <- "black"
V(hash.graph)$name <- paste("#",V(hash.graph)$name,
                            sep = "")
V(hash.graph)$label.cex = 0.75
V(hash.graph)$size <- 20
V(hash.graph)$size2 <- 2
hash.graph_simple<-delete.vertices(simplify(hash.graph),
                                   degree(hash.graph)<=5)

plot(hash.graph_simple, edge.width = 2,
     edge.color = "black", vertex.color = "SkyBlue2",
     vertex.frame.color="black", label.color = "black",
     vertex.label.font=2, edge.arrow.size=0.5)

###################################################

library("sentimentr")

plain.text<-vector()
for(i in 1:dim(Macrondata)[1]){
  plain.text[i]<-Macrondata_corpus[[i]][[1]]
}
sentence_sentiment<-sentiment(get_sentences(plain.text))
sentence_sentiment

average_sentiment<-mean(sentence_sentiment$sentiment)
average_sentiment

sd_sentiment<-sd(sentence_sentiment$sentiment)
sd_sentiment

#CI=[average-1.96*sd/sqrt(sample size); average+1.96*sd/sqrt(sample size)]

average_sentiment - 1.96*sd_sentiment/sqrt(3000)
average_sentiment + 1.96*sd_sentiment/sqrt(3000)

extract_sentiment_terms(get_sentences(plain.text))

library(tidytext)
library(topicmodels)
library(tidyverse)
library(rvest)
library(reshape2)

text_corpus2<-text_corpus[1:200]

doc.lengths<-rowSums(as.matrix(DocumentTermMatrix(text_corpus2)))

#doc.lengths
#DocumentTermMatrix(text_corpus2)

dtm <- DocumentTermMatrix(text_corpus2[doc.lengths > 0])


#Latence Diriged Allocation (LDA) #bayses formula ? (not exactly the same) 

# Pick a random seed for replication
SEED = sample(1:1000000, 1)
# Let's start with 2 topics
k = 2
Topics_results<-LDA(dtm, k = k, control = list(seed = SEED))

terms(Topics_results,15)

topics(Topics_results)

tidy_model_beta<-tidy(Topics_results, matrix = "beta")


tidy_model_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta),beta,fill=factor(topic)))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(x = "Topic",
       y = "beta score",
       title = "Topic modeling")


