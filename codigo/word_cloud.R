#------------------------------------------------------------------------------#
#
# Author: Jes�s S�nchez de Castro
# Impired by: Ana Valdivia
# Date: September 2017
#
#                       WORD CLOUD GRAPHIC
#
#------------------------------------------------------------------------------#
# Loading libraries
library(RXKCD)
library(wordcloud)
library(RColorBrewer)

source("utils.R")

# MUSEUMS:
# N�1 : Prado Museum: 1230 pages
# N�2 : Tyssen Museum: 380 pages
# N�3 : Reina sofia : 340 pages
# N�4 : Dali: 140 pages
# N�5 : Guggenheim: 400 pages

#Change path to match it with your pc's
pc.path="D:/TFG-/Data/"

for(i in 5:5){
  
  name <- getDatasetName(i)
  print(name)
  neg.unigram <- read.csv(paste0(pc.path,name,"/",name,"wordNegCloud.csv"))
  pos.unigram <- read.csv(paste0(pc.path,name,"/",name,"wordPosCloud.csv"))
  neg.bigram <- read.csv(paste0(pc.path,name,"/",name,"wordNegCloudB.csv"))
  pos.bigram <-read.csv(paste0(pc.path,name,"/",name,"wordPosCloudB.csv"))
  
  # Word clouds:
  
  # Negative unigrams
  wordcloud(neg.unigram$word, neg.unigram$tfidf, max.words=100,
            scale=c(2.5,0.1), random.order=FALSE, rot.per=0.15,
            use.r.layout=FALSE, colors=brewer.pal(6, "Set1"))

  # Positive unigrams
  wordcloud(pos.unigram$word, pos.unigram$tfidf, max.words=100,
            scale=c(2.5,0.1), random.order=FALSE,
            rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(6, "Dark2"))

  # Negative Bigrams
  wordcloud(neg.bigram$word, neg.bigram$tfidf, max.words=100,
            scale=c(2.5,0.1), random.order=FALSE,
            rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(6, "Set1"))

  # Positive Bigrams
  wordcloud(pos.bigram$word, pos.bigram$tfidf, max.words=100,
            scale=c(2.5,0.1), random.order=FALSE,
            rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(6, "Dark2"))
}


