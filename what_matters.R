#------------------------------------------------------------------------------#
#
# WHATMATTER SYSTEM: FEATURE EXTRACTION SYSTEM
#
# Description: Extract the features of a given text with an opinion
# Author: Jesús Sánchez de Castro, based on Henrique Siqueira and Flavia Barros'
# paper: http://sites.labic.icmc.usp.br/wti2010/IIIWTI_camera_ready/74769.pdf
# Date: Agosto 2017 
#
#------------------------------------------------------------------------------#

options(java.parameters = "- Xmx8192m")


# Load packages
library(data.table)
library(NLP)
library(openNLP)
library(beepr)
#Remember to change working directory
source("utils.R")
source("featureExtraction.R")

# gc()
# options(java.parameters = "-Xmx8g" )


debug_mode <- TRUE
dataset <- 2
# Load data
df <- LoadCSV(dataset,debug_mode,"CoreEng")

#df<- as.data.frame(df)
df.opinions <- df$titleopinion

# Compute tfidf and order words
df.freq <- WFNoStemming(df.opinions,sparsity = .999,mode=1)
df.freq <- df.freq[order(df.freq$freq, decreasing = TRUE),]


#-----------------------FIRST PART: FREQUENT NOUNS-----------------------------#
# First step of WhatMatter System: Extract the most frequent nouns. The top
# 3% of the nouns are considered frenquent

# apply the tagger to all the opinions and extract the NOUNS
tag <-"NN"
tagged <- lapply(df.freq,extractPOS,tag)
tagged <- strsplit(unlist(tagged),paste0("/",tag,sep=""))

# Remove some leftover characters from NNP, NNS tags
tagged <- lapply(tagged,function(x){
  gsub("P","",x)
})
tagged <- lapply(tagged,function(x){
  gsub("S","",x)
})
tagged <- lapply(tagged,function(x){
  gsub(" ","",x)
})

# In case a element of the list is empty ("") remove it
tagged <- lapply(tagged, function(x){
  x <- x[x!=""]
})
#print(tagged)

# i = nk
tagged <- unlist(tagged,use.names = FALSE)
threshold <- (length(tagged)*3)/100
threshold <- as.integer(threshold)+1

df.freq$word <- unlist(df.freq$word, use.names = FALSE)

# In tagged frequency we got the NOUNS and their frequencies
tagged.frequency <- df.freq[df.freq$word %in% tagged,]

# Filter the top 3%
top3.percent.nouns <- tagged.frequency[1:threshold,]

#------------------SECOND PART: ADJECTIVES AND RELEVANT NOUNS------------------#

# 2.1: get an adjective candidates list. For each word in tagged.frequency,
# search adjectives that are immediately after or before those nouns.

# 2.1.1: POS tagging

text <- df.opinions
#text <- tolower(text)
text <- removePunctuation(text)
text <- removeNumbers(text)

# s.tag is a list of lists of tagged words
s.tag <- lapply(text,extractPOS)

# Once the text has being processed, searchAdjectives looks for adjectives
# directly before or after a name, and returns a list.
top3.percent.nouns$word <- as.character(top3.percent.nouns$word)
adj.candidates<- unlist(sapply(s.tag,searchAdjectives, top3.percent.nouns$word))

# 2.2: find the adjectives from step 2.1 in the text and find new nouns
# inmediately before or after the name

# # SearchAdjNoun will add the new nouns to the top3 nouns list

noun.candidates <- unlist(sapply(s.tag,searchNouns,adj.candidates))
nouns <- unique(c(top3.percent.nouns$word,noun.candidates))
# nouns[63] <- "Figueres"
# nouns[length(nouns)+1]<- "Barcelona"

#------------------THIRD PART: MAPPING FEATURE INDICATORS----------------------#

# Feature Indicator candidates
# fic <- unlist(lapply(df.opinions,extractPOS,"JJ"))
# fic <- tolower(fic)
# fic <- unique(fic)
# fic <- sort(fic)
# write(x = fsc,file = "ficDali")

# fic <- list("cheap","expensive","crowded","overcrowded","price","spent","overpriced",
#             "handicap","impolite","waiting","expense")


fic<-list("expensive","noisy","early","packed","quickly","cheap","crowded",
          "overpriced","impolite")
# 
words <- unlist(c(nouns,fic))
words <- tolower(words)
words.stem <- wordStem(words)

#------------------------------------------------------------------------------#
# Calculate tfidf of positive and negative neviews
df.neg <- df$titleopinion[df$SentimentValue=="negative"]
df.pos <- df$titleopinion[df$SentimentValue=="positive"]

tfidf.neg <- WordTFIDF(df.neg,.999,1)$TFIDF
tfidf.pos <- WordTFIDF(df.pos,.999,1)$TFIDF

tfidf.pos <- as.data.table(tfidf.pos)
tfidf.pos <- tfidf.pos[order(tfidf, decreasing = TRUE),]
tfidf.neg <- as.data.table(tfidf.neg)
tfidf.neg <- tfidf.neg[order(tfidf, decreasing = TRUE),]

# take top 500 neg and pos words
if(nrow(tfidf.neg)>=500){
  tfidf.neg.top <- tfidf.neg[1:500,]
} else {
  tfidf.neg.top <- tfidf.neg[1:nrow(tfidf.neg),]
}

if(nrow(tfidf.pos)>=500){
  tfidf.pos.top <- tfidf.pos[1:500,]
} else {
  tfidf.pos.top <- tfidf.pos[1:nrow(tfidf.pos),]
}

top.words <- merge(tfidf.pos.top,tfidf.neg.top,by = "word", all=TRUE)
top.words <- top.words[order(tfidf.y,decreasing = TRUE),]

# interesting words
word.list <- c(as.character(top.words$word), words.stem)
word.list <- unique(word.list)

# Create features data.frame
df <- df[df$SentimentValue != "neutral",]

features <- WordTFIDF(df$titleopinion,.999,1)$DOC
features <- ifelse(features > 0, 1, 0)
features <- features[, colnames(features) %in% word.list]
features <- as.data.frame(features)

features <- features[ ,order(names(features))]

# Preparing the final set
df.features <- cbind(df, features)
df.features$pos <- NULL
df.features$neg <- NULL
df.features$X <- NULL

df.features <- df.features[df.features$SentimentCore!="neutral",]

SaveCSV(df.features,dataset,"WMFeatures")
beep(2)

