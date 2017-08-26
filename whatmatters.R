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

# Load data
df <- LoadCSV(4,debug_mode)

#df<- as.data.frame(df)
df.opinions <- df$titleopinion
df.opinions <- df.opinions[1:20]
rm(df)

# Compute tfidf and order words
df.freq <- WFNoStemming(df.opinions)
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
threshold <- (length(tagged)*10)/100
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

# Remove repeted adjectives
adj.candidates <- unique(adj.candidates)

# # 2.2: find the adjectives from step 2.1 in the text and find new nouns
# # inmediately before or after the name
# 
# # In order to find nouns and adjectives easier the stopwords are removed
# text <- rmSW(text)
# 
# # SearchAdjNoun will add the new nouns to the top3 nouns list
# candidates <- searchAdjNoun(text,adj.candidates,top3.percent.nouns)
# 
# nouns <- unique(c(top3.percent.nouns$word,candidates))
# 
# 
# #------------------THIRD PART: MAPPING FEATURE INDICATORS----------------------#
# 
# # Feature Indicator candidates
# # fic <- unlist(lapply(df.opinions,extractPOS,"JJ"))
# # fic <- tolower(fic)
# # fic <- unique(fic)
# # fic <- sort(fic)
# # write(x = fic,file = "ficDali")
# 
# fic <- list("cheap","expensive","crowded","overcrowded","price","spent","overpriced",
#             "handicap","impolite","waiting","expense")
# 
# #------------------------------------------------------------------------------#
# 
