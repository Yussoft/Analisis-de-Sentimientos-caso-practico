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

getPosSep<-function(char){
  return(str_locate_all(pattern ='-', char)[[1]][[1]])
}

getTag<-function(char,pos){
  return(substr(char,pos+1,nchar(char)))
}

getWord<-function(char,pos){
  return(substr(char,1,pos-1))
}
searchAdjectives<- function(text, nouns){
  
  # Adjective list
  adj <- list()
  end_loop <- FALSE
  for(i in 1:length(text)){
    
    elem <- text[[i]]
    pos<- getPosSep(elem)
    tag<- getTag(elem,pos)
    
    # If the word is a noun
    if(tag == "NN" | tag == "NNS" | tag == "NNP"){
      noun<-getWord(elem,pos)

      # If the noun is in the list of interesting nouns
      if(noun%in%nouns){  
        print(paste0("Nombre: ",noun))
        
        k <- 1
        end_loop <- FALSE
        # Check for adjectives before noun
        while(end_loop == FALSE){
          # Get previous tag and check
          if(i-k > 1){
            
            elem_n <- text[[i-k]]
            pos <- getPosSep(elem_n)
            size <- nchar(elem_n)
            new_tag <- getTag(elem_n,pos)
            new_word <- getWord(elem_n,pos)
          
            if(isStopWord(new_word) | new_tag == "IN"){
              k<-k+1
            } else if(new_tag == "JJ" | new_tag == "JJS" | new_tag == "JJR"){

              # Add the new adjective to the list
              adj[[length(adj)+1]]<- new_word
              end_loop <- TRUE
            } else {
              end_loop <- TRUE
            }
          }
          else {
            end_loop <- TRUE
          }
        }
        k <- 1
        end_loop <- FALSE
        # Check for adjectives before noun
        while(end_loop == FALSE){
          # Get previous tag and check
          if(i+k <= length(text)){
            elem_n <- text[[i+k]]
            pos <- getPosSep(elem_n)
            size <- nchar(elem_n)
            new_tag <- getTag(elem_n,pos)
            new_word <- getWord(elem_n,pos)

            if(isStopWord(new_word) | new_tag == "IN"){
              k<-k+1
            }
            else if(new_tag == "JJ" | new_tag == "JJS" | new_tag == "JJR"){

              # Add the new adjective to the list
              adj[[length(adj)+1]]<- new_word
              end_loop <- TRUE
            }
            else {
              end_loop <- TRUE
            }
          }
          else {
            end_loop <- TRUE
          }
        }
      }
    }
  } # End for 
  return(adj)
}

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
