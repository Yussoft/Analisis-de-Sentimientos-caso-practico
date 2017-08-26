################################################################################
#
# FEATURE EXTRACTION
#
# Description: Script with the implementation of several methods used in
# feature extraction and feature selection.
# Author: Jesús Sánchez de Castro
# Date: July 2017
#
################################################################################
library(tm)
library(SnowballC)
library(NLP)
library(openNLP)
library(stringr)
library(data.table)
library(RWeka)
require(textreuse)


options(mc.cores=4)

WordFrequency <- function(document.vector, sparsity = .999, mode){
  # Computes the frequency of each word in a document
  #
  # Args:
  #   dataframe: the one that it is going to be cleaned
  #
  # Returns:
  #   term-frequency dataframe
  
  # Construct the corpus
  #temp.corpus <- Corpus(VectorSource(document.vector))
  temp.corpus <- VectorSource(document.vector)
  
  temp.corpus <- Corpus(temp.corpus)

  # Construct term-frequency matrix and remove sparse terms
  if(mode==1){
    temp.tf <- DocumentTermMatrix(temp.corpus,
                                  control = list(stopwords = stopwords("SMART"),
                                                 stemming=TRUE, 
                                                 removePunctuation = TRUE,
                                                 removeNumbers = TRUE))
  } else if(mode == 2){
      temp.tf <- DocumentTermMatrix(temp.corpus,
                                  control = list(tokenize = BigramTokenizer,
                                                 stopwords = stopwords("SMART"), 
                                                 stemming=TRUE,
                                                 removePunctuation = TRUE, 
                                                 removeNumbers = TRUE))
  }

  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  docTerm.df <- as.data.frame(temp.tf)

  # construct word frequency df
  freq.df <- colMeans(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)

}


WordTFIDF <- function(document.vector, sparsity = .999, mode){
  # Computes term frequency - inverse document frequency, a numerical measure
  # which express how important a word is for a document in a colection.
  #
  # Args:
  #   dataframe: the one that it is going to be cleaned
  #
  # Returns:
  #   term-frequency dataframe
  
  
  # Construct the corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  
  if(mode == 1){
    # Construct term-frequency matrix and remove sparse terms
    temp.tf <- DocumentTermMatrix(temp.corpus,
                                  control = list(stopwords = stopwords("SMART"),
                                                 stemming=TRUE, 
                                                 removePunctuation = TRUE,
                                                 removeNumbers = TRUE,
                                                 weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  } else if(mode == 2){
    
    temp.tf <- DocumentTermMatrix(temp.corpus,
                                  control = list(tokenize = BigramTokenizer,
                                                 stopwords = stopwords("SMART"), 
                                                 stemming=TRUE, 
                                                 removePunctuation = TRUE, 
                                                 removeNumbers = TRUE,
                                                 weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  }
  
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  docTerm.df <- as.data.frame(temp.tf)
  
  # construct word frequency df
  freq.df <- colMeans(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  list(Freq = freq.df, Temp = docTerm.df)
}

BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
extractPOS <- function(x, POS.tag = "F") {
  # This function tags a given text 
  
  # Parse to String
  x <- as.String(x)
  
  # Annotate the sentence and words in the text
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  
  # Add the POS tags
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  a3 <- annotate(x, pos_tag_annotator, a2)
  
  # Filter the words
  a3w <- subset(a3,type=="word")
  tags <- sapply(a3w$features, '[[','POS')
  # If the default mode is on, add POSTags to every word in the text
  if(POS.tag=="F"){
    result <- (sprintf("%s-%s",x[a3w],tags))
    
    #list(sprintf("%s/%s", x[a3w],tags),tags)
  }
  # Else, just tag the required sintactic elements, e.g: nouns, adjectives..
  else{
    POS.index <- grep(POS.tag, tags)
    #result <- sprintf("%s-%s", x[a3w][POS.index],tags[POS.index])
    result <- x[a3w][POS.index]  }
  
  #untokenized.and.tagged <- paste(tokenized.and.tagged, collapse = " ")
  return(result)
}

WFNoStemming <- function(document.vector,sparsity=0.999){
  # Computes the frequency of each word in a document
  #
  # Args:
  #   dataframe: the one that it is going to be cleaned
  #
  # Returns:
  #   term-frequency dataframe
  
  # Construct the corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  
  # Construct term-frequency matrix and remove sparse terms
  
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords("SMART"),
                                               removePunctuation = TRUE,
                                               removeNumbers = TRUE,
                                               weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  docTerm.df <- as.data.frame(temp.tf)
  
  # construct word frequency df
  freq.df <- colMeans(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}


searchAdjNoun<- function(text, adj, nouns){
  
  candidates <- list()
  # This function searches adj-noun
  for(i in 1:length(adj)){
    
    # This sentence extract word+adj+word where adj belongs to the candidate
    # adjective list
    
    cad <- str_extract(text,paste0("\\w+\\s+",adj[i],"\\s+\\w+"))
    
    # POS tag the word+adj+word in order to know if the words are nouns
    tags <- extractPOS(cad)
    
    for(j in 1:length(tags)){
      
      #If there is a noun before the adjective
      w <- tags[1]
      pos <- getPosSep(w)
      tag <- substr(w,pos+1,nchar(w))
      
      # Case 1: adj+noun
      if(tag == "NN" | tag == "NNS" | tag == "NNP"){
        candidates[[length(candidates)+1]]<-substr(w,1,pos-1)
      }
      
      #If there is a noun after the adjective
      w <- tags[3]
      pos <- getPosSep(w)
      tag <- substr(w,pos+1,nchar(w))
      
      # Case 1: adj+noun
      if(tag == "NN" | tag == "NNS" | tag == "NNP"){
        candidates[[length(candidates)+1]]<-substr(w,1,pos-1)
      }
    }
  }
  
  return(unique(unlist(candidates)))
}


isStopWord <- function(word){
  
  n_w <- tolower(word)
  if(n_w%in%stopwords())
    return(TRUE)
  else 
    return(FALSE)
}




