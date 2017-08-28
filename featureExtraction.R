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

BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
                                      use.names = FALSE)

WordFrequency <- function(document.vector, sparsity = .999, mode){
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
  freq.df <- colSums(temp.tf)
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
  
  word.tfidf <- function(document.vector, sparsity = .999){
    # construct corpus
    temp.corpus <- Corpus(VectorSource(document.vector))
    # construct tf matrix and remove sparse terms
    temp.tf <- DocumentTermMatrix(temp.corpus,
                                  control = list(tokenize = BigramTokenizer, 
                                                 stopwords = stopwords("SMART"),
                                                 stemming=TRUE, 
                                                 removePunctuation = TRUE, 
                                                 removeNumbers = TRUE))
    temp.tf <- removeSparseTerms(temp.tf, sparsity)
    temp.tf <- as.matrix(temp.tf)
    docTerm.df <- as.data.frame(temp.tf)
    # construct word frequency df
    freq.df <- colMeans(temp.tf)
    freq.df <- data.frame(word = names(freq.df), freq = freq.df)
    rownames(freq.df) <- NULL
    list(Freq = freq.df, Temp = docTerm.df)
  }
  
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
                                                 removeNumbers = TRUE))
    
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




WFNoStemming <- function(document.vector,sparsity=0.999, mode=1){
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
  if(mode==1){
    temp.tf <- DocumentTermMatrix(temp.corpus,
                                  control = list(stopwords = stopwords("SMART"),
                                                 removePunctuation = TRUE,
                                                 removeNumbers = TRUE))
  }
  else if(mode==2){
    temp.tf <- DocumentTermMatrix(temp.corpus,
                                  control = list(stopwords = stopwords("SMART"),
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
  return(freq.df)
}

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

searchNouns<- function(text, adjectives){
  
  # Adjective list
  candidates <- list()
  end_loop <- FALSE
  for(i in 1:length(text)){
    
    elem <- text[[i]]
    pos<- getPosSep(elem)
    tag<- getTag(elem,pos)
    
    # If the word is an adjective
    if(tag == "JJ" | tag == "JJS" | tag == "JJR"){
      adj<-getWord(elem,pos)
      
      # If the noun is in the list of interesting nouns
      if(adj%in%adjectives){  
        #print(paste0("Adjetivo: ",adj))
        
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
            } else if(new_tag == "NN" | new_tag == "NNS" | new_tag == "NNP"){
              
              # Add the new adjective to the list
              candidates[[length(candidates)+1]]<- new_word
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
            else if(new_tag == "NN" | new_tag == "NNS" | new_tag == "NNP"){
              
              # Add the new adjective to the list
              candidates[[length(candidates)+1]]<- new_word
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
  return(unique(unlist(candidates)))
}

isStopWord <- function(word){
  
  n_w <- tolower(word)
  if(n_w%in%stopwords())
    return(TRUE)
  else 
    return(FALSE)
}




