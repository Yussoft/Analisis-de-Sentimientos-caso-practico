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
library(openNLP)

# Install the following libraries, remove #, copy and paste into R terminal

# install.packages(c("tm","SnowballC","NLP","openNLP","stringr","data.table",
#                    "RWeka","textreuse","openNLP"))

options(mc.cores=4)

BigramTokenizer <- function(x){
  # Splits the text into bigrams, pairs of words
  #
  # Args:
  #   x: The text.
  #
  # Returns:
  #   A vector of bigrams
  #
  # BigramTokenizer("Hello I am Yus") -> [1] "Hello I" "I am"    "am Yus" 
  unlist(lapply(ngrams(words(x), 2), paste, 
                collapse = " "),
         use.names = FALSE)
} 

WordFrequency <- function(documents, sparsity = .999, mode){
  # Computes the frequency of each word in a document
  #
  # Args:
  #  documents: the list of pieces of text or a complete text.
  #
  #  sparsity: sparsity refers to the threshold of relative document frequency 
  #  for a term, above which the term will be removed. .999 means that the terms
  #  that appear in at most 0.1% of the data will be removed
  #
  #  mode: choose 1 to extract frequency of unigrams, choose 2 for bigrams
  #
  # Returns:
  #   term-frequency dataframe 
  
  # Unigrams: remove stopwords, stemming, removePuntuation and numbers.
  if(mode==1){
    
    #Create the corpus
    corpus <- Corpus(VectorSource(documents))
    
    #Build documentTermMatrix
    dtm <- DocumentTermMatrix(corpus,
                                  control = list(stopwords = stopwords("SMART"),
                                  stemming=TRUE, 
                                  removePunctuation = TRUE,
                                  removeNumbers = TRUE))
  } 
  # Bigrams: use BigramTokenizer, remove stopwords, stemming, puntuation and 
  # numbers.
  else if(mode == 2){
    
    #Create the corpus
    corpus <- VCorpus(VectorSource(documents))
    
    #Build documentTermMatrix
    dtm <- DocumentTermMatrix(corpus,
                                  control = list(tokenize = BigramTokenizer,
                                  stopwords = stopwords("SMART"), 
                                  stemming=TRUE, 
                                  removePunctuation = TRUE, 
                                  removeNumbers = TRUE))
  }
  
  dtm <- removeSparseTerms(dtm, sparsity)
  dtm <- as.matrix(dtm)
  #freq.df will contain the tf of the terms
  freq.df <- colSums(dtm)
  freq.df <- data.frame(word = names(freq.df), tf = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}


WordTFIDF <- function(documents, sparsity = .999, mode){
  # Computes term frequency - inverse document frequency, a numerical measure
  # which express how important a word is for a document in a colection.
  #
  # Args:
  #   documents: the list of pieces of text or a complete text.
  #
  #  sparsity: sparsity refers to the threshold of relative document frequency 
  #  for a term, above which the term will be removed. .999 means that the terms
  #  that appear in at most 0.1% of the data will be removed
  #
  #  mode: choose 1 to extract frequency of unigrams, choose 2 for bigrams
  #
  # Returns:
  #   documentTermMatrix and frequency dataframe

  # Construct the corpus
  if(mode == 1){
    
    #Create the corpus
    corpus <- Corpus(VectorSource(documents))
    
    #Build documentTermMatrix
    dtm <- DocumentTermMatrix(corpus,
                                  control = list(stopwords = stopwords("SMART"),
                                                 stemming=TRUE, 
                                                 removePunctuation = TRUE,
                                                 removeNumbers = TRUE,
                    weighting = function(x) weightTfIdf(x, normalize = FALSE)))
    
    dtm <- removeSparseTerms(dtm, sparsity)
    dtm <- as.matrix(dtm)
    docTerm.df <- as.data.frame(dtm)
    # construct word frequency df
    freq.df <- colMeans(dtm)
    freq.df <- data.frame(word = names(freq.df), tfidf = freq.df)
    rownames(freq.df) <- NULL
    return(list(TFIDF= freq.df, DOC = docTerm.df))
    
  } else if(mode == 2){
    
    #Create the corpus
    corpus <- VCorpus(VectorSource(documents))
    
    #Build documentTermMatrix
    dtm <- DocumentTermMatrix(corpus,
                              control = list(tokenize = BigramTokenizer, 
                                             stopwords = stopwords("english"),
                                             stemming=TRUE, 
                                             removePunctuation = TRUE, 
                                             removeNumbers = TRUE))
    
    
    dtm <- removeSparseTerms(dtm, 0.999)
    dtm <- as.matrix(dtm)
    docTerm.df <- as.data.frame(dtm)
    # Term frequency 
    tf <- colMeans(dtm)
    
    # Check in how many docs the term occurs
    id<-function(col){sum(!col==0)}
    # Number of docs/ number documents where the term occurs
    idf <- log(nrow(dtm)/(apply(dtm, 2, id)))
    
    tfidf <- tf
    for(word in names(tf)){
      #term-frequency * inverse document frequency
      tfidf[word]<- tf[word]*idf[word]
    }
    
    freq.df <- data.frame(word = names(tf), tfidf = tfidf)
    rownames(freq.df) <- NULL
    return(list(TFIDF = freq.df, DOC = docTerm.df))    
  }
}

# Delete STOPWORDS bi-grams
deleteSWBigrams <- function(bigram){
  #print("Deleting stopwords bigrams.")
  numRows <- nrow(bigram)
  delRows <- c()
  for(i in 1:numRows){
    if(sapply(strsplit(as.character(bigram$word[i]), " "), length) == 2){
      word1 <- strsplit(as.character(bigram$word[i]), " ")[[1]][1]
      word2 <- strsplit(as.character(bigram$word[i]), " ")[[1]][2]

      if(word1 == "" | word2 == ""){
       # print(paste0("Word 1: ",word1,", Word 2: ",word2))
        delRows <- c(delRows, i)
      } else if(isStopWord(word1) & isStopWord(word2)){
        delRows <- c(delRows, i)
      }
    }
    else{
      delRows <- c(delRows, i)
    }
    
  }
  return(delRows)
}

extractPOS <- function(x, POS.tag = "F") {
  # Computes term frequency - inverse document frequency, a numerical measure
  # which express how important a word is for a document in a colection.
  #
  # Args:
  #   x: text that is goint to be tagged.
  #
  #   POST.tag: desired Tag. If tag is F, every words will be tagged, but if 
  #   the tag is one of the defined tags (NN - noun, JJ - adjective) only the 
  #   words with that word category will be returned
  #
  # Returns:
  #   tagged words
  
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
  }
  
  # Else, just tag the required word category, e.g: nouns, adjectives..
  else{
    POS.index <- grep(POS.tag, tags)
    result <- x[a3w][POS.index]  }
  
  return(result)
}

WFNoStemming <- function(documents,sparsity=0.999, mode=1){
  # Computes term frequency of the word in a document collection. There is no
  # stemming process in this function
  #
  # Args:
  #   documents: the list of pieces of text or a complete text.
  #
  #  sparsity: sparsity refers to the threshold of relative document frequency 
  #  for a term, above which the term will be removed. .999 means that the terms
  #  that appear in at most 0.1% of the data will be removed
  #
  #  mode: choose 1 to extract frequency of unigrams, choose 2 for bigrams
  #
  # Returns:
  #   term frequency dataframe
  
  # Construct term-frequency matrix and remove sparse terms
  if (mode == 1){
    
    # Construct the corpus
    corpus <- Corpus(VectorSource(documents))
    
    # Build the documentTermMatrix 
    dtm <- DocumentTermMatrix(corpus,
                                  control = list(stopwords = stopwords("SMART"),
                                                 removePunctuation = TRUE,
                                                 removeNumbers = TRUE))
  }
  else if (mode == 2){
    
    # Construct the corpus
    corpus <- VCorpus(VectorSource(documents))
    
    dtm <- DocumentTermMatrix(corpus,
                                  control = list(stopwords = stopwords("SMART"),
                                                 removePunctuation = TRUE,
                                                 removeNumbers = TRUE,
                     weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  }
  
  
  dtm <- removeSparseTerms(dtm, sparsity)
  dtm <- as.matrix(dtm)
  # construct word frequency df
  freq.df <- colMeans(dtm)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}

getPosSep<-function(char){
  # This function return the position of the hyphen (tag-word)
  # 
  # Args:
  #   char: pair of word-tag
  #
  # Returns:
  #   position of the hyphen
  return(str_locate_all(pattern ='-', char)[[1]][[1]])
}

getTag<-function(char,pos){
  # This function extracts the tag of the pair(word-tag)
  # 
  # Args:
  #   char: pair of word-tag
  #
  #   pos: position of the hyphen
  #
  # Returns:
  #   the tag e.g (museum-NN), NN is returned
  return(substr(char,pos+1,nchar(char)))
}

getWord<-function(char,pos){
  # This function extracts the word of the pair(tag-word)
  # 
  # Args:
  #   char: pair of tag-word (string)
  #
  #   pos: position of the hyphen
  #
  # Returns:
  #   the word e.g (museum-NN), museum is returned
  
  return(substr(char,1,pos-1))
}
searchAdjectives<- function(text, nouns){
  # This searches for adjectives next to nouns in a text
  # 
  # Args:
  #   text: text to be examined for adjectives
  #
  #   nouns: list of nouns near which may be adjectives
  #
  # Returns:
  #   list of adjectives found
  
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
  # This function searches for nouns next to adjectives in a text
  # 
  # Args:
  #   text: text to be examined for nouns
  #
  #   adjectives: list of adjectives near which may be nouns
  #
  # Returns:
  #   list of nouns found
  
  # noun list
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
  # This function chechs is a word is a stopword
  # 
  # Args:
  #   word: word to be checked
  #
  # Returns:
  #   true is the word is a stopword, false otherwise
  n_w <- tolower(word)
  if (n_w%in%stopwords())
    return(TRUE)
  else if (word == "im")
    return(TRUE)
  else if (word == "youre")
    return(TRUE)
  else if (word == "hes")
    return(TRUE)
  else if (word == "shes")
    return(TRUE)
  else if (word == "theyre")
    return(TRUE)
  else if (word == "theyve")
    return(TRUE)
  else if (word == "theyd")
    return(TRUE)
  else if (word == "isnt")
    return(TRUE)
  else if (word == "youd")
    return(TRUE)
  else if (word == "didnt")
    return(TRUE)
  else if (word == "dont")
    return(TRUE)
  else if (word == "wasnt")
    return(TRUE)
  else if (word == "arent")
    return(TRUE)
  else if (word == "werent")
    return(TRUE)
  else if (word == "hasnt")
    return(TRUE)
  else if (word == "havent")
    return(TRUE)
  else if (word == "hadnt")
    return(TRUE)
  else if (word == "doesnt")
    return(TRUE)
  else if (word == "wont")
    return(TRUE)
  else if (word == "wouldnt")
    return(TRUE)
  else if (word == "shouldnt")
    return(TRUE)
  else if (word == "cant")
    return(TRUE)
  else if (word == "lets")
    return(TRUE)
  else if (word == "thats")
    return(TRUE)
  else if (word == "couldnt")
    return(TRUE)
  else if (word == "mustnt")
    return(TRUE)
  else if (word == "whos")
    return(TRUE)
  else if (word == "whats")
    return(TRUE)
  else if (word == "heres")
    return(TRUE)
  else if (word == "theres")
    return(TRUE)
  else if (word == "whens")
    return(TRUE)
  else if (word == "wheres")
    return(TRUE)
  else if (word == "whys")
    return(TRUE)
  else if (word == "hows")
    return(TRUE)
  else 
    return(FALSE)
}
