# Load packages
library(tm)
library(SnowballC)
library(data.table)
require(textreuse)


# Remember to change working directory
# Load data

# Nº1 : Prado Museum: 1230 pages
# Nº2 : Tyssen Museum: 380 pages
# Nº3 : Reina sofia : 340 pages
# Nº4 : Dali: 140 pages
# Nº5 : City of art and science: 210 pages


TripAdvisor <- read.csv("D:/TFG-/Data/dali/daliCOREENG.csv",stringsAsFactors = FALSE)
TripAdvisor <- TripAdvisor[-c(1,2)]

BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
                                      use.names = FALSE)

word.freq <- function(document.vector, sparsity = .999){
  # construct corpus
  
  temp.corpus <- VCorpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  

  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(
                                               stopwords = stopwords("SMART"), 
                                               stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE))
  
 
  temp.tf <- removeSparseTerms(temp.tf, sparsity)

  
  temp.tf <- as.matrix(temp.tf)
  # # construct word frequency df
  freq.df <- colSums(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}

#print(BigramTokenizer(TripAdvisor$titleopinion[1]))
word.freq.pos <- word.freq(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "positive"])
word.freq.neg <- word.freq(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "negative"])
# word.freq.neutral <- word.freq(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "neutral"])
