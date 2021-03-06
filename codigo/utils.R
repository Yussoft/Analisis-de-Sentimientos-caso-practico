#------------------------------------------------------------------------------#
#                                                                              #
#  UTILS.R                                                                     #
#                                                                              #
# Description: R Script with utility functions used a lot for this project.    #
# Author: Jes�s S�nchez de Castro                                              #
# Date: July 2017                                                              #
#                                                                              #
#------------------------------------------------------------------------------#
library(beepr)

getPcPath <- function(){
  
  return ("D:/TFG-/Data/")
}

getDatasetName <- function(dataset){

  ds.name <- ""
  
  if(is.numeric(dataset)){
    if(dataset == 1){
      ds.name <- "prado"
    } else if(dataset == 2){
      ds.name <- "tyssen"
    } else if(dataset == 3){
      ds.name <- "reina"
    } else if(dataset == 4){
      ds.name <- "dali"
    } else if(dataset == 5){
      ds.name <- "guggen"
    } else {
      ds.name <- "guggen"
    }
  } else {
    ds.name = "guggen"
  }
  return(ds.name)
}
LoadCSV <- function (dataset = 5, verbose = FALSE, name = ""){
  # Reads a csv file and returns a dataframe
  #
  # Args:
  #   file: file.csv that is going to be read
  #   verbose: If TRUE, prints the possible files to load, if not, not. 
  #
  # N�1 : Prado Museum: 1230 pages
  # N�2 : Tyssen Museum: 380 pages
  # N�3 : Reina sofia : 340 pages
  # N�4 : Dali: 140 pages
  # N�5 : Guggenheim Bilbao: 400 pages
  #
  # Returns:
  #   data frame with the data from the csv file
  ds.name <- getDatasetName(dataset)

  #Change path to match it with your pc's
  pc.path <- getPcPath()
  
  # If no name, load the basic CSV, the data from the web
  if(name=="")
    path <- paste0(pc.path,ds.name,"/",ds.name,"ENG.csv")
  else if(name=="UnigramFeatures")
    path <- paste0(pc.path,ds.name,"/Unigram/",ds.name,name,".csv")
  else if(name=="BigramFeatures")
    path <- paste0(pc.path,ds.name,"/Bigram/",ds.name,name,".csv")
  
  # file name is: data+name+.csv e.g: pradoCORENG.csv for the data+CoreNLP
  # sentiment
  else 
    path <- paste0(pc.path,ds.name,"/",ds.name,name,".csv")
  
  # Load the data into a data.frame
  data.f <- read.csv(path,stringsAsFactors=FALSE)
  
  # This should not be necesary of read.csv has stringAsFactors=FALSE
  # data.f$titleopinion <- as.character(data.f$titleopinion)
  
  # Remove some HTML tags left in the opinions
  data.f$titleopinion <- removeHTMLTags(data.f)
  
  if(verbose) print(paste0(path,". CSV read sucessfully"))
  return(data.f)
}


SaveCSV <- function(dataframe, dataset = 4, name){
  # Saves a dataframe into a csv file
  #
  # Args:
  #   dataframe: the data that is going to be saved 
  #   dataset: data set name, e.g. dali
  #   file.name: e.g negativeFreq
  #
  # Returns:
  #   nothing
  
  ds.name <- getDatasetName(dataset)
  # Build the path 
  pc.path <- "D:/TFG-/Data/"
  
  path <- paste0(pc.path,ds.name,"/",ds.name,name,".CSV")
  
  write.csv(dataframe,path,row.names = FALSE)
}

removeHTMLTags <- function(dataframe){
  # Cleans some leftover HTML tags from the web scraping process
  #
  # Args:
  #   dataframe: the one that it is going to be cleaned
  #
  # Returns:
  #   nothing
  
  dataframe$titleopinion <- gsub("<br>"," ", dataframe$titleopinion)
  dataframe$titleopinion <- gsub("<p>"," ", dataframe$titleopinion)
  dataframe$titleopinion <- gsub("</p>"," ", dataframe$titleopinion)
  dataframe$titleopinion <- gsub("<&amp;>"," ", dataframe$titleopinion)
  dataframe$titleopinion <- gsub("\n"," ", dataframe$titleopinion)
  dataframe$titleopinion <- gsub('\"'," ", dataframe$titleopinion)

}

strDF <- function(dataset=4,name=""){
  df <- LoadCSV(dataset, verbose = FALSE,name)
  dsname <- getDatasetName(dataset)

  print(paste0("Dataset name: ",dsname,name))
  print(paste0("nRows: ",dim(df)[1]))
  print(paste0("nCols: ",dim(df)[2]))
  print(table(df$SentimentValue))
}

getMethod <- function(method){
  if (method == 1){
    return ("Unigram")
  } else if (method == 2){
    return ("Bigram")
  } else if (method == 3){
    return ("WhatMatter")
  }
}
#------------------------SPLITING DATA FUNCTIONS-------------------------------#

splitTrainTest <- function(dataset, perc, colsdel){
  
  # Two data sets are built, one with SentimentValue as class label, the second
  # one with SentimentCore as label.
  sentiment <- dataset
  core <- dataset

  # Set SentimentCoreNLP to null, it is going to be infered by the classifier
  # SentimentValue is the label, core is the output
  sentiment$SentimentCore <- NULL
  sentiment$SentimentValue <- as.factor(sentiment$SentimentValue)
  
  # For the core set, change SentimentCore (label) to SentimentValue and put 
  # SentimentCore to NULL, it is going to be infered
  core$SentimentValue <- as.factor(core$SentimentCore)
  core$SentimentCore <- NULL
  
  # Delete columns depending on colsdel
  sentiment <- sentiment[,!(colnames(sentiment) %in% colsdel)]
  core <- core[,!(colnames(core) %in% colsdel)]
  
  sample.size.s <- floor(perc * nrow(sentiment))
  sample.size.c <- floor(perc * nrow(core))
  
  # Set the seed to make your partition reproducible
  set.seed(210794)
  
  # Training Data set
  # Sentiment
  train.index.s <- sample(seq_len(nrow(sentiment)), size = sample.size.s)
  dataset.train.s <- sentiment[train.index.s, ]
  dataset.train.id.s <- sentiment$id
  dataset.train.s$id <- NULL
  
  # Core
  train.index.c <- sample(seq_len(nrow(core)), size = sample.size.c)
  dataset.train.c <- sentiment[train.index.c, ]
  dataset.train.id.c <- core$id
  dataset.train.c$id <- NULL
  
  # Test Data set (using the rest of the data not used in training)
  # Sentiment
  dataset.test.s <- sentiment[-train.index.s, ]
  dataset.test.id.s <- dataset.test.s$id
  dataset.test.s$id <- NULL
  
  # Core
  dataset.test.c <- core[-train.index.c, ]
  dataset.test.id.c <- dataset.test.c$id
  dataset.test.c$id <- NULL
  
  # Sentiment
  dataset.test.label.s <- dataset.test.s$SentimentValue
  dataset.test.s$SentimentValue <- NULL
  
  # Core
  dataset.test.label.c <- dataset.test.c$SentimentValue
  dataset.test.c$SentimentValue <- NULL
  
  # Remove id cols repeted
  # datasetTEST[,1] <- NULL
  # datasetTRAIN[,1] <- NULL
  
  result <-list(train.s = dataset.train.s, train.c = dataset.train.c,
                test.s = dataset.test.s, test.c = dataset.test.c, 
                label.s = dataset.test.label.s, label.c = dataset.test.label.c,
                id.s = dataset.test.id.s, id.c = dataset.test.id.c )
  
  return(result)
}