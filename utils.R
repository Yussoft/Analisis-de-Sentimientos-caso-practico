#------------------------------------------------------------------------------#
#                                                                              #
#  UTILS.R                                                                     #
#                                                                              #
# Description: R Script with utility functions used a lot for this project.    #
# Author: Jesús Sánchez de Castro                                              #
# Date: July 2017                                                              #
#                                                                              #
#------------------------------------------------------------------------------#
library(beepr)


getDatasetName <- function(dataset){
  
  ds.name <- "hola"
  
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
      ds.name <- "city"
    } else {
      ds.name <- "city"
    }
  } else {
    ds.name = "city"
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
  # Nº1 : Prado Museum: 1230 pages
  # Nº2 : Tyssen Museum: 380 pages
  # Nº3 : Reina sofia : 340 pages
  # Nº4 : Dali: 140 pages
  # Nº5 : City of art and science: 210 pages
  #
  # Returns:
  #   data frame with the data from the csv file
  ds.name <- getDatasetName(dataset)

  #Change path to match it with your pc's
  pc.path="D:/TFG-/Data/"
  
  # If no name, load the basic CSV, the data from the web
  if(name=="")
    path <- paste0(pc.path,ds.name,"/",ds.name,"ENG.csv")
  
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

#------------------------SPLITING DATA FUNCTIONS-------------------------------#

splitTrainTest <- function(name, database, perc, colsdel){
  # In this function SentimentValue refers to class label
  
  dataset <- LoadCSV(database,FALSE,name)
  
  # Set SentimentCoreNLP to null, it is going to be infered by the classifier
  dataset$SentimentCoreNLP <- NULL
  dataset$SentimentValue <- as.factor(dataset$SentimentValue)
  
  # Delete columns depending on colsdel
  dataset <- dataset[,!(colnames(dataset) %in% colsdel)]
  
  sampleSize <- floor(perc * nrow(dataset))
  
  # Set the seed to make your partition reproducible
  set.seed(210794)
  
  # Training Data set
  trainIndex <- sample(seq_len(nrow(dataset)), size = sampleSize)
  datasetTRAIN <- dataset[trainIndex, ]
  IDdatasetTRAIN <- dataset$id
  datasetTRAIN$id <- NULL
  
  # Test Data set (using the rest of the data not used in training)
  datasetTEST <- dataset[-trainIndex, ]
  IDdatasetTEST <- datasetTEST$id
  datasetTEST$id <- NULL
  
  LABELdatasetTEST <- datasetTEST$SentimentValue
  datasetTEST$SentimentValue <- NULL
  
  # Remove id cols repeted
  datasetTEST[,1] <- NULL
  datasetTRAIN[,1] <- NULL
  
  result <-list(train = datasetTRAIN, test = datasetTEST,
                labelTest = LABELdatasetTEST, idTest = IDdatasetTEST)
  return(result)
}