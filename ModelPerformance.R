#------------------------------------------------------------------------------#
#
# MODEL PERFORMANCE
#
# Description: Construct several models and print results
# Author: Jesús Sánchez de Castro
# Date: August 2017
#
#------------------------------------------------------------------------------#

# Load packages
library(data.table)
library(caret)
library(randomForest)
library(plyr)
library(xgboost)
library(RWeka)
library(kernlab)
library(unbalanced)
library(doParallel)

library(beepr)

source("utils.R")

#registerDoParallel(detectCores())
options(mc.cores = detectCores())


#--------------------------------PARAMETERS------------------------------------#
# Algorithms used
#models <- c("xgboost","svm","c45")
models <- c("c4.5")

# nº1 -> UnigramFeatures
# nº2 -> BigramFeatures
# nº3 -> WhatMatters 
method <- 1

# nº1 -> SentimentValue
# nº2 -> SentimentCore
# nº3 -> Mix
class.label <- 1

# Databases: 
# Nº1 : Prado Museum: 1230 pages
# Nº2 : Tyssen Museum: 380 pages
# Nº3 : Reina sofia : 340 pages
# Nº4 : Dali: 140 pages
# Nº5 : City of art and science: 210 pages
dataset <- 4

# # J48 Algorithm parameters:
# control <- trainControl(method="cv", number=5, classProbs = TRUE,
#                         summaryFunction = twoClassSummary, allowParallel = TRUE)
# 
# grid <- expand.grid(C = c(0.25) , M = 50)

dataset.IRs <- c(81,110,18,17,110)
#balances <- c("30","20","10","5","0")
balances <- c("10")

ds.name <- getDatasetName(dataset)
# Build the path 
pc.path <- "D:/TFG-/Data/"

print(paste0("Dataset: ",dataset))
print(paste0("Method: ",method))
print(paste0("Class label: ",class.label))
#------------------------------------------------------------------------------#
# Select feature extraction method
p <- ""

#If we are using unigram feature selection method
if(method == 1){
  p <- paste0(pc.path,ds.name,"/Unigram/")
  
  #If we are using bigram feature selection method
} else if(method == 2){
  p <- paste0(pc.path,ds.name,"/Bigram/")
}
#------------------------------------------------------------------------------#
# Select class label

l <- ""

if(class.label==1){
  p <- paste0(p,"SentimentValue/")
  l <- "s"
} else if(class.label == 2){
  p <- paste0(p,"SentimentCore/")
  l <- "c"
}

#------------------------------------------------------------------------------#
# Select feature extraction method
    
# For each type of balanced set
for(i in 1:length(balances)){
  
  # if dataset.ir > desired.ir
  if (balances[i]<dataset.IRs[dataset]){
    pathResults <- ""
    print(paste0("Balance set: ",balances[i]))
    
    # If the set is not balanced
    if(balances[i] == "0"){
      # Read the dataframe
      df <- read.csv(file = paste0(p,"tr_",l,"_NOBALANCE.csv"))
    # If the set is balanced
    } else {
      df <- read.csv(file = paste0(p,"tr_",l,"_IR_",balances[i],".csv"))
    }
    
    df <- df[,-c(1,2)]
    # For each algorithm
    for(j in 1:length(models)){
      print(paste0("Algorithm: ",models[j]))
      model.result <- ""
      
      # Prepare the result path depending on the model and dataset type
      if(balances[i] == "0"){
        path.results <- paste0(pc.path,ds.name,"/results/",models[j],"_",
                              ds.name,"_UNBALANCED.txt")
      } else {
        path.results <- paste0(pc.path,ds.name,"/results/",models[j],"_",
                              ds.name,"_IR",balances[i],".txt")
      }
  
        # c45 decision tree
      if (models[j]=="c4.5"){
        control <- trainControl(method="cv", number=5, classProbs = TRUE,
                                summaryFunction = twoClassSummary, 
                                allowParallel = TRUE)

        grid <- expand.grid(C = c(0.25), M = c(10))

        set.seed(17)
        time1 <- Sys.time()
        model.result<- caret::train(SentimentValue ~ ., data = df,
                                    method="J48",
                                    trControl = control,
                                    tuneGrid = grid,
                                    metric="ROC")
        time2 <- Sys.time()
      }
      
     
      
      sink(path.results)
      print(paste0("Model: ", models[j]))
      print(paste0("DataSet: ", ds.name))
      if(balances[i]=="0"){
        "Unbalanced: No"
      }else{
        "Unbalanced: Yes"
        print(paste0("IR: ", balances[i]))
      }
      print(paste0("Number of train instances: ", nrow(df)))
      print("Model summary: ")
      print(model.result)
      
      conf.matrix <- confusionMatrix(model.result)
      
      print("ConfusionMatrix: ")
      print(conf.matrix)
      
      conf.table <- (nrow(df)/100)*(conf.matrix$table)
      # Precision: tp/(tp+fp):
      precision <- conf.table[2,2]/sum(conf.table[2,1:2])
      
      # Recall: tp/(tp + fn):
      recall <- conf.table[2,2]/sum(conf.table[1:2,2])
      
      # F-Score: 2 * precision * recall /(precision + recall):
      fscore <- 2 * precision * recall /(precision + recall)
      
      # G-measure: sqrt(precision*recall)
      gmeasure <- sqrt(precision * recall)
      
      print(paste0("FscoreTRAIN: ", fscore))
      print(paste0("GmeasureTRAIN: ", gmeasure))
      
      print(paste0("CompTime:", time2-time1))
      
      sink()
      
    }# for models
  }
}# for balancess

beep(3)