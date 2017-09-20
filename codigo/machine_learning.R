#------------------------------------------------------------------------------#
#
# Author: Jesús Sánchez de Castro
# Impired by: Ana Valdivia
# Date: September 2017
#
#                       MODEL TRAINING AND PERFORMANCE
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
library(e1071)
source("utils.R")

#registerDoParallel(detectCores())
options(mc.cores = detectCores())


#--------------------------------PARAMETERS------------------------------------#
# Algorithms used
 models <- c("xgboost","svm","c4.5")
# models <- c("c4.5")
# models <- c("xgboost")
# models <- c("svm")

# nº1 -> UnigramFeatures
# nº2 -> BigramFeatures
# nº3 -> WhatMatters 
method <- 2

# nº1 -> SentimentValue
# nº2 -> SentimentCore
class.label <- 2

# Databases: 
# Nº1 : Prado Museum: 1230 pages
# Nº2 : Tyssen Museum: 380 pages
# Nº3 : Reina sofia : 340 pages
# Nº4 : Dali: 140 pages
# Nº5 : Guggenheim: 400 pages
dataset <- 3
  
# # J48 Algorithm parameters:
# control <- trainControl(method="cv", number=5, classProbs = TRUE,
#                         summaryFunction = twoClassSummary, allowParallel = TRUE)
# 
# grid <- expand.grid(C = c(0.25) , M = 50)

# IR is the value that show how unbalanced a dataset is.
dataset.IRs <- c(81,110,18,17,17)

balances <- c(20,15,10,5,1,0)
#balances <- c(0)

ds.name <- getDatasetName(dataset)
# Build the path 
pc.path <- "D:/TFG-/Data/"

print(paste0("Dataset: ",dataset))
print(paste0("Method: ",method))
print(paste0("Class label: ",class.label))
#------------------------------------------------------------------------------#
# Select feature extraction method

#If we are using unigram feature selection method
if(method == 1){
  p <- paste0(pc.path,ds.name,"/Unigram/")
  
  #If we are using bigram feature selection method
} else if(method == 2){
  p <- paste0(pc.path,ds.name,"/Bigram/")
}
#------------------------------------------------------------------------------#
# Select class label

if(class.label==1){
  p <- paste0(p,"SentimentValue/")
  l <- "s"
} else if(class.label == 2){
  p <- paste0(p,"SentimentCore/")
  l <- "c"
}

#------------------------------------------------------------------------------#
# Select feature extraction method

is.balanced <- FALSE

# Load dataframes for prediction
df.test <- read.csv(file = paste0(p,"ts_",l,".csv"))

print("Test set loaded.")
df.id <- read.csv(file = paste0(p,"id_",l,".csv"))
print("IDs loaded.")
df.label <- read.csv(file = paste0(p,"label_",l,".csv"))
print("Labels loaded.")


if(dataset==4){
  df.test<- df.test[,-c(1,2)]
} else {
  df.test<- df.test[,-c(1)]
}

# For each type of balanced set
for(i in 1:length(balances)){

  df.train <- data.frame()
  b <- balances[i]
  # if dataset.ir > desired.ir
  if (b<dataset.IRs[dataset]){
    print(paste0("-------------------------------------------------"))
    print(paste0("Balance set: ",b))
    
    
    # If the set is not balanced
    if(b == 0){
      # Read the dataframe
      is.balanced <- FALSE
      df.train <- read.csv(file = paste0(p,"tr_",l,"_NOBALANCE.csv"))
      print("Training set loaded.")
      
    # If the set is balanced
    } else {
      is.balanced <- TRUE
      df.train <- read.csv(file = paste0(p,"tr_",l,"_IR_",as.character(b),".csv"))
      print("Training set loaded.")
      
    }
    if(dataset==4){
      df.train <- df.train[,-c(1,2)]
    } else {
      df.train <- df.train[,-c(1)]
    }
    # For each algorithm
    for(j in 1:length(models)){
      print(paste0("Algorithm: ",models[j]))

      # Prepare the result path depending on the model and dataset type
      if(!is.balanced){
        path.results <- paste0(pc.path,ds.name,"/results/",models[j],"_",
                              ds.name,"_m",method,"_l",class.label,"_UNBALANCED.txt")
      } else if (is.balanced){
        path.results <- paste0(pc.path,ds.name,"/results/",models[j],"_",
                              ds.name,"_m",method,"_l",class.label,
                              "_IR",as.character(b),".txt")
      }
      
      #print(path.results)
      #------------------------------------------------------------------------#
      # c45 decision tree
      if (models[j]=="c4.5"){
        control <- trainControl(method="cv", number=5, classProbs = TRUE,
                                summaryFunction = twoClassSummary,
                                allowParallel = TRUE)

        grid <- expand.grid(C = c(0.25), M = c(10))

        set.seed(17)
        time1 <- Sys.time()
        model.result<- caret::train(SentimentValue ~ ., data = df.train,
                                    method="J48",
                                    trControl = control,
                                    tuneGrid = grid,
                                    metric="ROC")
        time2 <- Sys.time()

      #------------------------------------------------------------------------#
      #XGBOOST
      } else if (models[j] == "xgboost"){

        control <- trainControl(method="cv", number=5, classProbs = TRUE,
                                summaryFunction = twoClassSummary,
                                allowParallel = TRUE)
        set.seed(17)
        time1 <- Sys.time()
        # xgbGrid <- expand.grid(
        #   nrounds = 1,
        #   eta = 0.3,
        #   max_depth = 5,
        #   gamma = 0,
        #   colsample_bytree=1,
        #   min_child_weight=1)

        xgbGrid2 <- expand.grid(
          nrounds = 2,
          eta = 0.3,
          max_depth = 6,
          gamma = 0,
          colsample_bytree=1,
          min_child_weight=1,
          subsample=0.5)

        predictors <- df.train[,-1]
        for(i in 1:ncol(predictors)){
          predictors[,i] <- as.numeric(as.character(predictors[,i]))
        }

        label <- df.train$SentimentValue
        set.seed(17)
        time1 <- Sys.time()
        model.result <- caret::train(x=predictors,
                                     y=label,
                                     method="xgbTree",
                                     trControl=control,
                                     tuneGrid=xgbGrid2,
                                     metric="ROC")
        time2 <- Sys.time()

      } else if (models[j]=="svm"){
        control <- trainControl(method="cv", number=5, classProbs = TRUE,
                                summaryFunction = twoClassSummary,
                                allowParallel = TRUE)

        grid <- expand.grid(C = c(0.75))

        set.seed(17)
        time1 <- Sys.time()
        model.result <- caret::train(SentimentValue ~ .,
                                     data=df.train,
                                     method="svmLinear",
                                     trControl=control,
                                     tuneGrid = grid,
                                     metric="ROC")
        time2 <- Sys.time()
      #------------------------------------------------------------------------#
      #SVM
      } else if(models[j]=="svm"){
        control <- trainControl(method="cv", number=5, classProbs = TRUE,
                                summaryFunction = twoClassSummary,
                                allowParallel = TRUE)

        grid <- expand.grid(C = c(0.75))

        set.seed(17)
        time1 <- Sys.time()
        model.result <- caret::train(SentimentValue ~ .,
                                     data = df.train,
                                     method = "svmLinear",
                                     trControl = control,
                                     tuneGrid = grid,
                                     metric = "ROC")
        time2 <- Sys.time()
      }
      #------------------------------------------------------------------------#
      # Training results

      # Start saving output into text
      sink(path.results)

      print(paste0("Model: ", models[j]))
      print(paste0("DataSet: ", ds.name))
      if (is.balanced){
        print("Balanced: No")
      }else{
        print(paste0("Balanced IR: ", as.character(b)))
      }
      print(paste0("Number of training instances: ", nrow(df.train)))
      print("Model summary: ")
      print(model.result)

      conf.matrix <- confusionMatrix(model.result)

      print("ConfusionMatrix: ")
      print(conf.matrix)

      conf.table <- (nrow(df.train)/100)*(conf.matrix$table)
      
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


      #------------------------------------------------------------------------#

      # Prediction
      for(i in 1:ncol(df.test)){
        df.test[,i] <- as.numeric(as.character(df.test[,i]))
      }

      model.result.pred <- predict(model.result, df.test, type="prob")
      # print(confusionMatrix(xfbResults_pred[,2], LABELTripAdvisorFeaturesTEST))
      prediction <- data.frame(df.id , df.label, model.result.pred[,2])
      # setnames(TripAdvisorPrediction, old=c("IDTripAdvisorFeaturesTEST", "LABELTripAdvisorFeaturesTEST", "ModelResults_pred...2."),
      #          new=c("id", "SentimentValue", "ProbSentiment"))

      setnames(prediction,old=c("model.result.pred...2.","x","x.1"),
               new=c("ProbSentiment","id","SentimentValue"))
      prediction$X <- NULL
      prediction$X.1 <- NULL

      prediction$SentimentPred <- ifelse(prediction$ProbSentiment > 0.5, "positive", "negative")

      no.neg.pred <- FALSE
      if (sum(prediction$SentimentPred=="negative") == 0){
        no.neg.pred <- TRUE
      }

      # Print confusion matrix test
      conf.table.test <- table(prediction$SentimentPred, prediction$SentimentValue)
      print("ConfMatrix TEST: ")
      print(table(prediction$SentimentPred, prediction$SentimentValue))
      print(postResample(prediction$SentimentPred, prediction$SentimentValue))



      # In case there is no negative prediction, asigning conf.table.test[1,1]
      # and conf.table.test[2,1] out of bounds error will happen
      if(no.neg.pred){
        tp <- conf.table.test[1,2]
        fp <- conf.table.test[1,1]
        tn <- 0
        fn <- 0
      } else {
        tp <- conf.table.test[2,2]
        fp <- conf.table.test[1,2]
        tn <- conf.table.test[1,1]
        fn <- conf.table.test[1,2]
      }

      #precision <- conf.table.test[2,2]/sum(conf.table.test[2,1:2])
      precision <- tp/(tp+fp)

      # recall <- conf.table.test[2,2]/sum(conf.table.test[1:2,2])
      recall <- tp/(tp + fn)

      # F-Score: 2 * precision * recall /(precision + recall):
      fscore <- 2 * precision * recall /(precision + recall)

      # G-measure: sqrt(precision*recall)
      gmeasure <- sqrt(precision * recall)

      print(paste0("FscoreTEST: ", fscore))
      print(paste0("GmeasureTEST: ", gmeasure))

      # Stop saving output into text
      sink()
      
    }# for models
  }
}# for balancess

beep(3)