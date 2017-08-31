
#------------------------------------------------------------------------------#
#
# Original Author: Ana Valdivia
# Author: Jesús Sánchez de Castro
# Date: August 2017
# Description: script to split data for train, test and oversampling if needed
#                    SPLIT DATA: TRAIN&TEST + OVERSAMPLING
#
#------------------------------------------------------------------------------#

# Load packages
library(caret)
library(randomForest)
library(plyr)
library(data.table)
library(xgboost)
library(unbalanced)
library(doParallel)

source("featureExtraction.R")
source("utils.R")

#registerDoParallel(detectCores())
options(mc.cores = detectCores())

# select parameters
# percentage split train/test:
percentage <- 0.75
print(paste0("Spliting Data: percentage <- ",percentage))
# set to analyse:
set <- ("UnigramFeatures")
museums <- c(4)
cols_to_del <- c("username", "location", "userop", "quote", "rating", "date", "reviewnospace", "page", "titleopinion")


#For each museum, split data into 75% training 25% test
for(i in 1:length(museums)){
  
  # Split the selected set, with a training set of percentage% and delete
  # the listed cols in cols.to.del
  df <- LoadCSV(museums[i],FALSE,set)

  splitted_data <- splitTrainTest(df,percentage,cols_to_del)
  
  # Asign the train and test sets to variables
  df_TRAIN <- splitted_data$train
  df_TEST <- splitted_data$test
  df_ID_TEST <- splitted_data$idTest
  df_LABEL_TEST <- splitted_data$labelTest
  
  raw_IR <- floor(table(df_TRAIN$SentimentValue)[2]/table(df_TRAIN$SentimentValue)[1])
  print(paste0("IR value: ",raw_IR))
  
  # Save the splitted data in new files
  SaveCSV(df_TRAIN,museums[i],paste0("UNBALANCED_IR_",raw_IR,"_TRAIN_",set))
  SaveCSV(df_TEST,museums[i],paste0("TEST_",set))
  SaveCSV(df_ID_TEST,museums[i],paste0("IDTEST_",set))
  SaveCSV(df_LABEL_TEST,museums[i],paste0("LABELTEST_",set))
}

beep(2)