#------------------------------------------------------------------------------#
#
# CoreNLP aplied to a TripAdvisor Museum Database.
#
# Author: Jesus Sanchez de Castro:
#         https://github.com/Yussoft/TFG-Sentiment-Analysis-Spanish-Museums
# Based on Ana Valdivias' code:
#         https://github.com/anavaldi/TFM
#
# Date: 10/04/2017
#
#--------------------------FIRST TIME USING CORENLP----------------------------#
#
#  To download coreNLP package execute the next commands:
#    1.- IF DEVTOOLS NOT INSTALLED: 
#       1.1- install.packages("devtools")
#    2.- devtools::install_github("statsmaths/coreNLP")
#    3.- coreNLP::downloadCoreNLP()
#
#  source: https://github.com/statsmaths/coreNLP
#
#--------------------------LOADING JAVA ERROR----------------------------------#
#
# If there is an error loading a java .dll use: (depends on your version)
#   Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_121') 
#
# -------------------------DATA FRAME VARIABLES--------------------------------#
#
# - x indicating the number of the review
# - id is the id of the user in the tripAdvisor webpage
# - username in TripAdvisor
# - location of the user
# - userop, number of opinions published in the web
# - quote, quote about the attraction
# - rating (1-5)
# - date when the review was published
# - reviewnospace, the complete review. 
# - page of the review
# - titleopinion, quote and review together
#
#------------------------------------------------------------------------------#
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_121')
options(java.parameters = "-Xmx10000m")

library(coreNLP)
source("utils.R")

# Initialices the package can be used several times to set different parameters)
#initCoreNLP("C:/TFG/stanford-corenlp-full-2016-10-31")
initCoreNLP(libLoc ="D:\\TFG-\\stanford-corenlp-full-2015-12-09",mem="10g")

# Read the dataframe (remember that the / must point to the right)
# give data a value depending on what csv you want to read

# Nº1 : Prado Museum: 1230 pages
# Nº2 : Tyssen Museum: 380 pages
# Nº3 : Reina sofia : 340 pages
# Nº4 : Dali: 140 pages
# Nº5 : City of art and science: 210 pages

data <- 5

# data frame read
data.f <- LoadCSV(dataset = data,FALSE,"")

# Create a new column, Sentiment Value:
# 1-2 -> negative, 3 -> neutral, 4-5 -> positive

data.f$SentimentValue <- NA
data.f$SentimentValue <- ifelse(data.f$rating <= 2, "negative", 
                           ifelse(data.f$rating == 3, "neutral",
                            ifelse(data.f$rating >= 4, "positive", 
                                   data.f$SentimentValue)))

# Create a sentimentValueCore and use CoreNLP to predict it.
data.f$SentimentCore <- NA

# Change $titleopinion to character (currently factor)
if(typeof(data.f$titleopinion)=="factor"){
  print("IT IS")
  data.f$titleopinion <- as.character(data.f$titleopinion)
}

for(i in 1:nrow(data.f)){

  print(paste0("Intance nº",i))
  
  pos <- 0  # Variable used to store positive values
  neg <- 0  # Variable used to store negative values
    
  # Extract the sentiment using CoreNLP
  opinion <- getSentiment(annotateString(data.f$titleopinion[i]))
    
  # For each instance of opinion, increase the pos variable if the sentiment 
  # is verypositive or positive, otherwise decrease it if it is negative or 
  # verynegative.
  for(j in 1:nrow(opinion)){
    if(opinion$sentiment[j]=="Verypositive"){
      pos = pos + 2
    } else if(opinion$sentiment[j]=="Positive"){
      pos = pos + 1
    } else if(opinion$sentiment[j]=="Negative"){
      neg = neg + 1
    } else if(opinion$sentiment[j]=="Verynegative"){
      neg = neg + 2
    }
  }
    
  data.f$pos[i] <- pos
  data.f$neg[i] <- neg
}

# For each sentiment, positive > negative -> positive sentiment
# if positive < negative -> negative sentiment, else neutral.
data.f$SentimentCore <- ifelse(data.f$pos > data.f$neg, "positive", 
                                       ifelse(data.f$pos < data.f$neg, 
                                              "negative", "neutral"))
# Save the new data frame into a csv file 
# Depending on what csv you read, a different path will be used for each Data Frame
SaveCSV(data.f,data,name= "ENGCore")

beep(3)

