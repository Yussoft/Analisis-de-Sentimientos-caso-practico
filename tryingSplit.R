source("utils.R")
source("featureExtraction.R")

# Nº1 : Prado Museum: 1230 pages
# Nº2 : Tyssen Museum: 380 pages
# Nº3 : Reina sofia : 340 pages
# Nº4 : Dali: 140 pages
# Nº5 : City of art and science: 210 pages

dataset <- 1
cols.del <- c("username", "location", "userop", "quote", "rating", "date",
              "reviewnospace", "page", "titleopinion")
perc <- 0.75

# Desired IR values for the data
balance <- c(20,15,10,5,1)

# n1 -> UnigramFeatures
# n2 -> BigramFeatures
# n3 -> WhatMatters 
method <- 2
m <- ""

if(method == 1){
  m <- "UnigramFeatures"
} else if (method == 2){
  m <- "BigramFeatures"
}

df <- LoadCSV(dataset,TRUE,m)

# Split 75% train 25% test
result <- splitTrainTest(df,perc,cols.del)

# train sentiment / core
tr.s <- result$train.s
tr.c <- result$train.c

# test sentiment / core
ts.s <- result$test.s
ts.c <- result$test.c

# label sentiment / core
label.s <- result$label.s
label.c <- result$label.c

# id sentiment / core
id.s <- result$id.s
id.c <- result$id.c

ds.name <- getDatasetName(dataset)
# Build the path 
pc.path <- "D:/TFG-/Data/"

print(paste0("Dataset: ",getDatasetName(dataset)))
print(paste0("Method: ",getMethod(method)))

# Save unbalanced sentimentValue
if(method == 1){
  
  # NOT BALANCED TRAINING SETS
  write.csv(tr.s,file=paste0(pc.path,ds.name,"/Unigram/SentimentValue/tr_s_NOBALANCE.csv"))
  write.csv(tr.c,file=paste0(pc.path,ds.name,"/Unigram/SentimentCore/tr_c_NOBALANCE.csv"))
  
  # TEST SETS
  write.csv(ts.s,file=paste0(pc.path,ds.name,"/Unigram/SentimentValue/ts_s.csv"))
  write.csv(ts.c,file=paste0(pc.path,ds.name,"/Unigram/SentimentCore/ts_c.csv"))
  
  # TEST SETS
  write.csv(id.s,file=paste0(pc.path,ds.name,"/Unigram/SentimentValue/id_s.csv"))
  write.csv(id.c,file=paste0(pc.path,ds.name,"/Unigram/SentimentCore/id_c.csv"))
  
  # TEST SETS
  write.csv(label.s,file=paste0(pc.path,ds.name,"/Unigram/SentimentValue/label_s.csv"))
  write.csv(label.c,file=paste0(pc.path,ds.name,"/Unigram/SentimentCore/label_c.csv"))
  
  
} else if (method == 2){

  # NOT BALANCED TRAINING SETS
  write.csv(tr.s,file=paste0(pc.path,ds.name,"/Bigram/SentimentValue/tr_s_NOBALANCE.csv"))
  write.csv(tr.c,file=paste0(pc.path,ds.name,"/Bigram/SentimentCore/tr_c_NOBALANCE.csv"))
  
  # TEST SETS
  write.csv(ts.s,file=paste0(pc.path,ds.name,"/Bigram/SentimentValue/ts_s.csv"))
  write.csv(ts.c,file=paste0(pc.path,ds.name,"/Bigram/SentimentCore/ts_c.csv"))
  
  # TEST SETS
  write.csv(id.s,file=paste0(pc.path,ds.name,"/Bigram/SentimentValue/id_s.csv"))
  write.csv(id.c,file=paste0(pc.path,ds.name,"/Bigram/SentimentCore/id_c.csv"))
  
  # TEST SETS
  write.csv(label.s,file=paste0(pc.path,ds.name,"/Bigram/SentimentValue/label_s.csv"))
  write.csv(label.c,file=paste0(pc.path,ds.name,"/Bigram/SentimentCore/label_c.csv"))
}

print("Writing unbalanced data succes")

t <- table(tr.s$SentimentValue)
IR <- floor(table(tr.s$SentimentValue)[2]/table(tr.s$SentimentValue)[1])

for(i in 1:length(balance)){
  print(paste0("Trying to oversample for IR=",balance[i]))
  # This process is done for 2 datasets, SentimentValue and SentimentCore
  
  #Auxiliar sets not to change the originals
  tr.s.aux <- tr.s
  tr.c.aux <- tr.c
  
  # If the dataset IR is less than the desired one
  if (balance[i] < IR){
    
    print(paste0("Objective ",balance[i],"IR < ",IR))
    
    # Calculate the indices of the negative documents
    minority.indices <- (1:dim(tr.s)[1])[tr.s$SentimentValue == "negative"]
    
    start.length <- dim(tr.s)[1]
    
    # Set the desired IR value
    desired.IR <- balance[i]
    
    # How many instances are going to be added
    to.add <- round((1/desired.IR)*(start.length - length(minority.indices))-length(minority.indices))
    
    # generating oversampling intances
    duplicate <- sample(minority.indices, to.add, replace = T)
    
    # Add the intances to the dataset
    for( j in 1:length(duplicate)){
      tr.s.aux <- rbind(tr.s.aux, tr.s.aux[duplicate[j],])
      tr.c.aux <- rbind(tr.c.aux, tr.c.aux[duplicate[j],])
      
      tr.s.aux$SentimentValue[start.length+j] <- "negative"
      tr.c.aux$SentimentValue[start.length+j] <- "negative"
      
    }
    newIR <- table(tr.s$SentimentValue)[2]/table(tr.s$SentimentValue)[1]  
    
    # Save balanced data 
    if(method == 1){
      
      # BALANCED TRAINING SETS
      write.csv(tr.s.aux,file=paste0(pc.path,ds.name,"/Unigram/SentimentValue/",
                                     "tr_s_IR_",desired.IR,".csv"))
      write.csv(tr.c.aux,file=paste0(pc.path,ds.name,"/Unigram/SentimentCore/",
                                     "tr_c_IR_",desired.IR,".csv"))
      
    } else if (method == 2){
      
      # BALANCED TRAINING SETS
      write.csv(tr.s.aux,file=paste0(pc.path,ds.name,"/Bigram/SentimentValue/",
                                     "tr_s_IR_",desired.IR,".csv"))
      write.csv(tr.c.aux,file=paste0(pc.path,ds.name,"/Bigram/SentimentCore/",
                                     "tr_c_IR_",desired.IR,".csv"))
    }
  }
}
print("Writing Balanced data succes")

beep(3)