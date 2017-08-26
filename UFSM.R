#------------------------------------------------------------------------------#
# UNIGRAM FEATURE SELECTION METHOD (USFM)
#
# Original Author: Ana Valdivia
# Author: Jesús Sánchez de Castro
# Date: July 2017
#
#------------------------------------------------------------------------------#

# Load packages
library(data.table)

#Remember to change working directory
source("utils.R")
source("featureExtraction.R")

debug_mode <- TRUE

# Load data
# Nº1 : Prado Museum: 1230 pages
# Nº2 : Tyssen Museum: 380 pages
# Nº3 : Reina sofia : 340 pages
# Nº4 : Dali: 140 pages
# Nº5 : City of art and science: 210 pages

database <- 4
df <- LoadCSV(database,FALSE,"COREENG")

TripAdvisor <- df

#-------------------------------FREQUENCY--------------------------------------#
# We now calculate the word frequency of the positive, negative and neutral
# reviews.
word.freq.pos <- WordFrequency(TripAdvisor$titleopinion
                               [TripAdvisor$SentimentValue == "positive"],
                               0.999,1)
word.freq.neg <- WordFrequency(TripAdvisor$titleopinion
                               [TripAdvisor$SentimentValue == "negative"],
                               0.999,1)

# Order from higher frequency to lower.
word.freq.pos <- as.data.table(word.freq.pos)
word.freq.pos <- word.freq.pos[order(freq, decreasing = TRUE),]
word.freq.neg <- as.data.table(word.freq.neg)
word.freq.neg <- word.freq.neg[order(freq, decreasing = TRUE),]

#SaveCSV(word.freq.pos,4,"WordFreqPos")
#SaveCSV(word.freq.neg,4,"WordFreqNeg")

#-----------------------------------TFIDF--------------------------------------#

word.tfidf.pos <- WordTFIDF(TripAdvisor$titleopinion
                            [TripAdvisor$SentimentValue == "positive"],
                            sparsity = 0.99,
                            mode=1)$Freq
word.tfidf.neg <- WordTFIDF(TripAdvisor$titleopinion
                            [TripAdvisor$SentimentValue == "negative"],
                            sparsity = 0.99,
                            mode=1)$Freq

# Order from higher frequency to lower.
word.tfidf.pos <- as.data.table(word.tfidf.pos)
word.tfidf.pos <- word.tfidf.pos[order(freq, decreasing = TRUE),]
word.tfidf.neg <- as.data.table(word.tfidf.neg)
word.tfidf.neg <- word.tfidf.neg[order(freq, decreasing = TRUE),]

#SaveCSV(word.tfidf.pos,4,"WordTFIDFPos")
#SaveCSV(word.tfidf.neg,4,"WordTFIDFNeg")

#----------------ORDER THE DATA IN NEW DATAFRAMES------------------------------#

# Put tf*idf pos and neg together into
wordNeg <- merge(word.tfidf.neg, word.freq.neg, by="word", all = TRUE)
wordPos <- merge(word.tfidf.pos, word.freq.pos, by="word", all = TRUE)

setnames(wordNeg, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))
setnames(wordPos, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))

# Delete STOPWORDS
wordNeg <- wordNeg[!(wordNeg$word %in% stopwords("SMART"))]
wordPos <- wordPos[!(wordPos$word %in% stopwords("SMART"))]

# Order and select most 500 popular words
wordNeg <- wordNeg[order(tfidf, decreasing = TRUE),]
wordNeg500 <- wordNeg[1:500,]
wordPos <- wordPos[order(tfidf, decreasing = TRUE),]
wordPos500 <- wordPos[1:500,]

# Group up the 500 most popular words with positive and negative sentiment and
# change the columns name.
wordNegPos500 <- merge(wordNeg500, wordPos500, by = "word", all = TRUE)
setnames(wordNegPos500, old=c("tfidf.x", "freq.x", "tfidf.y", "freq.y"),
         new=c("tfidfNeg", "freqNeg", "tfidfPos", "freqPos"))

SaveCSV(wordNegPos500,4,"WordNP500")

# Order by higher tfidf and extract words
wordNegPos500 <- wordNegPos500[order(tfidfNeg, decreasing = TRUE),]
wordNegPos500Vector <- wordNegPos500$word


wordNegSelect <- wordNeg[!(wordNeg$word %in% wordPos$word),]
wordPosSelect <- wordPos[!(wordPos$word %in% wordNeg$word),]
wordCommonPosNeg <- wordNeg

# Order alphabeticaly to see repeated words because of a wrong stemming
wordAlphOrdered <- with(wordCommonPosNeg,wordCommonPosNeg[order(word)])

SaveCSV(wordAlphOrdered,4,"PreStemmingCleaningFeatures")

# Free memory
rm(word.freq.neg)
rm(word.freq.pos)
rm(word.tfidf.neg)
rm(word.tfidf.pos)

# DocumentTermMatrix for PositiveNegative
TripAdvisorPosNeg <- TripAdvisor[TripAdvisor$SentimentValue != "neutral",]
TripAdvisorFeatures <- WordTFIDF(TripAdvisorPosNeg$titleopinion,
                                 sparsity = 0.99,
                                 mode = 1)$Temp
TripAdvisorFeatures <- ifelse(TripAdvisorFeatures > 0, 1, 0)
TripAdvisorFeatures <- TripAdvisorFeatures[, colnames(TripAdvisorFeatures) %in% wordNegPos500Vector]
TripAdvisorFeatures <- as.data.frame(TripAdvisorFeatures)
# Due to the stemming not working properly with some words, they will be
# removed mannually.

#Features deleted for Dalí Museum
if(database == 4){


  TripAdvisorFeatures$addX <- TripAdvisorFeatures$add + TripAdvisorFeatures$addit
  TripAdvisorFeatures$addX <- ifelse(TripAdvisorFeatures$addX > 1, 1, TripAdvisorFeatures$addX)
  TripAdvisorFeatures$add<- NULL
  TripAdvisorFeatures$addit <- NULL

  # Art-artist-artwork
  TripAdvisorFeatures$artX <- TripAdvisorFeatures$art + TripAdvisorFeatures$artist +
    TripAdvisorFeatures$artwork

  TripAdvisorFeatures$artX <- ifelse(TripAdvisorFeatures$artX > 1, 1, TripAdvisorFeatures$artX)
  TripAdvisorFeatures$art <- NULL
  TripAdvisorFeatures$artist <- NULL
  TripAdvisorFeatures$artwork <- NULL


  # creation creative
  TripAdvisorFeatures$creationX <- TripAdvisorFeatures$creation+ TripAdvisorFeatures$creativ
  TripAdvisorFeatures$creationX <- ifelse(TripAdvisorFeatures$creationX > 1, 1, TripAdvisorFeatures$creationX)
  TripAdvisorFeatures$creation<- NULL
  TripAdvisorFeatures$creativ <- NULL

  # dali dalí
  TripAdvisorFeatures$daliX <- TripAdvisorFeatures$dali + TripAdvisorFeatures$dalí
  TripAdvisorFeatures$daliX <- ifelse(TripAdvisorFeatures$daliX > 1, 1, TripAdvisorFeatures$daliX)
  TripAdvisorFeatures$dali<- NULL
  TripAdvisorFeatures$dalí <- NULL

  # drive drove
  TripAdvisorFeatures$driveX <- TripAdvisorFeatures$drove + TripAdvisorFeatures$drive
  TripAdvisorFeatures$driveX <- ifelse(TripAdvisorFeatures$driveX > 1, 1, TripAdvisorFeatures$driveX)
  TripAdvisorFeatures$drive<- NULL
  TripAdvisorFeatures$drove <- NULL

  # easi easilli
  TripAdvisorFeatures$easiX <- TripAdvisorFeatures$easi + TripAdvisorFeatures$easili
  TripAdvisorFeatures$easiX <- ifelse(TripAdvisorFeatures$easiX > 1, 1, TripAdvisorFeatures$easiX)
  TripAdvisorFeatures$easi<- NULL
  TripAdvisorFeatures$easili <- NULL

  # famili familiar
  TripAdvisorFeatures$familyX <- TripAdvisorFeatures$famili + TripAdvisorFeatures$familiar
  TripAdvisorFeatures$familyX <- ifelse(TripAdvisorFeatures$familyX > 1, 1, TripAdvisorFeatures$familyX)
  TripAdvisorFeatures$famili<- NULL
  TripAdvisorFeatures$familiar <- NULL

  # info inform
  TripAdvisorFeatures$infoX <- TripAdvisorFeatures$info + TripAdvisorFeatures$inform
  TripAdvisorFeatures$infoX <- ifelse(TripAdvisorFeatures$infoX > 1, 1, TripAdvisorFeatures$infoX)
  TripAdvisorFeatures$info<- NULL
  TripAdvisorFeatures$inform <- NULL

  # jewel jewelleri jewelri
  TripAdvisorFeatures$jewelX <- TripAdvisorFeatures$jewelleri + TripAdvisorFeatures$jewel
  + TripAdvisorFeatures$jewelri

  TripAdvisorFeatures$jewelX <- ifelse(TripAdvisorFeatures$jewelX > 1, 1, TripAdvisorFeatures$jewelX)
  TripAdvisorFeatures$jewel<- NULL
  TripAdvisorFeatures$jewelleri <- NULL
  TripAdvisorFeatures$jewelri <- NULL

  # love lover
  TripAdvisorFeatures$loveX <- TripAdvisorFeatures$love + TripAdvisorFeatures$lover
  TripAdvisorFeatures$loveX <- ifelse(TripAdvisorFeatures$loveX > 1, 1, TripAdvisorFeatures$loveX)
  TripAdvisorFeatures$love<- NULL
  TripAdvisorFeatures$lover <- NULL


  #spend spent
  TripAdvisorFeatures$spendX <- TripAdvisorFeatures$spend + TripAdvisorFeatures$spent
  TripAdvisorFeatures$spendX <- ifelse(TripAdvisorFeatures$spendX > 1, 1, TripAdvisorFeatures$spendX)
  TripAdvisorFeatures$spend<- NULL
  TripAdvisorFeatures$spent <- NULL

  #theater theatr
  TripAdvisorFeatures$theaterX <- TripAdvisorFeatures$theater+ TripAdvisorFeatures$theatr
  TripAdvisorFeatures$theaterX <- ifelse(TripAdvisorFeatures$theaterX > 1, 1, TripAdvisorFeatures$theaterX)
  TripAdvisorFeatures$theater<- NULL
  TripAdvisorFeatures$theatr<- NULL

  #tour touristic
  TripAdvisorFeatures$tourX <- TripAdvisorFeatures$tour + TripAdvisorFeatures$tourist
  TripAdvisorFeatures$tourX <- ifelse(TripAdvisorFeatures$tourX > 1, 1, TripAdvisorFeatures$tourX)
  TripAdvisorFeatures$tour <- NULL
  TripAdvisorFeatures$tourist <- NULL

  #visit visitor
  TripAdvisorFeatures$visitX <- TripAdvisorFeatures$visit + TripAdvisorFeatures$visitor
  TripAdvisorFeatures$visitX <- ifelse(TripAdvisorFeatures$visitX > 1, 1, TripAdvisorFeatures$visitX)
  TripAdvisorFeatures$visitor<- NULL
  TripAdvisorFeatures$visit <- NULL
}

TripAdvisorFeatures <- TripAdvisorFeatures[ ,order(names(TripAdvisorFeatures))]
SaveCSV(TripAdvisorFeatures,4,"postStemmingCleaning")


# Preparing the final set
TripAdvisorAndFeatures <- cbind(TripAdvisorPosNeg, TripAdvisorFeatures)
TripAdvisorAndFeatures$pos <- NULL
TripAdvisorAndFeatures$neg <- NULL

# Delete Neutral opinions
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentValue!="neutral",]
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentCoreNLP!="neutral",]

# Complete set
SaveCSV(TripAdvisorAndFeatures,4,"UnigramFeatures")

# DataFrame where the machine sentiment does not match the expert's opinion
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures[!(TripAdvisorAndFeatures$SentimentValue=="positive" & TripAdvisorAndFeatures$SentimentCoreNLP=="negative"),]
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures_NOTMATCHING[!(TripAdvisorAndFeatures_NOTMATCHING$SentimentValue=="negative" & TripAdvisorAndFeatures_NOTMATCHING$SentimentCoreNLP=="positive"),]

SaveCSV(TripAdvisorAndFeatures_NOTMATCHING,4,"UnigramFeatures_NOMATCHING")


beep(2)