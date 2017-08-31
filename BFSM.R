  #------------------------------------------------------------------------------#
#
# Original Author: Ana Valdivia
# Author: Jesús Sánchez de Castro
# Date: August 2017
#
#                       BIGRAM FEATURE SELECTION METHOD 
#
#------------------------------------------------------------------------------#

# Load packages

source("featureExtraction.R")
source("utils.R")

# Before executing the script:
# 1.- Change working directory to where the code is
# 2.- Choose a museum, asign the number of the museum to dataset variable

# Nº1 : Prado Museum: 1230 pages
# Nº2 : Tyssen Museum: 380 pages
# Nº3 : Reina sofia : 340 pages
# Nº4 : Dali: 140 pages
# Nº5 : City of art and science: 210 pages

dataset <- 5

# df is the variable containing the dataframe
df <- LoadCSV(dataset,verbose = FALSE,name = "CoreEng")

#-------------------------------TERM-FREQUENCY---------------------------------#
# Term-frequency of positive & negative reviews (BIGRAMS - mode == 2)
tf.pos <- WordFrequency(df$titleopinion[df$SentimentValue=="positive"],
                        sparsity = .999, mode=2)
tf.neg <- WordFrequency(df$titleopinion[df$SentimentValue=="negative"],
                        sparsity = .999, mode=2)

# tf.neg/pos$row/$
tf.pos <- as.data.table(tf.pos)
tf.pos <- tf.pos[order(tf, decreasing = TRUE),]
tf.neg <- as.data.table(tf.neg)
tf.neg <- tf.neg[order(tf, decreasing = TRUE),]

#-----------------------------------TFIDF--------------------------------------#
#TFIDF of positive & negative reviews (BIGRAMS - mode == 2)

tfidf.pos <- WordTFIDF(df$titleopinion[df$SentimentValue=="positive"],
                       sparsity = .999, mode=2)$TFIDF
tfidf.neg <- WordTFIDF(df$titleopinion[df$SentimentValue=="negative"],
                       sparsity = .999, mode=2)$TFIDF

tfidf.pos <- as.data.table(tfidf.pos)
tfidf.pos <- tfidf.pos[order(tfidf, decreasing = TRUE),]
tfidf.neg <- as.data.table(tfidf.neg)
tfidf.neg <- tfidf.neg[order(tfidf, decreasing = TRUE),]

#------------------------------------------------------------------------------#
# Merge FREQ and TFIDF
word.neg <- merge(tfidf.neg, tf.neg, by="word", all = TRUE)
word.pos <- merge(tfidf.pos, tf.pos, by="word", all = TRUE)

# Remove bigrams with stopwords
word.neg <- word.neg[-deleteSWBigrams(word.neg),]
word.pos <- word.pos[-deleteSWBigrams(word.pos),]

# Free memmory
# rm(tf.pos)
# rm(tf.neg)
# rm(tfidf.pos)
# rm(tfidf.neg)

# Order and select most 500 interesting  words
word.neg <- word.neg[order(tfidf, decreasing = TRUE),]
if(nrow(word.neg) < 500){
  word.neg500 <- word.neg[1:nrow(word.neg),]
} else {
  word.neg500 <- word.neg[1:500,]
}

word.pos <- word.pos[order(tfidf, decreasing = TRUE),]
if(nrow(word.pos) < 500){
  word.pos500 <- word.pos[1:nrow(pos),]
} else {
  word.pos500 <- word.pos[1:500,]
}

# Change cols names
word.neg.pos500 <- merge(word.neg500, word.pos500, by = "word", all = TRUE)
setnames(word.neg.pos500, old=c("tfidf.x", "tf.x", "tfidf.y", "tf.y"),
         new=c("tfidfNeg", "freqNeg", "tfidfPos", "freqPos"))

# Top 500 bigrams
word.neg.pos500 <- word.neg.pos500[order(tfidfNeg, decreasing = TRUE),]
word.neg.pos500Vector <- word.neg.pos500$word

word.neg.select <- word.neg[!(word.neg$word %in% word.pos$word),]
word.pos.select <- word.pos[!(word.pos$word %in% word.neg$word),]
word.common.pos.neg <- word.neg[(word.neg$word %in% word.pos$word),]

word.neg.pos500Vector <- word.neg.pos500$word

# DocumentTermMatrix for PositiveNegative opinions
TripAdvisorPosNeg <- df[df$SentimentValue != "neutral",]
TripAdvisorFeatures <- WordTFIDF(TripAdvisorPosNeg$titleopinion,sparsity = .999,
                                 mode=2)$DOC
TripAdvisorFeatures <- ifelse(TripAdvisorFeatures > 0, 1, 0)
TripAdvisorFeatures <- TripAdvisorFeatures[, colnames(TripAdvisorFeatures) %in% as.character(word.neg.pos500Vector)]

sink(paste0("./Data/",dataset,"bigramColNames.txt"))
print(colnames(TripAdvisorFeatures))
sink()

SaveCSV(TripAdvisorFeatures,dataset,name="feat")
SaveCSV(TripAdvisorPosNeg, dataset,name="posneg")

# Preparing the final set
TripAdvisorAndFeatures <- cbind(TripAdvisorPosNeg, TripAdvisorFeatures)
TripAdvisorAndFeatures$pos <- NULL
TripAdvisorAndFeatures$neg <- NULL

# # Delete Neutral opinions
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentValue!="neutral",]
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentCore !="neutral",]

# Complete set
SaveCSV(dataframe = TripAdvisorAndFeatures,dataset,name = "BigramFeatures")

# Not matching
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures[!(TripAdvisorAndFeatures$SentimentValue=="positive" & TripAdvisorAndFeatures$SentimentCore=="negative"),]
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures_NOTMATCHING[!(TripAdvisorAndFeatures_NOTMATCHING$SentimentValue=="negative" & TripAdvisorAndFeatures_NOTMATCHING$SentimentCore=="positive"),]
SaveCSV(dataframe = TripAdvisorAndFeatures_NOTMATCHING,dataset,name = "BigramFeatures_NOMATCHING")

beep(2)