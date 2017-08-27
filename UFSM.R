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

dataset <- 1
df <- LoadCSV(dataset,FALSE,"EngCORE")

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
                            sparsity = 0.999,
                            mode=1)$Freq
word.tfidf.neg <- WordTFIDF(TripAdvisor$titleopinion
                            [TripAdvisor$SentimentValue == "negative"],
                            sparsity = 0.999,
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

# Order and select most 500 interesting words (tfidf)
wordNeg <- wordNeg[order(tfidf, decreasing = TRUE),]
if(nrow(wordNeg) < 500){
  wordNeg500 <- wordNeg[1:nrow(wordNeg),]
} else wordNeg500 <- wordNeg[1:500,]

if(nrow(wordPos) < 500){
  wordPos500 <- wordPos[1:nrow(wordPos),]
} else wordPos500 <- wordPos[1:500,]

# Group up the 500 most interesting words with positive and negative sentiment and
# change the columns name.
wordNegPos500 <- merge(wordNeg500, wordPos500, by = "word", all = TRUE)
setnames(wordNegPos500, old=c("tfidf.x", "freq.x", "tfidf.y", "freq.y"),
         new=c("tfidfNeg", "freqNeg", "tfidfPos", "freqPos"))

# Order by higher tfidf and extract words
wordNegPos500 <- wordNegPos500[order(tfidfNeg, decreasing = TRUE),]
wordNegPos500Vector <- wordNegPos500$word

wordNegSelect <- wordNeg[!(wordNeg$word %in% wordPos$word),]
wordPosSelect <- wordPos[!(wordPos$word %in% wordNeg$word),]
wordCommonPosNeg <- wordNeg[(wordNeg$word %in% wordPos$word),]


# Order alphabeticaly to see repeated words because of a wrong stemming
wordAlphOrdered <- with(wordCommonPosNeg,wordCommonPosNeg[order(word)])

SaveCSV(wordNegPos500Vector,dataset,"500V")

# Free memory
rm(word.freq.neg)
rm(word.freq.pos)
rm(word.tfidf.neg)
rm(word.tfidf.pos)

# DocumentTermMatrix for PositiveNegative
TripAdvisorPosNeg <- TripAdvisor[TripAdvisor$SentimentValue != "neutral",]
TripAdvisorFeatures <- WordTFIDF(TripAdvisorPosNeg$titleopinion,
                                 sparsity = 0.999,
                                 mode = 1)$Temp
TripAdvisorFeatures <- ifelse(TripAdvisorFeatures > 0, 1, 0)
TripAdvisorFeatures <- TripAdvisorFeatures[, colnames(TripAdvisorFeatures) %in% wordNegPos500Vector]
TripAdvisorFeatures <- as.data.frame(TripAdvisorFeatures)

sink(paste0("./",dataset,"rawColNames.txt"))
print(colnames(TripAdvisorFeatures))
sink()
# Due to the stemming not working properly with some words, they will be
# removed mannually.

#Features deleted 


if(dataset == 1){
  # Prado
  
  
} else if(dataset == 2){
  # Tyssen
  
  #Café cafe
  TripAdvisorFeatures$cafeX <- TripAdvisorFeatures$cafe + TripAdvisorFeatures$café +
    TripAdvisorFeatures$cafeteria
  TripAdvisorFeatures$cafeX <- ifelse(TripAdvisorFeatures$cafeX > 1, 1, TripAdvisorFeatures$cafeX)
  TripAdvisorFeatures$cafe<- NULL
  TripAdvisorFeatures$café <- NULL
  TripAdvisorFeatures$cafeteria <- NULL
  
  #Class classic
  TripAdvisorFeatures$classX <- TripAdvisorFeatures$classic + TripAdvisorFeatures$class
  TripAdvisorFeatures$classX <- ifelse(TripAdvisorFeatures$classX > 1, 1, TripAdvisorFeatures$classX)
  TripAdvisorFeatures$class<- NULL
  TripAdvisorFeatures$classic <- NULL
  
  #easi easili
  TripAdvisorFeatures$easiX <- TripAdvisorFeatures$easi + TripAdvisorFeatures$easili +
    TripAdvisorFeatures$eas
  TripAdvisorFeatures$easiX <- ifelse(TripAdvisorFeatures$easiX > 1, 1, TripAdvisorFeatures$easiX)
  TripAdvisorFeatures$easi<- NULL
  TripAdvisorFeatures$easili <- NULL
  TripAdvisorFeatures$eas <- NULL
  
  #feel felt
  TripAdvisorFeatures$feelX <- TripAdvisorFeatures$feel + TripAdvisorFeatures$felt
  TripAdvisorFeatures$feelX <- ifelse(TripAdvisorFeatures$feelX > 1, 1, TripAdvisorFeatures$feelX)
  TripAdvisorFeatures$feel<- NULL
  TripAdvisorFeatures$felt <- NULL
  
  #leav left
  TripAdvisorFeatures$leaveX <- TripAdvisorFeatures$leav + TripAdvisorFeatures$left
  TripAdvisorFeatures$leaveX <- ifelse(TripAdvisorFeatures$leaveX > 1, 1, TripAdvisorFeatures$leaveX)
  TripAdvisorFeatures$leav<- NULL
  TripAdvisorFeatures$left <- NULL
  
  #add addit
  TripAdvisorFeatures$addX <- TripAdvisorFeatures$add + TripAdvisorFeatures$addit
  TripAdvisorFeatures$addX <- ifelse(TripAdvisorFeatures$addX > 1, 1, TripAdvisorFeatures$addX)
  TripAdvisorFeatures$addit<- NULL
  TripAdvisorFeatures$addit <- NULL
  
  #advis advisor
  TripAdvisorFeatures$advisX <- TripAdvisorFeatures$advis + TripAdvisorFeatures$advisor
  TripAdvisorFeatures$advisX <- ifelse(TripAdvisorFeatures$advisX > 1, 1, TripAdvisorFeatures$advisX)
  TripAdvisorFeatures$advis<- NULL
  TripAdvisorFeatures$advisor <- NULL
  
  #america american 
  TripAdvisorFeatures$americaX <- TripAdvisorFeatures$america + TripAdvisorFeatures$american
  TripAdvisorFeatures$americaX <- ifelse(TripAdvisorFeatures$americaX > 1, 1, TripAdvisorFeatures$americaX)
  TripAdvisorFeatures$america<- NULL
  TripAdvisorFeatures$american <- NULL
  
  #baro baron
  TripAdvisorFeatures$baronX <- TripAdvisorFeatures$baro + TripAdvisorFeatures$baron
  TripAdvisorFeatures$baronX <- ifelse(TripAdvisorFeatures$baronX > 1, 1, TripAdvisorFeatures$baronX)
  TripAdvisorFeatures$baro<- NULL
  TripAdvisorFeatures$baron <- NULL
  
  #base basement
  TripAdvisorFeatures$baseX <- TripAdvisorFeatures$base + TripAdvisorFeatures$basement
  TripAdvisorFeatures$baseX <- ifelse(TripAdvisorFeatures$baseX > 1, 1, TripAdvisorFeatures$baseX)
  TripAdvisorFeatures$base<- NULL
  TripAdvisorFeatures$basement <- NULL
  
  #bath bathroom
  TripAdvisorFeatures$bathX <- TripAdvisorFeatures$bath + TripAdvisorFeatures$bathroom
  TripAdvisorFeatures$bathX <- ifelse(TripAdvisorFeatures$bathX > 1, 1, TripAdvisorFeatures$bathX)
  TripAdvisorFeatures$bath<- NULL
  TripAdvisorFeatures$bathroom <- NULL
  
  #beati beauti beautiful
  TripAdvisorFeatures$beautifulX <- TripAdvisorFeatures$beati + TripAdvisorFeatures$beauti +
    TripAdvisorAndFeatures$beautiful
  TripAdvisorFeatures$beautifulX <- ifelse(TripAdvisorFeatures$beautifulX > 1, 1, TripAdvisorFeatures$beautifulX)
  TripAdvisorFeatures$beati<- NULL
  TripAdvisorFeatures$beauti <- NULL
  TripAdvisorFeatures$beautiful <- NULL
  
  #began begin
  TripAdvisorFeatures$beginX <- TripAdvisorFeatures$begin + TripAdvisorFeatures$began
  TripAdvisorFeatures$beginX <- ifelse(TripAdvisorFeatures$beginX > 1, 1, TripAdvisorFeatures$beginX)
  TripAdvisorFeatures$begin<- NULL
  TripAdvisorFeatures$began <- NULL
  
  #big bigger biggest
  TripAdvisorFeatures$bigX <- TripAdvisorFeatures$big + TripAdvisorFeatures$bigger +
    TripAdvisorFeatures$biggest
  
  TripAdvisorFeatures$leaveX <- ifelse(TripAdvisorFeatures$bigX > 1, 1, TripAdvisorFeatures$bigX)
  TripAdvisorFeatures$big<- NULL
  TripAdvisorFeatures$bigger <- NULL
  TripAdvisorFeatures$biggest <- NULL
  
  #blow blown 
  TripAdvisorFeatures$blowX <- TripAdvisorFeatures$blow + TripAdvisorFeatures$blown
  TripAdvisorFeatures$blowX <- ifelse(TripAdvisorFeatures$blowX > 1, 1, TripAdvisorFeatures$blowX)
  TripAdvisorFeatures$blow<- NULL
  TripAdvisorFeatures$blown <- NULL
  
  #"bornemisza"    "bornemiza"
  TripAdvisorFeatures$bornemizaX <- TripAdvisorFeatures$bornemisza + TripAdvisorFeatures$bornemiza
  TripAdvisorFeatures$bornemizaX <- ifelse(TripAdvisorFeatures$bornemizaX> 1, 1, TripAdvisorFeatures$bornemizaX)
  TripAdvisorFeatures$bornemisza<- NULL
  TripAdvisorFeatures$bornemiza <- NULL
  
  #breath breadth
  TripAdvisorFeatures$breathX <- TripAdvisorFeatures$breadth + TripAdvisorFeatures$breath
  TripAdvisorFeatures$breathX <- ifelse(TripAdvisorFeatures$breathX > 1, 1, TripAdvisorFeatures$breathX)
  TripAdvisorFeatures$breath<- NULL
  TripAdvisorFeatures$breadth <- NULL
  
  #build built
  TripAdvisorFeatures$builtX <- TripAdvisorFeatures$build + TripAdvisorFeatures$built
  TripAdvisorFeatures$builtX <- ifelse(TripAdvisorFeatures$builtX > 1, 1, TripAdvisorFeatures$builtX)
  TripAdvisorFeatures$build<- NULL
  TripAdvisorFeatures$built <- NULL
 
  #caravaggio carravaggio
  TripAdvisorFeatures$caravaggioX <- TripAdvisorFeatures$carravaggio + TripAdvisorFeatures$caravaggio +
    TripAdvisorFeatures$carravagio
  TripAdvisorFeatures$caravaggioX <- ifelse(TripAdvisorFeatures$caravaggioX > 1, 1, TripAdvisorFeatures$caravaggioX)
  TripAdvisorFeatures$caravaggio<- NULL
  TripAdvisorFeatures$carravaggio <- NULL 
  TripAdvisorFeatures$carravagio <- NULL
  
  #cezann cézann
  TripAdvisorFeatures$cezannX <- TripAdvisorFeatures$cezann + TripAdvisorFeatures$cézann
  TripAdvisorFeatures$cezannX <- ifelse(TripAdvisorFeatures$cezannX > 1, 1, TripAdvisorFeatures$cezannX)
  TripAdvisorFeatures$cezann<- NULL
  TripAdvisorFeatures$cézann <- NULL 
  
  #cheap cheaper
  TripAdvisorFeatures$cheapX <- TripAdvisorFeatures$cheap + TripAdvisorFeatures$cheaper
  TripAdvisorFeatures$cheapX <- ifelse(TripAdvisorFeatures$cheapX > 1, 1, TripAdvisorFeatures$cheapX)
  TripAdvisorFeatures$cheap<- NULL
  TripAdvisorFeatures$cheaper <- NULL 
  
  #choic choos chose chosen
  TripAdvisorFeatures$choiceX <- TripAdvisorFeatures$choic + TripAdvisorFeatures$choos +
    TripAdvisorFeatures$chose + TripAdvisorFeatures$chosen
  TripAdvisorFeatures$choiceX <- ifelse(TripAdvisorFeatures$choiceX > 1, 1, TripAdvisorFeatures$choiceX)
  TripAdvisorFeatures$chose<- NULL
  TripAdvisorFeatures$chosen <- NULL
  TripAdvisorFeatures$choic <- NULL 
  TripAdvisorFeatures$choos <- NULL 

  #cheap cheaper
  TripAdvisorFeatures$collectX <- TripAdvisorFeatures$collect + TripAdvisorFeatures$colect +
    TripAdvisorFeatures$collector
  TripAdvisorFeatures$collectX <- ifelse(TripAdvisorFeatures$collectX > 1, 1, TripAdvisorFeatures$collectX)
  TripAdvisorFeatures$colect<- NULL
  TripAdvisorFeatures$collect <- NULL 
  TripAdvisorFeatures$collector <- NULL 
  
  #comment commentari
  TripAdvisorFeatures$commentX <- TripAdvisorFeatures$comment + TripAdvisorFeatures$commentari
  TripAdvisorFeatures$commentX <- ifelse(TripAdvisorFeatures$commentX > 1, 1, TripAdvisorFeatures$commentX)
  TripAdvisorFeatures$comment<- NULL
  TripAdvisorFeatures$commentari <- NULL 
  
  #compar comparison
  TripAdvisorFeatures$comparX <- TripAdvisorFeatures$compar + TripAdvisorFeatures$comparison
  TripAdvisorFeatures$comparX <- ifelse(TripAdvisorFeatures$comparX > 1, 1, TripAdvisorFeatures$comparX)
  TripAdvisorFeatures$compar<- NULL
  TripAdvisorFeatures$comparison <- NULL 
  
  #complain complaint
  TripAdvisorFeatures$complainX <- TripAdvisorFeatures$complain + TripAdvisorFeatures$complaint 
  TripAdvisorFeatures$complainX <- ifelse(TripAdvisorFeatures$complainX > 1, 1, TripAdvisorFeatures$complainX)
  TripAdvisorFeatures$complain<- NULL
  TripAdvisorFeatures$complaint <- NULL
  
  #dali dalí
  TripAdvisorFeatures$daliX <- TripAdvisorFeatures$dalí + TripAdvisorFeatures$dali 
  TripAdvisorFeatures$daliX <- ifelse(TripAdvisorFeatures$daliX > 1, 1, TripAdvisorFeatures$daliX)
  TripAdvisorFeatures$dali<- NULL
  TripAdvisorFeatures$dalí <- NULL
  
  #deep deepli
  TripAdvisorFeatures$deepX <- TripAdvisorFeatures$deepli + TripAdvisorFeatures$deep 
  TripAdvisorFeatures$deepX <- ifelse(TripAdvisorFeatures$deepX > 1, 1, TripAdvisorFeatures$deepX)
  TripAdvisorFeatures$deep<- NULL
  TripAdvisorFeatures$deepli <- NULL
  
  #defin definit
  TripAdvisorFeatures$definX <- TripAdvisorFeatures$definit + TripAdvisorFeatures$defin 
  TripAdvisorFeatures$definX <- ifelse(TripAdvisorFeatures$definX > 1, 1, TripAdvisorFeatures$definX)
  TripAdvisorFeatures$defin<- NULL
  TripAdvisorFeatures$definit <- NULL
  
  #defin definit
  TripAdvisorFeatures$didntX <- TripAdvisorFeatures$didnt + TripAdvisorFeatures$didn 
  TripAdvisorFeatures$didntX <- ifelse(TripAdvisorFeatures$didntX > 1, 1, TripAdvisorFeatures$didntX)
  TripAdvisorFeatures$didn<- NULL
  TripAdvisorFeatures$didnt <- NULL
  
  #discov disveri
  TripAdvisorFeatures$discoveriX <- TripAdvisorFeatures$discov + TripAdvisorFeatures$discoveri 
  TripAdvisorFeatures$discoveriX <- ifelse(TripAdvisorFeatures$discoveriX > 1, 1, TripAdvisorFeatures$discoveriX)
  TripAdvisorFeatures$discov<- NULL
  TripAdvisorFeatures$discoveri <- NULL
  
  #divers diversifi
  TripAdvisorFeatures$diversX <- TripAdvisorFeatures$diversifi + TripAdvisorFeatures$divers 
  TripAdvisorFeatures$diversX <- ifelse(TripAdvisorFeatures$diversX > 1, 1, TripAdvisorFeatures$diversX)
  TripAdvisorFeatures$divers<- NULL
  TripAdvisorFeatures$diversifi <- NULL
  
  #draw drawn
  TripAdvisorFeatures$drawX <- TripAdvisorFeatures$draw + TripAdvisorFeatures$drawn
  TripAdvisorFeatures$drawX <- ifelse(TripAdvisorFeatures$drawX > 1, 1, TripAdvisorFeatures$drawX)
  TripAdvisorFeatures$draw<- NULL
  TripAdvisorFeatures$drawn <- NULL
  
  #earli earliest earlier
  TripAdvisorFeatures$earliX <- TripAdvisorFeatures$earli + TripAdvisorFeatures$earlier +
    TripAdvisorFeatures$earliest
  TripAdvisorFeatures$earliX <- ifelse(TripAdvisorFeatures$earliX > 1, 1, TripAdvisorFeatures$earliX)
  TripAdvisorFeatures$earli<- NULL
  TripAdvisorFeatures$earlier <- NULL
  TripAdvisorFeatures$earliest <- NULL
  
  #eat eater
  TripAdvisorFeatures$eatX <- TripAdvisorFeatures$eat + TripAdvisorFeatures$eater
  TripAdvisorFeatures$eatX <- ifelse(TripAdvisorFeatures$eatX > 1, 1, TripAdvisorFeatures$eatX)
  TripAdvisorFeatures$eat<- NULL
  TripAdvisorFeatures$eater <- NULL
  
  #end endless
  TripAdvisorFeatures$endX <- TripAdvisorFeatures$endless + TripAdvisorFeatures$end
  TripAdvisorFeatures$endX <- ifelse(TripAdvisorFeatures$endX > 1, 1, TripAdvisorFeatures$endX)
  TripAdvisorFeatures$end<- NULL
  TripAdvisorFeatures$endless <- NULL
  
  #end endless
  TripAdvisorFeatures$explainX <- TripAdvisorFeatures$explain + TripAdvisorFeatures$explan
  TripAdvisorFeatures$explainX <- ifelse(TripAdvisorFeatures$explainX > 1, 1, TripAdvisorFeatures$explainX)
  TripAdvisorFeatures$explan<- NULL
  TripAdvisorFeatures$explain <- NULL
  
  #end endless
  TripAdvisorFeatures$organX <- TripAdvisorFeatures$organ + TripAdvisorFeatures$organis
  TripAdvisorFeatures$organX <- ifelse(TripAdvisorFeatures$organX > 1, 1, TripAdvisorFeatures$organX)
  TripAdvisorFeatures$organ<- NULL
  TripAdvisorFeatures$organis <- NULL
  
  #end endless
  TripAdvisorFeatures$showX <- TripAdvisorFeatures$show + TripAdvisorFeatures$shown
  TripAdvisorFeatures$showX <- ifelse(TripAdvisorFeatures$showX > 1, 1, TripAdvisorFeatures$showX)
  TripAdvisorFeatures$show<- NULL
  TripAdvisorFeatures$shown <- NULL

} else if(dataset == 3){
  # Reina Sofía
} else if(dataset == 4){
  # Dalí

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
  
} else if (dataset == 5){
  # city
}

TripAdvisorFeatures <- TripAdvisorFeatures[ ,order(names(TripAdvisorFeatures))]

# Preparing the final set
TripAdvisorAndFeatures <- cbind(TripAdvisorPosNeg, TripAdvisorFeatures)
TripAdvisorAndFeatures$pos <- NULL
TripAdvisorAndFeatures$neg <- NULL

# Delete Neutral opinions
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentValue!="neutral",]
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentCoreNLP!="neutral",]

# Complete set
SaveCSV(TripAdvisorAndFeatures,dataset,"UnigramFeatures")

# DataFrame where the machine sentiment does not match the expert's opinion
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures[!(TripAdvisorAndFeatures$SentimentValue=="positive" & TripAdvisorAndFeatures$SentimentCoreNLP=="negative"),]
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures_NOTMATCHING[!(TripAdvisorAndFeatures_NOTMATCHING$SentimentValue=="negative" & TripAdvisorAndFeatures_NOTMATCHING$SentimentCoreNLP=="positive"),]

SaveCSV(TripAdvisorAndFeatures_NOTMATCHING,dataset,"UnigramFeatures_NOMATCHING")


beep(2)