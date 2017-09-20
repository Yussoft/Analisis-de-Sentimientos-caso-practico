#------------------------------------------------------------------------------#
#
# Author: JesÃºs SÃ¡nchez de Castro
# Impired by: Ana Valdivia
# Date: September 2017
#
#                       UNIGRAM FEATURE SELECTION METHOD 
#
#------------------------------------------------------------------------------#

#Remember to change working directory
source("utils.R")
source("feature_extraction.R")

# MUSEUMS:
# NÂº1 : Prado Museum: 1230 pages
# NÂº2 : Tyssen Museum: 380 pages
# NÂº3 : Reina sofia : 340 pages
# NÂº4 : Dali: 140 pages
# NÂº5 : Guggenheim: 400 pages


dataset <- 5
df <- LoadCSV(dataset,FALSE,"CoreEng")

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
word.freq.pos <- word.freq.pos[order(tf, decreasing = TRUE),]
word.freq.neg <- as.data.table(word.freq.neg)
word.freq.neg <- word.freq.neg[order(tf, decreasing = TRUE),]

#-----------------------------------TFIDF--------------------------------------#

word.tfidf.pos <- WordTFIDF(TripAdvisor$titleopinion
                            [TripAdvisor$SentimentValue == "positive"],
                            sparsity = 0.999,
                            mode=1)$TFIDF
word.tfidf.neg <- WordTFIDF(TripAdvisor$titleopinion
                            [TripAdvisor$SentimentValue == "negative"],
                            sparsity = 0.999,
                            mode=1)$TFIDF

# Order from higher frequency to lower.
word.tfidf.pos <- as.data.table(word.tfidf.pos)
word.tfidf.pos <- word.tfidf.pos[order(tfidf, decreasing = TRUE),]
word.tfidf.neg <- as.data.table(word.tfidf.neg)
word.tfidf.neg <- word.tfidf.neg[order(tfidf, decreasing = TRUE),]

#----------------ORDER THE DATA IN NEW DATAFRAMES------------------------------#

# Put tf*idf pos and neg together into
word.neg <- merge(word.tfidf.neg, word.freq.neg, by="word", all = TRUE)
word.pos <- merge(word.tfidf.pos, word.freq.pos, by="word", all = TRUE)

# Delete STOPWORDS
word.neg <- word.neg[!(word.neg$word %in% stopwords("SMART"))]
word.pos <- word.pos[!(word.pos$word %in% stopwords("SMART"))]

# Order and select most 500 interesting words (tfidf)
word.neg <- word.neg[order(tfidf, decreasing = TRUE),]
if(nrow(word.neg) < 500){
  word.neg500 <- word.neg[1:nrow(word.neg),]
} else word.neg500 <- word.neg[1:500,]

if(nrow(word.pos) < 500){
  word.pos500 <- word.pos[1:nrow(word.pos),]
} else word.pos500 <- word.pos[1:500,]

# Group up the 500 most interesting words with positive and negative sentiment and
# change the columns name.
word.neg.pos500 <- merge(word.neg500, word.pos500, by = "word", all = TRUE)
word.neg.pos500 <- merge(word.neg500, word.pos500, by = "word", all = TRUE)
setnames(word.neg.pos500, old=c("tfidf.x", "tf.x", "tfidf.y", "tf.y"),
         new=c("tfidfNeg", "freqNeg", "tfidfPos", "freqPos"))

# Order by higher tfidf and extract words
word.neg.pos500 <- word.neg.pos500[order(tfidfNeg, decreasing = TRUE),]
word.neg.pos500Vector <- word.neg.pos500$word

word.neg.select <- word.neg[!(word.neg$word %in% word.pos$word),]
word.pos.select <- word.pos[!(word.pos$word %in% word.neg$word),]
word.common.pos.neg <- word.neg[(word.neg$word %in% word.pos$word),]

# Used in the word clouds
SaveCSV(word.neg,dataset,"wordNegCloud")
SaveCSV(word.pos,dataset,"wordPosCloud")

# Free memory
rm(word.freq.neg)
rm(word.freq.pos)
rm(word.tfidf.neg)
rm(word.tfidf.pos)

# mentTermMatrix for PositiveNegative
TripAdvisorPosNeg <- TripAdvisor[TripAdvisor$SentimentValue != "neutral",]
TripAdvisorFeatures <- WordTFIDF(TripAdvisorPosNeg$titleopinion,
                                 sparsity = 0.999,
                                 mode = 1)$DOC
TripAdvisorFeatures <- ifelse(TripAdvisorFeatures > 0, 1, 0)
TripAdvisorFeatures <- TripAdvisorFeatures[, colnames(TripAdvisorFeatures) %in% word.neg.pos500Vector]
TripAdvisorFeatures <- as.data.frame(TripAdvisorFeatures)

sink(paste0("./",dataset,"rawColNames.txt"))
print(colnames(TripAdvisorFeatures))
sink()
# Due to the stemming not working properly with some words, they will be
# removed mannually.

#Features deleted

if(dataset == 1){
  # Prado

  TripAdvisorFeatures$addX <- TripAdvisorFeatures$add + TripAdvisorFeatures$addit
  TripAdvisorFeatures$addX <- ifelse(TripAdvisorFeatures$addX > 1, 1, TripAdvisorFeatures$addX)
  TripAdvisorFeatures$add<- NULL
  TripAdvisorFeatures$addit <- NULL

  TripAdvisorFeatures$airX <- TripAdvisorFeatures$air + TripAdvisorFeatures$airi
  TripAdvisorFeatures$airX <- ifelse(TripAdvisorFeatures$airX > 1, 1, TripAdvisorFeatures$airX)
  TripAdvisorFeatures$air<- NULL
  TripAdvisorFeatures$airi <- NULL

  TripAdvisorFeatures$artifactX <- TripAdvisorFeatures$arti + TripAdvisorFeatures$artifact
  TripAdvisorFeatures$artifactX <- ifelse(TripAdvisorFeatures$artifactX > 1, 1, TripAdvisorFeatures$artifactX)
  TripAdvisorFeatures$arti<- NULL
  TripAdvisorFeatures$artifact <- NULL

  TripAdvisorFeatures$aimX <- TripAdvisorFeatures$aim + TripAdvisorFeatures$aimless
  TripAdvisorFeatures$aimX <- ifelse(TripAdvisorFeatures$aimX > 1, 1, TripAdvisorFeatures$aimX)
  TripAdvisorFeatures$aim<- NULL
  TripAdvisorFeatures$aimless <- NULL

  TripAdvisorFeatures$ampX <- TripAdvisorFeatures$amp + TripAdvisorFeatures$ampl
  TripAdvisorFeatures$ampX <- ifelse(TripAdvisorFeatures$ampX > 1, 1, TripAdvisorFeatures$ampX)
  TripAdvisorFeatures$amp<- NULL
  TripAdvisorFeatures$ampl <- NULL

  TripAdvisorFeatures$bigX <- TripAdvisorFeatures$big + TripAdvisorFeatures$bigger +
    TripAdvisorFeatures$biggest
  TripAdvisorFeatures$bigX <- ifelse(TripAdvisorFeatures$bigX > 1, 1, TripAdvisorFeatures$bigX)
  TripAdvisorFeatures$big<- NULL
  TripAdvisorFeatures$bigger<- NULL
  TripAdvisorFeatures$biggest <- NULL

  TripAdvisorFeatures$bitX <- TripAdvisorFeatures$bit + TripAdvisorFeatures$bite
  TripAdvisorFeatures$bitX <- ifelse(TripAdvisorFeatures$bitX > 1, 1, TripAdvisorFeatures$bitX)
  TripAdvisorFeatures$bit<- NULL
  TripAdvisorFeatures$bite <- NULL

  TripAdvisorFeatures$blowX <- TripAdvisorFeatures$blow + TripAdvisorFeatures$blown
  TripAdvisorFeatures$blowX <- ifelse(TripAdvisorFeatures$blowX > 1, 1, TripAdvisorFeatures$blowX)
  TripAdvisorFeatures$blow<- NULL
  TripAdvisorFeatures$blown <- NULL

  TripAdvisorFeatures$bookshopX <- TripAdvisorFeatures$bookshop + TripAdvisorFeatures$bookstor +
    TripAdvisorFeatures$booklet
  TripAdvisorFeatures$bookshopX <- ifelse(TripAdvisorFeatures$bookshopX > 1, 1, TripAdvisorFeatures$bookshopX)
  TripAdvisorFeatures$bookshop<- NULL
  TripAdvisorFeatures$bookstor <- NULL
  TripAdvisorFeatures$booklet <- NULL

  TripAdvisorFeatures$boschX <- TripAdvisorFeatures$bosch + TripAdvisorFeatures$bosh
  TripAdvisorFeatures$boschX <- ifelse(TripAdvisorFeatures$boschX > 1, 1, TripAdvisorFeatures$boschX)
  TripAdvisorFeatures$bosh<- NULL
  TripAdvisorFeatures$bosch <- NULL

  TripAdvisorFeatures$builtX <- TripAdvisorFeatures$build + TripAdvisorFeatures$built
  TripAdvisorFeatures$builtX <- ifelse(TripAdvisorFeatures$builtX > 1, 1, TripAdvisorFeatures$builtX)
  TripAdvisorFeatures$build<- NULL
  TripAdvisorFeatures$built <- NULL

  TripAdvisorFeatures$cafeX <- TripAdvisorFeatures$cafe + TripAdvisorFeatures$cafÃ©
  TripAdvisorFeatures$cafeX <- ifelse(TripAdvisorFeatures$cafeX > 1, 1, TripAdvisorFeatures$cafeX)
  TripAdvisorFeatures$cafe<- NULL
  TripAdvisorFeatures$cafÃ© <- NULL

  TripAdvisorFeatures$durerX <- TripAdvisorFeatures$durer + TripAdvisorFeatures$dÃ¼rer
  TripAdvisorFeatures$durerX <- ifelse(TripAdvisorFeatures$durerX > 1, 1, TripAdvisorFeatures$durerX)
  TripAdvisorFeatures$durer<- NULL
  TripAdvisorFeatures$dÃ¼rer <- NULL

  TripAdvisorFeatures$earliX <- TripAdvisorFeatures$earli + TripAdvisorFeatures$earlier
  TripAdvisorFeatures$earliX <- ifelse(TripAdvisorFeatures$earliX > 1, 1, TripAdvisorFeatures$earliX)
  TripAdvisorFeatures$earli<- NULL
  TripAdvisorFeatures$earlier <- NULL

  TripAdvisorFeatures$drawX <- TripAdvisorFeatures$draw + TripAdvisorFeatures$drawn
  TripAdvisorFeatures$drawX <- ifelse(TripAdvisorFeatures$drawX > 1, 1, TripAdvisorFeatures$drawX)
  TripAdvisorFeatures$draw<- NULL
  TripAdvisorFeatures$drawn <- NULL

  TripAdvisorFeatures$easiX <- TripAdvisorFeatures$easi + TripAdvisorFeatures$easili +
    TripAdvisorFeatures$eas
  TripAdvisorFeatures$easiX <- ifelse(TripAdvisorFeatures$easiX > 1, 1, TripAdvisorFeatures$easiX)
  TripAdvisorFeatures$easi<- NULL
  TripAdvisorFeatures$easili <- NULL
  TripAdvisorFeatures$eas <- NULL


  TripAdvisorFeatures$endX <- TripAdvisorFeatures$endless + TripAdvisorFeatures$end
  TripAdvisorFeatures$endX <- ifelse(TripAdvisorFeatures$endX > 1, 1, TripAdvisorFeatures$endX)
  TripAdvisorFeatures$end<- NULL
  TripAdvisorFeatures$endless <- NULL

  TripAdvisorFeatures$euroX <- TripAdvisorFeatures$eur + TripAdvisorFeatures$euro
  TripAdvisorFeatures$euroX <- ifelse(TripAdvisorFeatures$euroX > 1, 1, TripAdvisorFeatures$euroX)
  TripAdvisorFeatures$euro<- NULL
  TripAdvisorFeatures$eur <- NULL

  TripAdvisorFeatures$eventX <- TripAdvisorFeatures$eve + TripAdvisorFeatures$event +
    TripAdvisorFeatures$eventu
  TripAdvisorFeatures$eventX <- ifelse(TripAdvisorFeatures$eventX > 1, 1, TripAdvisorFeatures$eventX)
  TripAdvisorFeatures$eve<- NULL
  TripAdvisorFeatures$event <- NULL
  TripAdvisorFeatures$eventu <- NULL

  TripAdvisorFeatures$fullX <- TripAdvisorFeatures$full + TripAdvisorFeatures$fulli
  TripAdvisorFeatures$fullX <- ifelse(TripAdvisorFeatures$fullX > 1, 1, TripAdvisorFeatures$fullX)
  TripAdvisorFeatures$full<- NULL
  TripAdvisorFeatures$fulli <- NULL

  TripAdvisorFeatures$extendX <- TripAdvisorFeatures$extend + TripAdvisorFeatures$extens
  TripAdvisorFeatures$extendX <- ifelse(TripAdvisorFeatures$extendX > 1, 1, TripAdvisorFeatures$extendX)
  TripAdvisorFeatures$extend<- NULL
  TripAdvisorFeatures$extens <- NULL

  TripAdvisorFeatures$handX <- TripAdvisorFeatures$hand + TripAdvisorFeatures$handl
  TripAdvisorFeatures$handX <- ifelse(TripAdvisorFeatures$handX > 1, 1, TripAdvisorFeatures$handX)
  TripAdvisorFeatures$hand<- NULL
  TripAdvisorFeatures$handl <- NULL

  TripAdvisorFeatures$longX <- TripAdvisorFeatures$long + TripAdvisorFeatures$longer
  TripAdvisorFeatures$longX <- ifelse(TripAdvisorFeatures$longX > 1, 1, TripAdvisorFeatures$longX)
  TripAdvisorFeatures$long<- NULL
  TripAdvisorFeatures$longer <- NULL

  TripAdvisorFeatures$lourvX <- TripAdvisorFeatures$lourv + TripAdvisorFeatures$louvr
  TripAdvisorFeatures$lourvX <- ifelse(TripAdvisorFeatures$lourvX > 1, 1, TripAdvisorFeatures$lourvX)
  TripAdvisorFeatures$lourv<- NULL
  TripAdvisorFeatures$louvr <- NULL

  TripAdvisorFeatures$photoX <- TripAdvisorFeatures$photo + TripAdvisorFeatures$photographi
  TripAdvisorFeatures$photoX <- ifelse(TripAdvisorFeatures$photoX > 1, 1, TripAdvisorFeatures$photoX)
  TripAdvisorFeatures$photo<- NULL
  TripAdvisorFeatures$photographi <- NULL

  TripAdvisorFeatures$organX <- TripAdvisorFeatures$organ + TripAdvisorFeatures$organis
  TripAdvisorFeatures$organX <- ifelse(TripAdvisorFeatures$organX > 1, 1, TripAdvisorFeatures$organX)
  TripAdvisorFeatures$organ<- NULL
  TripAdvisorFeatures$organis <- NULL

  TripAdvisorFeatures$queueX <- TripAdvisorFeatures$queue + TripAdvisorFeatures$queu
  TripAdvisorFeatures$queueX <- ifelse(TripAdvisorFeatures$queueX > 1, 1, TripAdvisorFeatures$queueX)
  TripAdvisorFeatures$queu<- NULL
  TripAdvisorFeatures$queue <- NULL

  TripAdvisorFeatures$spendX <- TripAdvisorFeatures$spend + TripAdvisorFeatures$spent
  TripAdvisorFeatures$spendX <- ifelse(TripAdvisorFeatures$spendX > 1, 1, TripAdvisorFeatures$spendX)
  TripAdvisorFeatures$spend<- NULL
  TripAdvisorFeatures$spent <- NULL

  TripAdvisorFeatures$signX <- TripAdvisorFeatures$sign + TripAdvisorFeatures$signag
  TripAdvisorFeatures$signX <- ifelse(TripAdvisorFeatures$signX > 1, 1, TripAdvisorFeatures$signX)
  TripAdvisorFeatures$sign<- NULL
  TripAdvisorFeatures$signag <- NULL

  TripAdvisorFeatures$velazquezX <- TripAdvisorFeatures$velasquez + TripAdvisorFeatures$velazquez
  TripAdvisorFeatures$velazquezX <- ifelse(TripAdvisorFeatures$velazquezX > 1, 1, TripAdvisorFeatures$velazquezX)
  TripAdvisorFeatures$velasquez<- NULL
  TripAdvisorFeatures$velazquez <- NULL
  TripAdvisorFeatures$velazquez <- NULL


} else if(dataset == 2){
  # Tyssen-Bornemisza

  TripAdvisorFeatures$cafeX <- TripAdvisorFeatures$cafe + TripAdvisorFeatures$café +
    TripAdvisorFeatures$cafeteria
  TripAdvisorFeatures$cafeX <- ifelse(TripAdvisorFeatures$cafeX > 1, 1, TripAdvisorFeatures$cafeX)
  TripAdvisorFeatures$cafe<- NULL
  TripAdvisorFeatures$caf <- NULL
  TripAdvisorFeatures$cafeteria <- NULL

  TripAdvisorFeatures$easiX <- TripAdvisorFeatures$easi + TripAdvisorFeatures$easili +
    TripAdvisorFeatures$eas
  TripAdvisorFeatures$easiX <- ifelse(TripAdvisorFeatures$easiX > 1, 1, TripAdvisorFeatures$easiX)
  TripAdvisorFeatures$easi<- NULL
  TripAdvisorFeatures$easili <- NULL
  TripAdvisorFeatures$eas <- NULL

  TripAdvisorFeatures$feelX <- TripAdvisorFeatures$feel + TripAdvisorFeatures$felt
  TripAdvisorFeatures$feelX <- ifelse(TripAdvisorFeatures$feelX > 1, 1, TripAdvisorFeatures$feelX)
  TripAdvisorFeatures$feel<- NULL
  TripAdvisorFeatures$felt <- NULL

  TripAdvisorFeatures$leaveX <- TripAdvisorFeatures$leav + TripAdvisorFeatures$left
  TripAdvisorFeatures$leaveX <- ifelse(TripAdvisorFeatures$leaveX > 1, 1, TripAdvisorFeatures$leaveX)
  TripAdvisorFeatures$leav<- NULL
  TripAdvisorFeatures$left <- NULL

  TripAdvisorFeatures$addX <- TripAdvisorFeatures$add + TripAdvisorFeatures$addit
  TripAdvisorFeatures$addX <- ifelse(TripAdvisorFeatures$addX > 1, 1, TripAdvisorFeatures$addX)
  TripAdvisorFeatures$addit<- NULL
  TripAdvisorFeatures$addit <- NULL

  TripAdvisorFeatures$advisX <- TripAdvisorFeatures$advis + TripAdvisorFeatures$advisor
  TripAdvisorFeatures$advisX <- ifelse(TripAdvisorFeatures$advisX > 1, 1, TripAdvisorFeatures$advisX)
  TripAdvisorFeatures$advis<- NULL
  TripAdvisorFeatures$advisor <- NULL

  TripAdvisorFeatures$americaX <- TripAdvisorFeatures$america + TripAdvisorFeatures$american
  TripAdvisorFeatures$americaX <- ifelse(TripAdvisorFeatures$americaX > 1, 1, TripAdvisorFeatures$americaX)
  TripAdvisorFeatures$america<- NULL
  TripAdvisorFeatures$american <- NULL

  TripAdvisorFeatures$baronX <- TripAdvisorFeatures$baro + TripAdvisorFeatures$baron
  TripAdvisorFeatures$baronX <- ifelse(TripAdvisorFeatures$baronX > 1, 1, TripAdvisorFeatures$baronX)
  TripAdvisorFeatures$baro<- NULL
  TripAdvisorFeatures$baron <- NULL

  TripAdvisorFeatures$baseX <- TripAdvisorFeatures$base + TripAdvisorFeatures$basement
  TripAdvisorFeatures$baseX <- ifelse(TripAdvisorFeatures$baseX > 1, 1, TripAdvisorFeatures$baseX)
  TripAdvisorFeatures$base<- NULL
  TripAdvisorFeatures$basement <- NULL

  TripAdvisorFeatures$bathX <- TripAdvisorFeatures$bath + TripAdvisorFeatures$bathroom
  TripAdvisorFeatures$bathX <- ifelse(TripAdvisorFeatures$bathX > 1, 1, TripAdvisorFeatures$bathX)
  TripAdvisorFeatures$bath<- NULL
  TripAdvisorFeatures$bathroom <- NULL

  TripAdvisorFeatures$beautifulX <- TripAdvisorFeatures$beati + TripAdvisorFeatures$beauti +
    TripAdvisorFeatures$beautiful
  TripAdvisorFeatures$beautifulX <- ifelse(TripAdvisorFeatures$beautifulX > 1, 1, TripAdvisorFeatures$beautifulX)
  TripAdvisorFeatures$beati<- NULL
  TripAdvisorFeatures$beauti <- NULL
  TripAdvisorFeatures$beautiful <- NULL

  TripAdvisorFeatures$beginX <- TripAdvisorFeatures$begin + TripAdvisorFeatures$began
  TripAdvisorFeatures$beginX <- ifelse(TripAdvisorFeatures$beginX > 1, 1, TripAdvisorFeatures$beginX)
  TripAdvisorFeatures$begin<- NULL
  TripAdvisorFeatures$began <- NULL

  TripAdvisorFeatures$bigX <- TripAdvisorFeatures$big + TripAdvisorFeatures$bigger +
    TripAdvisorFeatures$biggest
  TripAdvisorFeatures$leaveX <- ifelse(TripAdvisorFeatures$bigX > 1, 1, TripAdvisorFeatures$bigX)
  TripAdvisorFeatures$big<- NULL
  TripAdvisorFeatures$bigger <- NULL
  TripAdvisorFeatures$biggest <- NULL

  TripAdvisorFeatures$blowX <- TripAdvisorFeatures$blow + TripAdvisorFeatures$blown
  TripAdvisorFeatures$blowX <- ifelse(TripAdvisorFeatures$blowX > 1, 1, TripAdvisorFeatures$blowX)
  TripAdvisorFeatures$blow<- NULL
  TripAdvisorFeatures$blown <- NULL

  TripAdvisorFeatures$bornemizaX <- TripAdvisorFeatures$bornemisza + TripAdvisorFeatures$bornemiza
  TripAdvisorFeatures$bornemizaX <- ifelse(TripAdvisorFeatures$bornemizaX> 1, 1, TripAdvisorFeatures$bornemizaX)
  TripAdvisorFeatures$bornemisza<- NULL
  TripAdvisorFeatures$bornemiza <- NULL

  TripAdvisorFeatures$breathX <- TripAdvisorFeatures$breadth + TripAdvisorFeatures$breath
  TripAdvisorFeatures$breathX <- ifelse(TripAdvisorFeatures$breathX > 1, 1, TripAdvisorFeatures$breathX)
  TripAdvisorFeatures$breath<- NULL
  TripAdvisorFeatures$breadth <- NULL

  TripAdvisorFeatures$builtX <- TripAdvisorFeatures$build + TripAdvisorFeatures$built
  TripAdvisorFeatures$builtX <- ifelse(TripAdvisorFeatures$builtX > 1, 1, TripAdvisorFeatures$builtX)
  TripAdvisorFeatures$build<- NULL
  TripAdvisorFeatures$built <- NULL

  TripAdvisorFeatures$caravaggioX <- TripAdvisorFeatures$carravaggio + TripAdvisorFeatures$caravaggio +
    TripAdvisorFeatures$carravagio
  TripAdvisorFeatures$caravaggioX <- ifelse(TripAdvisorFeatures$caravaggioX > 1, 1, TripAdvisorFeatures$caravaggioX)
  TripAdvisorFeatures$caravaggio<- NULL
  TripAdvisorFeatures$carravaggio <- NULL
  TripAdvisorFeatures$carravagio <- NULL

  TripAdvisorFeatures$cheapX <- TripAdvisorFeatures$cheap + TripAdvisorFeatures$cheaper
  TripAdvisorFeatures$cheapX <- ifelse(TripAdvisorFeatures$cheapX > 1, 1, TripAdvisorFeatures$cheapX)
  TripAdvisorFeatures$cheap<- NULL
  TripAdvisorFeatures$cheaper <- NULL

    TripAdvisorFeatures$choiceX <- TripAdvisorFeatures$choic + TripAdvisorFeatures$choos +
    TripAdvisorFeatures$chose + TripAdvisorFeatures$chosen
  TripAdvisorFeatures$choiceX <- ifelse(TripAdvisorFeatures$choiceX > 1, 1, TripAdvisorFeatures$choiceX)
  TripAdvisorFeatures$chose<- NULL
  TripAdvisorFeatures$chosen <- NULL
  TripAdvisorFeatures$choic <- NULL
  TripAdvisorFeatures$choos <- NULL

  TripAdvisorFeatures$collectX <- TripAdvisorFeatures$collect + TripAdvisorFeatures$colect +
    TripAdvisorFeatures$collector
  TripAdvisorFeatures$collectX <- ifelse(TripAdvisorFeatures$collectX > 1, 1, TripAdvisorFeatures$collectX)
  TripAdvisorFeatures$colect<- NULL
  TripAdvisorFeatures$collect <- NULL
  TripAdvisorFeatures$collector <- NULL

  TripAdvisorFeatures$commentX <- TripAdvisorFeatures$comment + TripAdvisorFeatures$commentari
  TripAdvisorFeatures$commentX <- ifelse(TripAdvisorFeatures$commentX > 1, 1, TripAdvisorFeatures$commentX)
  TripAdvisorFeatures$comment<- NULL
  TripAdvisorFeatures$commentari <- NULL

  TripAdvisorFeatures$comparX <- TripAdvisorFeatures$compar + TripAdvisorFeatures$comparison
  TripAdvisorFeatures$comparX <- ifelse(TripAdvisorFeatures$comparX > 1, 1, TripAdvisorFeatures$comparX)
  TripAdvisorFeatures$compar<- NULL
  TripAdvisorFeatures$comparison <- NULL

    TripAdvisorFeatures$complainX <- TripAdvisorFeatures$complain + TripAdvisorFeatures$complaint
  TripAdvisorFeatures$complainX <- ifelse(TripAdvisorFeatures$complainX > 1, 1, TripAdvisorFeatures$complainX)
  TripAdvisorFeatures$complain<- NULL
  TripAdvisorFeatures$complaint <- NULL

  TripAdvisorFeatures$daliX <- TripAdvisorFeatures$dalí + TripAdvisorFeatures$dali
  TripAdvisorFeatures$daliX <- ifelse(TripAdvisorFeatures$daliX > 1, 1, TripAdvisorFeatures$daliX)
  TripAdvisorFeatures$dali<- NULL
  TripAdvisorFeatures$dalí <- NULL

  TripAdvisorFeatures$deepX <- TripAdvisorFeatures$deepli + TripAdvisorFeatures$deep
  TripAdvisorFeatures$deepX <- ifelse(TripAdvisorFeatures$deepX > 1, 1, TripAdvisorFeatures$deepX)
  TripAdvisorFeatures$deep<- NULL
  TripAdvisorFeatures$deepli <- NULL

  TripAdvisorFeatures$definX <- TripAdvisorFeatures$definit + TripAdvisorFeatures$defin
  TripAdvisorFeatures$definX <- ifelse(TripAdvisorFeatures$definX > 1, 1, TripAdvisorFeatures$definX)
  TripAdvisorFeatures$defin<- NULL
  TripAdvisorFeatures$definit <- NULL

  TripAdvisorFeatures$didntX <- TripAdvisorFeatures$didnt + TripAdvisorFeatures$didn
  TripAdvisorFeatures$didntX <- ifelse(TripAdvisorFeatures$didntX > 1, 1, TripAdvisorFeatures$didntX)
  TripAdvisorFeatures$didn<- NULL
  TripAdvisorFeatures$didnt <- NULL

  TripAdvisorFeatures$discoveriX <- TripAdvisorFeatures$discov + TripAdvisorFeatures$discoveri
  TripAdvisorFeatures$discoveriX <- ifelse(TripAdvisorFeatures$discoveriX > 1, 1, TripAdvisorFeatures$discoveriX)
  TripAdvisorFeatures$discov<- NULL
  TripAdvisorFeatures$discoveri <- NULL

  TripAdvisorFeatures$diversiX <- TripAdvisorFeatures$diversifi + TripAdvisorFeatures$divers
  TripAdvisorFeatures$diversiX <- ifelse(TripAdvisorFeatures$diversiX > 1, 1, TripAdvisorFeatures$diversiX)
  TripAdvisorFeatures$divers<- NULL
  TripAdvisorFeatures$diversifi <- NULL

  TripAdvisorFeatures$drawX <- TripAdvisorFeatures$draw + TripAdvisorFeatures$drawn
  TripAdvisorFeatures$drawX <- ifelse(TripAdvisorFeatures$drawX > 1, 1, TripAdvisorFeatures$drawX)
  TripAdvisorFeatures$draw<- NULL
  TripAdvisorFeatures$drawn <- NULL

  TripAdvisorFeatures$earliX <- TripAdvisorFeatures$earli + TripAdvisorFeatures$earlier +
    TripAdvisorFeatures$earliest
  TripAdvisorFeatures$earliX <- ifelse(TripAdvisorFeatures$earliX > 1, 1, TripAdvisorFeatures$earliX)
  TripAdvisorFeatures$earli<- NULL
  TripAdvisorFeatures$earlier <- NULL
  TripAdvisorFeatures$earliest <- NULL

  TripAdvisorFeatures$eatX <- TripAdvisorFeatures$eat + TripAdvisorFeatures$eater
  TripAdvisorFeatures$eatX <- ifelse(TripAdvisorFeatures$eatX > 1, 1, TripAdvisorFeatures$eatX)
  TripAdvisorFeatures$eat<- NULL
  TripAdvisorFeatures$eater <- NULL

  TripAdvisorFeatures$endX <- TripAdvisorFeatures$endless + TripAdvisorFeatures$end
  TripAdvisorFeatures$endX <- ifelse(TripAdvisorFeatures$endX > 1, 1, TripAdvisorFeatures$endX)
  TripAdvisorFeatures$end<- NULL
  TripAdvisorFeatures$endless <- NULL

  TripAdvisorFeatures$explainX <- TripAdvisorFeatures$explain + TripAdvisorFeatures$explan
  TripAdvisorFeatures$explainX <- ifelse(TripAdvisorFeatures$explainX > 1, 1, TripAdvisorFeatures$explainX)
  TripAdvisorFeatures$explan<- NULL
  TripAdvisorFeatures$explain <- NULL

  TripAdvisorFeatures$organX <- TripAdvisorFeatures$organ + TripAdvisorFeatures$organis
  TripAdvisorFeatures$organX <- ifelse(TripAdvisorFeatures$organX > 1, 1, TripAdvisorFeatures$organX)
  TripAdvisorFeatures$organ<- NULL
  TripAdvisorFeatures$organis <- NULL

  TripAdvisorFeatures$showX <- TripAdvisorFeatures$show + TripAdvisorFeatures$shown
  TripAdvisorFeatures$showX <- ifelse(TripAdvisorFeatures$showX > 1, 1, TripAdvisorFeatures$showX)
  TripAdvisorFeatures$show<- NULL
  TripAdvisorFeatures$shown <- NULL

} else if(dataset == 3){

  # Reina Sofia
  TripAdvisorFeatures$actX <- TripAdvisorFeatures$act + TripAdvisorFeatures$action
  TripAdvisorFeatures$actX <- ifelse(TripAdvisorFeatures$actX > 1, 1, TripAdvisorFeatures$actX)
  TripAdvisorFeatures$act<- NULL
  TripAdvisorFeatures$action <- NULL

  TripAdvisorFeatures$addX <- TripAdvisorFeatures$add + TripAdvisorFeatures$addit
  TripAdvisorFeatures$addX <- ifelse(TripAdvisorFeatures$addX > 1, 1, TripAdvisorFeatures$addX)
  TripAdvisorFeatures$add<- NULL
  TripAdvisorFeatures$addit <- NULL

  TripAdvisorFeatures$aimX <- TripAdvisorFeatures$aim + TripAdvisorFeatures$aimless
  TripAdvisorFeatures$aimX <- ifelse(TripAdvisorFeatures$aimX > 1, 1, TripAdvisorFeatures$aimX)
  TripAdvisorFeatures$aim<- NULL
  TripAdvisorFeatures$aimless <- NULL

  TripAdvisorFeatures$airX <- TripAdvisorFeatures$air + TripAdvisorFeatures$airi
  TripAdvisorFeatures$airX <- ifelse(TripAdvisorFeatures$airX > 1, 1, TripAdvisorFeatures$airX)
  TripAdvisorFeatures$air<- NULL
  TripAdvisorFeatures$airi <- NULL

  TripAdvisorFeatures$americaX <- TripAdvisorFeatures$america + TripAdvisorFeatures$american
  TripAdvisorFeatures$americaX <- ifelse(TripAdvisorFeatures$americaX > 1, 1, TripAdvisorFeatures$americaX)
  TripAdvisorFeatures$america<- NULL
  TripAdvisorFeatures$american <- NULL

  TripAdvisorFeatures$ampX <- TripAdvisorFeatures$amp + TripAdvisorFeatures$ampl
  TripAdvisorFeatures$ampX <- ifelse(TripAdvisorFeatures$ampX > 1, 1, TripAdvisorFeatures$ampX)
  TripAdvisorFeatures$amp<- NULL
  TripAdvisorFeatures$ampl <- NULL

  TripAdvisorFeatures$architectX <- TripAdvisorFeatures$architect + TripAdvisorFeatures$architectur
  TripAdvisorFeatures$architectX <- ifelse(TripAdvisorFeatures$architectX > 1, 1, TripAdvisorFeatures$architectX)
  TripAdvisorFeatures$architect<- NULL
  TripAdvisorFeatures$architectur <- NULL

  TripAdvisorFeatures$artistX <- TripAdvisorFeatures$artist + TripAdvisorFeatures$artisit
  TripAdvisorFeatures$artistX <- ifelse(TripAdvisorFeatures$artistX > 1, 1, TripAdvisorFeatures$artistX)
  TripAdvisorFeatures$artisit<- NULL
  TripAdvisorFeatures$artisit <- NULL

  TripAdvisorFeatures$audioX <- TripAdvisorFeatures$audio + TripAdvisorFeatures$audioguid
  TripAdvisorFeatures$audioX <- ifelse(TripAdvisorFeatures$audioX > 1, 1, TripAdvisorFeatures$audioX)
  TripAdvisorFeatures$audio<- NULL
  TripAdvisorFeatures$audioguid <- NULL

  TripAdvisorFeatures$beginX <- TripAdvisorFeatures$begin + TripAdvisorFeatures$began
  TripAdvisorFeatures$beginX <- ifelse(TripAdvisorFeatures$beginX > 1, 1, TripAdvisorFeatures$beginX)
  TripAdvisorFeatures$begin<- NULL
  TripAdvisorFeatures$began <- NULL

  TripAdvisorFeatures$bigX <- TripAdvisorFeatures$big + TripAdvisorFeatures$bigger +
    TripAdvisorFeatures$biggest
  TripAdvisorFeatures$leaveX <- ifelse(TripAdvisorFeatures$bigX > 1, 1, TripAdvisorFeatures$bigX)
  TripAdvisorFeatures$big<- NULL
  TripAdvisorFeatures$bigger <- NULL
  TripAdvisorFeatures$biggest <- NULL

  TripAdvisorFeatures$bitX <- TripAdvisorFeatures$bit + TripAdvisorFeatures$bite
  TripAdvisorFeatures$bitX <- ifelse(TripAdvisorFeatures$bitX > 1, 1, TripAdvisorFeatures$bitX)
  TripAdvisorFeatures$bit<- NULL
  TripAdvisorFeatures$bite <- NULL

  TripAdvisorFeatures$breathX <- TripAdvisorFeatures$breath + TripAdvisorFeatures$breather
  TripAdvisorFeatures$breathX <- ifelse(TripAdvisorFeatures$breathX > 1, 1, TripAdvisorFeatures$breathX)
  TripAdvisorFeatures$breath<- NULL
  TripAdvisorFeatures$breather <- NULL

  TripAdvisorFeatures$brilliantX <- TripAdvisorFeatures$brilliant + TripAdvisorFeatures$brillant
  TripAdvisorFeatures$brilliantX <- ifelse(TripAdvisorFeatures$brilliantX > 1, 1, TripAdvisorFeatures$brilliantX)
  TripAdvisorFeatures$brillant<- NULL
  TripAdvisorFeatures$brilliant <- NULL

  TripAdvisorFeatures$builtX <- TripAdvisorFeatures$build + TripAdvisorFeatures$built
  TripAdvisorFeatures$builtX <- ifelse(TripAdvisorFeatures$builtX > 1, 1, TripAdvisorFeatures$builtX)
  TripAdvisorFeatures$build<- NULL
  TripAdvisorFeatures$built <- NULL

  TripAdvisorFeatures$busiX <- TripAdvisorFeatures$busier + TripAdvisorFeatures$busi
  TripAdvisorFeatures$busiX <- ifelse(TripAdvisorFeatures$busiX > 1, 1, TripAdvisorFeatures$busiX)
  TripAdvisorFeatures$busi<- NULL
  TripAdvisorFeatures$busier <- NULL

  TripAdvisorFeatures$cafeX <- TripAdvisorFeatures$cafe + TripAdvisorFeatures$café
  TripAdvisorFeatures$cafeX <- ifelse(TripAdvisorFeatures$cafeX > 1, 1, TripAdvisorFeatures$cafeX)
  TripAdvisorFeatures$cafe<- NULL
  TripAdvisorFeatures$café<- NULL

  TripAdvisorFeatures$centerX <- TripAdvisorFeatures$centerpiec + TripAdvisorFeatures$center +
    TripAdvisorFeatures$centr
  TripAdvisorFeatures$centerX <- ifelse(TripAdvisorFeatures$centerX > 1, 1, TripAdvisorFeatures$centerX)
  TripAdvisorFeatures$center<- NULL
  TripAdvisorFeatures$centerpiec <- NULL
  TripAdvisorFeatures$centr <- NULL

  TripAdvisorFeatures$cheapX <- TripAdvisorFeatures$cheap + TripAdvisorFeatures$cheaper
  TripAdvisorFeatures$cheapX <- ifelse(TripAdvisorFeatures$cheapX > 1, 1, TripAdvisorFeatures$cheapX)
  TripAdvisorFeatures$cheap<- NULL
  TripAdvisorFeatures$cheaper <- NULL

  TripAdvisorFeatures$civilX<- TripAdvisorFeatures$civilian + TripAdvisorFeatures$civil
  TripAdvisorFeatures$civilX <- ifelse(TripAdvisorFeatures$civilX > 1, 1, TripAdvisorFeatures$civilX)
  TripAdvisorFeatures$civil<- NULL
  TripAdvisorFeatures$civilian <- NULL

  TripAdvisorFeatures$childX <- TripAdvisorFeatures$children + TripAdvisorFeatures$child
  TripAdvisorFeatures$childX <- ifelse(TripAdvisorFeatures$childX > 1, 1, TripAdvisorFeatures$childX)
  TripAdvisorFeatures$child<- NULL
  TripAdvisorFeatures$children <- NULL

  TripAdvisorFeatures$choiceX <- TripAdvisorFeatures$choic + TripAdvisorFeatures$choos +
    TripAdvisorFeatures$chose
  TripAdvisorFeatures$choiceX <- ifelse(TripAdvisorFeatures$choiceX > 1, 1, TripAdvisorFeatures$choiceX)
  TripAdvisorFeatures$chose<- NULL
  TripAdvisorFeatures$choic <- NULL
  TripAdvisorFeatures$choos <- NULL

  TripAdvisorFeatures$cinemaX <- TripAdvisorFeatures$cinema + TripAdvisorFeatures$cinemat
  TripAdvisorFeatures$cinemaX <- ifelse(TripAdvisorFeatures$cinemaX > 1, 1, TripAdvisorFeatures$cinemaX)
  TripAdvisorFeatures$cinema<- NULL
  TripAdvisorFeatures$cinemat <- NULL

  TripAdvisorFeatures$classicX <- TripAdvisorFeatures$classic + TripAdvisorFeatures$classi
  TripAdvisorFeatures$classicX <- ifelse(TripAdvisorFeatures$classicX > 1, 1, TripAdvisorFeatures$classicX)
  TripAdvisorFeatures$classi<- NULL
  TripAdvisorFeatures$classic <- NULL

  TripAdvisorFeatures$colourX <- TripAdvisorFeatures$color + TripAdvisorFeatures$colour
  TripAdvisorFeatures$colourX <- ifelse(TripAdvisorFeatures$colourX > 1, 1, TripAdvisorFeatures$colourX)
  TripAdvisorFeatures$color<- NULL
  TripAdvisorFeatures$colour <- NULL

  TripAdvisorFeatures$companiX <- TripAdvisorFeatures$compani + TripAdvisorFeatures$companion
  TripAdvisorFeatures$companiX <- ifelse(TripAdvisorFeatures$companiX > 1, 1, TripAdvisorFeatures$companiX)
  TripAdvisorFeatures$compani<- NULL
  TripAdvisorFeatures$companion <- NULL

  TripAdvisorFeatures$contextX <- TripAdvisorFeatures$contextu + TripAdvisorFeatures$context
  TripAdvisorFeatures$contextX <- ifelse(TripAdvisorFeatures$contextX > 1, 1, TripAdvisorFeatures$contextX)
  TripAdvisorFeatures$context<- NULL
  TripAdvisorFeatures$contextu <- NULL

  TripAdvisorFeatures$courtX <- TripAdvisorFeatures$court + TripAdvisorFeatures$courtyard
  TripAdvisorFeatures$courtX <- ifelse(TripAdvisorFeatures$contextX > 1, 1, TripAdvisorFeatures$contextX)
  TripAdvisorFeatures$court<- NULL
  TripAdvisorFeatures$courtyard <- NULL

  TripAdvisorFeatures$daliX <- TripAdvisorFeatures$dali + TripAdvisorFeatures$dalí 
  TripAdvisorFeatures$daliX <- ifelse(TripAdvisorFeatures$daliX > 1, 1, TripAdvisorFeatures$daliX)
  TripAdvisorFeatures$dali<- NULL
  TripAdvisorFeatures$dalí <- NULL


  TripAdvisorFeatures$deepX <- TripAdvisorFeatures$deep + TripAdvisorFeatures$deeper +
    TripAdvisorFeatures$deepli
  TripAdvisorFeatures$deepX <- ifelse(TripAdvisorFeatures$deepX > 1, 1, TripAdvisorFeatures$deepX)
  TripAdvisorFeatures$deep<- NULL
  TripAdvisorFeatures$deeper <- NULL
  TripAdvisorFeatures$deepli <- NULL

  TripAdvisorFeatures$definitX <- TripAdvisorFeatures$definet + TripAdvisorFeatures$definit
  TripAdvisorFeatures$definitX <- ifelse(TripAdvisorFeatures$definitX > 1, 1, TripAdvisorFeatures$definitX)
  TripAdvisorFeatures$definet<- NULL
  TripAdvisorFeatures$definit <- NULL

  TripAdvisorFeatures$democraciX <- TripAdvisorFeatures$democraci + TripAdvisorFeatures$democrat
  TripAdvisorFeatures$democraciX <- ifelse(TripAdvisorFeatures$democraciX > 1, 1, TripAdvisorFeatures$democraciX)
  TripAdvisorFeatures$democrat<- NULL
  TripAdvisorFeatures$democraci <- NULL

  TripAdvisorFeatures$dictatX <- TripAdvisorFeatures$dictatorship + TripAdvisorFeatures$dictat
  TripAdvisorFeatures$dictatX <- ifelse(TripAdvisorFeatures$dictatX > 1, 1, TripAdvisorFeatures$dictatX)
  TripAdvisorFeatures$dictat<- NULL
  TripAdvisorFeatures$dictatorship <- NULL

  TripAdvisorFeatures$didntX <- TripAdvisorFeatures$didn + TripAdvisorFeatures$didnt
  TripAdvisorFeatures$didntX <- ifelse(TripAdvisorFeatures$didntX > 1, 1, TripAdvisorFeatures$didntX)
  TripAdvisorFeatures$didn<- NULL
  TripAdvisorFeatures$didnt <- NULL

  TripAdvisorFeatures$dificultX <- TripAdvisorFeatures$difficult + TripAdvisorFeatures$difficulti
  TripAdvisorFeatures$dificultX <- ifelse(TripAdvisorFeatures$dificultX > 1, 1, TripAdvisorFeatures$dificultX)
  TripAdvisorFeatures$difficult<- NULL
  TripAdvisorFeatures$difficulti <- NULL

  TripAdvisorFeatures$dinnerX <- TripAdvisorFeatures$dinner + TripAdvisorFeatures$dine
  TripAdvisorFeatures$dinnerX <- ifelse(TripAdvisorFeatures$dinnerX > 1, 1, TripAdvisorFeatures$dinnerX)
  TripAdvisorFeatures$dine  <- NULL
  TripAdvisorFeatures$dinner <- NULL

  TripAdvisorFeatures$disapointX <- TripAdvisorFeatures$disapoint + TripAdvisorFeatures$disappoint +
    TripAdvisorFeatures$dissapoint
  TripAdvisorFeatures$disapointX <- ifelse(TripAdvisorFeatures$disapointX > 1, 1, TripAdvisorFeatures$disapointX)
  TripAdvisorFeatures$disapoint  <- NULL
  TripAdvisorFeatures$disappoint <- NULL
  TripAdvisorFeatures$dissappoint <- NULL

  TripAdvisorFeatures$diveX <- TripAdvisorFeatures$divers + TripAdvisorFeatures$dive
  TripAdvisorFeatures$diveX <- ifelse(TripAdvisorFeatures$diveX > 1, 1, TripAdvisorFeatures$diveX)
  TripAdvisorFeatures$dive  <- NULL
  TripAdvisorFeatures$divers <- NULL

  TripAdvisorFeatures$commentX <- TripAdvisorFeatures$commentari + TripAdvisorFeatures$comment
  TripAdvisorFeatures$commentX <- ifelse(TripAdvisorFeatures$commentX > 1, 1, TripAdvisorFeatures$commentX)
  TripAdvisorFeatures$comment  <- NULL
  TripAdvisorFeatures$commentari <- NULL

  TripAdvisorFeatures$guernicaX <- TripAdvisorFeatures$guernika + TripAdvisorFeatures$guernica
  TripAdvisorFeatures$guernicaX <- ifelse(TripAdvisorFeatures$guernicaX > 1, 1, TripAdvisorFeatures$guernicaX)
  TripAdvisorFeatures$guernica  <- NULL
  TripAdvisorFeatures$guernika <- NULL

  TripAdvisorFeatures$museumX <- TripAdvisorFeatures$museo + TripAdvisorFeatures$museum
  TripAdvisorFeatures$museumX <- ifelse(TripAdvisorFeatures$museumX > 1, 1, TripAdvisorFeatures$museumX)
  TripAdvisorFeatures$museo  <- NULL
  TripAdvisorFeatures$museum <- NULL

  TripAdvisorFeatures$photoX <- TripAdvisorFeatures$photo + TripAdvisorFeatures$photographi
  TripAdvisorFeatures$photoX <- ifelse(TripAdvisorFeatures$photoX > 1, 1, TripAdvisorFeatures$photoX)
  TripAdvisorFeatures$photo<- NULL
  TripAdvisorFeatures$photographi <- NULL

  TripAdvisorFeatures$spendX <- TripAdvisorFeatures$spend + TripAdvisorFeatures$spent
  TripAdvisorFeatures$spendX <- ifelse(TripAdvisorFeatures$spendX > 1, 1, TripAdvisorFeatures$spendX)
  TripAdvisorFeatures$spend<- NULL
  TripAdvisorFeatures$spent <- NULL

  TripAdvisorFeatures$worstX <- TripAdvisorFeatures$wors + TripAdvisorFeatures$worst
  TripAdvisorFeatures$worstX <- ifelse(TripAdvisorFeatures$worstX > 1, 1, TripAdvisorFeatures$worstX)
  TripAdvisorFeatures$wors<- NULL
  TripAdvisorFeatures$worst <- NULL

} else if(dataset == 4){

  # Theater-Museum Dali

  TripAdvisorFeatures$addX <- TripAdvisorFeatures$add + TripAdvisorFeatures$addit
  TripAdvisorFeatures$addX <- ifelse(TripAdvisorFeatures$addX > 1, 1, TripAdvisorFeatures$addX)
  TripAdvisorFeatures$add<- NULL
  TripAdvisorFeatures$addit <- NULL

  TripAdvisorFeatures$artX <- TripAdvisorFeatures$art + TripAdvisorFeatures$artist +
    TripAdvisorFeatures$artwork
  TripAdvisorFeatures$artX <- ifelse(TripAdvisorFeatures$artX > 1, 1, TripAdvisorFeatures$artX)
  TripAdvisorFeatures$art <- NULL
  TripAdvisorFeatures$artist <- NULL
  TripAdvisorFeatures$artwork <- NULL

  TripAdvisorFeatures$creationX <- TripAdvisorFeatures$creation+ TripAdvisorFeatures$creativ
  TripAdvisorFeatures$creationX <- ifelse(TripAdvisorFeatures$creationX > 1, 1, TripAdvisorFeatures$creationX)
  TripAdvisorFeatures$creation<- NULL
  TripAdvisorFeatures$creativ <- NULL

  TripAdvisorFeatures$daliX <- TripAdvisorFeatures$dali + TripAdvisorFeatures$dalí
  TripAdvisorFeatures$daliX <- ifelse(TripAdvisorFeatures$daliX > 1, 1, TripAdvisorFeatures$daliX)
  TripAdvisorFeatures$dali<- NULL
  TripAdvisorFeatures$dalí <- NULL

  TripAdvisorFeatures$easiX <- TripAdvisorFeatures$easi + TripAdvisorFeatures$easili
  TripAdvisorFeatures$easiX <- ifelse(TripAdvisorFeatures$easiX > 1, 1, TripAdvisorFeatures$easiX)
  TripAdvisorFeatures$easi<- NULL
  TripAdvisorFeatures$easili <- NULL

  TripAdvisorFeatures$familyX <- TripAdvisorFeatures$famili + TripAdvisorFeatures$familiar
  TripAdvisorFeatures$familyX <- ifelse(TripAdvisorFeatures$familyX > 1, 1, TripAdvisorFeatures$familyX)
  TripAdvisorFeatures$famili<- NULL
  TripAdvisorFeatures$familiar <- NULL

  TripAdvisorFeatures$infoX <- TripAdvisorFeatures$info + TripAdvisorFeatures$inform
  TripAdvisorFeatures$infoX <- ifelse(TripAdvisorFeatures$infoX > 1, 1, TripAdvisorFeatures$infoX)
  TripAdvisorFeatures$info<- NULL
  TripAdvisorFeatures$inform <- NULL


  TripAdvisorFeatures$jewelX <- TripAdvisorFeatures$jewelleri + TripAdvisorFeatures$jewel
  + TripAdvisorFeatures$jewelri
  TripAdvisorFeatures$jewelX <- ifelse(TripAdvisorFeatures$jewelX > 1, 1, TripAdvisorFeatures$jewelX)
  TripAdvisorFeatures$jewel<- NULL
  TripAdvisorFeatures$jewelleri <- NULL
  TripAdvisorFeatures$jewelri <- NULL

  TripAdvisorFeatures$loveX <- TripAdvisorFeatures$love + TripAdvisorFeatures$lover
  TripAdvisorFeatures$loveX <- ifelse(TripAdvisorFeatures$loveX > 1, 1, TripAdvisorFeatures$loveX)
  TripAdvisorFeatures$love<- NULL
  TripAdvisorFeatures$lover <- NULL

  TripAdvisorFeatures$theaterX <- TripAdvisorFeatures$theater+ TripAdvisorFeatures$theatr
  TripAdvisorFeatures$theaterX <- ifelse(TripAdvisorFeatures$theaterX > 1, 1, TripAdvisorFeatures$theaterX)
  TripAdvisorFeatures$theater<- NULL
  TripAdvisorFeatures$theatr<- NULL

  TripAdvisorFeatures$tourX <- TripAdvisorFeatures$tour + TripAdvisorFeatures$tourist
  TripAdvisorFeatures$tourX <- ifelse(TripAdvisorFeatures$tourX > 1, 1, TripAdvisorFeatures$tourX)
  TripAdvisorFeatures$tour <- NULL
  TripAdvisorFeatures$tourist <- NULL

  TripAdvisorFeatures$visitX <- TripAdvisorFeatures$visit + TripAdvisorFeatures$visitor
  TripAdvisorFeatures$visitX <- ifelse(TripAdvisorFeatures$visitX > 1, 1, TripAdvisorFeatures$visitX)
  TripAdvisorFeatures$visitor<- NULL
  TripAdvisorFeatures$visit <- NULL

} else if (dataset == 5){
  # Guggenheim Museum
  
  TripAdvisorFeatures$actX <- TripAdvisorFeatures$act + TripAdvisorFeatures$action
  TripAdvisorFeatures$actX <- ifelse(TripAdvisorFeatures$actX > 1, 1, TripAdvisorFeatures$actX)
  TripAdvisorFeatures$act<- NULL
  TripAdvisorFeatures$action <- NULL
  
  TripAdvisorFeatures$addX <- TripAdvisorFeatures$add + TripAdvisorFeatures$addit
  TripAdvisorFeatures$addX <- ifelse(TripAdvisorFeatures$addX > 1, 1, TripAdvisorFeatures$addX)
  TripAdvisorFeatures$addit<- NULL
  TripAdvisorFeatures$addit <- NULL
  
  TripAdvisorFeatures$africaX <- TripAdvisorFeatures$africa + TripAdvisorFeatures$african
  TripAdvisorFeatures$africaX <- ifelse(TripAdvisorFeatures$africaX > 1, 1, TripAdvisorFeatures$africaX)
  TripAdvisorFeatures$africa<- NULL
  TripAdvisorFeatures$african <- NULL
  
  TripAdvisorFeatures$airX <- TripAdvisorFeatures$air + TripAdvisorFeatures$airi
  TripAdvisorFeatures$airX <- ifelse(TripAdvisorFeatures$airX > 1, 1, TripAdvisorFeatures$airX)
  TripAdvisorFeatures$air<- NULL
  TripAdvisorFeatures$airi <- NULL
  
  TripAdvisorFeatures$ampX <- TripAdvisorFeatures$amp + TripAdvisorFeatures$ampl
  TripAdvisorFeatures$ampX <- ifelse(TripAdvisorFeatures$ampX > 1, 1, TripAdvisorFeatures$ampX)
  TripAdvisorFeatures$amp<- NULL
  TripAdvisorFeatures$ampl <- NULL
  
  TripAdvisorFeatures$architectX <- TripAdvisorFeatures$architectur + TripAdvisorFeatures$architect +
    TripAdvisorFeatures$arch + TripAdvisorFeatures$arquitectur
  TripAdvisorFeatures$architectX <- ifelse(TripAdvisorFeatures$architectX > 1, 1, TripAdvisorFeatures$architectX)
  TripAdvisorFeatures$architect<- NULL
  TripAdvisorFeatures$architectur <- NULL
  TripAdvisorFeatures$arch <- NULL
  TripAdvisorFeatures$arquitectur <- NULL
  
  TripAdvisorFeatures$artistX <- TripAdvisorFeatures$art + TripAdvisorFeatures$artist + 
    TripAdvisorFeatures$artistri + TripAdvisorFeatures$artistri + TripAdvisorFeatures$artsi
  TripAdvisorFeatures$artistX <- ifelse(TripAdvisorFeatures$artistX > 1, 1, TripAdvisorFeatures$artistX)
  TripAdvisorFeatures$arti<- NULL
  TripAdvisorFeatures$art<- NULL
  TripAdvisorFeatures$artistri <- NULL
  TripAdvisorFeatures$artist <- NULL
  TripAdvisorFeatures$artsi <- NULL
  
  TripAdvisorFeatures$audioX <- TripAdvisorFeatures$audio + TripAdvisorFeatures$audioguid
  TripAdvisorFeatures$audioX <- ifelse(TripAdvisorFeatures$audioX > 1, 1, TripAdvisorFeatures$audioX)
  TripAdvisorFeatures$audio<- NULL
  TripAdvisorFeatures$audioguid <- NULL
  
  TripAdvisorFeatures$beautifulX <- TripAdvisorFeatures$beauti + TripAdvisorFeatures$beautiful
  TripAdvisorFeatures$beautifulX <- ifelse(TripAdvisorFeatures$beautifulX > 1, 1, TripAdvisorFeatures$beautifulX)
  TripAdvisorFeatures$beauti <- NULL
  TripAdvisorFeatures$beautiful <- NULL
  
  TripAdvisorFeatures$beginX <- TripAdvisorFeatures$begin + TripAdvisorFeatures$began
  TripAdvisorFeatures$beginX <- ifelse(TripAdvisorFeatures$beginX > 1, 1, TripAdvisorFeatures$beginX)
  TripAdvisorFeatures$begin<- NULL
  TripAdvisorFeatures$began <- NULL
  
  TripAdvisorFeatures$beliefX <- TripAdvisorFeatures$belief + TripAdvisorFeatures$believ
  TripAdvisorFeatures$beliefX <- ifelse(TripAdvisorFeatures$beliefX > 1, 1, TripAdvisorFeatures$beliefX)
  TripAdvisorFeatures$belief<- NULL
  TripAdvisorFeatures$believ <- NULL
  
  TripAdvisorFeatures$bigX <- TripAdvisorFeatures$big + TripAdvisorFeatures$bigger +
    TripAdvisorFeatures$biggest
  TripAdvisorFeatures$bigX <- ifelse(TripAdvisorFeatures$bigX > 1, 1, TripAdvisorFeatures$bigX)
  TripAdvisorFeatures$big<- NULL
  TripAdvisorFeatures$bigger<- NULL
  TripAdvisorFeatures$biggest <- NULL
  
  TripAdvisorFeatures$bilbaoX <- TripAdvisorFeatures$bilbao + TripAdvisorFeatures$bilbo +
    TripAdvisorFeatures$bilboa
  TripAdvisorFeatures$bilbaoX <- ifelse(TripAdvisorFeatures$bilbaoX > 1, 1, TripAdvisorFeatures$bilbaoX)
  TripAdvisorFeatures$bilbao<- NULL
  TripAdvisorFeatures$bilbo<- NULL
  TripAdvisorFeatures$bilboa <- NULL
  
  TripAdvisorFeatures$blowX <- TripAdvisorFeatures$blow + TripAdvisorFeatures$blown +
    TripAdvisorFeatures$blew
  TripAdvisorFeatures$blowX <- ifelse(TripAdvisorFeatures$blowX > 1, 1, TripAdvisorFeatures$blowX)
  TripAdvisorFeatures$blow<- NULL
  TripAdvisorFeatures$blown <- NULL
  TripAdvisorFeatures$blew <- NULL
  
  TripAdvisorFeatures$bourgeoiX <- TripAdvisorFeatures$bourgeoi + TripAdvisorFeatures$bourgeois +
    TripAdvisorFeatures$bourgoi
  TripAdvisorFeatures$bourgeoiX <- ifelse(TripAdvisorFeatures$bourgeoiX > 1, 1, TripAdvisorFeatures$bourgeoiX)
  TripAdvisorFeatures$bourgeoi<- NULL
  TripAdvisorFeatures$bourgeois <- NULL
  TripAdvisorFeatures$bourgoi <- NULL
  
  TripAdvisorFeatures$builtX <- TripAdvisorFeatures$build + TripAdvisorFeatures$built +
    TripAdvisorFeatures$buidl
  TripAdvisorFeatures$builtX <- ifelse(TripAdvisorFeatures$builtX > 1, 1, TripAdvisorFeatures$builtX)
  TripAdvisorFeatures$build<- NULL
  TripAdvisorFeatures$built <- NULL
  TripAdvisorFeatures$buidl <- NULL
  
  TripAdvisorFeatures$busiX <- TripAdvisorFeatures$busier + TripAdvisorFeatures$busi
  TripAdvisorFeatures$busiX <- ifelse(TripAdvisorFeatures$busiX > 1, 1, TripAdvisorFeatures$busiX)
  TripAdvisorFeatures$busi<- NULL
  TripAdvisorFeatures$busier <- NULL
  
  TripAdvisorFeatures$cafeX <- TripAdvisorFeatures$cafe + TripAdvisorFeatures$café +
    TripAdvisorFeatures$cafeteria + TripAdvisorFeatures$coffe 

  TripAdvisorFeatures$bookshopX <- TripAdvisorFeatures$bookshop + TripAdvisorFeatures$bookstor +
   TripAdvisorFeatures$book
  TripAdvisorFeatures$bookshopX <- ifelse(TripAdvisorFeatures$bookshopX > 1, 1, TripAdvisorFeatures$bookshopX)
  TripAdvisorFeatures$bookshop<- NULL
  TripAdvisorFeatures$bookstor <- NULL
  TripAdvisorFeatures$book<- NULL

  TripAdvisorFeatures$cafeX <- TripAdvisorFeatures$cafe + TripAdvisorFeatures$café +
    TripAdvisorFeatures$cafeteria
  TripAdvisorFeatures$cafeX <- ifelse(TripAdvisorFeatures$cafeX > 1, 1, TripAdvisorFeatures$cafeX)
  TripAdvisorFeatures$cafe<- NULL
  TripAdvisorFeatures$café<- NULL
  TripAdvisorFeatures$cafeteria <- NULL
  TripAdvisorFeatures$coffe <- NULL
  
  TripAdvisorFeatures$canvaX <- TripAdvisorFeatures$canva + TripAdvisorFeatures$canvas
  TripAdvisorFeatures$canvaX <- ifelse(TripAdvisorFeatures$canvaX > 1, 1, TripAdvisorFeatures$canvaX)
  TripAdvisorFeatures$canva<- NULL
  TripAdvisorFeatures$canvas <- NULL
  
  TripAdvisorFeatures$broadX <- TripAdvisorFeatures$broad + TripAdvisorFeatures$broader
  TripAdvisorFeatures$broadX <- ifelse(TripAdvisorFeatures$broadX > 1, 1, TripAdvisorFeatures$broadX)
  TripAdvisorFeatures$broad<- NULL
  TripAdvisorFeatures$broader <- NULL

  TripAdvisorFeatures$builtX <- TripAdvisorFeatures$build + TripAdvisorFeatures$built
  TripAdvisorFeatures$builtX <- ifelse(TripAdvisorFeatures$builtX > 1, 1, TripAdvisorFeatures$builtX)
  TripAdvisorFeatures$build<- NULL
  TripAdvisorFeatures$built <- NULL

  TripAdvisorFeatures$caravaggioX <- TripAdvisorFeatures$carravaggio + TripAdvisorFeatures$caravaggio +
    TripAdvisorFeatures$carravagio
  TripAdvisorFeatures$caravaggioX <- ifelse(TripAdvisorFeatures$caravaggioX > 1, 1, TripAdvisorFeatures$caravaggioX)
  TripAdvisorFeatures$carravaggio<- NULL
  TripAdvisorFeatures$carravagio <- NULL
  TripAdvisorFeatures$caravagio <- NULL

  TripAdvisorFeatures$cheapX <- TripAdvisorFeatures$cheap + TripAdvisorFeatures$cheaper
  TripAdvisorFeatures$cheapX <- ifelse(TripAdvisorFeatures$cheapX > 1, 1, TripAdvisorFeatures$cheapX)
  TripAdvisorFeatures$cheap<- NULL
  TripAdvisorFeatures$cheaper <- NULL
  
  TripAdvisorFeatures$childX <- TripAdvisorFeatures$children + TripAdvisorFeatures$child
  TripAdvisorFeatures$childX <- ifelse(TripAdvisorFeatures$childX > 1, 1, TripAdvisorFeatures$childX)
  TripAdvisorFeatures$child<- NULL
  TripAdvisorFeatures$children <- NULL
  
  TripAdvisorFeatures$choiceX <- TripAdvisorFeatures$choic + TripAdvisorFeatures$choos +
    TripAdvisorFeatures$chose
  TripAdvisorFeatures$choiceX <- ifelse(TripAdvisorFeatures$choiceX > 1, 1, TripAdvisorFeatures$choiceX)
  TripAdvisorFeatures$choos<- NULL
  TripAdvisorFeatures$chose <- NULL
  TripAdvisorFeatures$choic <- NULL
  
  TripAdvisorFeatures$closeX <- TripAdvisorFeatures$closer + TripAdvisorFeatures$close
  TripAdvisorFeatures$closeX <- ifelse(TripAdvisorFeatures$closeX > 1, 1, TripAdvisorFeatures$closeX)
  TripAdvisorFeatures$close<- NULL
  TripAdvisorFeatures$closer <- NULL

  TripAdvisorFeatures$cloudX <- TripAdvisorFeatures$cloud + TripAdvisorFeatures$cloudi
  TripAdvisorFeatures$cloudX <- ifelse(TripAdvisorFeatures$cloudX > 1, 1, TripAdvisorFeatures$cloudX)
  TripAdvisorFeatures$cloud<- NULL
  TripAdvisorFeatures$cloudi <- NULL
  
  TripAdvisorFeatures$colourX <- TripAdvisorFeatures$color + TripAdvisorFeatures$colour
  TripAdvisorFeatures$colourX <- ifelse(TripAdvisorFeatures$colourX > 1, 1, TripAdvisorFeatures$colourX)
  TripAdvisorFeatures$color<- NULL
  TripAdvisorFeatures$colour <- NULL
  
  TripAdvisorFeatures$commentX <- TripAdvisorFeatures$comment + TripAdvisorFeatures$commentari
  TripAdvisorFeatures$commentX <- ifelse(TripAdvisorFeatures$commentX > 1, 1, TripAdvisorFeatures$commentX)
  TripAdvisorFeatures$comment<- NULL
  TripAdvisorFeatures$commentari <- NULL
  
  TripAdvisorFeatures$composX <- TripAdvisorFeatures$composit + TripAdvisorFeatures$compos
  TripAdvisorFeatures$composX <- ifelse(TripAdvisorFeatures$composX > 1, 1, TripAdvisorFeatures$composX)
  TripAdvisorFeatures$compos<- NULL
  TripAdvisorFeatures$composit <- NULL
  
  TripAdvisorFeatures$countriX <- TripAdvisorFeatures$countri + TripAdvisorFeatures$countrysid
  TripAdvisorFeatures$countriX <- ifelse(TripAdvisorFeatures$countriX > 1, 1, TripAdvisorFeatures$countriX)
  TripAdvisorFeatures$countri<- NULL
  TripAdvisorFeatures$countrysid <- NULL
  
  TripAdvisorFeatures$creationX <- TripAdvisorFeatures$creation+ TripAdvisorFeatures$creativ +
    TripAdvisorFeatures$creat + TripAdvisorFeatures$creator
  TripAdvisorFeatures$creationX <- ifelse(TripAdvisorFeatures$creationX > 1, 1, TripAdvisorFeatures$creationX)
  TripAdvisorFeatures$creation<- NULL
  TripAdvisorFeatures$creativ <- NULL
  TripAdvisorFeatures$creat <- NULL
  TripAdvisorFeatures$creator <- NULL
  
  TripAdvisorFeatures$cubismX <- TripAdvisorFeatures$cubism + TripAdvisorFeatures$cubist
  TripAdvisorFeatures$cubismX <- ifelse(TripAdvisorFeatures$cubismX > 1, 1, TripAdvisorFeatures$cubismX)
  TripAdvisorFeatures$cubism<- NULL
  TripAdvisorFeatures$cubist <- NULL
  
  TripAdvisorFeatures$curiosX <- TripAdvisorFeatures$curious + TripAdvisorFeatures$curios
  TripAdvisorFeatures$curiosX <- ifelse(TripAdvisorFeatures$curiosX > 1, 1, TripAdvisorFeatures$curiosX)
  TripAdvisorFeatures$curios<- NULL
  TripAdvisorFeatures$curious <- NULL


  TripAdvisorFeatures$complainX <- TripAdvisorFeatures$complain + TripAdvisorFeatures$complaint
  TripAdvisorFeatures$complainX <- ifelse(TripAdvisorFeatures$complainX > 1, 1, TripAdvisorFeatures$complainX)
  TripAdvisorFeatures$complain<- NULL
  TripAdvisorFeatures$complaint <- NULL

  TripAdvisorFeatures$daliX <- TripAdvisorFeatures$dali + TripAdvisorFeatures$dalí
  TripAdvisorFeatures$daliX <- ifelse(TripAdvisorFeatures$daliX > 1, 1, TripAdvisorFeatures$daliX)
  TripAdvisorFeatures$dali<- NULL
  TripAdvisorFeatures$dalí <- NULL

  TripAdvisorFeatures$deepX <- TripAdvisorFeatures$deep + TripAdvisorFeatures$deepli
  TripAdvisorFeatures$deepX <- ifelse(TripAdvisorFeatures$deepX > 1, 1, TripAdvisorFeatures$deepX)
  TripAdvisorFeatures$dee<- NULL
  TripAdvisorFeatures$deepli <- NULL

  TripAdvisorFeatures$definitX <- TripAdvisorFeatures$defin + TripAdvisorFeatures$definit
  TripAdvisorFeatures$definitX <- ifelse(TripAdvisorFeatures$definitX > 1, 1, TripAdvisorFeatures$definitX)
  TripAdvisorFeatures$defin<- NULL
  TripAdvisorFeatures$definit <- NULL

  TripAdvisorFeatures$didntX <- TripAdvisorFeatures$didn + TripAdvisorFeatures$didnt
  TripAdvisorFeatures$didntX <- ifelse(TripAdvisorFeatures$didntX > 1, 1, TripAdvisorFeatures$didntX)
  TripAdvisorFeatures$didn<- NULL
  TripAdvisorFeatures$didnt <- NULL
  
  TripAdvisorFeatures$dinnerX <- TripAdvisorFeatures$dinner + TripAdvisorFeatures$dine
  TripAdvisorFeatures$dinnerX <- ifelse(TripAdvisorFeatures$dinnerX > 1, 1, TripAdvisorFeatures$dinnerX)
  TripAdvisorFeatures$dine  <- NULL
  TripAdvisorFeatures$dinner <- NULL
  
  TripAdvisorFeatures$diveX <- TripAdvisorFeatures$divers + TripAdvisorFeatures$dive
  TripAdvisorFeatures$diveX <- ifelse(TripAdvisorFeatures$diveX > 1, 1, TripAdvisorFeatures$diveX)
  TripAdvisorFeatures$divers<- NULL
  TripAdvisorFeatures$dive <- NULL
  
  TripAdvisorFeatures$doesn <- NULL
  TripAdvisorFeatures$don <- NULL
  TripAdvisorFeatures$dont <- NULL
  
  TripAdvisorFeatures$easiX <- TripAdvisorFeatures$easi + TripAdvisorFeatures$easili 
  TripAdvisorFeatures$easiX <- ifelse(TripAdvisorFeatures$easiX > 1, 1, TripAdvisorFeatures$easiX)
  TripAdvisorFeatures$easi<- NULL
  TripAdvisorFeatures$easili <- NULL

  TripAdvisorFeatures$euroX <- TripAdvisorFeatures$europ + TripAdvisorFeatures$euro
  TripAdvisorFeatures$euroX <- ifelse(TripAdvisorFeatures$euroX > 1, 1, TripAdvisorFeatures$euroX)
  TripAdvisorFeatures$euro<- NULL
  TripAdvisorFeatures$europ <- NULL
  
  TripAdvisorFeatures$explainX <- TripAdvisorFeatures$explain + TripAdvisorFeatures$explan
  TripAdvisorFeatures$explainX <- ifelse(TripAdvisorFeatures$explainX > 1, 1, TripAdvisorFeatures$explainX)
  TripAdvisorFeatures$explan<- NULL
  TripAdvisorFeatures$explain <- NULL
  
  TripAdvisorFeatures$feelX <- TripAdvisorFeatures$feel + TripAdvisorFeatures$felt
  TripAdvisorFeatures$feelX <- ifelse(TripAdvisorFeatures$feelX > 1, 1, TripAdvisorFeatures$feelX)
  TripAdvisorFeatures$feel<- NULL
  TripAdvisorFeatures$felt <- NULL
  
  TripAdvisorFeatures$findX <- TripAdvisorFeatures$found + TripAdvisorFeatures$found
  TripAdvisorFeatures$findX <- ifelse(TripAdvisorFeatures$findX > 1, 1, TripAdvisorFeatures$findX)
  TripAdvisorFeatures$found<- NULL
  TripAdvisorFeatures$find <- NULL
  
  TripAdvisorFeatures$hideX <- TripAdvisorFeatures$hide + TripAdvisorFeatures$hidden
  TripAdvisorFeatures$hideX <- ifelse(TripAdvisorFeatures$hideX > 1, 1, TripAdvisorFeatures$hideX)
  TripAdvisorFeatures$hide<- NULL
  TripAdvisorFeatures$hidden <- NULL
  
  TripAdvisorFeatures$leaveX <- TripAdvisorFeatures$left + TripAdvisorFeatures$leav
  TripAdvisorFeatures$leaveX <- ifelse(TripAdvisorFeatures$leaveX > 1, 1, TripAdvisorFeatures$leaveX)
  TripAdvisorFeatures$leav<- NULL
  TripAdvisorFeatures$left <- NULL
  
  TripAdvisorFeatures$museumX <- TripAdvisorFeatures$museum + TripAdvisorFeatures$museo
  TripAdvisorFeatures$museumX <- ifelse(TripAdvisorFeatures$museumX > 1, 1, TripAdvisorFeatures$museumX)
  TripAdvisorFeatures$museo<- NULL
  TripAdvisorFeatures$museum <- NULL  
  
  TripAdvisorFeatures$organX <- TripAdvisorFeatures$organ + TripAdvisorFeatures$organis
  TripAdvisorFeatures$organX <- ifelse(TripAdvisorFeatures$organX > 1, 1, TripAdvisorFeatures$organX)
  TripAdvisorFeatures$organ<- NULL
  TripAdvisorFeatures$organis <- NULL
  
  TripAdvisorFeatures$visitX <- TripAdvisorFeatures$visit + TripAdvisorFeatures$visitor
  TripAdvisorFeatures$visitX <- ifelse(TripAdvisorFeatures$visitX > 1, 1, TripAdvisorFeatures$visitX)
  TripAdvisorFeatures$visitor<- NULL
  TripAdvisorFeatures$visit <- NULL
  
  TripAdvisorFeatures$workX <- TripAdvisorFeatures$worker + TripAdvisorFeatures$work
  TripAdvisorFeatures$workX <- ifelse(TripAdvisorFeatures$workX > 1, 1, TripAdvisorFeatures$workX)
  TripAdvisorFeatures$work<- NULL
  TripAdvisorFeatures$worker <- NULL
  
  TripAdvisorFeatures$worthX <- TripAdvisorFeatures$worth + TripAdvisorFeatures$worthi
  TripAdvisorFeatures$worthX <- ifelse(TripAdvisorFeatures$worthX > 1, 1, TripAdvisorFeatures$worthX)
  TripAdvisorFeatures$worth<- NULL
  TripAdvisorFeatures$worthi <- NULL
  #AQUI
}

TripAdvisorFeatures <- TripAdvisorFeatures[ ,order(names(TripAdvisorFeatures))]

# Preparing the final set
TripAdvisorAndFeatures <- cbind(TripAdvisorPosNeg, TripAdvisorFeatures)
TripAdvisorAndFeatures$pos <- NULL
TripAdvisorAndFeatures$neg <- NULL

# Delete Neutral opinions
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentValue!="neutral",]
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentCore!="neutral",]

# Complete set
SaveCSV(TripAdvisorAndFeatures,dataset,"UnigramFeatures")

# DataFrame where the machine sentiment does not match the expert's opinion
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures[!(TripAdvisorAndFeatures$SentimentValue=="positive" & TripAdvisorAndFeatures$SentimentCore=="negative"),]
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures_NOTMATCHING[!(TripAdvisorAndFeatures_NOTMATCHING$SentimentValue=="negative" & TripAdvisorAndFeatures_NOTMATCHING$SentimentCore=="positive"),]

SaveCSV(TripAdvisorAndFeatures_NOTMATCHING,dataset,"UnigramFeatures_NOMATCHING")


beep(2)
