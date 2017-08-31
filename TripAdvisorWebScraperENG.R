###############################################################################
#
# TRIP ADVISOR OPINIONS 
#
# Inspired by:
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html
#
# Author: Ana Valdivia
# https://github.com/anavaldi/TFM/blob/master/scrapTripAdvisorLoop_anony_ENG_complete.R
#
# Modified Version, Author: Jesús Sánchez de Castro.
# Date: 16/03/2017
#
###############################################################################
#
# READ ME :)
# 
# This code is made by Ana Valdivia, but I (Jesús Sánchez) will make some 
# modifications and add comments so anyone can use it easily by just modifying 
# some variables.
# 
# 1.- Variables to keep in mind:
#
#     - totalpages: In tripAdvisor you can find pages of reviews, 
#       each one containing 10 reviews. In totalpages you have to put the 
#       maximun number of pages that you want to download.
# 
#     - last: say that you want to download from the page number x 
#       to the page number y, then set last to x and totalpages to y. Otherwise
#       just comment that line and use from 1 to totalpages.
# 
#     - html_directory: variable that contains the path to the
#       directory where the HTML file will be downloaded. Used to read it too.
#       
#     - csv_directory: variable that contains the path to the csv 
#       file with the data frame.
# 
#     - messages: Boolean used to know wheter the debug messages will be printed
#       or not.
#
#     - complete_review_url: variable that contains the modified URL used to 
#       get the complete reviews. 
#       
# 2.- Downloading the HMTL file:
#     
#     Depending on what page we want to get the data from, the URL given to 
#     url ("string") may be like this:
#         
#         - "https://www.tripadvisor.co.uk/Attraction_Review-g187514-d190152-
#           Reviews-Queen_Sofia_Arts_Center_Museo_Nacional_Centro_de_Arte_Reina_
#           Sofia-Madrid.html#REVIEW" (ended in #REVIEWS)
#           
#           if it is the main page shown in the web page or like this:
#               
#         - "https://www.tripadvisor.co.uk/Attraction_Review-g187514-d190152-
#           Reviews-or",k-1,"0-Queen_Sofia_Arts_Center_Museo_Nacional_Centro_
#           de_Arte_Reina_Sofia-Madrid.html" (ended in .html)
#           
#           if we are downloading the page number x where x > 1. In this case
#           you must split the URL in two parts, one from the begining to 
#           Reviews-or and the second one from 0-<name_of_the_attraction.html>
#               
#     In case you have any doubt see the examples given in the code for 5 museums.
#           
# 3.- Downloading the complete reviews:
#      
#     In this case we need a third type of URL, this one can be copied when 
#     opening a review in TripAdvisor. The URL is like this:
#          
#          - https://www.tripadvisor.co.uk/ShowUserReviews-g187514-d198867-
#            r467490717-Thyssen_Bornemisza_Museum_Museo_Thyssen_Bornemisza-
#            Madrid.html#REVIEWS
# 
#     where rXXXXXXXXX is the user's ID. So as we have already extracted these
#     IDs, we change the URL so it looks like this:
# 
#         - https://www.tripadvisor.co.uk/ShowUserReviews-g315921-d254665-r"
#           ,gsub("rn", "", id[i]),"-Dali_Theatre_Museum-Figueres_Province_of_
#           Girona_Catalonia.html#REVIEWS"
#         
#     ,gsub("rn", "", id[i]), is the part where we use the IDs stored by this
#     script, the rest is the normal URL. 
# 
###############################################################################

# Libraries used 
library(rvest)
library(beepr)
source("Utils.R")
# change language
Sys.setlocale("LC_TIME", "English")

# Variables: Here are the variables you need to change to download a data base 
# from a TripAdvisor web page in english.

TripAdvisor <- data.frame()# Data frame containing the result

#-------------------------------SELECT DATA------------------------------------#

# Nº1 : Prado Museum: 1230 pages
# Nº2 : Tyssen Museum: 380 pages
# Nº3 : Reina sofia : 340 pages
# Nº4 : Guggenheim Museum: 340 pages
# Nº5 : City of art and science: 210 pages


totalpages <- list(1230,380,340,340,210) # Number of pages, 10 reviews per page
dataset <- 5

if(dataset == 1){
  ds.name <- "prado"
} else if(dataset == 2){
  ds.name <- "tyssen"
} else if(dataset == 3){
  ds.name <- "reina"
} else if(dataset == 4){
  ds.name <- "guggen"
} else if(dataset == 5){
  ds.name <- "city"
} else {
  ds.name <- "city"
}

# Unused museums, only the top5 with more reviews
# Dalí Museum: 140
# Picasso: 180

# The folder of the project in your PC
pc.path <- "D:/TFG-Sentiment-Analysis-Spanish-Museums/Data/"

# The path of each HTML file
html.directory <- paste0(pc.path,ds.name,"/","webPage.html")

messages <- TRUE 
#last <- 1
#Also change complete_review_url, lines 280-290.

print(paste0("Dataset = ",dataset,", Name = ",ds.name,""))
print(paste0("Pages = ",totalpages[dataset]))

#---------------------------DOWNLOAD AND PROCESS HTML--------------------------#


#for(k in last:totalpages){ #Download from page "last" to "totalpages".
#All the reviews are downloaded, from 1 to max.
for(k in 1:totalpages[[dataset]]){     
  if(messages)
    print(paste0("Page :",k))
    
  # If the desired data.frame is Prado National Museum, Madrid.
  if(dataset == 1){
    if(k == 1) {
        url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187514-d190143-Reviews-Prado_National_Museum-Madrid.html#REVIEWS")
    } else {
        url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187514-d190143-Reviews-or",k-1,"0-Prado_National_Museum-Madrid.html")
    }
  # If the desired data.frame is Thyssen-Bornemisza Museum, Malaga
  } else if(dataset == 2){
    if(k == 1) {
      url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187514-d198867-Reviews-Thyssen_Bornemisza_Museum_Museo_Thyssen_Bornemisza-Madrid.html#REVIEWS")
    } else {
      url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187514-d198867-Reviews-or",k-1,"0-Thyssen_Bornemisza_Museum_Museo_Thyssen_Bornemisza-Madrid.html")
    }
  # If the desired data.frame is Thyssen-Bornemisza Museum, Malaga
  } else if(dataset == 3){
     if(k == 1) {
       url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187514-d190152-Reviews-Queen_Sofia_Arts_Center_Museo_Nacional_Centro_de_Arte_Reina_Sofia-Madrid.html#REVIEW")
     } else {
       url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187514-d190152-Reviews-or",k-1,"0-Queen_Sofia_Arts_Center_Museo_Nacional_Centro_de_Arte_Reina_Sofia-Madrid.html")
     }
  # If the desired data.frame is Thyssen-Bornemisza Museum, Malaga
  } else if(dataset == 4){
    if(k == 1) {
        url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187454-d190276-Reviews-Guggenheim_Museum_Bilbao-Bilbao_Province_of_Vizcaya_Basque_Country.html#REVIEWS")
    } else {
        url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187454-d190276-Reviews-or",k-1,"0-Guggenheim_Museum_Bilbao-Bilbao_Province_of_Vizcaya_Basque_Country.html")
    }
  # If the desired data.frame is Thyssen-Bornemisza Museum, Malaga
  } else if(dataset == 5){
    if(k == 1) {
        url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187529-d244246-Reviews-City_of_the_Arts_and_Sciences-Valencia_Province_of_Valencia_Valencian_Country.html#REVIEWS")
    } else {
        url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187529-d244246-Reviews-or",k-1,"0-City_of_the_Arts_and_Sciences-Valencia_Province_of_Valencia_Valencian_Country.html")
    }
  }

  # Museo Teatro Dalí
  # if(k == 1) {
  #   url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g315921-d254665-Reviews-Dali_Theatre_Museum-Figueres_Province_of_Girona_Catalonia.html#REVIEWS")
  # } else {
  #   url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g315921-d254665-Reviews-or",k-1,"0-Dali_Theatre_Museum-Figueres_Province_of_Girona_Catalonia.html")
  # }
  # 
  # Picasso Museum
  # if(k == 1) {
  #     url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187438-d288435-Reviews-Museo_Picasso_Malaga-Malaga_Costa_del_Sol_Province_of_Malaga_Andalucia.html#REVIEWS")
  # } else {
  #     url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187438-d288435-Reviews-or",k-1,"0-Museo_Picasso_Malaga-Malaga_Costa_del_Sol_Province_of_Malaga_Andalucia.html")
  # }
    
  #Download the html file
  download.file(url, destfile = html.directory, quiet = TRUE)
    if(messages)
      print("HTML Files downloaded.")
    
    #--------------------------------------------------------------------------#
    # This part of the code reads the HTML file and extracts the information 
    # that will be added to the data frame.
    # Information: 
    #   - id: id of a user in tripAdvisor, e.g: rn467434236
    #   - username: user's username, e.g: Love2Travel
    #   - location: where the user is from or lives, e.g: Texas
    #   - userop: number of reviews published by the user.
    #   - quote: shot phrase about the attraction, e.g: "Dalí is amazing"
    #   - rating: rating given by the user: 1 to 5.
    #   - date: date when the review was published
    #   - reviewnospace: The complete review.
    #--------------------------------------------------------------------------#
  
  # User's information:
  # Name
  users <- url %>%
    read_html(html.directory) %>%
      html_nodes(".member_info")
  # .memberOverlayLink")
    
#   rev <- c()
#   for(i in 1:length(users)){
#     user.ch <- as.character(users[i])
#     if(substr(user.ch, 27, 33) == "<div id"){
#       rev <- append(rev, i, after=length(rev))
#     }
#     else if(substr(user.ch, 27, 44) == '<div class="avatar' & k > 1){
#       rev <- append(rev, i, after=length(rev))
#     }
#   }
#     
#   users <- users[rev] # Only user's Reviews
#   
#   username <- users %>%
#     html_node(".username") %>%
#     html_text()
#   username <- gsub("\n", "", username)
#   
#   # Location
#   users <- url %>%
#     read_html(html.directory) %>%
#     html_nodes(".member_info")
#   
#   rev <- c()
#   for(i in 1:length(users)){
#     user.ch <- as.character(users[i])
#     if(substr(user.ch, 27, 33) == "<div id"){
#       rev <- append(rev, i, after=length(rev))
#     }
#     else if(substr(user.ch, 27, 44) == '<div class="avatar'){
#       rev <- append(rev, i, after=length(rev))
#     }
#   }
#   users <- users[rev]
#   
#   location <- users %>%
#     html_nodes(".location") %>%
#     html_text()
#   location <- gsub("\n", "", location)
#   
#   # Opinions
#   users <- url %>%
#     read_html() %>%
#     html_nodes(".memberBadging .reviewerBadge")
#   
#   userop <- users %>%
#     html_node(".badgeText") %>%
#     html_text()
#   
#   # For anonymous users
#   for(i in 1:length(username)){
#     if(username[i] == "A TripAdvisor Member "){
#       username[i] <- "A TripAdvisor Member"
#       location <- append(location, NA, after=i-1)
#       userop <- append(userop, NA, after=i-1)
#     }
#   }
#   
#   
#   # About reviews 
#   reviews <- url %>%
#     read_html(html.directory) %>%
#     html_nodes("#REVIEWS .innerBubble")
#   
#   # ID reviewrs
#   id <- reviews %>%
#     html_node(".quote a") %>%
#     html_attr("id")
#   
#   # 
#   quote <- reviews %>%
#     html_node(".quote span") %>%
#     html_text()
#   
#   rating <- reviews %>%
#     html_node(".rating .rating_s_fill") %>%
#     html_attr("alt") %>%
#     gsub(" of 5 bubbles", "", .) %>%
#     as.integer()
#   
#   # Date of the opinion
#   date <- reviews %>%
#     html_node(".rating .ratingDate") %>%
#     html_attr("title") %>%
#     as.Date("%d %b %Y")
#   
#   # Another format
#   for(i in 1:length(date)){
#     if(is.na(date[i])){
#       date.aux <- reviews %>%
#         html_node(".ratingDate")
#       date[i] <- as.Date(substr(regmatches(as.character(date.aux[i]), 
#                                            regexpr('Reviewed .+\n', as.character(date.aux[i]))), 10, nchar(regmatches(as.character(date.aux[i]), regexpr('Reviewed .+\n', as.character(date.aux[i]))))-1), format="%d %b %Y")
#     }
#   }
#     
#   #   PARTIAL REVIEWS:
#   #     review <- reviews %>%
#   #       html_node(".entry .partial_entry") %>%
#   #       html_text()
#   #     
#   #     reviewnospace <- gsub("\n", "", review)
#     
#   # COMPLETE REVIEWS:
#   reviewnospace <- as.character(c(1:length(id)))
#   for(i in 1:length(id)){
#       
#     # completeReviewURL is used here
#     
#     # completeReviewURL <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187514-d190152-r",gsub("rn", "", id[i]),"-Queen_Sofia_Arts_Center_Museo_Nacional_Centro_de_Arte_Reina_Sofia-Madrid.html#REVIEWS")
#     # completeReviewURL <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187514-d190143-r",gsub("rn", "", id[i]),"-Prado_National_Museum-Madrid.html#REVIEWS")
#     # completeReviewURL <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187514-d198867-r",gsub("rn", "", id[i]),"-Thyssen_Bornemisza_Museum_Museo_Thyssen_Bornemisza-Madrid.html#REVIEWS")
#     # completeReviewURL <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g315921-d254665-r",gsub("rn", "", id[i]),"-Dali_Theatre_Museum-Figueres_Province_of_Girona_Catalonia.html#REVIEWS")
#     # completeReviewURL <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187454-d190276-r",gsub("rn", "", id[i]),"-Guggenheim_Museum_Bilbao-Bilbao_Province_of_Vizcaya_Basque_Country.html#REVIEWS")
#     completeReviewURL <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187529-d244246-r",gsub("rn", "", id[i]),"-City_of_the_Arts_and_Sciences-Valencia_Province_of_Valencia_Valencian_Country.html#REVIEWS")
#     # completeReviewURL <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187438-d288435-r",gsub("rn", "", id[i]),"-Museo_Picasso_Malaga-Malaga_Costa_del_Sol_Province_of_Malaga_Andalucia.html#REVIEWS")
#     reviews_aux <- completeReviewURL %>%
#       read_html(html.directory) %>%
#       html_nodes(".entry")
#         
#     reviewnospace[i] <- gsub(paste0('.*"review_',gsub("rn", "", id[i]),'\">\n|\n.*'), "",
#                              as.character(reviews_aux[1]))
#   }
#   if(messages)
#     print("Extracting information finished")
#     
#   #Page in TripAdvisor
#   page <- rep(k, length(username))
#   TripAdvisor <- data.frame(id, username, location, userop, quote, rating, date, reviewnospace, page, stringsAsFactors = FALSE)
#   
# }
# 
# #------------------------------SAVE RESULTS------------------------------------#
#   
# # # Delete duplicated reviews
# # TripAdvisor <- TripAdvisor[!(duplicated(TripAdvisor$reviewnospace)),] 
# # # Remove <br/> symbol
# # TripAdvisor$reviewnospace <- gsub("<br/>", "", TripAdvisor$reviewnospace)
# # # Merge title and opinion
# # TripAdvisor$titleopinion <- paste(TripAdvisor$quote, TripAdvisor$reviewnospace, 
# #                                   sep=". ")
# # TripAdvisor2 <- TripAdvisor[TripAdvisor$date <= as.Date("2017-08-10"),]
# # 
# # SaveCSV(TripAdvisor2,dataset,file.name = "ENG")
# # 
# # if(messages){
# #     beep(2)
# #     print("Saved to a csv file.")
# # }
