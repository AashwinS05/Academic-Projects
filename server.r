#-------------------------------------------Installing and Loading required packages
#----
# install.packages("rvest")
# install.packages("quanteda")
# install.packages("tm")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("tidytext")
# install.packages("readr")
# install.packages("tm")
# install.packages("magrittr")
# install.packages("data.table")
# install.packages("xgboost")
# install.packages("caret")
# install.packages("e1071")
# install.packages("sqldf")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages('lattice')
# install.packages('tidyverse')
# install.packages('textdata')


# library(rvest)
# library(quanteda)
# library(tm)
# library(dplyr)
# library(stringr)
# library(tidytext)
# library(readr)
# library(magrittr)
# library(ggplot2)
# library(data.table)
# library(xgboost)
# library(caret)
# library(e1071)
# library(sqldf)
# library(shiny)
# library(shinythemes)
# library(tidyverse)
# library(textdata)

#----

server <- function(input,output)
  {
  
#   #-------------------------- Getting the data - Toyota reviews 2017 - 2012
#   toyota_reviews_2017_pg1 <- read_html("https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?pg=1&nr=250")
#   toyota_reviews_2017_pg2 <- read_html("https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?pg=2&nr=250")
#   toyota_reviews_2016 <- read_html("https://www.cars.com/research/toyota-camry-2016/consumer-reviews/?nr=250&pg=1")
#   toyota_reviews_2015 <- read_html("https://www.cars.com/research/toyota-camry-2015/consumer-reviews/?nr=250&pg=1")
#   toyota_reviews_2014 <- read_html("https://www.cars.com/research/toyota-camry-2014/consumer-reviews/?nr=250&pg=1")
#   toyota_reviews_2013 <- read_html("https://www.cars.com/research/toyota-camry-2013/consumer-reviews/?nr=250&pg=1")
#   toyota_reviews_2012 <- read_html("https://www.cars.com/research/toyota-camry-2012/consumer-reviews/?nr=250&pg=1")
#   
#   #---------------------------------------- Function to find ratings from specific HTML
#   Rating_HTML_FN <- function(review_HTML){
#     ratings <- review_HTML %>%
#       html_nodes("header noscript") %>%
#       html_text()
#     no_ratings <- (length(ratings)/2)
#     a <- rep(1:2,no_ratings)
#     a_logical <- ifelse(a == 1,TRUE,FALSE)
#     ratings <- ratings[a_logical]
#     ratings <- gsub("\n", "",ratings)
#     rating_FN <- function(char){
#       split <- unlist(strsplit(char," "))
#       rating_No <- as.numeric(split[53])
#       print(rating_No)
#     } 
#     ratings <- sapply(ratings,rating_FN, USE.NAMES = FALSE)
#     year <- review_HTML%>%
#       html_nodes("span.mmy-reviews__date") %>%
#       html_text()
#     year_fn <- function(char){
#       year <- unlist(strsplit(char," "))
#       year <- as.numeric(year[5])
#       print(year)  
#     }
#     year <- sapply(year,year_fn, USE.NAMES = FALSE)
#     ratings <- as.data.frame(matrix(c(year,ratings), nrow = length(year)))
#     names(ratings) <- c("Year of Review", "Star Rating")
#     print(ratings)
#   }
#   
#   #------------------------------------ Function to find review text from specific HTML
#   Reviews_HTML_FN <- function(review_HTML){
#     reviews <- review_HTML %>%
#       html_nodes("header div.mmy-reviews__blurb") %>%
#       html_text()
#     no_reviews <- (length(reviews)) 
#     reviews <- gsub("\n", "",reviews)
#     reviews <- trimws(reviews)
#     year <- review_HTML%>%
#       html_nodes("span.mmy-reviews__date") %>%
#       html_text()
#     year_fn <- function(char){
#       year <- unlist(strsplit(char," "))
#       year <- as.numeric(year[5])
#       print(year)  
#     }
#     year <- sapply(year,year_fn, USE.NAMES = FALSE)
#     reviews <- as.data.frame(matrix(c(year,reviews), nrow = length(year)))
#     names(reviews) <- c("Year of Review", "Text of Review")
#     reviews[] <- lapply(reviews, as.character)
#     reviews$`Year of Review` <- as.numeric(reviews$`Year of Review`)
#     print(reviews)
#   }
#   
#   #--------------------------------------------------------- Ratings for specific years 
#   rating_17.1 <- Rating_HTML_FN(toyota_reviews_2017_pg1)
#   rating_17.2 <- Rating_HTML_FN(toyota_reviews_2017_pg2)
#   rating_16 <- Rating_HTML_FN(toyota_reviews_2016)
#   rating_15 <- Rating_HTML_FN(toyota_reviews_2015)
#   rating_14 <- Rating_HTML_FN(toyota_reviews_2014)
#   rating_13 <- Rating_HTML_FN(toyota_reviews_2013)
#   rating_12 <- Rating_HTML_FN(toyota_reviews_2012)
#   
#   #----------------------------------------------------------------- Final Rating Table
#   rating_final_table <- rbind(rating_12,rating_13,rating_14,rating_15,
#                               rating_16,rating_17.1,rating_17.2)
#   
#   # removing all previous tables for convenience 
#   rm("rating_12","rating_13","rating_14","rating_15","rating_16",
#      "rating_17.1","rating_17.2")
#   
#   #--------------------------------------------------------- reviews for specific years 
#   reviews_17.1 <- Reviews_HTML_FN(toyota_reviews_2017_pg1)
#   reviews_17.2 <- Reviews_HTML_FN(toyota_reviews_2017_pg2)
#   reviews_16 <- Reviews_HTML_FN(toyota_reviews_2016)
#   reviews_15 <- Reviews_HTML_FN(toyota_reviews_2015)
#   reviews_14 <- Reviews_HTML_FN(toyota_reviews_2014)
#   reviews_13 <- Reviews_HTML_FN(toyota_reviews_2013)
#   reviews_12 <- Reviews_HTML_FN(toyota_reviews_2012)
#   
#   #------------------------------------------------------------ Final Review text Table
#   review_final_table <- rbind(reviews_12,reviews_13,reviews_14,reviews_15,
#                               reviews_16,reviews_17.1,reviews_17.2)
#   
#   # removing all previous tables for convenience 
#   rm("reviews_12","reviews_13","reviews_14","reviews_15","reviews_16",
#      "reviews_17.1","reviews_17.2")
  
colClasses = c("factor", "character", "integer")
col.names = c('Year of Review','Text of Review','Star Rating')
Review_Data <- read.table(text = "",
                          colClasses = colClasses,
                          col.names = col.names,
                          check.names = FALSE)

Review_Data_Years <- c('2012', '2013', '2014', '2015', '2016','2017')

for( i in Review_Data_Years)
{
  Read_Link <- read_html(paste0("https://www.cars.com/research/toyota-camry-",i,"/consumer-reviews/?nr=500&pg=1.html"))
  ratings_Char <- xml_attr(html_nodes(Read_Link,xpath = "//article/cars-star-rating"),"rating")
  reviews_Char <- trimws(gsub("\n","",(html_text(html_nodes(Read_Link,xpath = "//article/p[@class='review-card-text']")))))
  Train.Data_i <- list(ratings_Char, reviews_Char) %>%
    as.data.frame(stringAsFactors = FALSE)
  Train.Data_i <- mutate(Train.Data_i, 'Year of Review' = i) # Adding new Column and assigning the value as i
  colnames(Train.Data_i) <- c('Star Rating','Text of Review','Year of Review')
  Train.Data_i <- Train.Data_i[c('Year of Review','Text of Review','Star Rating')]
  Review_Data <- rbind(Review_Data,Train.Data_i)
}

# Removing all the un-needed objects
remove(Read_Link, Train.Data_i, col.names, colClasses,i, ratings_Char, Review_Data_Years, reviews_Char) 

# Assigning appropriate the Data Types to cols. in Reviews_Data
Review_Data$`Star Rating` <- as.integer(Review_Data$`Star Rating`)
Review_Data$`Year of Review` <- as.factor(Review_Data$`Year of Review`)

#-------------------------------------------Splitting the Reviews Data Frame into Test = 2017 and train = 2012:2016
#----
train.data <- Review_Data[1:1206,]
test.data <- Review_Data[1207:1644,]
#----




  #----------------------------------------------------------------------- Objective 1
  # train.data <- cbind(review_final_table[1:840,],rating_final_table[1:840,2])
  # names(train.data) <- c("Year of review", "Text of Review", "Star Rating")
  
  output$view_1 <- renderTable({ 
    table <- train.data
    names(table) <- c("Year of Review", "Text of Review", "Star Rating")
    table[1:input$numeric_1,]
  })
  
  #----------------------------------------------------------------------- Objective 2
  # test.data <- cbind(review_final_table[-(1:840),],rating_final_table[-(1:840),2])
  # names(test.data) <- c("Year of review", "Text of Review", "Star Rating")
  
  
  output$view_2 <- renderTable({ 
    table <- test.data
    names(table) <- c("Year of Review", "Text of Review", "Star Rating")
    table[1:input$numeric_1,]
  })
  #----------------------------------------------------------------------- Objective 3
  # train Data
  train_wo_punct_lower <- removePunctuation(train.data[,2], 
                                            preserve_intra_word_contractions = TRUE,
                                            preserve_intra_word_dashes = FALSE) %>%
    tolower()
  
  F_train <- cbind(train.data, train_wo_punct_lower)
  names(F_train) <- c("Year of review", "Text of Review", "Star Rating", "Normalized Text Review")
  F_train$`Normalized Text Review` <- as.character(F_train$`Normalized Text Review`)
  # test Data
  test_wo_punct_lower <- removePunctuation(test.data[,2], 
                                           preserve_intra_word_contractions = TRUE,
                                           preserve_intra_word_dashes = FALSE) %>%
    tolower()
  
  
  F_test <- cbind(test.data, test_wo_punct_lower)
  names(F_test) <- c("Year of review", "Text of Review", "Star Rating", "Normalized Text Review")
  F_test$`Normalized Text Review` <- as.character(F_test$`Normalized Text Review`)
  
  output$view_3 <- renderTable({
    setcolorder(F_train, c("Year of review", "Text of Review","Normalized Text Review","Star Rating"))
    names(F_train) <- c("Year of Review", "Text of Review","Normalized Text Review","Star Rating")
    F_train[1:input$numeric_2,]
  })
  output$view_4 <- renderTable({
    setcolorder(F_test, c("Year of review", "Text of Review","Normalized Text Review","Star Rating"))
    names(F_test) <- c("Year of Review", "Text of Review","Normalized Text Review","Star Rating")
    F_test[1:input$numeric_2,]
  })
  
  #------------------------------------------------------------------------ objective 4
  tagging <- function(char)
    {
    a <- unlist(strsplit(char,' '))
    interior <- (grep("^interior$", a, value = TRUE))
    service <- (grep("^service$", a, value = TRUE))
    price <- (grep("^price$", a, value = TRUE))
    handling <- (grep("^handling$", a, value = TRUE))
    tags <- c(interior[1], service[1], price[1], handling[1])
    tags
    }
  
  #-------------------------------------------Train Data
  
  col5_train <- as.data.frame(t(sapply(F_train$`Normalized Text Review`,
                                       tagging, USE.NAMES = FALSE)),
                              stringsAsFactors = FALSE)
  
  col5_train$v5 <- paste(col5_train$V1, col5_train$V2, col5_train$V3, col5_train$V4)
  
  tags_train <- trimws(gsub("NA", "",col5_train$v5))
  
  F_train_tagged <- cbind(F_train,tags_train)
  
  F_train_tagged$tags_train <- as.character(F_train_tagged$tags_train)
  
  #-------------------------------------------Test data 
  
  col5_test <- as.data.frame(t(sapply(F_test$`Normalized Text Review`,
                                      tagging, USE.NAMES = FALSE)), 
                             stringsAsFactors = FALSE)
  
  col5_test$v5 <- paste(col5_test$V1, col5_test$V2, col5_test$V3, col5_test$V4)
  
  tags_test <- trimws(gsub("NA", "",col5_test$v5))
  
  F_test_tagged <- cbind(F_test,tags_test)
  
  F_test_tagged$tags_test <- as.character(F_test_tagged$tags_test)
  
  
  output$view_5 <- renderTable({
    setcolorder(F_train_tagged, c("Year of review","Text of Review", "Normalized Text Review","tags_train","Star Rating"))
    names(F_train_tagged) <- c("Year of Review", "Text of Review","Normalized Text Review","Tags","Star Rating")
    F_train_tagged[1:input$numeric_2,]
  })
  output$view_6 <- renderTable({
    setcolorder(F_test_tagged, c("Year of review","Text of Review", "Normalized Text Review","tags_test","Star Rating"))
    names(F_test_tagged) <- c("Year of Review", "Text of Review","Normalized Text Review","Tags","Star Rating")
    F_test_tagged[1:input$numeric_2,]
  })
  
  #------------------------------------------------------------------------ objective 5
  
  # creating a tbl_df with AFINN scores
  # AFINN <- sentiments %>%filter(lexicon == "AFINN") %>%
  #   select(word, afinn_score = score)
  
  get_sentiments(lexicon = 'afinn')
  
  AFINN <- get_sentiments(lexicon = 'afinn') %>% select(word, afinn_score = value)
    
  
  #------------------------------------------------------- Train data
  # trying to convert the data frame into 
  # tidy text and filtering for stop words 
  
  tidy_text_tr <- F_train_tagged %>%
    group_by(as.factor(F_train$`Year of review`)) %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word,`Normalized Text Review`) %>%
    filter(!word %in% stop_words$word,str_detect(word, "^[a-z']+$")) %>%
    select(`Year of review`,linenumber,`Text of Review`,
           `Star Rating`, word, tags_train)
  
  
  
  #------------------------------------------------------- Test data
  
  # sentiment analysis
  
  tidy_text_tst <- F_test_tagged %>%
    group_by(as.factor(F_test$`Year of review`)) %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word,`Normalized Text Review`) %>%
    filter(!word %in% stop_words$word,str_detect(word, "^[a-z']+$")) %>%
    select(`Year of review`,linenumber,`Text of Review`,
           `Star Rating`, word , tags_test)
  
  
  # performing inner join to find sentiment 
  
  sentiment_tst <- tidy_text_tst %>%
    inner_join(AFINN, by = 'word') %>%
    group_by(`Year of review`, linenumber,`Text of Review`,
             `Star Rating`, tags_test) %>% 
    summarize(sentiment = sum(afinn_score))
  
  
  
  #------------------------------------- option 3
  # this actually is the best option 
  # for objective 5 
  
  sentiment_tr_3 <- tidy_text_tr %>%
    inner_join(AFINN, by = 'word') %>%
    group_by(`Year of review`, linenumber,`Text of Review`,
             `Star Rating`, tags_train) %>%
    summarize(sentiment = sum(afinn_score))
  
  names(sentiment_tr_3) <- c("Year_of_review", "linenumber",
                             "Text_of_Review", "Star_Rating", "tags_train",
                             "sentiment")
  
  output$view_7 <- renderTable({
    setcolorder(sentiment_tr_3, c("linenumber","Year_of_review","Text_of_Review","tags_train","sentiment","Star_Rating"))
    names(sentiment_tr_3) <- c("linenumber", "Year of Review", "Text of Review","tags","Sentiment(Sum of AFINN)","Star Rating")
    sentiment_tr_3[1:input$numeric_3,c(2,3,4,5,6)]
  })
  
 #------------------------------------- Vizualization
 # did the sentiment analysis work 
 #does it show any corealtion to the star rating
  
 # we should expect the sentiment score to correlate 
 # with the star rating? does it?
  output$plot_1 <- renderPlot({
    theme_set(theme_dark())
    plot <- ggplot(sentiment_tr_3, aes(`Star_Rating`,
                                       sentiment, group = `Star_Rating`, 
                                       colour = factor(`Year_of_review`))) +
      geom_boxplot() +
      ylab("Average sentiment score")
    plot
  })
  
  #------------------------------------------------------------------------ objective 6
  
  #-------------------------------------- a.
  
  avg_sentiment_tr <- mean(sentiment_tr_3$sentiment)
  avg_star_tr <- mean(sentiment_tr_3$`Star_Rating`)
  avg_tr <- data.frame(cbind(avg_star_tr, avg_sentiment_tr))
  
  output$view_8 <- renderTable({
    setcolorder(avg_tr, c("avg_sentiment_tr","avg_star_tr")) 
    names(avg_tr) <- c("Avg_Sentiment_Rating","Avg_Star_Rating")
    avg_tr
  })
   
  #-------------------------------------- b.
  

  avg_interior_sentiment_tr <- cbind(sqldf("SELECT AVG(Star_Rating),AVG(sentiment),tags_train
                                           FROM sentiment_tr_3
                                           WHERE tags_train LIKE '%interior%'"),avg_star_tr)
  
  avg_service_sentiment_tr <- cbind(sqldf("SELECT AVG(Star_Rating),AVG(sentiment),tags_train
                                          FROM sentiment_tr_3
                                          WHERE tags_train LIKE '%service%'"),avg_star_tr)
  
  avg_handling_sentiment_tr <- cbind(sqldf("SELECT AVG(Star_Rating),AVG(sentiment),tags_train
                                           FROM sentiment_tr_3
                                           WHERE tags_train LIKE '%handling%'"),avg_star_tr)
  
  avg_price_sentiment_tr <- cbind(sqldf("SELECT AVG(Star_Rating),AVG(sentiment),tags_train
                                        FROM sentiment_tr_3
                                        WHERE tags_train LIKE '%price%'"),avg_star_tr)
  
  obj_6b <- rbind(avg_interior_sentiment_tr,
                  avg_service_sentiment_tr,
                  avg_handling_sentiment_tr,
                  avg_price_sentiment_tr)
  
  output$view_9 <- renderTable({
    setcolorder(obj_6b, c("tags_train","AVG(sentiment)", "AVG(Star_Rating)","avg_star_tr"))
    names(obj_6b) <- c("Tag","Avg Sentiment/Tag", "Avg Star Rating/Tag","Avg Star Rating/Train data")
    obj_6b
  })
  
  #------------------------------------------------------------------------ objective 7 & 8
  
  #------------------------------------------------------------------------ objective 7
  
  #------------------------------------- Data exploration
  # before we start running ML 
  # models to predict the star ratings,
  # we should do some data exploration
  
  #------------------------------------- Train data
  
  # cleaning the dataset for easier use
  sentiment_data <- sentiment_tr_3
  sentiment_data <- sentiment_data[,-2]
  names(sentiment_data) <- c("Year", "text","Star_Rating","tags_train","Avg_AFINN_Sentiment")
  
  # lets explore the data - look at distribution of class labels (star Ratings)
  
  class_dist_percent <- as.data.frame(prop.table(table(sentiment_data$Star_Rating)))
  
  # g <- ggplot(class_dist_percent, aes(x = 'var1'))
  # g + geom_bar()
  # this isnt working - need to work on this.
  
  # as can be seen some review are longer than the others
  # maybe there is correaltion bw length of text and star rating
  
  sentiment_data$Review_Length <- nchar(sentiment_data$text)
  
  summary(sentiment_data$Review_Length)
  
  # dividing the ratings into labels 1-2 (low) / 3(mid) / 4-5 (high)
  # doing this to further find insights 
  sentiment_data$category <- ifelse(sentiment_data$Star_Rating < 3,
                                    "Low",
                                    ifelse(sentiment_data$Star_Rating == 3,
                                           "mid",
                                           "high"))
  summary(sentiment_data$category)
  
  # as can be seen from the summary of text lengths  - 
  # there is lot of variation in the text length
  # and thus this could help us in future in predicting the star rating.
  
  # vizualize the text lengths to see any interesting patters 
  
  ggplot(sentiment_data, aes(x = Review_Length, fill = as.factor(Star_Rating))) +
    theme_dark() +
    geom_histogram(binwidth = 80) +
    labs(y = "Text Count", x = "Length of Review",
         title = "Distribution of Text Lengths with Category Labels")
  
  
  # random data cleaning
  setcolorder(sentiment_data, c("Year","text","Review_Length",
                                "Avg_AFINN_Sentiment","category",
                                "tags_train","Star_Rating"))
  
  #--------------------------------------- Test Data
  
  # cleaning the data set for further use
  
  # cleaning the dataset for easier use
  sentiment_tst <- sentiment_tst[,-2]
  names(sentiment_tst) <- c("Year", "text","Star_Rating","tags_test","Avg_AFINN_Sentiment")
  
  # Making new variable for text length
  
  sentiment_tst$Review_Length <- nchar(sentiment_tst$text)
  
  summary(sentiment_tst$Review_Length)
  
  # dividing the ratings into labels 1-2 (low) / 3(mid) / 4-5 (high)
  # doing this to further find insights 
  sentiment_tst$category <- ifelse(sentiment_tst$Star_Rating < 3,
                                   "Low",
                                   ifelse(sentiment_tst$Star_Rating == 3,
                                          "mid",
                                          "high"))
  summary(sentiment_data$category)
  
  # random data cleaning
  
  setcolorder(sentiment_tst, c("Year","text","tags_test","Review_Length",
                               "Avg_AFINN_Sentiment","category",
                               "Star_Rating"))
  
  
  set.seed(1234)
  # The XGBoost algorithm requires that the class labels (Site names) start at 0 and 
  # increase sequentially to the maximum number of classes. This is a bit of an 
  # inconvenience as you need to keep track of what Site name goes with which label.
  # Also, you need to be very careful when you add or remove a 1 to go from the zero 
  # based labels to the 1 based labels.
  
  dat_train <- sentiment_data[,c(1,3,4,7)] 
  dat_train$Star_Rating <- as.numeric(dat_train$Star_Rating)
  
  dat_train <- dat_train %>%
    mutate(Star_Rating = Star_Rating - 1)
  
  #The XGBoost algorithm requires the data 
  # to be passed as a matrix. 
  # Here I use xgb.DMatrix() function to make a 
  # dataset of class xgb.DMatrix which is native to XGBoost.
  
  data_label_train <- (dat_train$Star_Rating)
  data_matrix_train <- xgb.DMatrix(data = as.matrix(dat_train[,1:3]), label = data_label_train)
  
  #-------------------K-folds Cross-validation to Estimate Error
  
  numberOfClasses_train <- length(unique(dat_train$Star_Rating))
  xgb_params_train <- list("objective" = "multi:softprob",
                           "eval_metric" = "mlogloss",
                           "num_class" = numberOfClasses_train)
  nround_train    <- 50 # number of XGBoost rounds
  cv.nfold_train  <- 5
  
  #-Fit cv.nfold * cv.nround XGB models and save OOF predictions
  
  cv_model_train <- xgb.cv(params = xgb_params_train,
                           data = data_matrix_train, 
                           nrounds = nround_train,
                           nfold = cv.nfold_train,
                           verbose = FALSE,
                           prediction = TRUE)
  
  #-------------------------Assess Out-of-Fold Prediction Error
  
  OOF_prediction_train <- data.frame(cv_model_train$pred) %>%
    mutate(max_prob = max.col(., ties.method = "last"),
           Star_Rating = data_label_train + 1)
  
  head(OOF_prediction_train)
  
  #--------------------------------------------confusion matrix
  confusion_matrix_train <- confusionMatrix(factor(OOF_prediction_train$max_prob),
                                            factor(OOF_prediction_train$Star_Rating),
                                            mode = "everything")
  
  View(confusion_matrix_train$overall)
  
  
  #------------------------------------------------------------------------ Objective 8 
  
  #------------------------------------------------ test data preparation
  dat_tst <- sentiment_tst[,c(1,4,5,7)] 
  dat_tst$Star_Rating <- as.numeric(dat_tst$Star_Rating)
  
  dat_tst <- dat_tst %>%
    mutate(Star_Rating = Star_Rating - 1)
  
  #The XGBoost algorithm requires the data to be passed as a matrix. 
  # Here I use xgb.DMatrix() function to make a dataset of class xgb.DMatrix which 
  # is native to XGBoost.
  
  data_label_tst <- (dat_tst$Star_Rating)
  data_matrix_tst <- xgb.DMatrix(data = as.matrix(dat_tst[,1:3]), label = data_label_tst)
  
  #-----------------------------Train Full Model and Assess Test Set Error
  
  bst_model <- xgb.train(params = xgb_params_train,
                         data = data_matrix_train,
                         nrounds = nround_train)
  
  #---------------------------------------------Predict hold-out test set
  test_pred <- predict(bst_model, newdata = data_matrix_tst)
  test_prediction <- matrix(test_pred, nrow = numberOfClasses_train,
                            ncol=length(test_pred)/numberOfClasses_train) %>%
    t() %>%
    data.frame() %>%
    mutate(Star_Rating = data_label_tst + 1, max_prob = max.col(., "last"))
  #-----------------------------------------confusion matrix of test set
  confusion_matrix_tst <- confusionMatrix(factor(test_prediction$max_prob),factor(test_prediction$Star_Rating),mode = "everything")
  
  a <- data.frame((confusion_matrix_tst$table))
  
  f <- cbind((data.frame(confusion_matrix_tst$overall)), rownames((data.frame(confusion_matrix_tst$overall))))
  
  output$view_15 <- renderTable({
    a
  })
  
  output$view_14 <- renderTable({
    names(f) <- c("score", "parameter")
    f
  })
  
  # View(data.frame(confusion_matrix_tst$table))
  
  
  
  # the basic model is giving an accuracy of around 88.77% which is 10% better than a 
  # random blind guess of 70% as can be seen from the data
  
  
  # blind_guesstimate <- ((length(which(sentiment_tr_3$`Star Rating` == 5)))+(length(which(sentiment_tst$Star_Rating == 5)))/740+294)*100
  
  
  # ((length(which(test_prediction$max_prob == 5)))/294)*100
  
  
  # complete the above part later.
  
  
  
  
  
  #------------------------------------------------------------------------ objective 9
  
  # making seperate datasets with different tags 
  
  train_service <- sqldf("SELECT *
                         FROM tidy_text_tr
                         WHERE tags_train LIKE '%service%'")
  
  train_interior <- sqldf("SELECT *
                          FROM tidy_text_tr
                          WHERE tags_train LIKE '%interior%'")
  
  train_price <- sqldf("SELECT *
                       FROM tidy_text_tr
                       WHERE tags_train LIKE '%price%'")
  
  train_handling <- sqldf("SELECT *
                          FROM tidy_text_tr
                          WHERE tags_train LIKE '%handling%'")
  
  
  #Calculating tf-idf for Service Tag Text reviews
  
  train_service_tf_idf <- train_service %>% 
    count(linenumber, word, sort = TRUE) %>% 
    ungroup() %>%
    bind_tf_idf(word, linenumber,n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))), tag = 'service') 
  
  output$view_10 <- renderTable({
    setcolorder(train_service_tf_idf, c("linenumber","word","n","tf","idf","tf_idf","tag"))
    names(train_service_tf_idf) <- c("linenumber","word","No. of Occurences","Term Frequency","Inverse Document Frequency","tf_idf","tag")
    train_service_tf_idf[1:input$numeric_5,2:6]
  })
  
  top_ten_service <- head(train_service_tf_idf,10)
  
  
  #Calculating tf-idf for handling Tag Text reviews
  
  train_handling_tf_idf <- train_handling %>% 
    count(linenumber, word, sort = TRUE) %>% 
    ungroup() %>%
    bind_tf_idf(word, linenumber,n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))), tag = 'handling') 
  
  output$view_11 <- renderTable({
    setcolorder(train_handling_tf_idf, c("linenumber","word","n","tf","idf","tf_idf","tag"))
    names(train_handling_tf_idf) <- c("linenumber","word","No. of Occurences",
                                     "Term Frequency","Inverse Document Frequency","tf_idf","tag")
    train_handling_tf_idf[1:input$numeric_5,2:6]
  })
  
  top_ten_handling <- head(train_handling_tf_idf,10)
  
  #Calculating tf-idf for interior Tag Text reviews
  
  train_interior_tf_idf <- train_interior %>% 
    count(linenumber, word, sort = TRUE) %>% 
    ungroup() %>%
    bind_tf_idf(word, linenumber,n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))), tag = 'interior') 
  
  output$view_12 <- renderTable({
    setcolorder(train_interior_tf_idf, c("linenumber","word","n","tf","idf","tf_idf","tag"))
    names(train_interior_tf_idf) <- c("linenumber","word","No. of Occurences",
                                      "Term Frequency","Inverse Document Frequency","tf_idf","tag")
    train_interior_tf_idf[1:input$numeric_5,2:6]
  })
  
  top_ten_interior <- head(train_interior_tf_idf,10)
  
  #Calculating tf-idf for price Tag Text reviews
  
  train_price_tf_idf <- train_price %>% 
    count(linenumber, word, sort = TRUE) %>% 
    ungroup() %>%
    bind_tf_idf(word, linenumber,n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))), tag = 'price') 
  
  output$view_13 <- renderTable({
    setcolorder(train_price_tf_idf, c("linenumber","word","n","tf","idf","tf_idf","tag"))
    names(train_price_tf_idf) <- c("linenumber","word","No. of Occurences",
                                      "Term Frequency","Inverse Document Frequency","tf_idf","tag")
    train_price_tf_idf[1:input$numeric_5,2:6]
  })
  
  top_ten_price <- head(train_price_tf_idf,10)
  
  # vizualization 
  
  top_ten_allTags <- rbind(top_ten_service,top_ten_interior, 
                           top_ten_price, top_ten_handling) %>%
    ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = tag)) +
    geom_bar(show.legend = FALSE, stat = "identity") +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~tag, ncol = 2, scales = "free") +
    coord_flip()
  
  output$plot_2 <- renderPlot({
    theme_set(theme_dark())
    plot <- top_ten_allTags
    plot
  })
  }
