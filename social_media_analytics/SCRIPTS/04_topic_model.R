library("dplyr")
library('data.table')
library("lattice")
library("ggplot2")
library("textclean")
library("mgsub")
library("stringi")
library("kernlab")
library("caret")
library("tm")
library("dplyr")
library("splitstackshape")
library("e1071")
library("tictoc")


setwd('C:/Users/silvia.bordogna/Documents/Uni/sma/progetto/')



# Load topic data ---------------------------

df_cinema <- read.csv('./DATA/twitter/topic/cinema.csv')
df_cinema <- df_cinema %>% distinct()

df_fashion <- read.csv('./DATA/twitter/topic/fashion.csv')
df_fashion <- df_fashion %>% distinct()

#df_cinema$topic = 'cinema'
#df_fashion$topic = 'fashion'

treshold <- 17760
train_cinema <- df_cinema[1:treshold, ]
train_fashion <- df_fashion[1:treshold, ]

test_cinema <- df_cinema[(treshold+1):dim(df_cinema)[1], ]
test_fashion <- df_fashion[(treshold+1):dim(df_fashion)[1], ]


train <- rbind(train_cinema, train_fashion)
test <- rbind(test_cinema, test_fashion)

names(train) <- c('doc_id', 'text')
names(test)  <- c('doc_id', 'text')



# Load Cannes Tweet ---------------------------

filenames <- list.files("./DATA/twitter/en/", pattern = "*.rds", full.names = TRUE)

tweet_ls <- do_call_rbind(lapply(filenames, FUN = function(file) {
  readRDS(file)
}))

tweet_df <- data.frame(lapply(tweet_ls, as.character), stringsAsFactors = FALSE) %>% distinct()

tweet_df <- tweet_df[tweet_df$is_retweet == F, c("status_id","text", "screen_name", "created_at")]
names(tweet_df) <- c('doc_id', 'text',"screen_name", "created_at")





# Preprocess Function ---------------------------

preprocess_dataset <- function(set) {
  corpus <- VCorpus(DataframeSource(set))
  # Strip white spaces at the beginning and at the end to overcome some problems
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  # User replace_contraction function from textclean package
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  corpus <- tm_map(corpus, content_transformer(removeNumbers))
  corpus <- tm_map(corpus, stemDocument, language = "english")
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  return(corpus)
}

apply_feature_selection_on_dtm <- function(dtm_fs, sparsity_value = 0.99, verbose = FALSE) {
  if (verbose) {
    print("DTM before sparse term removal")
    inspect(dtm_fs)
  }
  dtm_fs = removeSparseTerms(dtm_fs, sparsity_value)
  if (verbose) {
    print("DTM after sparse term removal")
    inspect(dtm_fs)
  }
  return(dtm_fs)
}


# Binary matrix
create_binary_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating binary matrix...")
  }
  dtm_binary <- DocumentTermMatrix(corpus, control = list(weighting = weightBin))
  dtm_binary <- apply_feature_selection_on_dtm(dtm_binary, sparsity_value, verbose)
  matrix_binary <- as.matrix(dtm_binary)
  return(matrix_binary)
}
# Bigram binary matrix
create_bigram_binary_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating bigram binary matrix...")
  }
  BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
  dtm_bigram_binary <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer, weighting = weightBin))
  dtm_bigram_binary <- apply_feature_selection_on_dtm(dtm_bigram_binary, sparsity_value, verbose)
  matrix_bigram_binary <- as.matrix(dtm_bigram_binary)
  return(matrix_bigram_binary)
}
# TF matrix
create_tf_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating tf matrix...")
  }
  dtm_tf <- DocumentTermMatrix(corpus)
  dtm_tf <- apply_feature_selection_on_dtm(dtm_tf, sparsity_value, verbose)
  matrix_tf <- as.matrix(dtm_tf)
  return(matrix_tf)
}
# Bigram TF matrix
create_bigram_tf_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating bigram tf matrix...")
  }
  BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
  dtm_bigram_tf <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
  dtm_bigram_tf <- apply_feature_selection_on_dtm(dtm_bigram_tf, sparsity_value, verbose)
  matrix_bigram_tf <- as.matrix(dtm_bigram_tf)
  return(matrix_bigram_tf)
}
# TF-IDF matrix
create_tfidf_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating tf-idf matrix...")
  }
  dtm_tfidf <- DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  dtm_tfidf <- apply_feature_selection_on_dtm(dtm_tfidf, sparsity_value, verbose)
  matrix_tfidf <- as.matrix(dtm_tfidf)
  return(matrix_tfidf)
}
# Bigram TF-IDF matrix
create_bigram_tfidf_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating bigram tf-idf matrix...")
  }
  BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
  
  dtm_bigram_tfidf <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  dtm_bigram_tfidf <- apply_feature_selection_on_dtm(dtm_bigram_tfidf, sparsity_value, verbose)
  matrix_bigram_tfidf <- as.matrix(dtm_bigram_tfidf)
  return(matrix_bigram_tfidf)
}



# Create matrix
create_matrix <- function(corpus, matrix_type, sparsity_value = 0.99, verbose = NULL) {
  if (matrix_type == 'binary') {
    matrix <- create_binary_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'bigram_binary') {
    matrix <- create_bigram_binary_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'tf') {
    matrix <- create_tf_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'bigram_tf') {
    matrix <- create_bigram_tf_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'tfidf') {
    matrix <- create_tfidf_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'bigram_tfidf') {
    matrix <- create_bigram_tfidf_matrix(corpus, sparsity_value, verbose)
  } else {
    print('Invalid matrix type!')
  }
  return(matrix)
}


find_intersection_and_create_dataframe <- function(matrix_1, matrix_2) {
  intersection_matrix <- data.frame(matrix_1[,intersect(colnames(matrix_1), colnames(matrix_2))])
  return(intersection_matrix)
}



label_training_set <- function(df) {
  df$Topic <- ''
  df[1:treshold,]$Topic <- 'cinema'
  df[(treshold+1):dim(df)[1],]$Topic <- 'fashion'
  return(df)
}


label_test_set <- function(df) {
  df$Topic <- ''
  df[1:4440,]$Topic <- 'cinema'
  df[(4440+1):dim(df)[1],]$Topic <- 'fashion'
  return(df)
}

summarize_distribution <- function(df) {
  df_percentage <- prop.table(table(df$Topic)) * 100
  distribution_summary <- cbind(freq=table(df$Topic), df_percentage)
  return(distribution_summary)
}










# Preprocess ---------------------------

# Possible values:  binary, bigram_binary, tf, bigram_tf, tfidf, bigram_tfidf
wanted_matrix_type <- 'binary'
wanted_sparsity_value <- 0.99
wanted_verbose <- FALSE


print("Training Set preprocessing...")
train_set <- preprocess_dataset(train)

print("Test Set preprocessing...")
test_set <- preprocess_dataset(test)


train_matrix <- create_matrix(train_set, wanted_matrix_type, wanted_sparsity_value, wanted_verbose)
test_matrix <- create_matrix(test_set, wanted_matrix_type, wanted_sparsity_value, wanted_verbose)


# Preprocess Cannes Tweet ---------------------------

tweet_df <- tweet_df %>%
  mutate(text = str_replace_all(text, "cannes film festival" , ""))


print("Test Set preprocessing...")
tweet_set <- preprocess_dataset(tweet_df)

tweet_matrix <- create_tfidf_matrix(tweet_set, 0.96, F)
dim(tweet_matrix)


tweet_matrix <- create_binary_matrix(tweet_set, 0.96, F)
dim(tweet_matrix_binary)


# Preprocess 2 ---------------------------

train_df <- find_intersection_and_create_dataframe(train_matrix, test_matrix)
dim(train_df)
train_df <- find_intersection_and_create_dataframe(train_df, tweet_matrix)
dim(train_df)

test_df <- find_intersection_and_create_dataframe(test_matrix, train_df)
dim(test_df)

x <- find_intersection_and_create_dataframe(tweet_matrix, train_df)
dim(x)
x[setdiff(colnames(train_df), colnames(tweet_matrix))] <- 0


train_df <- label_training_set(train_df)
test_df <- label_test_set(test_df)


print(summarize_distribution(train_df))
print(summarize_distribution(test_df))






# Model Function ---------------------------

train_svm_classifier <- function(train_df, metric, control) {
  tic("SVM")
  set.seed(7)
  model <- train(Topic~., data = train_df, method = "svmRadial", metric = metric, trControl = control)
  toc()
  return(model)
}

train_knn_classifier <- function(train_df, metric, control) {
  tic("KNN")
  set.seed(7)
  model <- train(Topic~., data = train_df, method = "knn", metric = metric, trControl = control)
  toc()
  return(model)
}

train_rf_classifier <- function(train_df, metric, control) {
  tic("Random Forest")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="rf", metric=metric, trControl=control)
  toc()
  return(model)
}






# Model Training ---------------------------

control <- trainControl(method = "cv", number = 8, classProbs = TRUE)
metric <- "Accuracy"

svm_model <- train_svm_classifier(train_df, metric, control)
saveRDS(svm_model, "./SCRIPTS/svm_model_binary.rds")
#saveRDS(svm_model, "./SCRIPTS/svm_model_binary.rds")
svm_model <- readRDS("./SCRIPTS/svm_model_tfidf.rds")








# Model Testing ---------------------------
svm_predictions <- predict(svm_model, newdata = test_df)
svm_confusion_matrix <- confusionMatrix(table(svm_predictions, test_df$Topic))
cat('SVM test accuracy: ', unname(svm_confusion_matrix$overall[1]), '\n')



# Apply model on Cannes Tweet ---------------------------
svm_predictions <- predict(svm_model, newdata = x, type = "prob")

tweet_df$svm_predictions_cinema <- svm_predictions$cinema
tweet_df$svm_predictions_fashion <- svm_predictions$fashion
write.csv(tweet_df, './DATA/twitter/topic/topic_predicted_svm_tfidf_clean.csv', row.names = F)


