library(rtweet)
library(stringr)
library(tidyr)
library(tidyverse)
library(reshape2)

setwd("C:/Users/silvia.bordogna/Documents/Uni/sma/progetto/")




# Download data ---------------------------

token <- create_token(
  app = "sma_uni_project",
  consumer_key = "cmJ77CEWYWA4rWLcNihU3EHOb",
  consumer_secret = "2Z1115GZpbt6H8r5Nz7bf8RHRNSaWJeqmb4UEYqfPrI1oQm3Sj",
  #access_token = "2209328678-QNnt7NyJM34AwNIzqiAkwMZgrX243zj8oa44G9P",
  #access_secret = "qdsB2XdzrOBqP6TeHSqfoSZGHEtdNBaQJdF9hP2pSpGBz",
  set_renv = F
)

keyword_list <- c("#Cannes2019")#, "@Festival_Cannes")

dates_list <- seq(from=Sys.Date()-9, to=Sys.Date()-3, by=1)

#eng
for (keyword in keyword_list) {
  for (i in seq_along(dates_list)) {
    date <- dates_list[i]
    tweet_list <- search_tweets(keyword, 
                                n = 100000, 
                                since = date, 
                                until = date + 1, 
                                include_rts = T,
                                lang = "en", 
                                token = token, 
                                retryonratelimit = T, 
                                type = "mixed") 
    saveRDS(tweet_list, file = paste0("./DATA/twitter/tweet_", keyword, "_", date, ".rds"))
  }
}


#fr
for (keyword in keyword_list) {
  for (i in seq_along(dates_list)) {
    date <- dates_list[i]
    tweet_list <- search_tweets(keyword, 
                                n = 100000, 
                                since = date, 
                                until = date + 1, 
                                include_rts = TRUE,
                                lang = "fr", 
                                token = token, 
                                retryonratelimit = T,
                                type = "mixed") 
    saveRDS(tweet_list, file <- paste0("./DATA/twitter/fr/tweet_", keyword, "_", date, ".rds"))
  }
}

#it
for (keyword in keyword_list) {
  for (i in seq_along(dates_list)) {
    date <- dates_list[i]
    tweet_list <- search_tweets(keyword, 
                                n = 100000, 
                                since = date, 
                                until = date + 1, 
                                include_rts = TRUE,
                                lang = "it", 
                                token = token, 
                                retryonratelimit = T, 
                                type = "mixed")
    saveRDS(tweet_list, file = paste0("./DATA/twitter/it/tweet_", keyword, "_", date, ".rds"))
  }
}


