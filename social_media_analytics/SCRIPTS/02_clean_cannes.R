library(stringr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(rtweet)


setwd("C:/Users/silvia.bordogna/Documents/Uni/sma/progetto/")


# Load data 
---------------------------
filenames <- list.files("./DATA/twitter/en/", pattern="*.rds", full.names=TRUE)

tweet_ls <- do_call_rbind(lapply(filenames, FUN = function(file) {
  readRDS(file)
}))

tweet_df <- data.frame(lapply(tweet_ls, as.character), stringsAsFactors = FALSE) %>% distinct()




# ETL gephi data ---------------------------
tweet_df_isretweet <- tweet_df[tweet_df$is_retweet == TRUE, ]
target <- tweet_df_isretweet$retweet_screen_name
source <- tweet_df_isretweet$screen_name
df <- data.frame(cbind(source, target))

df_w <- df %>% 
  group_by(source, target) %>%
  summarise(Weigth=n())  %>%
  
  group_by(source) %>%
  mutate(outdegree=n())  %>%
  group_by(target) %>%
  mutate(indegree=n()) %>%
  filter(outdegree>3 ) %>%
  select(source, target, Weigth ) %>%
  distinct() 



write.csv(df_w, './GEPHI/gephi_links_w_out2.csv', row.names = F)


# ETL tableau data ---------------------------
tweet_df_subset <- tweet_df[,c('screen_name','status_id','created_at', 'source', 'display_text_width', 'reply_to_screen_name',
                              'is_retweet', 'retweet_screen_name', "favorite_count", "retweet_count", "followers_count" ,"friends_count", "listed_count",
                              "statuses_count", "favourites_count", "account_created_at", "verified","account_lang", "location")]
write.csv(tweet_df_subset, './TABLEAU/tweet.csv', row.names = F)




# ETL hashtag data ---------------------------
tweet_df$hashtags <- gsub( "\"", "", tweet_df$hashtags)
tweet_df$hashtags <- gsub( ")", "", tweet_df$hashtags)
tweet_df$hashtags <- gsub( "c\\(", "", tweet_df$hashtags)
tweet_df$hashtags <- gsub( " " , "", tweet_df$hashtags)


hashtag <- data.frame(
  status_id = rep(tweet_df$status_id, unlist(lapply(strsplit(tweet_df$hashtags, ","), length))),
  is_retweet = rep(tweet_df$is_retweet, unlist(lapply(strsplit(tweet_df$hashtags, ","), length))),
  created_at = substr(rep(tweet_df$created_at, unlist(lapply(strsplit(tweet_df$hashtags, ","), length))), 1, 10),
  screen_name = rep(tweet_df$screen_name, unlist(lapply(strsplit(tweet_df$hashtags, ","), length))),
  hashtags = tolower(unlist(strsplit(tweet_df$hashtags, ","))),
  stringsAsFactors = F
)


hashtag_w <-  
  hashtag %>% 
  group_by(created_at, hashtags, is_retweet) %>%
  summarise(n = n())
  #dcast( ... ~ is_retweet, value.var = "n", fill = 0)


write.csv(hashtag, './TABLEAU/hashtag.csv', row.names = F)
write.csv(hashtag_w, './TABLEAU/hashtag_w.csv', row.names = F)
