library(rtweet)
library(stringr)
library(tidyr)
library(tidyverse)
library(reshape2)

setwd('C:/Users/silvia.bordogna/Documents/Uni/sma/progetto/')


# Download data ---------------------------

stream_tweets(
  "fashion,dress",
  timeout = 60*800 ,
  language = "en",
  file_name = "./DATA/twitter/topic/fashion3.json",
  parse = FALSE,
  include_rts=F
)


stream_tweets(
  "movie, film",
  timeout = 60*800 ,
  language = "en",
  file_name = "cinema.json",
  parse = FALSE
)



# Clean & Save data ---------------------------

df_cinema <- parse_stream("cinema.json")
write.csv(df_cinema[1:22200, c("status_id","text")], './DATA/twitter/topic/cinema.csv', row.names = F)


df_fashion <- parse_stream("./DATA/twitter/topic/fashion.json")
df_fashion2 <- parse_stream("./DATA/twitter/topic/fashion.json")
df_fashion3 <- parse_stream("./DATA/twitter/topic/fashion2.json")
df_fashion_all = rbind(df_fashion, df_fashion2, df_fashion3)
write.csv(df_fashion_all[1:22200, c("status_id","text")], './DATA/twitter/topic/fashion.csv', row.names = F)


df_cinema <- parse_stream("redcarpet.json")
write.csv(df_cinema[1:22200, c("status_id","text")], './DATA/twitter/topic/redcarpet.csv', row.names = F)