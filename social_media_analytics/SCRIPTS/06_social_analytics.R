

library(tm)
library(tidyverse)
#library(readxl)
#library(dplyr)
#library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
#library(reshape2)
library(tidytext)
library(htmlwidgets)
#library(syuzhet)
library(tidyr)
#library(ggplot2)
#library(plotly)
#library(lubridate) 
#library(scales)
library(extrafont)
#font_import()
library("NLP")
library("openNLP")
library("rtweet")

setwd("C:/Users/silvia.bordogna/Documents/Uni/sma/progetto/")


# Read data ---------------------------
filenames <- list.files("./DATA/twitter/en/", pattern = "*.rds", full.names = TRUE)

tweet_ls <- do_call_rbind(lapply(filenames, FUN = function(file) {
  readRDS(file)
}))

tweet_df <- data.frame(lapply(tweet_ls, as.character), stringsAsFactors = FALSE) %>% distinct()

tweets <- tweet_df %>% 
  filter (is_retweet == F) %>%
  select(status_id, screen_name, text, created_at)



# Stats ---------------------------

tweets_picture <- tweets %>%
  filter(!str_detect(text, '^"')) %>% #remove citations
  count(presence =  ifelse(str_detect(text, "t.co"), "Yes", "No")) %>%
  mutate(type = "picture")

tweets_hashtag <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(presence =  ifelse(str_detect(text, "#[a-z A-Z 0-9]"), "Yes", "No")) %>%
  mutate(type = "hashtag")

tweets_mention <-tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(presence = ifelse(str_detect(text, "@[a-z A-Z 0-9]"), "Yes", "No")) %>%
  mutate(type = "mention")


# Words ---------------------------

reg <- "([^A-Za-z\\d~@]|(?![A-Za-z\\d#@]))"

tweets_words <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% #remove url
  unnest_tokens(word, text, token = "regex", pattern=reg) %>% # tokenize but keep # and @
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

words <- tweets_words %>%
  count(word) 

top <- tweets_words %>% count(word) %>% top_n(100) %>% filter(!str_detect(word, "cannes")) 
names(top) <- c("word", "freq")
#write.csv(top, './TABLEAU/top.csv', row.names = F)

my_graph <- wordcloud2(top, 
                       fontFamily = "Georgia",
                       fontWeight = "100", 
                       color = "#212a1dff",
                       size = 2,
                       shape = "circle",
                       backgroundColor ='#ffffff') # '#B9D9CC')
saveWidget(my_graph, "tmp.html", selfcontained = F)


# Sentiment Bing ---------------------------
words_sentiment = words %>%
  inner_join(get_sentiments("bing")) 


words_sentiment %>% group_by(sentiment) %>%
  summarise(count = sum(n)) %>%
  ggplot(aes(x = sentiment, count, fill = sentiment))+
  geom_bar(stat = 'identity', position = 'dodge')

x <- words_sentiment %>%
  filter(n > 20)
  #%>%
  #filter(!str_detect(word,"dead")) %>%
  #filter(!str_detect(word,"die")) 
#%>%  acast(word ~ sentiment, value.var = "n", fill=0)
#comparison.cloud(x, scale=c(3,.5), max=500, title.size = 1)

#write.csv(x, './TABLEAU/bing.csv', row.names = F)
#x = read.csv('./TABLEAU/bing.csv')


my_graph <- wordcloud2(x, 
                       fontFamily = "Georgia",
                       fontWeight = "100", 
                       color = ifelse(x$sentiment == 'negative', '#DB5E81', '#B9D9CC'), 
                       size = 2,
                       shape = "circle",
                       backgroundColor = '#ffffff') #"#212a1dff")
saveWidget(my_graph, "tmp.html", selfcontained = F)


#webshot("tmp.html", "my_graph.png", delay = 5)


# Sentiment NRC ---------------------------
sentiment = tweets_words %>%
  inner_join(get_sentiments("nrc")) %>% 


sentiment %>%
  group_by(screen_name, sentiment) %>%
  summarise(n = n()) %>%
  #acast(screen_name ~ sentiment, value.var = "n", fill=0) %>%
  group_by(sentiment) %>%
  summarise(count = sum(n)) %>%
  ggplot(aes(x = sentiment, count, fill = sentiment))+
  geom_bar(stat = 'identity', position = 'dodge')


# Load topic ---------------------------

tweets_words

tweet_topic <- read.csv('./DATA/twitter/topic/topic_predicted_svm_tfidf_clean.csv')
tweet_topic$doc_id <- as.character(tweet_topic$doc_id)

tweet_all = tweets_words %>%
  left_join(words_sentiment, by= 'word')  %>%
  left_join(tweet_topic, by= c("status_id" = "doc_id")) 


xxx <- tweet_all %>%
  group_by(status_id, screen_name.x, created_at.x, svm_predictions_cinema, svm_predictions_fashion) %>%
  count(sentiment) %>%
  dcast(... ~ sentiment, value.var = "nn", fill=0) %>%
  mutate(pol_neg = negative / sum(negative + positive + `NA`))



cinema = tweet_all %>% 
  filter(svm_predictions_cinema > 0.9) %>% 
  group_by(word) %>% 
  count(word) %>% 
  filter(!str_detect(word, "cannes")) %>%
  filter(!str_detect(word, "festival")) %>%
  filter(!str_detect(word, "film")) %>%
  filter(!str_detect(word, "movies")) %>%
  filter(nn>2) 

names(cinema) = c("word", "freq")

my_graph <- wordcloud2(cinema, 
                       fontFamily = "Georgia",
                       fontWeight = "100", 
                       color = "#212a1dff",
                       size = 2,
                       shape = "circle",
                       backgroundColor = '#B9D9CC')
saveWidget(my_graph, "tmp.html", selfcontained = F)



fashion = tweet_all %>% 
  filter(svm_predictions_fashion > 0.9) %>% 
  group_by(word) %>% 
  count(word) %>% 
  filter(!str_detect(word, "cannes")) %>%
  filter(!str_detect(word, "festival")) %>%
  filter(!str_detect(word, "@")) %>%
  filter(nn>10) 

names(fashion) = c("word", "freq")

my_graph <- wordcloud2(fashion, 
                       fontFamily = "Georgia",
                       fontWeight = "100", 
                       color = "#212a1dff",
                       size = 2,
                       shape = "circle",
                       backgroundColor = '#B9D9CC')
saveWidget(my_graph, "tmp_f.html", selfcontained = F)

write.csv(xxx, './TABLEAU/topic.csv', row.names = F, na = "")





# Sentiment on TF matrix ---------------------------
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


df <- tweets[, c('status_id', 'text')]
names(df) <- c('doc_id', 'text')
corpus <- preprocess_dataset(df)
tdm <- TermDocumentMatrix(corpus)

findFreqTerms(tdm, lowfreq = 900, highfreq = Inf)

tdm_sparse <- removeSparseTerms(tdm, sparse=0.999)
dim(tdm_sparse)

findAssocs(tdm_sparse, "hinakhan", corlimit = 0.2)

findAssocs(tdm_sparse, "gisaengchung", corlimit = 0.001)

findAssocs(tdm_sparse, "dead", corlimit = 0.1)

findAssocs(tdm_sparse, "para", corlimit = 0.001)


y <- as.data.frame(as.matrix(removeSparseTerms(tdm, sparse = 0.999)))
words <- data.frame(rowSums(y))
words$item <- row.names(words)
row.names(words) <- NULL
names(words) <- c('freq', 'word')
str(words)


words_sentiment <- 
  words %>%
  inner_join(get_sentiments("bing")) 

x <- 
  words_sentiment %>%
  filter(freq>2)

x <- x[,c(2,1,3)]

my_graph_3 <- wordcloud2(x, fontFamily="Georgia", fontWeight="100", 
                         color=ifelse(x$sentiment == 'negative', '#DB5E81', '#B9D9CC'), 
                         size=2, shape="circle", backgroundColor="#212a1dff")
saveWidget(my_graph_3, "tmp.html", selfcontained = F)






# Tag POS ---------------------------

sent_token_annotator <-  Maxent_Sent_Token_Annotator(language = 'en')
word_token_annotator <-  Maxent_Word_Token_Annotator(language = 'en')
pos_tag_annotator <-  Maxent_POS_Tag_Annotator(language = 'en')

#pos
get_tags <- function(tweets){
  
  tags <- character() 
  for (i in 1:length(tweets$text)){ 
    a2 <- NLP::annotate(tweets$text[i], list(sent_token_annotator, word_token_annotator)) 
    a3 <- NLP::annotate(tweets$text[i], pos_tag_annotator, a2) 
    a3w <- subset(a3, type == "word") 
    tags <- append(tags, sapply(a3w$features, `[[`, "POS")) 
  }
  
  # we remove some irrelevant tags   
  punctuations <- c(",", ".", "-", "-LRB-", "-RRB-", "POS", "``", ";", ":", "'", "$")
  return(tags[!tags %in% punctuations]) 
}


tweet_text <- tweets %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% #remove url
  filter(!str_detect(text, '^"')) %>%
  filter(str_trim(text) != "")%>%
  select(text)

tags <- as.tibble(get_tags(tweet_text))
  
pos <- tags %>%
  group_by(value) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(10, wt = freq) 

write.csv(pos, './TABLEAU/pos.csv', row.names = F, na = "")

p <- pos %>% ggplot(aes(x = value, y = freq))+
  geom_bar(stat = 'identity')+
  scale_y_continuous(limits = c(0, 0.3))+
  labs(x = "Part-of-speech tags", y = "Relative frequency", color = "")
