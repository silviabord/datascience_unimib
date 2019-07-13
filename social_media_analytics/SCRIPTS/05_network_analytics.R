library(igraph)
library(readr)
library(stringr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(rtweet)
library(wordcloud)
library(wordcloud2)
library(htmlwidgets)
options("scipen"=100, "digits"=4)

setwd("C:/Users/silvia.bordogna/Documents/Uni/sma/progetto/")


# Load data ---------------------------
filenames <- list.files("./DATA/twitter/en/", pattern="*.rds", full.names=TRUE)

tweet_ls <- do_call_rbind(lapply(filenames, FUN = function(file) {
  readRDS(file)
}))

tweet_df <- data.frame(lapply(tweet_ls, as.character), stringsAsFactors = FALSE) %>% distinct()

info_vertices <- 
  tweet_df %>% 
  select(screen_name, country, account_lang, 
         followers_count, favourites_count, friends_count) %>% 
  distinct() %>%
  group_by(screen_name, country, account_lang) %>%
  summarise(
    followers_count = sum(as.numeric(followers_count)),
    favourites_count = sum(as.numeric(favourites_count)),
    friends_count = sum(as.numeric(friends_count))
    ) 

link <- read.csv('./GEPHI/gephi_links.csv')
graph <- graph_from_edgelist(as.matrix(link), directed=T )
graph <- graph.edgelist(as.matrix(link), directed=T )



# Network Stats ---------------------------
x = which(degree(graph)==0)
graph = delete.vertices(graph,x)

is.simple(graph)

vcount(graph)
ecount(graph)


degree <- degree(graph)
mean(degree)
head(sort(degree, decreasing=T),20)
head(sort(degree, decreasing=F),20)
hist(unname(degree), breaks = 100)

degree_in <- degree(graph, mode="in")
mean(degree_in)
head(sort(degree_in, decreasing=T),100)
head(sort(degree_in, decreasing=F),20)
summary(degree_in)

degree_out <- degree(graph, mode="out")
mean(degree_out)
head(sort(degree_out, decreasing=T),1000)
head(sort(degree_out, decreasing=F),20)

df_degreeo <- data.frame(
  screen_name = names(degree_out),
  degree_out = unname(degree_out)
  )

#write.csv(df_degreeo, './TABLEAU/degree_out.csv', row.names = F)
summary(degree_out)

x <- df_degreeo %>% inner_join(info_vertices)

x %>% 
  group_by(account_lang) %>%
  summarise(degree_out = mean(degree_out),
            n = n()
            ) %>%
  ggplot(aes(n , degree_out)) + 
  geom_point()

centr_degree(graph, mode="in", normalized=T)

closeness <- closeness(graph, normalize=T)
summary(closeness)

head(sort(closeness, decreasing=T),20)
head(sort(closeness, decreasing=F),20)
hist(unname(closeness), breaks = 100)


betweenness <- (graph, normalize = F, weights = NULL)
quantile(betweenness, c(0.5, 0.6, 0.8, 0.985, 1))
summary(betweenness)
head(sort(betweenness, decreasing=T),20)
head(sort(betweenness, decreasing=F),20)
hist(unname(betweenness), breaks = 100)

mean_distance(graph, directed = T)
#mean_distance(graph, directed = F)
#distances(graph, v=V(graph),  to=V(graph), weights = NA)



centr_eigen(graph, directed=T, normalized=T)$centralization


hs <- hub_score(graph, weights=NA)$vector
head(sort(hs, decreasing=T),10)
as <- authority_score(graph, weights=NA)$vector
head(sort(as, decreasing=T),10)


edge_density(graph, loops=F)
edge_density(graph, loops=F)



reciprocity(graph)
dyad_census(graph)
triad_census(graph)


transitivity(graph, type="global")  # net is treated as an undirected network
transitivity(as.undirected(graph, mode="collapse")) # same as above
#transitivity(graph, type="local")
triad_census(graph) # for directed networks 



diameter(graph, directed=T, weights=NA)



largest_cliques(graph)


# Import Clustering from gephi ---------------------------

cluster = read.csv("./GEPHI/export_community.csv")
names(cluster)


cluster %>%
  top_n(10, wt = betweenesscentrality)

cluster %>%
  top_n(-10, wt = closnesscentrality)

cluster %>%
  top_n(10, wt = Eccentricity) %>%
  select(Id, Eccentricity)




a = cluster %>%
  arrange(Authority) %>%
  group_by(modularity_class) %>%
  mutate(n=n()) %>%
  top_n(1, wt = Authority)



a = cluster %>%
  arrange(Hub) %>%
  group_by(modularity_class) %>%
  mutate(n=n()) %>%
  top_n(1, wt = Hub)


head(cluster)


# Add hashtag to modularity ---------------------------


hashtag <- read.csv('./TABLEAU/hashtag.csv')


xxx <- hashtag %>%
  left_join(cluster, by = c("screen_name" = "Id")) %>%
  filter(
           is_retweet==F 
         & modularity_class %in% c(7) #(0, 11, 1, 6, 17, 2, 7) 
         & !grepl("cannes", hashtags, ignore.case = T)
         & !grepl("<", hashtags, ignore.case = T)
         & !is.na(hashtags)
         ) %>%
  select(modularity_class, hashtags) %>%
  group_by(modularity_class, hashtags) %>%
  summarise(n = n()) %>%
  top_n(40, wt=n)


names(xxx) <- c('modularity_class', 'word', 'freq')
xxx <- xxx[,c(2,3,1)]

my_graph_3 <- wordcloud2(xxx, fontFamily = "Georgia", fontWeight = "80", 
                         color = "#F19693", 
                         size = 1,  shape = "circle", backgroundColor = "#B9D9CC")
saveWidget(my_graph_3, "tmp.html", selfcontained = F)



