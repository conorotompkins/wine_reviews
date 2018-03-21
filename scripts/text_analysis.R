##Text analysis

library(tidyverse)
library(tidytext)
library(tidyr)
library(ggraph)
library(igraph)
library(widyr)

set.seed(1234)

theme_set(theme_bw(base_size = 18))

df <- read_csv("data/winemag-data_first150k.csv") %>% 
  rename(id= X1)

###bigrams

df %>% 
  #filter(variety == "Cabernet Sauvignon") %>% 
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>% 
  #count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  count(word1, word2, sort = TRUE) -> df_bigrams

df_bigrams

df_bigrams %>% 
  filter(word1 == "not")

df_bigrams %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) -> df_bigrams

df_bigrams %>% 
  filter(word1 == "not")




df_bigrams %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  arrange(n) %>% 
  filter(n >= 1500) %>% 
  mutate(bigram = factor(bigram, levels = unique(bigram))) %>% 
  ggplot(aes(bigram, n)) +
  geom_col() +
  coord_flip()

df %>% 
  filter(variety == "Cabernet Sauvignon") %>% 
  select(description) %>% 
  filter(str_detect(description, "100|cabernet"))

###trigrams
df %>% 
  #filter(variety == "Cabernet Sauvignon") %>% 
  unnest_tokens(trigram, description, token = "ngrams", n = 3) %>% 
  #count(bigram, sort = TRUE) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  count(word1, word2, word3, sort = TRUE) -> df_trigrams

df_trigrams %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) -> df_trigrams
df_trigrams

df_trigrams %>% 
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  arrange(n) %>% 
  filter(n >= 300) %>% 
  mutate(trigram = factor(trigram, levels = unique(trigram))) %>% 
  ggplot(aes(trigram, n)) +
  geom_col() +
  coord_flip()

##network analysis
df_bigrams %>% 
  filter(n >= 500) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  scale_edge_width_continuous(range = c(.1, 2)) +
  scale_edge_alpha(range = c(.1, .6)) +
  theme_void(base_size = 18)

##bigram word correlation
wine_reviews <- df %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, description) %>%
  filter(!word %in% stop_words$word,
         !str_detect(word, "\\d"))

wine_reviews

#word_pairs <- wine_reviews %>% 
#  pairwise_count(word, section, sort = TRUE)

#word_pairs

word_cors <- wine_reviews %>%
  group_by(word) %>%
  filter(n() >= 800) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors


word_cors %>%
  filter(correlation > .2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width_continuous(range = c(.4, 2)) +
  scale_edge_alpha(range = c(.1, .6)) +
  theme_void(base_size = 18)


#trigram word correlation
wine_reviews <- df %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(trigram, description, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d"),
         !str_detect(word3, "\\d")) %>% 
  unite(trigram, word1, word2, word3, sep = " ")

wine_reviews

word_cors <- wine_reviews %>%
  group_by(trigram) %>%
  filter(n() >= 20) %>%
  pairwise_cor(trigram, section, sort = TRUE)

word_cors

word_cors %>%
  filter(correlation > .3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width_continuous(range = c(.4, 2)) +
  scale_edge_alpha(range = c(.1, .6)) +
  theme_void(base_size = 18)
