##Text analysis

library(tidyverse)
library(tidytext)
library(tidyr)
#library(ggraph)

theme_set(theme_bw(base_size = 18))

df <- read_csv("data/winemag-data_first150k.csv") %>% 
  rename(id= X1)

###bigrams

df %>% 
  filter(variety == "Cabernet Sauvignon") %>% 
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
  filter(n >= 300) %>% 
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
  filter(variety == "Cabernet Sauvignon") %>% 
  unnest_tokens(bigram, description, token = "ngrams", n = 3) %>% 
  #count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>% 
  count(word1, word2, word3, sort = TRUE) -> df_trigrams

df_trigrams %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) -> df_trigrams
df_trigrams

df_trigrams %>% 
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  arrange(n) %>% 
  filter(n >= 50) %>% 
  mutate(trigram = factor(trigram, levels = unique(trigram))) %>% 
  ggplot(aes(trigram, n)) +
  geom_col() +
  coord_flip()
