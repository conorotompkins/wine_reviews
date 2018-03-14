library(tidyverse)

theme_set(theme_bw(base_size = 18))

df <- read_csv("data/winemag-data_first150k.csv") %>% 
  rename(id= X1)

glimpse(df)


df %>% 
  count(country, sort = TRUE)

df %>% 
  filter(is.na(country))

df %>% 
  filter(!is.na(country)) %>% 
  count(country) %>% 
  arrange(n) %>% 
  mutate(country = factor(country, levels = unique(country))) %>% 
  ggplot(aes(country, n)) +
  geom_col() +
  #scale_x_reverse() +
  coord_flip()

df %>% 
  count(variety) %>% 
  arrange(n) %>% 
  filter(n >= 500) %>% 
  mutate(variety = factor(variety, levels = unique(variety))) %>% 
  ggplot(aes(variety, n)) +
  geom_col() +
  #scale_x_reverse() +
  coord_flip()

df %>% 
  count(winery) %>% 
  arrange(n) %>% 
  filter(n >= 150) %>% 
  mutate(winery = factor(winery, levels = unique(winery))) %>% 
  ggplot(aes(winery, n)) +
  geom_col() +
  #scale_x_reverse() +
  coord_flip()

df %>% 
  filter(!is.na(designation)) %>% 
  count(designation) %>% 
  arrange(n) %>% 
  filter(n >= 300) %>% 
  mutate(designation = factor(designation, levels = unique(designation))) %>% 
  ggplot(aes(designation, n)) +
  geom_col() +
  #scale_x_reverse() +
  coord_flip()

df %>% 
  filter(is.na(designation))

df %>% 
  count(winery, variety, sort = TRUE)
