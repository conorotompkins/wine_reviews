##Text analysis

library(tidyverse)
library(tidytext)
library(widyr)

theme_set(theme_bw(base_size = 18))

df <- read_csv("data/winemag-data_first150k.csv") %>% 
  rename(id= X1)

