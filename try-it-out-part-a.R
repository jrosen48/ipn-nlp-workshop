library(tidyverse)
library(tidytext)
library(here)

d <- read_csv(here("data", "generality-data.csv"))

d %>% 
    unnest_tokens(text, word)
