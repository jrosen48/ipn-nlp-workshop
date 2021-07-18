set.seed(0719)

library(tidymodels) # doesn't load forcats, stringr, readr from tidyverse
library(readr)
library(vip)
library(quanteda)

d <- read_csv("all-new-data.csv")
