# ---------------------
# Part 1: loading, setting up
# ---------------------

# load packages

set.seed(300)

library(tidyverse)
library(tidytext)
library(here)
library(janitor)
library(quanteda.textmodels)
library(caret)

# load data

d <- read_csv(here("data", "generality-data.csv"))
d <- clean_names(d) # so the names are easier to type
d <- mutate(d, unique_id = 1:nrow(d))
d <- mutate(d, code = round(code)) # this is to ignore sub-codes

# ---------------------
# Part 2: process data
# ---------------------

id_train <- sample(1:nrow(d), 700, replace = FALSE)

my_corpus <- corpus(d)

my_corpus$id_numeric <- 1:ndoc(my_corpus)

my_dfm <- dfm(my_corpus,
              remove_punct = TRUE, 
              stem = FALSE,
              remove = stopwords('en'))

# get training set
dfmat_training <- dfm_subset(my_dfm, id_numeric %in% id_train)

# get test set (documents not in id_train)
dfmat_test <- dfm_subset(my_dfm, !id_numeric %in% id_train)

# ---------------------
# Part 3: estimate model (naive bayes) and calculate fit
# ---------------------

tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$code)
# summary(tmod_nb)

dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

actual_class <- dfmat_matched$code
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)

tab_class <- table(actual_class, predicted_class)
tab_class

confusionMatrix(tab_class, mode = "everything")
