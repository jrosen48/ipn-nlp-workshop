# ---------------------
# Part 1: loading, setting up
# ---------------------

# load packages

library(tidyverse)
library(tidytext)
library(here)
library(janitor)
library(topicmodels)
library(ldatuning)

# load data

d <- read_csv(here("data", "generality-data.csv"))
d <- clean_names(d) # so the names are easier to type

# ---------------------
# Part 2: process data
# ---------------------

my_corpus <- corpus(d)

my_dfm <- dfm(my_corpus,
              remove_punct = TRUE, 
              stem = FALSE,
              remove = stopwords('en'))

my_dfm <- my_dfm[ntoken(my_dfm) > 0,] # remove any docs with no included terms - would mess up the topic modeling

dtm <- convert(my_dfm, to = "topicmodels") # convert to the right object for the package we're using

# ---------------------
# Part 2: determine k, the number of topics
# ---------------------

result <- FindTopicsNumber(
    dtm,
    topics = 2:10,
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    verbose = TRUE
)

FindTopicsNumber_plot(result)

# ---------------------
# Part 3: estimate the model and examine output
# ---------------------

# below, add an integer for a value of k 
k5 <- LDA(my_dfm, k = )

my_topics <- tidy(k5, matrix = "beta")

my_topics

my_top_terms <- my_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

my_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
