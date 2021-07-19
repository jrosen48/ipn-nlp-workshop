# ---------------------
# Part 1: loading, setting up
# ---------------------

# load packages

library(tidyverse)
library(tidytext)
library(here)
library(janitor)

# load data

d <- read_csv(here("data", "generality-data.csv"))
d <- clean_names(d) # so the names are easier to type

coding_frame_dictionary <- read_csv(here("data", "coding-frame-dictionary.csv"))
coding_frame_dictionary <- clean_names(coding_frame_dictionary)
# to distinction this from a code in another language
coding_frame_dictionary <- rename(coding_frame_dictionary, qual_code = code)

# ---------------------
# Part 2: explore the most frequent terms
# ---------------------

# after you run this, consider how the objects d and d_tokens differ
d_tokens <- d %>% 
    unnest_tokens(word, text)

# count words
d_tokens %>% 
    count(word, sort = TRUE) # what happens if you change TRUE to FALSE?

# what do you notice about the most frequent terms?

# remove stopwords and then count words
d_tokens %>% 
    anti_join(stop_words) %>% # stop_words is a built-in data set 
    count(word, sort = TRUE)

# what do you now notice about the most frequent terms?

# ---------------------
# Part 3: use dictionaries to identify key terms
# ---------------------

# let's start with sentiment
sentiment_scores <- get_sentiments("bing") # take a look at sentiment scores

# inspect the sentiment scores data frame (by printing its name or viewing it)

# join the sentiment scores dictionary to our tokenized text
d_tokens %>% 
    left_join(sentiment_scores)

# what do you notice about what words are positive and negative?
# next, add the following line after a pipe symbol to your code: count(sentiment)
# what do you notice after doing this?

# let's move on to a dictionary
# consider the following dictionary as a set of key words indicating one of six codes
coding_frame_dictionary

d_tokens %>% 
    anti_join(stop_words) %>% #  removes stop words
    left_join(coding_frame_dictionary) %>% # joins the coding frame dictionary
    filter(!is.na(qual_code)) %>% # removes words without matches in the dictionary
    distinct(id, lesson, word, qual_code)

# use View() to explore more of the output - what do you notice?
# next, count up the qual codes with: count(qual_code)
# what do you notice then?
