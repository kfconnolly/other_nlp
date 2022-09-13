install.packages("tidytext")
install.packages("bigrquery")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("textdata")
install.packages("pacman")
install.packages("sentimentr")




library(dplyr)
library(bigrquery)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(reshape2)
library(formattable)
library(ggridges)
library(readr)
library(sentimentr)
library(magrittr)




# load data
movie_quotes = read_csv("/Users/connolk/Downloads/movie_quotes.csv")

# get word tokens from responses in 'Quote' column
word_tokens <- movie_quotes %>%
  unnest_tokens(word, input = Quote)

# remove stop words
word_tokens_no_stop <- word_tokens %>% anti_join(get_stopwords())


# join word_counts to bing sentiment lexicon
bing_word_counts <- word_tokens_no_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# make df of sentiment lexicon with -5 to +5 sentiment scores
afinn_sm <- get_sentiments("afinn")

# join sentiment scored to tokes
word_tokens_no_stop_sm <- word_tokens_no_stop %>% left_join(afinn_sm)





# unnest SENTENCES
sentences <- unnest_tokens(movie_quotes, input = "Quote", output = "Sentence", token = "sentences")


sentences %>% 
  unnest %>% 
  sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% 
  mutate(characters = nchar(stripWhitespace(text)))
