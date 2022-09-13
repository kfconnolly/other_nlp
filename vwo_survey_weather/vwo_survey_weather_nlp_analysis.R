install.packages("tidytext")
install.packages("bigrquery")
install.packages("tidyverse")
install.packages("dplyr")

library(dplyr)
library(bigrquery)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)




####-----SELECT & TOKENIZE DATA-----####



#---select the data from GBQ---#

# put GBQ project ID here
project <- "ga360-173318" 


# SQL query to select dat from GBQ
sql <- "SELECT * FROM `ga360-173318.surveymonkey.vwo_survey_weather_aug2019`"


# execute SQL query & save results in vwo_deb2019 df
vwo_weather_aug2019 <- query_exec(sql, project = project, use_legacy_sql = F)



#---cleaning---#

# remove Kat's test submission
vwo_weather_aug2019_clean <- vwo_weather_aug2019 %>%
  filter(row_id != "344")



# create one column for all answers to 'How useful is the local weather on this page?'
vwo_weather_aug2019_useful <- vwo_weather_aug2019_clean %>%
  gather(useful, not_useful, no_opinion, didnt_see_it, 
         key = "weather_usefulness", value = value, -row_id) %>%
  filter(value) %>%       # filter for all values that are TRUE; removes 21 repsonses
  group_by(row_id)



# create one column for all answers to 'What brought you to this website today? Check all that apply.'
# join to 'useful' table above
vwo_weather_aug2019_updated <- vwo_weather_aug2019_clean %>%
  gather(sports_news, search_for_specific_story, opinion_pieces, latest_news, todays_digital_edition, obituaries, customer_service, local_weather, national_political_news, business_news, breaking_news, job_search, food_news, other, key = "visit_reason", value = value, -row_id) %>%
  filter(value) %>%
  group_by(row_id) %>%
  summarise(visit_reason = paste0(visit_reason, collapse = ",")) %>%
  full_join(vwo_weather_aug2019_useful, by = "row_id")


#---tokenization---#

# get word tokens from responses in 'Question' column
word_tokens <- vwo_weather_aug2019_updated %>%
  unnest_tokens(word, input = answer)


# remove stop words
word_tokens_no_stop <- word_tokens %>% anti_join(get_stopwords())


# final word counts
word_counts <- word_tokens_no_stop %>% count(word, sort = T)


#word counts with 'usefulness' category included
word_counts_useful <- word_tokens_no_stop %>% count(word, weather_usefulness, sort = T)





# remove single-character responses from word_counts
word_counts <- 
  word_counts %>%
  mutate(word_length = (nchar(word_counts$word))) %>%     # add a word_length column with word character counts
  filter(word_length > 1) %>%                             # filter to remove words less than 2 characters
  select(-c(word_length))                                 # remove word_length column, because the ngram plots will select by that column rather than the n column



#---initial word count plot---#

# top 20 words, all surveys
top_terms <- word_counts %>%                                                      
  drop_na(word)%>%
  top_n(20, n) %>%                                                                   # select top 20 terms
  arrange(word, -n)                                                                  # used for sorting bars

top_terms %>%                                                                        # used for sorting bars
  mutate(word = reorder(word, n)) %>%
  group_by(word) %>%    
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  ungroup %>%
  mutate(word = factor(paste(word, sep = "__"), 
                       levels = rev(paste(word, sep = "__")))) %>%
  
  ggplot(aes(word, n)) +
  theme_light() +
  geom_col(show.legend = FALSE) +                    # fill used for sorting bars
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                      # used for sorting bars
  labs(x = "word",
       y = "number of times word appeared") +
  coord_flip() +
  ggtitle("Top 20 Words, All Responses") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))   # x axis formatting (center, padding)





# top 10 words, by usefulness response
top_terms_useful <- word_counts_useful %>%                                                      
  drop_na(word)%>%
  group_by(weather_usefulness) %>%
  top_n(10, n) %>%                                                                   # select top 20 terms
  ungroup() %>%
  arrange(weather_usefulness, -n)                                                                  # used for sorting bars

top_terms_useful %>%                                                                        # used for sorting bars
  mutate(word = reorder(word, n)) %>%
  group_by(weather_usefulness, word) %>%    
  arrange(desc(n)) %>% 
  top_n(10) %>% 
  ungroup() %>%
  mutate(word = factor(paste(word, weather_usefulness, sep = "__"), 
                       levels = rev(paste(word, weather_usefulness, sep = "__")))) %>%
  
  ggplot(aes(word, n)) +
  theme_light() +
  geom_col(aes(fill = as.factor(weather_usefulness)), show.legend = FALSE) +                    # fill used for sorting bars
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                      # used for sorting bars
  scale_y_continuous(labels = scales::number_format(accuracy = 1.0)) +                        # set decimal points on axis
  labs(x = "word",
       y = "number of times bigram appeared") +
  coord_flip() +
  facet_wrap(~weather_usefulness, ncol = 2, scales = "free") +
  ggtitle("Top 10 Words, by 'usefulness' response") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 5, l = 0)))   # x axis formatting (center, padding)




#---tokenization---#

# to remove stop words, re-aggregate responses by row id
no_stop_resp_frame <- word_tokens_no_stop %>% 
  group_by(row_id) %>% 
  mutate(no_stop_resps = paste(word, collapse = " ")) %>% 
  select(row_id, no_stop_resps) %>%
  unique()


# bigram tokenization
#bigrams <- no_stop_resp_frame %>%                            
#  unnest_tokens(ngram, token = "ngrams", n = 2,
#                input = no_stop_resps)


# tokenize bigrams
bigrams <- vwo_weather_aug2019_updated %>%
  unnest_tokens(ngram, token = "ngrams", n = 2,
                input = answer)


# get bigram counts
bigram_counts_useful <- bigrams %>% count(ngram, weather_usefulness, sort = T)


# trigram tokenization 
# not using no_stop_resp_frame created above, because I want to include stop words
trigrams <- vwo_feb2019_clean %>%                             
  unnest_tokens(ngram, token = "ngrams", n = 3,
                input = Question)


# get trigram counts
trigram_counts <- trigrams %>% count(ngram, sort = T)




#---plots---#

# plot 1: top 10 bigrams for each survey

# top 20 bigram sort plot

#bi_top_terms <- bigram_counts %>%
#  filter(!is.na(ngram)) %>% 
#  group_by(ngram) %>%
#  top_n(10) %>%
#  ungroup() %>%
#  arrange(ngram, -n)


bigram_counts %>%
  filter(!is.na(ngram)) %>% 
  mutate(ngram = reorder(ngram, n)) %>%                                              # used for bar sorting
#  group_by(ngram) %>%    
  arrange(desc(n)) %>%                                                               # used for bar sorting
  top_n(20) %>%                                                                      # select top 10 results
#  ungroup () %>%
  mutate(ngram = factor(paste(ngram, sep = "__"),                                    # used for bar sorting
                        levels = rev(paste(ngram, sep = "__")))) %>%
  
  ggplot(aes(ngram, n)) +
  theme_light() +
  geom_col(show.legend = FALSE) +                                                     # fill used for bar sorting
#  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                      # used for bar sorting
  scale_y_continuous(labels = scales::number_format(accuracy = 1.0)) +
  labs(x = "word",
       y = "number of times bigram appeared") +
  coord_flip() +
  ggtitle("Top 20 Bigrams, All Responses") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))   # x axis formatting (center, padding)








# bigrams useful
bi_top_terms_useful <- bigram_counts_useful %>%
  filter(!is.na(ngram)) %>% 
  group_by(weather_usefulness) %>%
  top_n(10) %>%
  ungroup() %>%
  arrange(weather_usefulness, -n)



bi_top_terms_useful %>%
  filter(!is.na(ngram)) %>% 
  mutate(ngram = reorder(ngram, n)) %>%                                              # used for bar sorting
  group_by(weather_usefulness, ngram) %>%    
  arrange(desc(n)) %>%                                                               # used for bar sorting
  top_n(10) %>%                                                                      # select top 10 results
  ungroup () %>%
  mutate(ngram = factor(paste(ngram, weather_usefulness, sep = "__"),               # used for bar sorting
                        levels = rev(paste(ngram, weather_usefulness, sep = "__")))) %>%
  
  ggplot(aes(ngram, n)) +
  theme_light() +
  geom_col(aes(fill = as.factor(weather_usefulness)), show.legend = FALSE) +          # fill used for bar sorting
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                       # used for bar sorting
  scale_y_continuous(labels = scales::number_format(accuracy = 1.0)) +
  labs(x = "word",
       y = "number of times bigram appeared") +
  coord_flip() +
  facet_wrap(~weather_usefulness, ncol = 2, scales = "free") +                                  # create individual graph for each survey          
  ggtitle("Top 10 Bigrams, by 'usefulness' response") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))   # x axis formatting (center, padding)


