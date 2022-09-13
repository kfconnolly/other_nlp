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
  full_join(vwo_weather_aug2019_useful, by = "row_id") %>%

  # add word_count column
  mutate(word_count = str_count(vwo_weather_aug2019_clean$answer, "\\w+"))
  

#---tokenization---#

# get word tokens from responses in 'answer' column
word_tokens <- vwo_weather_aug2019_updated %>%
  unnest_tokens(word, input = answer)


# remove stop words
word_tokens_no_stop <- word_tokens %>% anti_join(get_stopwords())


# final word counts
word_counts <- word_tokens_no_stop %>% count(word, sort = T)


# word counts with 'usefulness' category included
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
  scale_y_continuous(labels = scales::number_format(accuracy = 1.0), breaks = seq(0, 12, 2)) +                        # set decimal points on axis
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




##---WORD COUNT---##

# average word_count of all responses
vwo_weather_aug2019_updated %>% 
  filter(!is.na(word_count)) %>%                             # remove NA reponses in word_count
  mutate(word_count = as.integer(word_count)) %>%            # convert word_count back to interger
  summarize(median(word_count))                              # calculate median word count
  # output: 5

# average word_count of all responses
vwo_weather_aug2019_updated %>% 
  filter(!is.na(word_count)) %>%                             # remove NA reponses in word_count
  group_by(weather_usefulness) %>%                           # group by weather_usefulness to calculate each individually
  mutate(word_count = as.integer(word_count)) %>%            # convert word_count back to interger
  summarize(median(word_count))                              # calculate median word count
  #  <chr>                             <dbl>
  #  1 didnt_see_it                        5  
  #  2 no_opinion                          4.5
  #  3 not_useful                          5  
  #  4 useful                              6  
  #  5 NA                                  5  



# count number of responses that have a text answer (no NA word_count)
vwo_weather_aug2019_updated %>% 
  filter(!is.na(word_count)) %>%                             # remove NA reponses in word_count
  summarize(count = n())                                     # calculate number of rows
  # output: 152

# to double-check, count number of responses that have no text answer (NA word_count)
vwo_weather_aug2019_updated %>% 
  filter(is.na(word_count)) %>%                              # remove NA reponses in word_count
  summarize(count = n())                                     # calculate number of rows
  # output: 203

# count number of responses with 1 word
vwo_weather_aug2019_updated %>% 
  filter(word_count == 2) %>%                                # remove NA reponses in word_count
  summarize(count = n())                                     # calculate number of rows
 # output: 18






library(sentimentr)


####-----TF-IDF-----####



# df with tf-idf calcs; documents are the different surveys
tf_idf <- word_counts_useful %>%
  filter(!is.na(word)) %>% 
  #  mutate(word_length = (nchar(word_counts$word))) %>%
  #  filter(word_length > 2) %>%                                                   # remove words less than 2 characters long
  select(word, weather_usefulness, n) %>%
  group_by(word, weather_usefulness) %>% mutate(n = sum(n)) %>%                  # sum word counts by survey
  ungroup() %>%
  bind_tf_idf(word, weather_usefulness, n)


# plot tf-idf by survey
tf_idf %>%
  filter(n > 1) %>%                                                   # only select words appearing at least twice
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(weather_usefulness) %>%
  top_n(5) %>%
  ungroup %>%
  
  ggplot(aes(word, tf_idf, fill = weather_usefulness)) +
  theme_light() +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~weather_usefulness, ncol = 2, scales = "free") +                                      # create individual graph for each survey
  ggtitle("tf-idf (uniquely important words appearing at least twice, by 'usefulness' response)") +
  theme(plot.title = element_text(hjust = 0.5,                                           # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +     # x axis formatting (center, padding)
  coord_flip()



####-----SENTIMENT ANALYSIS WORDS (BING)-----####



#---find most common positive and negative words---#

# join word_counts to bing sentiment lexicon
bing_word_counts <- word_tokens_no_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# join word_counts, include segment & device (same as above, except this df will include segment & device)
bing_word_counts_segments <- word_tokens_no_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sort = TRUE) %>%
  ungroup()


# plot 1: initial plot for most common positive & negative words
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(word, n, fill = sentiment)) +
  theme_light() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +                                          # create individual graph for each survey
  scale_y_continuous(labels = scales::number_format(accuracy = 1.0)) +                 # sets decimal place for x axis
  labs(y = "number of times word appeared",
       x = "word") +
  ggtitle("Top 20 Words by Sentiment, All Responses") +
  theme(plot.title = element_text(hjust = 0.5,                                         # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)),     # axis formatting (center, padding)
        axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20)) ) +   
  coord_flip()



####-----SENTIMENT ANALYSIS WORDS (AFINN)-----####

# make df of sentiment lexicon with -5 to +5 sentiment scores
afinn_sm <- get_sentiments("afinn")


# join sentiment scored to tokes
word_tokens_no_stop_sm <- word_tokens_no_stop %>% left_join(afinn_sm)


# make df of average sentiment score per row ID
response_sm_score <- 
  word_tokens_no_stop_sm %>% 
  filter(!is.na(word)) %>%                    # remove any NA scores
  group_by(row_id) %>%                        # group results by row ID
  summarise(avg_sm_score = mean(score))       # calculate the avg sentiment score 


vwo_weather_sm <- left_join(vwo_weather_analysis , response_sm_score, by="row_id") %>%
  filter(word_count > 1) %>%
  drop_na()

####-----SENTIMENT ANALYSIS WORDS (SENTIMENTR)-----####


sentiment=sentiment_by(vwo_weather_aug2019_updated$answer)

sentence_sm <- vwo_weather_aug2019_updated %>%
  get_sentences() %>%
  sentiment()




sentence_sm %>%
  group_by(weather_usefulness) %>% 
  filter(!is.na(weather_usefulness)) %>%                   # remove any NA scores
  summarize(mean = mean(sentiment)) %>%
  
  ggplot(aes(weather_usefulness, mean)) +                                                    # fill breaks out by device
  geom_col(aes(fill = mean > 0), position= "dodge", alpha = 0.8, show.legend = FALSE) +                                                       # dodge breaks the devices out into separate bars
  theme_light() +
  ggtitle("Avg. Sentiment Score of All Sentences") +
  labs (x = "'usefulnuess'  response",
        y = "avg. sentiment score of all sentences") +
  theme(plot.title = element_text(hjust = 0.5,                                                     # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = , l = 0)),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        legend.position = "top",                                                                    # puts legend at the top, rather than on the side
        legend.box = "horizontal") +                                                                # makes legend horizontal, rather than vertical)) +                # x axis formatting (center, padding)
  scale_y_continuous(limits=c(-0.5, 0.5)) +     # adjust y axis to be from -5 to 5
  scale_fill_manual(name = 'mean > 0', values = setNames(c('#70C3C9', '#E88C84'), c(T,F))) +                                      # customize colors for device category
  geom_hline(yintercept = 0, color = "grey48") +                                                    # add vertical, grey line at 0 
  coord_flip()