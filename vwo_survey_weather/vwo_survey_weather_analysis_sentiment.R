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



