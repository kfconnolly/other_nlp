#install.packages("sentimentr")
#install.packages("textdata")

library(sentimentr)
library(textdata)



#########----sentiment analysis words (BING)-----#########

# join word_counts to bing sentiment lexicon
sm_bing_word_counts <- word_tokens_no_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# join word_counts, include segment & device (same as above, except this df will include segment & device)
sm_bing_word_counts_segments <- word_tokens_no_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sort = TRUE) %>%
  ungroup()


# plot 1: initial plot for most common positive & negative words
sm_bing_word_counts %>%
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




#########----sentiment analysis responses (AFINN)-----#########


#---get afinn scores---#

# make df of sentiment lexicon with -5 to +5 sentiment scores
sm_afinn <- get_sentiments("afinn")


# join sentiment scored to tokes
sm_afinn_word_tokens_no_stop <- word_tokens_no_stop %>% left_join(sm_afinn)


## check that there is not duplication
# check NA count in the score column (word token not in afinn lexicon)
sm_afinn_word_tokens_no_stop %>% filter(is.na(value)) %>% select(word) %>% unique() %>% count()
## out: 4584


# check non-NA count
sm_afinn_word_tokens_no_stop %>% filter(!is.na(value)) %>% select(word) %>% unique() %>% count()
## out: 426


# total in word_tokens_no_stop, should equal the sum of the 2 outputs above
sm_afinn_word_tokens_no_stop %>% select(value) %>% unique() %>% count()
## out: 4457


# make df of average sentiment score per row ID
sm_afinn_response_score <- 
  sm_afinn_word_tokens_no_stop %>% 
  filter(!is.na(value)) %>%                   # remove any NA scores
  group_by(row_id) %>%                        # group results by row ID
  summarise(avg_sm_score = mean(value))       # calculate the avg sentiment score 





# join average sentiment score to original responses, drop NA & one-word answers
sm_afinn_app_responses <- left_join(app_responses, sm_afinn_response_score, by="row_id") %>%
  #filter(word_count > 1) %>%
  drop_na()


sentiment = sentiment_by(app_responses$message)

# get sentiment for each sentence
sm_afinn_sentence <- app_responses$message %>%
  get_sentences() %>%
  sentiment()

sm_afinn_sentence <- left_join(app_responses, sm_afinn_sentence, 
                               by = c("row_id" = "element_id"),
                               multiple = "all")

# calculate average sentiment of each response
# mutate is having issues, so just calculating and joning back up to original repsonse table
sm_afinn_sentence_avg <- sm_afinn_sentence %>%
  group_by(row_id) %>%
  #mutate(sm_sentence = summarize(mean = mean(sentiment)))
  summarize(mean = mean(sentiment)) %>%
  left_join(app_responses, by = "row_id")



#---plot---#

sm_afinn_sentence %>%
  group_by(row_id) %>% 
  summarize(mean = mean(sentiment)) %>%
  #filter(!split_2 %in% c("cahungpath", "zzz", "hungtestpath", "ellipsis", "pb")) %>%
  
  ggplot(aes(reason, mean)) +                                                                     # fill breaks out by device
  geom_col(position= "dodge", alpha = 0.8) +                                                       # dodge breaks the devices out into separate bars
  theme_light() +
  ggtitle("Avg. Sentiment Score of Full Responses by Reason") +
  labs (x = NULL,     
        y = "Overall Avg. Sentiment Score") +
  theme(plot.title = element_text(hjust = 0.5,                                                     # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        legend.position = "top",                                                                    # puts legend at the top, rather than on the side
        legend.box = "horizontal") +                                                                # makes legend horizontal, rather than vertical)) +                # x axis formatting (center, padding)
  scale_y_continuous(name="overall avg. sentiment score (entire responses)", limits=c(-1, 1)) +     # adjust y axis to be from -5 to 5
  scale_fill_manual(values=c("dodgerblue3","gold", "plum3")) +                                      # customize colors for device category
  geom_hline(yintercept = 0, color = "grey48") +                                                    # add vertical, grey line at 0 
  coord_flip()
