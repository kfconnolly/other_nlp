install.packages("sentimentr")
library(sentimentr)



# join word_counts to bing sentiment lexicon
bing_word_counts <- word_tokens_no_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# join word_counts, include segment & device (same as above, except this df will include segment & device)
bing_word_counts_path <- word_tokens_no_stop_path %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, split_2, sort = TRUE) %>%
  ungroup()


# plot 1: initial plot for most common positive & negative words
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(word, n, fill = sentiment)) +
  theme_new() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +                                          # create individual graph for each survey
  labs(y = "number of times word appeared",
       x = "word") +
  ggtitle("Top 20 Words by Sentiment") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0),
                                  size = 20),
        axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 5, l = 20)),  # x axis formatting (center, padding)
        axis.title.y = element_text(size = 10, margin = margin(t = 10, r = 10, b = 5, l = 10)),  # x axis formatting (center, padding)
        axis.text.x = element_text(size = 12, margin = margin(t = 10, r = 5, b = 5, l = 20)),
        axis.text.y = element_text(size = 12, margin = margin(t = 10, r = 5, b = 5, l = 20)),
        strip.background = element_rect(fill="grey"),
        strip.text = element_text(size = 14)) +
  coord_flip()




####-----SENTIMENT ANALYSIS RESPONSES (AFINN)-----####



#---get afinn scores---#

# make df of sentiment lexicon with -5 to +5 sentiment scores
afinn_sm <- get_sentiments("afinn")


# join sentiment scored to tokes
word_tokens_no_stop_sm <- word_tokens_no_stop %>% left_join(afinn_sm)


## check that there is not duplication
# check NA count in the score column (word token not in afinn lexicon)
word_tokens_no_stop_sm %>% filter(is.na(score)) %>% select(word) %>% unique() %>% count()
## out: 4052


# check non-NA count
word_tokens_no_stop_sm %>% filter(!is.na(score)) %>% select(word) %>% unique() %>% count()
## out: 405


# total in word_tokens_no_stop, should equal the sum of the 2 outputs above
word_tokens_no_stop_sm %>% select(word) %>% unique() %>% count()
## out: 4457


# make df of average sentiment score per row ID
response_sm_score <- 
  word_tokens_no_stop_sm %>% 
  filter(!is.na(score)) %>%                   # remove any NA scores
  group_by(row_id) %>%                        # group results by row ID
  summarise(avg_sm_score = mean(score))       # calculate the avg sentiment score 


# join avgerage sentiment score to original responses, drop NA & one-word answers
commenting_june2019_sm <- left_join(commenting_june2019_sm, response_sm_score, by="row_id") %>%
  filter(word_count > 1) %>%
  drop_na()





sentiment=sentiment_by(commenting_june2019_clean$reply_removed)

sentence_sm <- commenting_june2019_clean %>%
  get_sentences() %>%
  sentiment()




sentence_sm %>%
  group_by(split_2) %>% 
  summarize(mean = mean(sentiment)) %>%
  filter(!split_2 %in% c("cahungpath", "zzz", "hungtestpath", "ellipsis", "pb")) %>%

  ggplot(aes(split_2, mean)) +                                                    # fill breaks out by device
  geom_col(position= "dodge", alpha = 0.8) +                                                       # dodge breaks the devices out into separate bars
  theme_light() +
  ggtitle("Avg. Sentiment Score of Full Responses by Survey & Device") +
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
