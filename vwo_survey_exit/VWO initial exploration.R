#install.packages("tidytext")
#install.packages("bigrquery")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("quanteda")

library(dplyr)
library(bigrquery)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(ggraph)
library(igraph)
library(wordcloud)
library(reshape2)
library(quanteda)


# SELECT & TOKENIZE DATA ####

# select the data from GBQ
project <- "ga360-173318" # put your project ID here

sql <- "SELECT row_id, Question, question_2, segment FROM `ga360-173318.surveymonkey.all_vwo_surveys_feb2019`"
vwo_feb2019 <- query_exec(sql, project = project, use_legacy_sql = F)


vwo_feb2019 <- vwo_feb2019 %>%
  # add word_count column
  mutate(word_count = str_count(vwo_feb2019$Question, "\\w+")) %>%
  # identify the device for each observation in device
  mutate(device = str_replace_all(segment, ".*desktop$","desktop")) %>%
  mutate(device = str_replace_all(device, ".*mobile$","mobile")) %>%
  mutate(device = str_replace_all(device, ".*subscription.*","")) %>%
  # replace all original survey names with updated ones in segment_2 column
  mutate(segment_2 = str_replace_all(segment, "subscription_checkout_abandon_desktop","subscription_checkout_abandon")) %>%
  mutate(segment_2 = str_replace_all(segment_2, "subscription_checkout_abandon_mobile","subscription_checkout_abandon")) %>%
  mutate(segment_2 = str_replace_all(segment_2, "section_front_article_non_sub_desktop","section_front_article_non_sub")) %>%
  mutate(segment_2 = str_replace_all(segment_2, "section_front_article_non_sub_mobile","section_front_article_non_sub")) %>%
  mutate(segment_2 = str_replace_all(segment_2, "my_account_desktop","my_account")) %>%
  mutate(segment_2 = str_replace_all(segment_2, "my_account_mobile","my_account")) %>%
  mutate(segment_2 = str_replace_all(segment_2, "homepage_exit_desktop","homepage_exit")) %>%
  mutate(segment_2 = str_replace_all(segment_2, "homepage_exit_mobile","homepage_exit"))


# tokenization
word_tokens <- vwo_feb2019 %>%
  unnest_tokens(word, input = Question)


# remove stop words
word_tokens_no_stop <- word_tokens %>% anti_join(get_stopwords())


# final word counts
word_counts <- word_tokens_no_stop %>% count(row_id, word, segment, segment_2, device, sort = T)


# remove words less than 2 characters
word_counts <- 
  word_counts %>%
  mutate(word_length = (nchar(word_counts$word))) %>%     # add a word_length column with word character counts
  filter(word_length > 1)                                 # filter on worth_length column


# remove word_length column, because the ngram plots will select by that column rather than the n column
word_counts <- select(word_counts, -c(word_length))



# INITIAL WORD COUNT PLOT ####

word_counts %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(segment) %>%    
  top_n(10) %>%
  ungroup %>%
  
  ggplot(aes(word, n)) +
  theme_light() +
  geom_col(aes(fill = segment), show.legend = F) +
  facet_wrap(~segment, ncol = 3, scales = "free") +                   # separate by segment
  labs(y = "# of times word appeared", x = "word") +
  coord_flip() +
  ggtitle("Top 10 Words by Survey, All Surveys") +
  theme(plot.title = element_text(hjust = 0.5,                        # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)))        
  


# N GRAMS ####

## bigram tokenization
bigrams <- vwo_feb2019 %>%
  unnest_tokens(ngram, token = "ngrams", n = 2,
                input = Question)

# get bigram counts
bigram_counts <- bigrams %>% count(ngram, segment, sort = T)
                                    # remove mentions of "yes no"
  
# remove bigram_length column, because the bigram plot will select by that column rather than the n column
bigram_counts <- select(bigram_counts, -c(bigram_length))


## trigram tokenization
trigrams <- vwo_feb2019 %>%
  unnest_tokens(ngram, token = "ngrams", n = 3,
                input = Question)

#get trigram counts, removing results that are less than 5 characters
trigram_counts <- trigrams %>% count(ngram, segment, sort = T)
  
# clean trigram_counts
trigram_counts <- trigram_counts %>%
  mutate(trigram_length = (nchar(trigram_counts$ngram))) %>%          # add a word_length column with word character counts
  filter(trigram_length > 5)                                          # filter on word_length column
#  filter(ngram != "no no no") %>%                                     # remove mentions of "no no no"
#  filter(ngram != "yes yes yes") %>%                                  # remove mentions of "yes yes yes"
#  filter(ngram != "no no yes") %>%                                    # remove mentions of "no no yes"
#  filter(ngram != "no yes yes") %>%                                   # remove mentions of "no yes yes"
#  filter(ngram != "na no no") %>%                                     # remove mentions of "na no no"
#  filter(ngram != "x no no")                                          # remove mentions of "x no no"
  
# remove trigram_length column, because the bigram plot will select by that column rather than the n column
trigram_counts <- select(trigram_counts, -c(trigram_length))




# N GRAM PLOTTING ####

# plot top 10 bigrams for each segment
bigram_counts %>%
  filter(ngram != "NA") %>%
  arrange(desc(n)) %>%
  mutate(ngram = factor(ngram, levels = rev(unique(ngram)))) %>%
  group_by(segment) %>%
  top_n(10) %>%
  ungroup %>%
  
  ggplot(aes(ngram, n)) + geom_col(aes(fill = segment), show.legend = F) +    
  theme_light() +
  facet_wrap(~ segment, ncol = 3, scales = "free") +                              # separate by segment
  labs(y = "# of times phrase appeared", x = "phrase") +
  coord_flip() +
  ggtitle("Top 10 Bi-grams by Survey") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))                    # title formatting (center, bold)
  


# plot top 10 trigrams for each segment
trigram_counts %>%
  filter(ngram != "NA") %>%
  arrange(desc(n)) %>%
  mutate(ngram = factor(ngram, levels = rev(unique(ngram)))) %>%
  group_by(segment) %>%
  top_n(10) %>%
  ungroup %>%
  
  ggplot(aes(ngram, n)) + geom_col(aes(fill = segment), show.legend = F) +
  theme_light() +
  facet_wrap(~ segment, ncol = 3, scales = "free") +                          # separate by segment
  labs(y = "# of times phrase appeared", x = "phrase") +
  coord_flip() +
  ggtitle("Top 10 Tri-grams by Survey") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))                # title formatting (center, bold)



# BIGRAM NETWORKING ####

# separate the bigrams into individual words
# use the name of the bigram column with separate (in this case, 'ngram')
# make sure separation is on a space (in sep=)
bigrams_separated <- bigram_counts %>%
  separate(ngram, c("word1","word2"), sep=" ")


# remove stop words
data('stop_words')

custom_stop_words <- stop_words

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word) %>%
  drop_na()   # drop remaining N/A values

# new bigram counts with separated words
bigram_counts_separate <- bigrams_filtered %>%
  count(word1, word2, sort=TRUE)


# filter for only relatively common combinations
brigram_graph <- bigram_counts_separate %>%
  filter(n > 3) %>%
  graph_from_data_frame()

# graph the network
set.seed(2017)

# add directionality arrows
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# plot
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,              # edge_alpha makes links transparent based on how common or rare the bigram is
                 arrow = a, end_cap = circle(.05, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +                       # adjusts node size / color
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# TF-IDF ####

# df with tf-idf calcs; documents are the different surveys
tf_idf <- word_counts %>%
  mutate(word_length = (nchar(word_counts$word))) %>%
  filter(word_length > 2) %>%                             # remove words less than 2 characters long
  select(word, segment, n) %>%
  group_by(word, segment) %>% mutate(n = sum(n)) %>%      # sum word counts by survey
  ungroup() %>%
  bind_tf_idf(word, segment, n)


# tf-idf plot
tf_idf %>%
  filter(n > 1) %>%           # words appearing at least twice
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(segment) %>%
  top_n(10) %>%
  ungroup %>%
  
  ggplot(aes(word, tf_idf, fill = segment)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~segment, ncol = 3, scales = "free") +
  coord_flip() +
  ggtitle("tf-idf (uniquely important words appearing at least twice by survey)")



# SENTIMENT ANALYSIS ##### 

### most common positive and negetive words

# join word_counts to sentiment lexicons (afinn & bing)
afinn_word_counts <- word_counts %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, score, sort=TRUE) %>%
  ungroup()

bing_word_counts <- word_tokens_no_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

# join word_counts & include segment
bing_word_counts_segments <- word_tokens_no_stop %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, segment, sort=TRUE) %>%
  ungroup()



## spot check results for misidentified words
# adjust the assigned sentiment for misidentifed words with `replace`
# filter out words that should be neutral
bing_word_counts <-
  bing_word_counts %>%
  mutate(sentiment=replace(sentiment, word=='afford', 'negative')) %>%
  mutate(sentiment=replace(sentiment, word=='worth', 'negative')) %>%
  mutate(sentiment=replace(sentiment, word=='better', 'negative')) %>%
  mutate(sentiment=replace(sentiment, word=='free', 'negative')) %>%
  filter(word != 'trump') %>%
  filter(word != 'enough') %>%
  filter(word != 'work') %>%
  filter(word != 'limited')

# do the same for df with segments included
bing_word_counts_segments <-
  bing_word_counts_segments %>%
  mutate(sentiment=replace(sentiment, word=='afford', 'negative')) %>%
  mutate(sentiment=replace(sentiment, word=='worth', 'negative')) %>%
  mutate(sentiment=replace(sentiment, word=='better', 'negative')) %>%
  mutate(sentiment=replace(sentiment, word=='free', 'negative')) %>%
  filter(word != 'trump') %>%
  filter(word != 'enough') %>%
  filter(word != 'work') %>%
  filter(word != 'limited')
  

### PLOTS
# plot for common positive and negative words, overall
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(word, n, fill = sentiment)) +
  theme_light() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() +
  ggtitle("Top 20 Words by Sentiment, All Surveys") +
  theme(plot.title = element_text(hjust = 0.5,                        # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))


# plot for positive words by segment
bing_word_counts_segments %>%
  filter(sentiment == "positive") %>%
  group_by(segment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(word, n)) +
  theme_light() +
  geom_col(show.legend = FALSE, fill = "#00BFC4") +
  facet_wrap(~segment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() +
  ggtitle("Top 5 Positive Words by Segment") +
  theme(plot.title = element_text(hjust = 0.5,                        # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) 

# plot for negative words by segment
bing_word_counts_segments %>%
  filter(sentiment == "negative") %>%
  group_by(segment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
 
  ggplot(aes(word, n)) +
  theme_light() +
  geom_col(show.legend = FALSE, fill = "#F8776D") +
  facet_wrap(~segment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() +
  ggtitle("Top 5 Negative Words by Segment") +
  theme(plot.title = element_text(hjust = 0.5,                        # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))



### polarity by segment
segment_polarity_bing <- bing_word_counts_segments %>%
  group_by(segment, sentiment) %>%
  count(segment, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(polarity = positive - negative,
         ratio = polarity / (positive + negative)) # use polarity ratio in next graph

word_counts %>%
  left_join(segment_polarity_bing) %>%
  filter(word != " ") %>% # account for bad data
  mutate(sentiment = ifelse(positive > negative,
                            "positive", "negative")) %>%
  ggplot(aes(segment, polarity, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_light() + theme(legend.position = "none") +
  xlab(NULL) +
  ggtitle("Sentiment by Segment") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        axis.title.x = element_text(face="bold")) +
  coord_flip()




### Testing positive words

# words occurring *after* 'free' or 'like' or 'love' or 'better'
bigrams_separated %>%
  filter(word2 != 'i') %>%
  filter(word2 != 'free') %>%
  filter(word1 == 'free' | word1 == 'like' | word1 == 'love') %>%
  top_n(30) %>%
  
  ggplot(aes(x = reorder(word2, n), y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ word1, scales = "free") +
#  xlab('Top words occurring after') +
  ylab('count') +
  coord_flip()


# words occurring *before* 'free' or 'like' or 'love
bigrams_separated %>%
  filter(word2 == 'free' | word2 == 'like' | word2 == 'love') %>%
#  filter(word1 != 'free') %>%
  top_n(30) %>%
  
  ggplot(aes(x = reorder(word1, n), y = n)) +   # reorder to sort by n 
  geom_bar(stat = "identity") +
  facet_wrap(~ word2, scales = "free") +
  xlab('Top words occurring before') +
  ylab('count') +
  coord_flip()



### Testing other words

# words occurring *after* 'free' or 'like' or 'love' or 'better'
bigrams_separated %>%
  filter(word1 == 'enough' | word1 == 'work' | word1 == 'afford' | word1 == 'worth') %>%
  top_n(24) %>%
  
  ggplot(aes(x = reorder(word2, n), y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ word1, ncol = 2, scales = "free") +
  xlab('Top words occurring after') +
  ylab('count') +
  coord_flip()


# words occurring *before* 'free' or 'like' or 'love
bigrams_separated %>%
  filter(word2 == 'enough' | word2 == 'work' | word2 == 'afford' | word2 == 'worth') %>%
  top_n(30) %>%
  
  ggplot(aes(x = reorder(word1, n), y = n)) +   # reorder to sort by n 
  geom_bar(stat = "identity") +
  facet_wrap(~ word2, ncol = 2, scales = "free") +
  xlab('Top words occurring before') +
  ylab('count') +
  coord_flip()


# comparison cloud for negative and positive words
bing_word_counts %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8776D", "#00BFC4"),
                   max.words = 125,
                   rot.per = 0,
                   title.size = 2,
                   use.r.layout=FALSE)

#--------ENTIRE RESPONES--------####




# need to do another tokenization with the row_ids included

# tokenization
word_tokens_ids <- vwo_feb2019_ids %>%
  unnest_tokens(word, input = Question)

# remove stop words
word_tokens_no_stop_ids <- word_tokens_ids %>% anti_join(get_stopwords())


# make df of sentiment lexicon with -5 to +5 sentiment scores
afinn_sm <- get_sentiments("afinn")


# based on the sentiment misidentification above, reassign some scores in the afinn_sm data set
afinn_sm <- afinn_sm %>%
  mutate(score=replace(score, word=="worth", "-2")) %>%
  mutate(score=replace(score, word=="better", "-1")) %>%
  mutate(score=replace(score, word=="free", "-1")) %>%
  mutate(score = as.numeric(score))


# join sentiment scored to tokes
word_tokens_no_stop_sm <- word_tokens_no_stop_ids %>% left_join(afinn_sm)


## check that all words are accounted for in the join
# check NA count in the score column (word token not in afinn lexicon)
word_tokens_no_stop_sm %>% filter(is.na(score)) %>% select(word) %>% unique() %>% count()
## out: 4052


# check non-NAS
word_tokens_no_stop_sm %>% filter(!is.na(score)) %>% select(word) %>% unique() %>% count()
## out: 405


# total in word_tokens_no_stop
word_tokens_no_stop_sm %>% select(word) %>% unique() %>% count()
## out: 4457


# make df of average adjusted sentiment score per row ID
response_sm_score <- 
  word_tokens_no_stop_sm %>% 
  filter(!is.na(score)) %>% 
  group_by(row_id) %>%
  summarise(avg_sm_score = mean(score))

# join avg. sentiment score to original responses, drop NA, filter out 'yes' & ' no' answers with char count
vwo_feb2019_sm <- left_join(vwo_feb2019, response_sm_score, by='row_id') %>%
  mutate(word_length = (nchar(vwo_feb2019$Question))) %>%
  filter(word_length > 3) %>%
  drop_na()

# remove char length chart
vwo_feb2019_sm <- select(vwo_feb2019_sm, -c(word_length))


# plot
vwo_feb2019_sm %>%
  ggplot(aes(segment, avg_sm_score)) +
  geom_bar(stat = "identity") +
  theme_minimal() + 
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Avg. Sentiment Score of Entire Responses") +
  ggtitle("Response Sentiment Score by Segment") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        axis.title.x = element_text(face="bold")) +
  geom_hline(yintercept = 0, color = "black") +
  coord_flip()




#
### sentiment of entire response


# need to do another tokenization with the row_ids included

# tokenization
word_tokens_ids <- vwo_feb2019_ids %>%
  unnest_tokens(word, input = Question)

# remove stop words
word_tokens_no_stop_ids <- word_tokens_ids %>% anti_join(get_stopwords())


# make df of sentiment lexicon with -5 to +5 sentiment scores
afinn_sm <- get_sentiments("afinn")


# based on the sentiment misidentification above, reassign some scores in the afinn_sm data set
afinn_sm <- afinn_sm %>%
  mutate(score=replace(score, word=="worth", "-2")) %>%
  mutate(score=replace(score, word=="better", "-1")) %>%
  mutate(score=replace(score, word=="free", "-1")) %>%
  mutate(score = as.numeric(score))


# join sentiment scored to tokes
word_tokens_no_stop_sm <- word_tokens_no_stop %>% left_join(afinn_sm)


## check that all words are accounted for in the join
# check NA count in the score column (word token not in afinn lexicon)
word_tokens_no_stop_sm %>% filter(is.na(score)) %>% select(word) %>% unique() %>% count()
## out: 4052


# check non-NAS
word_tokens_no_stop_sm %>% filter(!is.na(score)) %>% select(word) %>% unique() %>% count()
## out: 405


# total in word_tokens_no_stop
word_tokens_no_stop_sm %>% select(word) %>% unique() %>% count()
## out: 4457






# make df of average adjusted sentiment score per row ID
response_sm_score <- 
  word_tokens_no_stop_sm %>% 
  filter(!is.na(score)) %>% 
  group_by(row_id) %>%
  summarise(avg_sm_score = mean(score))

# join avg. sentiment score to original responses, drop NA & one-word answers
vwo_feb2019_sm <- left_join(vwo_feb2019, response_sm_score, by="row_id") %>%
  filter(word_count > 1) %>%
  drop_na()


# plot
vwo_feb2019_sm %>%
  ggplot(aes(segment, avg_sm_score)) +
  geom_bar(stat = "identity") +
  theme_minimal() + 
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Avg. Sentiment Score of Entire Responses") +
  ggtitle("Response Sentiment Score by Survey") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        axis.title.x = element_text(face="bold")) +
  geom_hline(yintercept = 0, color = "black") +
  coord_flip()



vwo_feb2019_sm %>%
  ggplot(aes(x = segment_2, y = avg_sm_score, fill = device)) +
  geom_bar(position= "dodge", stat = "identity", alpha = 0.3) +
  theme_light() +
  labs(x = NULL) +
  scale_y_continuous(name="Avg. Sentiment Score of Entire Responses", limits=c(-5, 5)) +
  ggtitle("Response Sentiment Score by Survey & Device") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  scale_fill_manual(values=c("lightgoldenrod1","plum3", "dodgerblue3")) +
  geom_hline(yintercept = 0, color = "grey") +
  coord_flip()



vwo_feb2019_sm %>%
  ggplot(aes(x = segment_2, y = avg_sm_score)) +
  geom_boxplot(aes(fill = device), alpha = 0.5, outlier.alpha = 0.1) +
  theme_light() +
  ggtitle("Word Count Distribution by Survey") +
  labs(x = "survey name",
       y= "word count") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  scale_y_continuous(name="Avg. Sentiment Score of Entire Responses", limits=c(-5, 5)) +
  scale_fill_manual(values=c("lightgoldenrod1","plum3", "dodgerblue3")) +
  coord_flip() 


vwo_feb2019_sm %>%
  group_by(segment_2, device) %>% 
  summarize(m = mean(avg_sm_score)) %>%
  ungroup() %>%
  
  ggplot(aes(x = segment_2, y = m, fill = device)) +
  geom_col(position= "dodge", stat = "identity", alpha = 0.7) +
  theme_light() +
  ggtitle("Avg. Sentiment Score of All Entire Responses by Survey & Device") +
  labs (x = NULL,
        y = "Overall Avg. Sentiment Score") +
  scale_y_continuous(name="Avg. Sentiment Score of Entire Responses", limits=c(-5, 5)) +
  scale_fill_manual(values=c("lightgoldenrod1","plum3", "dodgerblue3")) +
  geom_hline(yintercept = 0, color = "grey48") +
  coord_flip()


