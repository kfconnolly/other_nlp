#install.packages("tidytext")
#install.packages("bigrquery")
#install.packages("tidyverse")
#install.packages("dplyr")

library(dplyr)
library(bigrquery)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(reshape2)
library(formattable)



####-----SELECT & TOKENIZE DATA-----####



#---select the data from GBQ---#

# put GBQ project ID here
project <- "ga360-173318" 


# SQL query to select dat from GBQ
sql <- "SELECT row_id, Question, question_2, segment FROM `ga360-173318.surveymonkey.all_vwo_surveys_feb2019`"


# execute SQL query & save results in vwo_deb2019 df
vwo_feb2019 <- query_exec(sql, project = project, use_legacy_sql = F)



#---some cleaning & categorization---#

# device column will distinguish desktop & mobile
# segment_2 will list survey names without 'desktop' & 'mobile' appended

vwo_feb2019_clean <- vwo_feb2019 %>%
  # add word_count column
  mutate(word_count = str_count(vwo_feb2019$Question, "\\w+")) %>%
  
  # identify the device for each observation in device
  mutate(device = str_replace_all(segment, ".*desktop$","desktop")) %>%
  mutate(device = str_replace_all(device, ".*mobile$","mobile")) %>%
  mutate(device = str_replace_all(device, ".*subscription*","mobile & desktop")) %>%
 
   # replace all original survey names with updated ones in segment_2 column
  mutate(segment_name = str_replace_all(segment, "subscription_checkout_abandon_desktop","subscription_checkout_abandon")) %>%
  mutate(segment_name = str_replace_all(segment_name, "subscription_checkout_abandon_mobile","subscription_checkout_abandon")) %>%
  mutate(segment_name = str_replace_all(segment_name, "section_front_article_non_sub_desktop","section_front_article_non_sub")) %>%
  mutate(segment_name = str_replace_all(segment_name, "section_front_article_non_sub_mobile","section_front_article_non_sub")) %>%
  mutate(segment_name = str_replace_all(segment_name, "my_account_desktop","my_account")) %>%
  mutate(segment_name = str_replace_all(segment_name, "my_account_mobile","my_account")) %>%
  mutate(segment_name = str_replace_all(segment_name, "homepage_exit_desktop","homepage_exit")) %>%
  mutate(segment_name = str_replace_all(segment_name, "homepage_exit_mobile","homepage_exit"))



#---tokenization---#

# get word tokens from responses in 'Question' column
word_tokens <- vwo_feb2019_clean %>%
  unnest_tokens(word, input = Question)


# remove stop words
word_tokens_no_stop <- word_tokens %>% anti_join(get_stopwords())


# final word counts
word_counts <- word_tokens_no_stop %>% count(word, segment, segment_name, device, sort = T)


# remove single-character responses from word_counts
word_counts <- 
  word_counts %>%
  mutate(word_length = (nchar(word_counts$word))) %>%     # add a word_length column with word character counts
  filter(word_length > 1)                                 # filter to remove words less than 2 characters


# remove word_length column, because the ngram plots will select by that column rather than the n column
word_counts <- select(word_counts, -c(word_length))



#---initial word count plot---#

# top 10 words by survey 
top_terms <- word_counts %>%
  group_by(segment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  arrange(segment, -n)

top_terms %>%
  mutate(word = reorder(word, n)) %>%
  group_by(segment, word) %>%    
  arrange(desc(n)) %>% 
  top_n(10) %>% 
  ungroup %>%
  mutate(word = factor(paste(word, segment, sep = "__"), 
                       levels = rev(paste(word, segment, sep = "__")))) %>%
  
  ggplot(aes(word, n)) +
  theme_light() +
  geom_col(aes(fill = as.factor(segment)), show.legend = F) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(x = "word",
       y = "number of times word appeared") +
  coord_flip() +
  facet_wrap(~segment, ncol = 3, scales = "free") +      
  ggtitle("Top 10 Words by Survey, All Surveys") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))   # x axis formatting (center, padding)



####-----NGRAMS-----####



#---cleaning---#

# looking at responses that are one word versus multiple words


vwo_feb2019_multiple_words <- vwo_feb2019_clean %>%                         # make a copy df of the original data
  filter(word_count > 1)                                                    # remove all rows with only 1 word


# total survey responses overall
vwo_feb2019_clean %>% select(Question) %>% count()
## out: 6662


# total survey responses with more than one word
vwo_feb2019_multiple_words %>% select(Question) %>% count()
## out: 4471



#---tokenization---#

# to remove stop words, re-aggregate responses by row id
no_stop_resp_frame <- word_tokens_no_stop %>% 
  group_by(row_id) %>% 
  mutate(no_stop_resps = paste(word, collapse = " ")) %>% 
  select(-word) %>%
  unique() %>% 
  ungroup()


# bigram tokenization
bigrams <- no_stop_resp_frame %>%                            
  unnest_tokens(ngram, token = "ngrams", n = 2,
                input = no_stop_resps)


# get bigram counts
bigram_counts <- bigrams %>% count(ngram, segment, sort = T)


# trigram tokenization
trigrams <- vwo_feb2019_multiple_words %>%                            # use data with multiple word responses
  unnest_tokens(ngram, token = "ngrams", n = 3,
                input = Question)


# get trigram counts
trigram_counts <- trigrams %>% count(ngram, segment, sort = T)



#---plots---#

# plot 1: top 10 bigrams for each survey
bigram_counts %>%
  filter(ngram != "NA") %>%
  arrange(desc(n)) %>%
  mutate(ngram = factor(ngram, levels = rev(unique(ngram)))) %>%
  group_by(segment) %>%
  top_n(10) %>%
  ungroup %>%
  
  ggplot(aes(ngram, n)) + geom_col(aes(fill = segment), show.legend = F) +    
  theme_light() +
  facet_wrap(~ segment, ncol = 3, scales = "free") +                                   # separate by survey, set to 3 columns & scales for each survey
  labs(x = "phrase",
       y = "# of times phrase appeared") +
  ggtitle("Top 10 Bi-grams by Survey") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +                       # title formatting (center, bold)
  theme(plot.title = element_text(hjust = 0.5,                                         # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +   # x axis formatting (center, padding)
  coord_flip()


# plot 2: top 10 trigrams for each survey
trigram_counts %>%
  filter(ngram != "NA") %>%
  arrange(desc(n)) %>%
  mutate(ngram = factor(ngram, levels = rev(unique(ngram)))) %>%
  group_by(segment) %>%
  top_n(10) %>%
  ungroup %>%
  
  ggplot(aes(ngram, n)) + geom_col(aes(fill = segment), show.legend = F) +
  theme_light() +
  facet_wrap(~ segment, ncol = 3, scales = "free") +                                  # separate by survey, set to 3 columns & scales for each survey
  labs(x = "phrase",
       y = "# of times phrase appeared") +
  ggtitle("Top 10 Tri-grams by Survey") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +                      # title formatting (center, bold)
  theme(plot.title = element_text(hjust = 0.5,                                        # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))+   # x axis formatting (center, padding)
  coord_flip()




####-----TF-IDF-----####



# df with tf-idf calcs; documents are the different surveys
tf_idf <- word_counts %>%
  mutate(word_length = (nchar(word_counts$word))) %>%
  filter(word_length > 2) %>%                                         # remove words less than 2 characters long
  select(word, segment, n) %>%
  group_by(word, segment) %>% mutate(n = sum(n)) %>%                  # sum word counts by survey
  ungroup() %>%
  bind_tf_idf(word, segment, n)


# plot tf-idf by survey
tf_idf %>%
  filter(n > 1) %>%                                                   # only select words appearing at least twice
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(segment) %>%
  top_n(10) %>%
  ungroup %>%
  
  ggplot(aes(word, tf_idf, fill = segment)) +
  theme_light() +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~segment, ncol = 3, scales = "free") +
  ggtitle("tf-idf (uniquely important words appearing at least twice by survey)") +
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
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

# join word_counts, include segment & device (same as above, except this df will include segment & device)
bing_word_counts_segments <- word_tokens_no_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, segment, device, sort=TRUE) %>%
  ungroup()


# plot 1: initial plot for most common positive & negative words
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(word, n, fill = sentiment)) +
  theme_light() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "contribution to sentiment",
       x = NULL) +
  ggtitle("Top 20 Words by Sentiment, All Surveys") +
  theme(plot.title = element_text(hjust = 0.5,                                         # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +   # axis formatting (center, padding)
  coord_flip()
  




#---testing & understanding potentially misidentified words---#


# separate the bigrams into individual words (word1 & word2)
# use the name of the bigram column with separate function (in this case, 'ngram')
# make sure separation action is done on the space between the words (in sep=)
bigrams_separated <- bigram_counts %>%
  separate(ngram, c("word1","word2"), sep=" ")


##  Test 1

# plot 2: words occurring *AFTER* 'free' or 'like' or 'love' or 'better'
bigrams_separated %>%
  filter(word2 != "i") %>%
  filter(word2 != "free") %>%
  filter(word1 == "free" | word1 == "like" | word1 == "love" | word1 == "better") %>%
  group_by(word1) %>%
  top_n(10) %>%
  ungroup() %>%
  
  ggplot(aes(x = reorder(word2, n), y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ word1, scales = "free") +
  labs(x = "Top words occurring after",
       y = "count") +
  coord_flip()


# plot 3: words occurring *BEFORE* 'free' or 'like' or 'love' or 'better'
bigrams_separated %>%
  filter(word2 == "free" | word2 == "like" | word2 == "love" | word2 =="better") %>%
  group_by(word2) %>%
  top_n(5) %>%
  ungroup() %>%
  
  ggplot(aes(x = reorder(word1, n), y = n)) +   # reorder to sort by n 
  geom_bar(stat = "identity") +
  facet_wrap(~ word2, scales = "free") +
  labs(x = "Top words occurring before",
       y = "count") +
  coord_flip()


## Test 2

# plot 4: words occurring *AFTER* 'enough' or 'work' or 'afford' or 'worth'
bigrams_separated %>%
  filter(word1 == "enough" | word1 == "work" | word1 == "afford" | word1 == "worth" | word1 == "limited") %>%
  top_n(24) %>%
  
  ggplot(aes(x = reorder(word2, n), y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ word1, ncol = 2, scales = "free") +
  labs(x = "Top words occurring after",
       y = "count") +
  coord_flip()


# plot 5: words occurring *BEFORE* 'free' or 'like' or 'love
bigrams_separated %>%
  filter(word2 == "enough" | word2 == "work" | word2 == "afford" | word2 == "worth" | word2 == "limited") %>%
  top_n(30) %>%
  
  ggplot(aes(x = reorder(word1, n), y = n)) +   # reorder to sort by n 
  geom_bar(stat = "identity") +
  facet_wrap(~ word2, ncol = 2, scales = "free") +
  labs(x = "Top words occurring before",
       y = "count") +
  coord_flip()



#---based on exploration, change the sentiment of some words---#

# adjust the assigned sentiment for misidentifed words with mutate & replace
# filter out words that should be neutral
bing_word_counts <-
  bing_word_counts %>%
  mutate(sentiment=replace(sentiment, word=="afford", "negative")) %>%
  mutate(sentiment=replace(sentiment, word=="worth", "negative")) %>%
  mutate(sentiment=replace(sentiment, word=="better", "negative")) %>%
  mutate(sentiment=replace(sentiment, word=="free", "negative")) %>%
  filter(word != "trump") %>%
  filter(word != "enough") %>%
  filter(word != "work") %>%
  filter(word != "limited")


# do the same for df with segments included
bing_word_counts_segments <-
  bing_word_counts_segments %>%
  mutate(sentiment=replace(sentiment, word=="afford", "negative")) %>%
  mutate(sentiment=replace(sentiment, word=="worth", "negative")) %>%
  mutate(sentiment=replace(sentiment, word=="better", "negative")) %>%
  mutate(sentiment=replace(sentiment, word=="free", "negative")) %>%
  filter(word != "trump") %>%
  filter(word != "enough") %>%
  filter(word != "work") %>%
  filter(word != "limited")



#---general plots---#

# plot 6: updated for common positive and negative words, overall
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(word, n, fill = sentiment)) +
  theme_light() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "number of times word appeared",
       x = NULL) +
  ggtitle("Top 20 Words by Sentiment, All Surveys") +
  theme(plot.title = element_text(hjust = 0.5,                                          # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +    # axis formatting (center, padding)
  coord_flip()
  

# plot 7: positive words by survey
top_negative_words <- bing_word_counts_segments %>% 
  filter(sentiment == "positive") %>%                                                  # filter on positive words only
  filter(!is.na(word)) %>%                                                             # remove any NA values
  group_by(segment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%                                                  # used for bar sorting     
  arrange(segment, -n)                                                                 # used for bar sorting     

top_negative_words %>%
  mutate(word = reorder(word, n)) %>%                                                  # used for bar sorting     
  group_by(segment, word) %>%    
  arrange(desc(n)) %>%                                                                 # used for bar sorting     
  top_n(10) %>% 
  ungroup %>%
  mutate(word = factor(paste(word, segment, sep = "__"),                               # used for bar sorting     
                       levels = rev(paste(word, segment, sep = "__")))) %>%
  
  ggplot(aes(word, n)) +
  theme_light() +
  geom_col(aes(fill = as.factor(segment)), show.legend = FALSE, fill = "#00BFC4") +
  facet_wrap(~segment, ncol = 3, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                        # used for bar sorting     
  labs(y = "count",
       x = "word") +
  ggtitle("Top 5 Psotive Words by Survey") +
  theme(plot.title = element_text(hjust = 0.5,                                         # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +   # axis formatting (center, padding) 
  coord_flip() 
  

# plot 8: negative words by survey
top_negative_words <- bing_word_counts_segments %>% 
  filter(sentiment == "negative") %>%                                                  # filter on negative words only
  filter(!is.na(word)) %>%                                                             # remove any NA values
  filter(n > 1) %>%                                                                    # only look at words occurring more than once; too many ties at n=1, unreadable
  group_by(segment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%                                                  # used for bar sorting     
  arrange(segment, -n)                                                                 # used for bar sorting     

top_negative_words %>%
  mutate(word = reorder(word, n)) %>%                                                  # used for bar sorting     
  group_by(segment, word) %>%    
  arrange(desc(n)) %>%                                                                 # used for bar sorting     
  top_n(10) %>% 
  ungroup %>%
  mutate(word = factor(paste(word, segment, sep = "__"),                               # used for bar sorting     
                       levels = rev(paste(word, segment, sep = "__")))) %>%
  
  ggplot(aes(word, n)) +
  theme_light() +
  geom_col(aes(fill = as.factor(segment)), show.legend = FALSE, fill = "#F8776D") +
  facet_wrap(~segment, ncol = 3, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                        # used for bar sorting     
  labs(y = "count",
       x = "word") +
  ggtitle("Top Negative Words by Survey") +
  theme(plot.title = element_text(hjust = 0.5,                                         # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +   # axis formatting (center, padding) 
  coord_flip() 


# plot 9: comparison cloud for negative and positive words
bing_word_counts %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8776D", "#00BFC4"),
                   max.words = 125,
                   rot.per = 0,
                   title.size = 2,
                   use.r.layout=FALSE) 



#---polarity by segment---#

# calculate polarity score by survey
segment_polarity_bing <- bing_word_counts_segments %>%
  group_by(segment, sentiment) %>%
  count(segment, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(polarity = positive - negative,
         ratio = polarity / (positive + negative))               # polarity ratio for use in next plot


# plot 10: polarity of survey results
word_counts %>%
  left_join(segment_polarity_bing) %>%                           # join word_counts to the pdf with polarity ration      
  filter(word != " ") %>%                                        # account for any missing data
  mutate(sentiment = ifelse(positive > negative,
                            "positive", "negative")) %>%         # join & mutate to create sentiment field for fill
  ggplot(aes(segment, polarity, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_light() + 
  theme(legend.position = "none") +
  xlab(NULL) +
  ggtitle("Sentiment Polarity by Survey") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +   # title formatting (bold, center)
  coord_flip()



####-----SENTIMENT ANALYSIS RESPONSES (AFINN)-----####



#---get afinn scores---#

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
vwo_feb2019_sm <- left_join(vwo_feb2019, response_sm_score, by="row_id") %>%
  filter(word_count > 1) %>%
  drop_na()



#---plots---#

# plot 1: boxplot viz of avgerage sentiment distribution by survey & device
vwo_feb2019_sm %>%
  ggplot(aes(segment_name, avg_sm_score)) +
  geom_boxplot(aes(fill = device), alpha = 0.5, outlier.alpha = 0.1) +                             # break out by device, make outliers transparent
  theme_light() +
  ggtitle("Distribution of Full Response Avg. Sentiment Scores") +
  labs(x = "survey name",
       y = "word count") +
  theme(plot.title = element_text(hjust = 0.5,                                                     # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +               # x axis formatting (center, padding)
  scale_y_continuous(name="avg. sentiment score (entire responses)", limits=c(-5, 5)) +            # adjust y axis to be from -5 to 5
  scale_fill_manual(values=c("dodgerblue3","gold", "plum3")) +                                     # customize colors for device category
  coord_flip()



# plot 2: more user-friendly viz of overall, full response avgerage sentiment by survey & device
vwo_feb2019_sm %>%
  group_by(segment_name, device) %>% 
  summarize(m = mean(avg_sm_score)) %>%
  ungroup() %>%
  
  ggplot(aes(segment_name, m, fill = device)) +                                                    # fill breaks out by device
  geom_col(position= "dodge", alpha = 0.8) +                                                       # dodge breaks the devices out into separate bars
  theme_light() +
  ggtitle("Avg. Sentiment Score of Full Responses by Survey & Device") +
  labs (x = NULL,
        y = "Overall Avg. Sentiment Score") +
  theme(plot.title = element_text(hjust = 0.5,                                                     # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +                # x axis formatting (center, padding)
  scale_y_continuous(name="overall avg. sentiment score (entire responses)", limits=c(-5, 5)) +     # adjust y axis to be from -5 to 5
  scale_fill_manual(values=c("dodgerblue3","gold", "plum3")) +                                      # customize colors for device category
  geom_hline(yintercept = 0, color = "grey48") +                                                    # add vertical, grey line at 0 
  coord_flip()

  
# plot 3: same as above, but by survey overall (not broken out by device)
vwo_feb2019_sm %>%
  group_by(segment_name, device) %>% 
  summarize(m = mean(avg_sm_score)) %>%
  ungroup() %>%
  
  ggplot(aes(segment_name, m)) + 
  geom_col(fill = "gray67") +
  theme_light() +
  ggtitle("Avg. Sentiment Score of Entire Responses by Survey") +
  labs (x = NULL,
        y = "Overall Avg. Sentiment Score") +
  theme(plot.title = element_text(hjust = 0.5,                                                      # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +                # x axis formatting (center, padding)
  scale_y_continuous(name="overall avg. sentiment score (entire responses)", limits=c(-5, 5)) +     # adjust y axis to be from -5 to 5
  geom_hline(yintercept = 0, color = "black") +                                                     # add vertical, grey line at 0 
  coord_flip()


  
####-----MISC-----####

  

# plot 1: word distribution boxplot, by survey & by device (when applicable)
vwo_feb2019 %>%
  ggplot(aes(x = segment_name, y = word_count)) +
  geom_boxplot(aes(fill = device), alpha = 0.4, outlier.alpha = 0.1) +
  theme_light() +
  ggtitle("Word Count Distribution by Survey") +
  labs(x = "survey name",
      y= "word count") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  scale_fill_manual(values=c("dodgerblue3","gold", "plum3")) +                                     
  coord_flip() 


# table to go with this plot might be helpful
# fuction to collapse a specified column in a df; will be used in formattable 
collapse_rows_df <- function(df, variable){
  
  group_var <- enquo(variable)
  
  df %>%
    group_by(!! group_var) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    select(-c(groupRow))
}

# isolate data & plot
vwo_feb2019 %>%
  group_by(segment, device) %>%
  summarize(mean = mean(word_count),                                                            # calculate mean word count
            median = median(word_count)) %>%                                                    # calculate median word count
  mutate_at(vars(mean, median), funs(round(., 0))) %>%                                          # round the calcs to 0 decimal places 
  collapse_rows_df(segment) %>%                                                                 # collapse segment_name rows to minimize duplication of text
  
  # formattable to utilize heatmapping in the chart
  formattable(align =c("l","l","c","c"),                                                        # column text alignment  
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")), 
                   `mean`= color_tile("#FFEFDB", "#FF8000"),                                    # color range for column
                   `median`= color_tile("#FFEFDB", "#FF8000")))                                 # color range for column
 
  

#---other plots that are perhaps less helpful visually---#

# plot 2: assign distribution of response length overall plot to dist_plot
dist_plot <- vwo_feb2019_clean %>%
  ggplot(aes(x = word_count)) +
  geom_histogram(binwidth = 5, color = "black", fill = "gray87") +
  theme_light() +
  ggtitle("Word Count Distribution of All Survey Responses") +
  labs(x = "word count",
       y = "count of responses") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +            # title formatting (center, bold)
  geom_vline(aes(xintercept = mean(word_count),                           # add mean line
                 color = (blue = "Mean")),
             linetype = "dashed",
             size = 1,
             show.legend = TRUE) +
  geom_vline(aes(xintercept = median(word_count),                         # add median line
                 color = (red = "Median")),
             linetype = "dashed",
             size = 1,
             show.legend = TRUE)

# generate plot with customized legend
dist_plot +
  scale_color_manual(name = "", 
                     labels = c(Mean = "Mean", Median = "Median"),
                     values = c(Mean = "dodgerblue2", Median = "indianred3"))



## distribution of response length by survey

# plot 3: this is pretty impossible to decipher; corresponding frequency polygon in plot 5 is probably better
vwo_feb2019_clean %>%
  ggplot(aes(x = word_count, group = segment, fill = segment)) +
  geom_histogram(position = "dodge", binwidth = 5) +
  theme_light() +
  ggtitle("Word Count Distribution of All Survey Responses") +
  labs(x = "word count",
       y = "count of responses") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),              # title formatting (center, bold)
        legend.title = element_blank())                                   # leave legend title blank   
        

# plot 4: same as above, not grouped by survey but faceted instead
vwo_feb2019_clean %>%
  ggplot(aes(x = word_count)) +
  geom_histogram(binwidth = 5) +
  theme_light() +
  ggtitle("Word Count Distribution of All Survey Responses") +
  labs(x = "word count",
       y = "count of responses") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +            # title formatting (center, bold)
  facet_wrap(~ segment)


# plot 5: frequency polygon to compare distribution by segment 
vwo_feb2019 %>%
  mutate(word_count = str_count(vwo_feb2019$Question, "\\w+")) %>%        # add word_count column  
  
  ggplot(aes(word_count, color = segment)) +
  geom_freqpoly() +
  labs( x = "word count",
        y = "count of responses") + 
  ggtitle("Word Count Distribution by Survey") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),              # title formatting (center, bold)
        legend.title = element_blank(),                                   # leave legend title blank
        panel.grid.minor.y = element_blank())


# plot 6: same plot as above, but by density rather than count 
vwo_feb2019 %>%
  mutate(word_count = str_count(vwo_feb2019$Question, "\\w+")) %>%        # add word_count column  
  
  ggplot(aes(word_count, stat(density), color = segment)) +               # set to density instead of count
  geom_freqpoly() +
  labs( x = "word count",
        y = "density of responses") + 
  ggtitle("Word Count Distribution by Segment") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),              # title formatting (center, bold)
        legend.title = element_blank(),                                   # leave legend title blank
        panel.grid.minor.y = element_blank())