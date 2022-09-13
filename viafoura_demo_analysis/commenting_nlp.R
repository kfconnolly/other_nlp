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
library(ggridges)
library(tidyverse)



####-----SELECT & TOKENIZE DATA-----####



#---select the data from GBQ---#

# put GBQ project ID here
project <- "ga360-173318" 


# SQL query to select dat from GBQ
sql <- "SELECT ROW_NUMBER() OVER() AS row_id, DATE(TIMESTAMP_MILLIS(date_created)) AS date_created, user_name, page_url, content FROM `ga360-173318.viafoura.comment_create`"

# execute SQL query & save results in vwo_deb2019 df
commenting_june2019 <- query_exec(sql, project = project, max_pages = Inf, use_legacy_sql = F)


# remove "reply to @" from beginning of content
commenting_june2019_clean <- commenting_june2019 %>%
  mutate(reply_removed = str_replace(commenting_june2019$content, ".*:", ":"))





#---tokenization---#

# get word tokens from responses in 'Question' column
word_tokens <- commenting_june2019_clean %>%
  unnest_tokens(word, input = reply_removed)


# remove stop words
word_tokens_no_stop <- word_tokens %>% anti_join(get_stopwords())



# final word counts
word_counts_combined <- word_tokens_no_stop %>% count(word, sort = T)
word_counts_cat <- word_tokens_no_stop %>% count(word, date_created, user_name, page_url, sort = T)


word_counts_cat <- word_counts_cat %>%
  separate(page_url, into = paste("split", 1:2, sep = "/")) 

# adjust column names
colnames(word_counts_cat) <- c("word", "date_created", "user_name", "split_1", "split_2", "n")






#---initial word count plot---#

word_counts_combined %>%
  top_n(25, n) %>%
  ungroup() %>% 
  arrange(desc(n)) %>% 
  
  ggplot(aes(x = reorder(word, n), n, fill = "#98C5D0")) +                                             # reorder sorts the chart by n
  scale_fill_manual(values=c("#98C5D0")) +
  theme_new() +
  geom_col(show.legend = FALSE) +                                                    # fill used for sorting bars
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                      # used for sorting bars
  labs(x = "word",
       y = "number of times word appeared") +
  coord_flip() +
  ggtitle("Top Words") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0),
                                  size = 20),
        axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 5, l = 0)),  # x axis formatting (center, padding)
        axis.title.y = element_text(size = 10, margin = margin(t = 10, r = 20, b = 5, l = 20)),  # x axis formatting (center, padding)
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
  

# delineate page path
commenting_june2019_clean <- commenting_june2019_clean %>%
  separate(page_url, into = paste("split", 1:2, sep = "/")) 

# adjust column names
colnames(commenting_june2019_clean) <- c("row_id", "date_created", "user_name", "split_1", "split_2", "split_3", "content", "reply_removed")







# bigram tokenization
bigrams <- word_tokens_no_stop %>%
  unnest_tokens(ngram, token = "ngrams", n =2,
                input = word)


# get bigram counts
bigram_counts <- bigrams %>% count(ngram, sort = T)


# trigram tokenization 
# not using no_stop_resp_frame created above, because I want to include stop words
trigrams <- word_tokens_no_stop %>%                             
  unnest_tokens(ngram, token = "ngrams", n = 3,
                input = word)


# get trigram counts
trigram_counts <- trigrams %>% count(ngram, sort = T)




#----tokenization by url------#

word_tokens_no_stop_path <- word_tokens_no_stop %>%
  separate(page_url, into = paste("split", 1:2, sep = "/")) 

# adjust column names
colnames(word_tokens_no_stop_path) <- c("row_id", "date_created", "user_name", "split_1", "split_2", "content", "word")





# bigram tokenization
bigrams_path <- word_tokens_no_stop_path %>%
  unnest_tokens(ngram, token = "ngrams", n =2,
                input = word)


# get bigram counts
bigram_counts_path <- bigrams_path %>% count(ngram, split_2, sort = T)


# bigram sort plot
bi_top_terms <- bigram_counts_path %>%
  filter(!is.na(ngram)) %>% 
  filter(split_2 != cahungpath, zzz, hungtestpath, ellipsis, pb) %>%
  group_by(split_2) %>%
  top_n(10) %>%
  ungroup() %>%
  arrange(split_2, -n)

bi_top_terms %>%
  mutate(ngram = reorder(ngram, n)) %>%                                              # used for bar sorting
  group_by(split_2, ngram) %>%    
  arrange(desc(n)) %>%                                                               # used for bar sorting
  top_n(10) %>%                                                                      # select top 10 results
  ungroup %>%
  mutate(ngram = factor(paste(ngram, split_2, sep = "__"),                           # used for bar sorting
                        levels = rev(paste(ngram, split_2, sep = "__")))) %>%
  
  ggplot(aes(ngram, n)) +
  theme_light() +
  geom_col(aes(fill = as.factor(split_2)), show.legend = FALSE) +                    # fill used for bar sorting
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                      # used for bar sorting
  labs(x = "word",
       y = "number of times word appeared") +
  coord_flip() +
  facet_wrap(~split_2, scales = "free") +                                  # create individual graph for each survey          
  ggtitle("Top 10 Bigrams by Survey, All Surveys") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))   # x axis formatting (center, padding)



# top bigrams by section
bigram_counts_path %>%
  filter(!is.na(ngram)) %>% 
  group_by(split_2) %>%
  top_n(25) %>%
  ungroup() %>%
  mutate(split_2 = as.factor(split_2),
          ngram = reorder_within(ngram, n, split_2)) %>%
  filter(split_2 == "news" | split_2 == "sixers" | split_2 == "opinion") %>%

  ggplot(aes(ngram, n, fill = "#98C5D0")) +                                             # reorder sorts the chart by n
#  scale_fill_manual(values=c("#98C5D0")) +
  theme_new() +
  geom_col(show.legend = FALSE) +                                                    # fill used for sorting bars
  facet_wrap(~split_2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0.0)) +
  labs(x = "word",
       y = "number of times word appeared") +
  ggtitle("Top Bigrams by 'Section'") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0),
                                  size = 20),
        axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 5, l = 0)),  # x axis formatting (center, padding)
        axis.title.y = element_text(size = 10, margin = margin(t = 10, r = 10, b = 5, l = 10)),  # x axis formatting (center, padding)
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.background = element_rect(fill="grey"),
        strip.text = element_text(size = 14)) 







# generating new theme
theme_new <- function(base_size = 11,
                      base_family = "",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        face = "bold",
        hjust = 0),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.5)),
      panel.grid.major = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted"),   
      panel.grid.minor = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted", 
        size = rel(4)),   
      
      complete = TRUE
    )
}




####-------TF-IDF---------####


# make dfs with tf-idf calcs by detractor & promoter, where documents are the different sections

tf_idf <- word_counts_cat %>% 
#  filter(split2 == "news") %>%
  select(word, split_2, n) %>%
  group_by(word, split_2) %>%
  mutate(n = sum(n)) %>%
  ungroup() %>%
  bind_tf_idf(word, split_2, n)


# plots
tf_idf %>%
  filter(n > 10) %>%
  arrange(desc(tf_idf)) %>%
  mutate (word = factor(word, levels = rev(unique(word)))) %>%
  group_by(split_2) %>%
  top_n(10) %>%
  ungroup() %>%
  filter(split_2 == "news" | split_2 == "sixers" | split_2 == "opinion") %>%
  
  
  ggplot(aes(word, tf_idf, fill = split_2)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~split_2, ncol = 3, scales = "free") +
  coord_flip() +
  ggtitle("tf-idf (uniquely important words) by section")

