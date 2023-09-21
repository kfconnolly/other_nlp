#install.packages('tidyverse')
#install.packages('googlesheets4')
#install.packages('tidytext')


library(tidyverse)
library(googlesheets4)
library(tidytext)



######################################################
# LOAD & CLEAN DATA #
######################################################


# import data from google sheet
app_responses <- read_sheet('https://docs.google.com/spreadsheets/d/13LeXKuoNISAvbFQCtVp960U8jkwNfnYsHh8ZWGvk0E8/edit#gid=0',
                        sheet = 'original')

# standardize column names
names(app_responses) <- names(app_responses) %>%
  stringr::str_replace_all('\\s','_') %>% 
  tolower


# select only certain open text responses
# remove qa / test responses
app_responses <- app_responses %>%
  dplyr::filter(reason %in% c('App issue', 'Something else')) %>%
  dplyr::filter(!str_detect(email, '^tdang*')) %>%
  dplyr::filter(!str_detect(tolower(message), '^test.*'))

# add a row id to each response
app_responses <- app_responses %>% dplyr::mutate(row_id = row_number())





######################################################
# TOKENIZATION #
######################################################

#-----------word counts--------#
# get word tokens from responses in 'answer' column
word_tokens <- app_responses %>%
  unnest_tokens(word, input = message)

# remove stop words
word_tokens_no_stop <- word_tokens %>% anti_join(get_stopwords())


# final word counts
word_counts <- word_tokens_no_stop %>% count(word, sort = T)

# remove single-character responses from word_counts
word_counts <- word_counts %>%
  mutate(word_length = (nchar(word_counts$word))) %>%     # add a word_length column with word character counts
  filter(word_length > 1) %>%                             # filter to remove words less than 2 characters
  select(-c(word_length))                                 # remove word_length column, because the ngram plots will select by that column rather than the n column

# word counts with 'reason' category included
word_counts_reason <- word_tokens_no_stop %>% count(word, reason, sort = T)

# top word counts with 'reason' category included
top_word_counts_reason <- word_counts_reason %>%                                                      
  drop_na(word)%>%
  group_by(reason) %>%
  top_n(10, n) %>%                                                                   # select top 20 terms
  ungroup() %>%
  arrange(reason, -n)       


#-----------bigrams--------#

# tokenize bigrams
bigrams <- app_responses %>%
  unnest_tokens(bigram, message, token = 'ngrams', n = 2)

# get bigram counts
bigram_counts <- bigrams %>% count(bigram, sort = T)

bigram_counts_reason <- bigrams %>% count(bigram, reason, sort = T)

# remove bigrams with stop words
bigrams_no_stop <- bigrams %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") &
           str_detect(second, "[a-z]"))

bigram_counts_no_stop <- bigrams_no_stop %>% count(bigram, sort = T)

bigram_counts_no_stop_reason <- bigrams %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") &
           str_detect(second, "[a-z]"))

bigram_counts_no_stop_reason <- bigram_counts_no_stop_reason %>% count(bigram, reason, sort = T)



#-----------trigrams--------#

# trigram tokenization 
# not using no_stop_resp_frame created above, because I want to include stop words
trigrams <- app_responses %>%
  unnest_tokens(trigram, message, token = 'ngrams', n = 3)

# get trigram counts
trigram_counts <- trigrams %>% count(trigram, sort = T)

trigram_counts_reason <- trigrams %>% count(trigram, reason, sort = T)





######################################################
# PLOTS #
######################################################

#### function for top 20 ngrams from a data set 
# data = [df to be passed]
# x = [value to plot] // e.g., ngram, bigram, trigram, etc. 
# x_label = [text string for labels] //e.g. "bigram"
ngram_top_20 <- function(data, x, x_label) {
  
  data %>%
  filter(!is.na({{x}})) %>% 
  mutate(x = reorder({{x}}, n)) %>%                                              
  arrange(desc(n)) %>%                                                             
  top_n(20) %>%                                                                       
  mutate(x = factor(paste({{x}}, sep = "__"),                                    
                        levels = rev(paste({{x}}, sep = "__")))) %>%
  
  ggplot(aes(x, n)) +
  theme_light() +
  geom_col(show.legend = FALSE) +                                                       
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                        
  labs(x = paste(x_label),
       y = paste("number of times", x_label, "appeared"),
       title =paste("Top 20 ", x_label,"s, All Responses",
                    sep="")) +
  coord_flip() +

  theme(plot.title = element_text(hjust = 0.5,                           
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0),
                                  size = 22),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)),
        axis.text=element_text(size=12))  

}

#### function for top 20 ngrams from a data set, faceted by a dimension
# data = [df to be passed]
# text_field = [text unit values to plot] // e.g., ngram, bigram, trigram, etc. 
# text_label = [string for label names] //e.g. "bigram", should usually match text_field
# dimension = [dimension to facet on] 
ngram_top_20_facet <- function(data, text_field, text_label, dimension) {
  
  data %>%
    drop_na({{text_field}})%>%
    group_by({{dimension}}) %>%
    # select top 20 terms for each reason
    top_n(20, n) %>%                                                                   
    ungroup() %>%
    arrange(desc(n)) %>%      
    # used for bar sorting
    mutate(text_order = factor(paste({{text_field}}, {{dimension}}, sep = "__"),              
                         levels = rev(paste({{text_field}}, {{dimension}}, sep = "__")))) %>%
    
    # fill used for bar sorting
    ggplot(aes(text_order, n, fill = {{dimension}})) +
    theme_light() +
    geom_col(show.legend = FALSE) +          
    # used for bar sorting
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                       
    scale_y_continuous(labels = scales::number_format(accuracy = 1.0)) +
    labs(x = paste(text_label),
         y = paste("number of times", text_label, "appeared"),
         title =paste("Top 20 ", text_label,"s, by feedback 'reason'",
                      sep="")) +
    coord_flip() +
    # create individual graph for each reason   
    facet_wrap(vars({{dimension}}), ncol = 2, scales = "free") +                                  
    # title formatting (center, bold, padding)
    theme(plot.title = element_text(hjust = 0.5,                                       
                                    line = 15, 
                                    face = "bold", 
                                    margin = margin(t = 0, r = 0, b = 25, l = 0),
                                    size = 22),
          # x axis formatting (center, padding)
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)),
          axis.text=element_text(size=10))  
    
}


    
# basic plots
                     
word_counts%>%
  filter(word != "app") %>%
  ngram_top_20(word, "word")

ngram_top_20(bigram_counts_no_stop, bigram, "bigram")

ngram_top_20(trigram_counts, trigram, "trigram")

# basic facet plots
ngram_top_20_facet(word_counts_reason, word, "word", reason)
ngram_top_20_facet(bigram_counts_no_stop_reason, bigram, "bigram", reason)
ngram_top_20_facet(trigram_counts_reason, trigram, "trigram", reason)



# top bigrams for each feedback reason

top_word_counts_reason %>%
  drop_na(word) %>%
  group_by(reason) %>%
  # select top 20 terms for each reason
  top_n(20, n) %>%                                                                   
  ungroup() %>%
  arrange(reason, -n) %>%      
  
  # used for bar sorting
  mutate(word = factor(paste(word, reason, sep = "__"),              
                       levels = rev(paste(word, reason, sep = "__")))) %>%
  
  ggplot(aes(word, n)) +
  theme_light() +
  # fill used for bar sorting
  geom_col(aes(fill = as.factor(reason)), show.legend = FALSE) +          
  # used for bar sorting
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                       
  scale_y_continuous(labels = scales::number_format(accuracy = 1.0)) +
  labs(x = "word",
       y = "number of times word appeared") +
  coord_flip() +
  # create individual graph for each reason   
  facet_wrap(~reason, ncol = 2, scales = "free") +                                  
  ggtitle("Top 20 Words, by feedback 'reason'") +
  # title formatting (center, bold, padding)
  theme(plot.title = element_text(hjust = 0.5,                                       
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        # x axis formatting (center, padding)
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))   


                   

######################################################
# TF-IDF #
######################################################

# df with tf-idf calcs; documents are the different surveys
tf_idf <- word_counts_reason %>%
  filter(!is.na(word)) %>% 
  # remove words less than 2 characters long
  #  mutate(word_length = (nchar(word_counts$word))) %>%
  #  filter(word_length > 2) %>%                                                   
  select(word, reason, n) %>%
  # sum word counts by survey
  group_by(word, reason) %>% mutate(n = sum(n)) %>%                  
  ungroup() %>%
  bind_tf_idf(word, reason, n)


# plot tf-idf by survey
tf_idf %>%
  # only select words appearing at least twice
  filter(n > 1) %>%                                                   
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(reason) %>%
  top_n(5) %>%
  ungroup %>%
  
  ggplot(aes(word, tf_idf, fill = reason)) +
  theme_light() +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  # create individual graph for each survey
  facet_wrap(~reason, ncol = 2, scales = "free") +                                      
  ggtitle("tf-idf (uniquely important words appearing at least twice, by 'reason')") +
  # title formatting (center, bold, padding)
  theme(plot.title = element_text(hjust = 0.5,                                           
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        # x axis formatting (center, padding)
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0))) +     
  coord_flip()
