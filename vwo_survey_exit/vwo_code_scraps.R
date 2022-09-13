#install.packages("tidytext")
#install.packages("bigrquery")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggridges")



library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)
library(data.table)
library(formattable)
library(knitr)
library(ggridges)


project <- "ga360-173318" # put your project ID here


sql <- "SELECT * except(Respondent_ID), CAST(Respondent_ID AS STRING) Respondent_ID FROM `ga360-173318.surveymonkey._oct2018_nps_poc_agg`"
nps_oct2018 <- query_exec(sql, project = project, use_legacy_sql = FALSE)


word_tokens <- nps_oct2018 %>%
  unnest_tokens(word, input = What_is_the_primary_reason_for_the_score_you_just_gave_)

word_tokens_no_stop <- word_tokens %>% anti_join(get_stopwords())

word_counts <- word_tokens_no_stop %>% count(word, survey_segment, detractor_promoter, sort = TRUE)


word_counts %>%
  filter(detractor_promoter != 'neutral') %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(detractor_promoter) %>%    
  top_n(75) %>%
  ungroup %>%
  ggplot(aes(word, n)) +
  geom_col(aes(fill = detractor_promoter), show.legend = FALSE) +
  facet_wrap(~detractor_promoter, ncol = 2, scales = "free") +
  labs(y = "# of times word appeared", x = "word") +
  coord_flip() +
  ggtitle("Top 75 Words by Detractor & Promoter, All Surveys")

word_counts %>%
  filter(survey_segment == 'all_digital_nps_oct2018' &
           detractor_promoter != 'neutral') %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(survey_segment, detractor_promoter) %>%
  top_n(50) %>%
  ungroup %>%
  
  ggplot(aes(word, n)) +
  geom_col(aes(fill = detractor_promoter), show.legend = FALSE) +
  facet_wrap(survey_segment~detractor_promoter, ncol = 2, scales = "free") +
  labs(y = "# of times word appeared", x = "word") +
  coord_flip() +
  ggtitle("Top 50 Words by Detractor & Promoter")

#----------------------------------------------------------

#install.packages("plotrix")
#install.packages("tm")
#library(plotrix)
#library(tm)

# clean word_counts
all_words <- VectorSource(word_counts)
all_corpus <- VCorpus(all_words)

# Clean the corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en")))
  return(corpus)
}

all_clean <- clean_corpus(all_corpus)

# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean)

# Give the columns distinct names
colnames(all_tdm) <- c("detractor", "promoter")

# Create all_m
all_m <- as.matrix(all_tdm)

# Create comparison cloud
install.packages("wordcloud")
library(wordcloud)

wordcloud(all_m,
          colors = c("orange", "blue"),
          max.words = 50)

########


test_chart <- word_tokens_no_stop %>% count(word, detractor_promoter, sort = TRUE) %>% filter(detractor_promoter != 'neutral') %>%  group_by(detractor_promoter) %>% top_n(20)

ggplot(test_chart, aes(x = word, color = detractor_promoter))+
  geom_linerange(data = test_chart[test_chart$detractor_promoter=="detractor",], 
                 aes(ymin = -n, ymax = 0), size = 3.5, alpha = 1) +
  geom_linerange(data = test_chart[test_chart$detractor_promoter=="promoter",], 
                 aes(ymin = 0, ymax = n), size = 3.5, alpha = 1) +
  geom_label(aes(x = word, y = 0, label = word, family = "Arial"), 
             inherit.aes = F,
             size = 4, label.padding = unit(0.1, "lines"), label.size = 0,
             label.r = unit(0.0, "lines"), fill = "#FFFFFF", color = "#5D646F") +
  scale_y_continuous(breaks = c(c(-500, -375, -250, -125, 0) + 0, c(0, 125, 250, 375, 500)+0),
                     labels = c("500", "375", "250", "125", "0", "0", "125", "250", "375", "500")) +
  coord_flip() +
  labs(title = "Frequency of Top Words Comparison",
       subtitle = "Top words broken down by detractor use & promoter use",
       caption = "") +
  scale_color_manual(name = "", values = c(detractor = "#8C3F4D", promoter = "#3E606F"),
                     labels = c("detractor", "promoter"))+
  theme_minimal(base_family = "Arial")+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 36, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 16, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.x = element_text(size = 12, color = "#5D646F"),
        axis.text.y = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 18, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#FFFFFF"),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        legend.position = "top",
        legend.spacing  = unit(0.1, "lines"),
        legend.text  = element_text(family = "Arial", size = 14),
        legend.text.align = 0)


#####


#create a df that's the same as word_counts, but without the segments
word_counts_no_segment <- word_tokens_no_stop %>% count(word, detractor_promoter, sort = TRUE)  

#create 2 dataframes; 1 will be detractor words, the other will be promoter words
detractor_words <- word_counts_no_segment %>% filter(detractor_promoter == 'detractor')
promoter_words <- word_counts_no_segment %>% filter(detractor_promoter == 'promoter')

#merge those two dataframes to find the common words between the two
#have to determine what column to select top 50 by
common_words <- merge(detractor_words, promoter_words, by.x = "word", by.y = "word") %>% top_n(25)

#rename the columns to facilitate the removal of them
#the columns that just list "detractor" or "promoter" in each row
colnames(common_words) <- c("word", "cat1", "detractor_n", "cat2", "promoter_n")

#identify and remove the columns that we don't want 
cols.dont.want <- c("cat1", "cat2") 
common_words <- common_words[, ! names(common_words) %in% cols.dont.want, drop = FALSE]


#start pyramid plot
library(plotrix)

#x = detractor, y = promoter
top25_df <- data.frame(x = common_words[1:25, 2],
                       y = common_words[1:25, 3],
                       labels = common_words[1:25, 1])


pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 50,
             laxlab = NULL,
             raxlab = NULL, 
             unit = NULL,
             top.labels = c("Detractor",
                            "Words",
                            "Promoter"),
             lxcol = "#8C3F4D",
             rxcol = "#3E606F"
)

#### same as above, just finding the difference between frequency across categories
#############

#create a df that's the same as word_counts, but without the segments
word_counts_no_segment <- word_tokens_no_stop %>% count(word, detractor_promoter, sort = T)  

#create 2 dataframes; 1 will be detractor words, the other will be promoter words
detractor_words <- word_counts_no_segment %>% filter(detractor_promoter == 'detractor')
promoter_words <- word_counts_no_segment %>% filter(detractor_promoter == 'promoter')

#merge those two dataframes to find the common words between the two
#have to determine what column to select top 50 by
common_words <- merge(detractor_words, promoter_words, by.x = "word", by.y = "word")

#rename the columns to facilitate the removal of them
#the columns that just list "detractor" or "promoter" in each row
colnames(common_words) <- c("word", "cat1", "detractor_n", "cat2", "promoter_n")

#identify and remove the columns that we don't want 
cols.dont.want <- c("cat1", "cat2") 
common_words <- common_words[, ! names(common_words) %in% cols.dont.want, drop = FALSE]

#calc common words and difference
difference <- abs(common_words[, 2] - common_words[, 3])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 4],
                                   decreasing = T), ]
head(common_words)

#x = detractor, y = promoter
top25_df <- data.frame(x = common_words[1:25, 2],
                       y = common_words[1:25, 3],
                       labels = common_words[1:25, 1])


pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common by Detractor & Promoter",
             gap = 50,
             laxlab = NULL,
             raxlab = NULL, 
             unit = NULL,
             top.labels = c("Detractor",
                            "Word",
                            "Promoter"),
             lxcol = "#8C3F4D",
             rxcol = "#3E606F"
)

######## same as above, but like the ggplot pyramid (includes words not common to both cats)
##############

#create a df that's the same as word_counts, but without the segments
word_counts_no_segment <- word_tokens_no_stop %>% count(word, detractor_promoter, sort = TRUE) 

#create 2 dataframes; 1 will be detractor words, the other will be promoter words
detractor_words <- word_counts_no_segment %>% filter(detractor_promoter == 'detractor')
promoter_words <- word_counts_no_segment %>% filter(detractor_promoter == 'promoter')

#merge those two dataframes to find the common words between the two
#have to determine what column to select top 50 by
common_words <- merge(detractor_words, promoter_words, by.x = "word", by.y = "word")

#rename the columns to facilitate the removal of them
#the columns that just list "detractor" or "promoter" in each row
colnames(common_words) <- c("word", "cat1", "detractor_n", "cat2", "promoter_n")

#identify and remove the columns that we don't want
cols.dont.want <- c("cat1", "cat2")
common_words <- common_words[, ! names(common_words) %in% cols.dont.want, drop = F]

#calc common words and difference
difference <- abs(common_words[, 2] - common_words[, 3])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 4],
                                   decreasing = T), ]
head(common_words)

#x = detractor, y = promoter
top25_df <- data.frame(x = common_words[1:25, 2],
                       y = common_words[1:25, 3],
                       labels = common_words[1:25, 1])


pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels,
             main = "Words in Common",
             gap = 50,
             laxlab = NULL,
             raxlab = NULL,
             unit = NULL,
             top.labels = c("Detractor",
                            "Words",
                            "Promoter"),
             lxcol = "#8C3F4D",
             rxcol = "#3E606F"
)

# isolate data & plot
vwo_feb2019 %>%
  group_by(segment_name, device) %>%
  summarize(mean = mean(word_count),                                                            # calculate mean word count
            median = median(word_count)) %>%                                                    # calculate median word count
  mutate_at(vars(mean, median), funs(round(., 0))) %>%                                          # round the calcs to 0 decimal places 
#  collapse_rows_df(segment_name) %>%                                                            # collapse segment_name rows to minimize duplication of text
  
  # formattable to utilize heatmapping in the chart
  formattable(align =c("l","l","c","c"),                                                        # column text alignment  
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")), 
                   `mean`= color_tile("#FFEFDB", "#FF8000"),                                    # color range for column
                   `median`= color_tile("#FFEFDB", "#FF8000")))  




## reshape data (tidy/tall form)
vwo_feb2019 %>%
  tbl_df() %>%
  rownames_to_column('device') %>%
  gather(Var2, value, -Var1) %>%
  mutate(
    Var1 = factor(Var1, levels=1:10),
    Var2 = factor(gsub("V", "", Var2), levels=1:10)
  )

## plot data
ggplot(dat2, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "white", high = "red") 



# heatmap
vwo_feb2019_clean %>%
  group_by(segment_name) %>%
  summarize(median = median(word_count)) %>%
  mutate_at(vars(median), funs(round(., 2))) %>%

vwo_feb2019_clean %>%
  group_by(segment_name, device) %>%
  summarize(median = median(word_count)) %>%
  mutate_at(vars(median), funs(round(., 0))) %>%
  
  ggplot(aes(fct_rev(device), segment_name)) +                                                      # fct_rev reorders the device category to be in deisred format
  geom_tile(aes(fill = median),                                                                     # set tiles to be median adoption time
            color = "white") + 
  scale_fill_gradient(low = "aliceblue",                                                            # set tile gradient colors
                      high = "steelblue") +
  geom_text(aes(label=median,
                size = 12)) +                                                                       # adds number to the tile
  theme_bw() +
  labs(x = NULL,                                                                                    # set plot labels
       y = NULL) +
  ggtitle("Median Word Count Heatmap") +                                                            # set plot title
  scale_x_discrete(expand = c(0, 0)) +                                                              # visual editing, used to expand tiles to entire plot area on both axes
  scale_y_discrete(expand = c(0, 0)) +                                
  theme(legend.position = "none",                                                                   # remove legend
        panel.grid.major = element_blank(),                                                         # remove gridlines in the background
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,                                                      # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  coord_flip()




# plot 4: same as above, not grouped by survey but faceted instead
vwo_feb2019_clean %>%
  ggplot(aes(x = word_count, y = segment_name, fill = device)) +
  geom_density_ridges(alpha = 0.5, scale = 1) +                              # alpha adjusts the opacity, scale adjusts level of ridge overlap
  theme_ridges(center_axis_labels = TRUE) +                                  # center the axes
  ggtitle("Word Count Distribution of All Survey Responses") +
  labs(x = "word count",
       y = NULL) +
  scale_fill_manual(values=c("dodgerblue3","#FFD700", "#3CB371")) +          # customize colors for device category
  theme(plot.title = element_text(hjust = 0.5, face="bold"),                 # title formatting (center, bold)
        legend.position="top",                                               # puts legend at the top, rather than on the side
        legend.box="horizontal")                                             # makes legend horizontal, rather than vertical




## COUNT OF "YES" OR "NO" RESPONSES
# new df for one-word responses
vwo_feb2019_one_word <- vwo_feb2019_clean %>%                        
  filter(word_count < 2)  %>%

# get count of responses that are "yes" or "no"  
vwo_feb2019_clean %>%  
  filter(Question == "free" | Question == "Yes" | Question == "no" | Question == "No") %>%
  count()




### BIGRAMS NO STOP SORTED #####------------------------------------------------

no_stop_resp_frame <- word_tokens_no_stop %>% 
  group_by(row_id) %>% 
  mutate(no_stop_resps = paste(word, collapse = " ")) %>% 
  select(-word) %>%
  unique() %>% 
  ungroup()

bigrams <- no_stop_resp_frame %>%                            
  unnest_tokens(ngram, token = "ngrams", n = 2,
                input = no_stop_resps)


# get bigram counts
bigram_counts <- bigrams %>% count(ngram, segment, sort = TRUE)

# bigram sort plot
bi_top_terms <- bigram_counts %>%
  filter(!is.na(ngram)) %>% 
  group_by(segment) %>%
  top_n(10) %>%
  ungroup() %>%
  arrange(segment, -n)


bi_top_terms %>%
  #arrange(desc(n), .by_group = T) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  group_by(segment, ngram) %>%    
  arrange(desc(n)) %>% 
  top_n(10) %>% #View()
  ungroup %>%
  mutate(ngram = factor(paste(ngram, segment, sep = "__"), 
                        levels = rev(paste(ngram, segment, sep = "__")))) %>%
  
  ggplot(aes(ngram, n)) +
  theme_light() +
  geom_col(aes(fill = as.factor(segment)), show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(x = "bigram",
       y = "number of times bigram appeared") +
  coord_flip() +
  facet_wrap(~segment, ncol = 3, scales = "free") +      
  ggtitle("Top 10 Bigrams by Survey, All Surveys") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))   # x axis formatting (center, padding)
  


### TRIGRAMS NO STOP SORTED #####------------------------------------------------

# using no_stop_resp_frame created above:
trigrams <- no_stop_resp_frame %>%                             
  unnest_tokens(ngram, token = "ngrams", n = 3,
                input = no_stop_resps)

# get trigram counts
trigram_counts <- trigrams %>% count(ngram, segment, sort = TRUE)
tri_top_terms <- trigram_counts %>%
  filter(!is.na(ngram)) %>% 
  filter(n > 1) %>%                        # filter cuz top_n will return hella results with "1" otherwise (cuz of "ties")
  group_by(segment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  arrange(segment, -n)


tri_top_terms %>%
  #arrange(desc(n), .by_group = T) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  group_by(segment, ngram) %>%    
  arrange(desc(n)) %>% 
  top_n(10) %>% #View()
  ungroup %>%
  mutate(ngram = factor(paste(ngram, segment, sep = "__"), 
                        levels = rev(paste(ngram, segment, sep = "__")))) %>%
  
  ggplot(aes(ngram, n)) +
  theme_light() +
  geom_col(aes(fill = as.factor(segment)), show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(x = "word",
       y = "number of times word appeared") +
  coord_flip() +
  facet_wrap(~segment, ncol = 3, scales = "free") +      
  ggtitle("Top 10 Trigrams by Survey, All Surveys") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))   # x axis formatting (center, padding)





# using no_stop_resp_frame created above:
trigrams <- no_stop_resp_frame %>%                             
  unnest_tokens(ngram, token = "ngrams", n = 3,
                input = no_stop_resps)

# get trigram counts
trigram_counts <- trigrams %>% count(ngram, segment, sort = T)
tri_top_terms <- trigram_counts %>%
  filter(!is.na(ngram)) %>% 
  filter(n > 1) %>%                        # filter cuz top_n will return hella results with "1" otherwise (cuz of "ties")
  group_by(segment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  arrange(segment, -n)


tri_top_terms %>%
  #arrange(desc(n), .by_group = T) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  group_by(segment, ngram) %>%    
  arrange(desc(n)) %>% 
  top_n(10) %>% #View()
  ungroup %>%
  mutate(ngram = factor(paste(ngram, segment, sep = "__"), 
                        levels = rev(paste(ngram, segment, sep = "__")))) %>%
  
  ggplot(aes(ngram, n)) +
  theme_light() +
  geom_col(aes(fill = as.factor(segment)), show.legend = F) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(x = "trigram",
       y = "number of times trigram appeared") +
  coord_flip() +
  facet_wrap(~segment, ncol = 3, scales = "free") +      
  ggtitle("Top 10 Trigrams by Survey, All Surveys") +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)))   # x axis formatting (center, padding)
