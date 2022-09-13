install.packages("topicmodels")
install.packages("stm")


library(topicmodels)
library(tm)


library(stm)
library(quanteda)



weather_dfm <- word_counts_useful %>%                              # set df to dfm
  filter(!is.na(word)) %>% 
  count(weather_usefulness, word, sort = TRUE) %>%
  cast_dfm(weather_usefulness, word, n)


topic_model <- stm(weather_dfm, K = 4, init.type = "Spectral")     # select number of topics and initialization type
summary(topic_model)                                               # results will be in console


# topic model related plots
plot(topic_model, type = "summary", xlim = c(0, .3))

plot(topic_model, type = "labels", n = 10, labelType = "frex", main = "FREX")


# beta matrix: what are the words contributing to each topic
# measuring probabiliy of each word being present
td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(100) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,                                       # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0),
                                  size = 20),
        axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 5, l = 0)),  # x axis formatting (center, padding)
        axis.title.y = element_text(size = 10, margin = margin(t = 10, r = 10, b = 5, l = 10)),  # x axis formatting (center, padding)
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.background = element_rect(fill="grey"),
        strip.text = element_text(size = 14)) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()




# gamma matrix: in this document, how much did the topic contribute
# measuring probabiliy that a document belongs in a topic
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(word_counts_useful))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~topic, ncol = 3)


