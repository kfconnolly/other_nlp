install.packages("topicmodels")
install.packages("stm")


library(topicmodels)
library(tm)


comm_corpus <- Corpus(VectorSource(commenting_june2019_clean))
comm_dtm <- DocumentTermMatrix(comm_corpus)

commenting_lda <- LDA(commenting_june2019_clean, k = 2, control = list(seed = 1234))

commenting_lda


library(stm)
library(quanteda)



word_counts_cat_dfm <- word_counts_cat %>%
  filter(!word %in% c("like", "just", "one", "get", "can"))

comm_dfm <- word_counts_cat_dfm %>%
  count(word, sort = TRUE) %>%
  cast_dfm(word, n)


topic_model <- stm(comm_dfm, K = 12, init.type = "Spectral")
summary(topic_model)

# beta matrix: what are the words contributing to each topic
# measuring probabiliy of each word being present
td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  theme_new() +
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
                 document_names = rownames(word_counts_cat))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~topic, ncol = 3)


plot(topic_model, type = "summary", xlim = c(0, .3))

plot(topic_model, type = "labels", n = 10, labelType = "frex", main = "FREX")



