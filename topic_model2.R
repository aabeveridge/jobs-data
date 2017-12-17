library(topicmodels)
library(tm)
library(tidytext
library(dplyr)

# Read in the Job Description data into R
d <- read.csv("id-data.csv")
corpus <- d$description

#Removes carrige returns from descriptions
corpus <- gsub("\r?\n|\r", " ", corpus)

# remove apostrophes
corpus <- gsub("'", "", corpus)

# replace remaining punctuation with space
corpus <- gsub("[[:punct:]]", " ", corpus)

# force to lowercase
corpus <- tolower(corpus)

#remove numbers
corpus <- removeNumbers(corpus)

#remove stopwords
corpus <- removeWords(corpus, stopwords(kind="SMART"))

# removes remaining whitespace
corpus <- stripWhitespace(corpus)

corpus <- Corpus(VectorSource(corpus))

dtm <- DocumentTermMatrix(corpus)

corpus_lda <- LDA(dtm, k = 7, control = list(seed = 1234))

corpus_topics <- tidy(corpus_lda, matrix = "beta")
corpus_topics

top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

png(filename="topic.png")
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
dev.off()
