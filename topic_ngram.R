####################################
# This code draws on examples from #
# Text Mining with R to produce    #
# topic modeling and ngram visuals #
# https://www.tidytextmining.com/  #
####################################

library(topicmodels)
library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(textstem)

# Read in the Job Description data into R
d1 <- read.csv("topic-data/medical.csv", header=FALSE, stringsAsFactors=FALSE)
d1 <- mutate(d1, job_type = "medical")
d2 <- read.csv("topic-data/social.csv", header=FALSE, stringsAsFactors=FALSE)
d2 <- mutate(d2, job_type = "social")
d3 <- read.csv("topic-data/tech.csv", header=FALSE, stringsAsFactors=FALSE)
d3 <- mutate(d3, job_type = "tech")
d4 <- read.csv("topic-data/grant.csv", header=FALSE, stringsAsFactors=FALSE)
d4 <- mutate(d4, job_type = "grant")
d5 <- read.csv("topic-data/content.csv", header=FALSE, stringsAsFactors=FALSE)
d5 <- mutate(d5, job_type = "content")
topicData <- rbind(d1, d2, d3, d4, d5)

# Custom list of words for removal
rwords <- read.csv("scrub.csv", stringsAsFactors=FALSE)
rwords <- rwords$term

# Replace punctuation with a space
topicData$V2 <- gsub("[[:punct:]]", " ", topicData$V2)

# Remove all one and two letter words
topicData$V2 <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", topicData$V2)

# Make all characters lowercase
topicData$V2 <- tolower(topicData$V2)

# Remove numbers
topicData$V2 <- removeNumbers(topicData$V2)

# Remove stopwords and a custom list of words defined by rwords
topicData$V2 <- removeWords(topicData$V2, stopwords(kind="SMART"))
topicData$V2 <- removeWords(topicData$V2, words=rwords)

# Lemmatize the corpus
# lemma_dictionary_hs <- make_lemma_dictionary(y, engine = 'hunspell')
topicData$V2 <- lemmatize_strings(topicData$V2) #, dictionary = lemma_dictionary_hs)

# Term corrections
topicData$V2 <- gsub("tech ", "technical ", topicData$V2)
topicData$V2 <- gsub("medium ", "media ", topicData$V2)


# Stemming the corpus
# corpus <- stem_strings(corpus)

# Removes remaining whitespace
topicData$V2 <- stripWhitespace(topicData$V2)

# Uses TM to create a Document Term Matrix
corpus <- topicData$V2
corpus <- VCorpus(VectorSource(corpus))
dtm <- DocumentTermMatrix(corpus)

##############################################
# Topic modeling for topics within documents #
##############################################

corpus_lda <- LDA(dtm, k = 5, control = list(seed = 1234))
corpus_topics <- tidy(corpus_lda, matrix = "beta")

corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

png(file="topic1.png")
  corpus_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
dev.off()

#comparing topic differentiation if there were only two topics
corpus_lda <- LDA(dtm, k = 2, control = list(seed = 1234))
corpus_topics <- tidy(corpus_lda, matrix = "beta")

beta_spread <- corpus_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

png(file="topic2.png")
  beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()
dev.off()

##########################
# ngram analysis of data #
##########################

corpus_bigrams <- topicData %>%
  unnest_tokens(bigram, V2, token = "ngrams", n = 2)

corpus_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- corpus_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# bigrams_filtered <- bigrams_separated %>%
#   filter(!word1 %in% stop_words$word) %>%
#   filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_separated %>%
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_separated %>%
    unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(job_type, bigram) %>%
  bind_tf_idf(bigram, job_type, n) %>%
  arrange(desc(tf_idf))

png(file="ngram.png")
  bigram_tf_idf %>%
    arrange(desc(tf_idf)) %>%
    group_by(job_type) %>%
    top_n(10, tf_idf) %>%
    ungroup() %>%
    mutate(bigram = reorder(bigram, tf_idf)) %>%
    ggplot(aes(bigram, tf_idf, fill = job_type)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ job_type, ncol = 2, scales = "free") +
    coord_flip() +
    labs(y = "tf-idf of bigram to Job Type",
         x = "")
dev.off()
