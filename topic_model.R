# ##################################################
# Topic Modeling ID Data                           #
#                                                  #
# Code tutorial:                                   #
# https://ldavis.cpsievert.me/reviews/reviews.html #

library(tm)
library(Rmpfr)

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
dtm <- TermDocumentMatrix(corpus)

# This Harmonic Mean function is from Martin Ponweiser's thesis:
# # http://epub.wu.ac.at/3558/1/main.pdf. It is used to determine k
# harmonicMean <- function(logLikelihoods, precision = 2000L) {
#   llMed <- median(logLikelihoods)
#   as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
#                                        prec = precision) + llMed))))
# }
#
# term_tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) *
#   log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))
# summary(term_tfidf)
#
#
# ## Keeping the rows with tfidf >= to the 0.155
# Rdtm <- dtm[,term_tfidf >= 0.155]
# summary(slam::col_sums(Rdtm))

# k <- 25
# burnin <- 1000
# iter <- 1000
# keep <- 50
# fitted <- topicmodels::LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )
# ## assuming that burnin is a multiple of keep
# logLiks <- fitted@logLiks[-c(1:(burnin/keep))]
#
# ## This returns the harmomnic mean for k = 25 topics.
# harmonicMean(logLiks)
#
# seqk <- seq(2, 100, 1)
# burnin <- 1000
# iter <- 1000
# keep <- 50
# system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(dtm, k = k,
#                                                      method = "Gibbs",control = list(burnin = burnin,
#                                                                          iter = iter, keep = keep) )))
#
# # extract logliks from each topic
# logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
#
# # compute harmonic means
# hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
#
# ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
#   theme(text = element_text(family= NULL),
#         axis.title.y=element_text(vjust=1, size=16),
#         axis.title.x=element_text(vjust=-.5, size=16),
#         axis.text=element_text(size=16),
#         plot.title=element_text(size=20)) +
#   xlab('Number of Topics') +
#   ylab('Harmonic Mean') +
#      annotate("text", x = 25, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
#   ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of NEN LLIS", atop(italic("How many distinct topics in the abstracts?"), ""))))
#
#   ldaplot
#
#   seqk[which.max(hm_many)]

  system.time(llis.model <- topicmodels::LDA(Rdtm, 20, method = "Gibbs", control = list(iter=2000, seed = 0622)))

  llis.topics <- topicmodels::topics(llis.model, 1)
## In this case I am returning the top 30 terms.
llis.terms <- as.data.frame(topicmodels::terms(llis.model, 30), stringsAsFactors = FALSE)
llis.terms[1:5]
