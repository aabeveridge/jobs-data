# ##################################################
# Topic Modeling Jobs Data                         #
#                                                  #
# Code tutorial:                                   #
# https://ldavis.cpsievert.me/reviews/reviews.html #
####################################################
library(tm)
library(lda)
library(LDAvis)

# Read in the Job Description data into R
d <- read.csv("jobs-data.csv")
d <- d$text

##################
# pre-processing #
##################

#set stopwords
stop_words <- stopwords("SMART")
# remove apostrophes
d <- gsub("'", "", d)
# replace punctuation with space
d <- gsub("[[:punct:]]", " ", d)
# replace control characters with space
d <- gsub("[[:cntrl:]]", " ", d)
# remove whitespace at beginning of documents
d <- gsub("^[[:space:]]+", "", d)
# remove whitespace at end of documents
d <- gsub("[[:space:]]+$", "", d)
# force to lowercase
d <- tolower(d)
d <- removeNumbers(d)

# tokenize on space and output as a list:
doc.list <- strsplit(d, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
# number of documents (2,000)
D <- length(documents)
# number of terms in the vocab (14,568)
W <- length(vocab)
# number of tokens per document [312, 288, 170, 436, 291, ...]
doc.length <- sapply(documents, function(x) sum(x[2, ]))
# total number of tokens in the data (546,827)
N <- sum(doc.length)
# frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]
term.frequency <- as.integer(term.table)

# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,
                                   num.iterations = G, alpha = alpha,
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

##########################
# Visualizing the Models #
##########################

# estimating document topic distributions
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))


# tokens and frequencies across corpus
corpus <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

# create the JSON object to feed the visualization:
json <- createJSON(phi = corpus$phi,
                   theta = corpus$theta,
                   doc.length = corpus$doc.length,
                   vocab = corpus$vocab,
                   term.frequency = corpus$term.frequency)

# write json to a file in the 'vis' directory
serVis(json, out.dir = 'vis', open.browser = FALSE)
