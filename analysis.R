# read in some stopwords:
library(tm)
stop_words <- stopwords("SMART")

# pre-processing:

# remove apostrophes
reviews <- gsub("'", "", reviews)
# replace punctuation with space
reviews <- gsub("[[:punct:]]", " ", reviews)
# replace control characters with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)
# remove whitespace at beginning of documents
reviews <- gsub("^[[:space:]]+", "", reviews)
# remove whitespace at end of documents
reviews <- gsub("[[:space:]]+$", "", reviews)
# force to lowercase
reviews <- tolower(reviews)

# tokenize on space and output as a list:
doc.list <- strsplit(reviews, "[[:space:]]+")

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
