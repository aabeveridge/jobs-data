library(tm)

#Read in the terms lists and set them to terms1 variable
terms1 <- read.csv("strings.csv", stringsAsFactors=FALSE)
terms_list <- terms1$terms
terms_list <- tolower(terms_list)

category_list <- terms1$coding

#Read in the list of documents
corpus <- read.csv("topic-data/tech.csv", stringsAsFactors=FALSE, header=FALSE)
corpus <- corpus$V1
corpus <- corpus[!apply(is.na(corpus) | corpus == "", 1, all),]

#Remove punctuation
corpus <- removePunctuation(corpus, preserve_intra_word_dashes=TRUE)

#Removes carrige returns from descriptions
corpus <- gsub("\r?\n|\r", " ", corpus)

#Changes corpus to all lower case
corpus <- tolower(corpus)


# this loop counts each time that a word from skillList appears in the job description
# and then add adds that data to that job description in a new column
for (desc in 1:length(corpus)) {

  d1 <- c()

  #iterates through each job description, looking for matches to skillList terms
  for (a_term in 1:length(terms_list)) {

    #This checks to see if the d1 vector is empty, if so, it adds "none_listed" to the row matching that description
    if (length(d1) < 1 & a_term == length(terms_list)) {
      d1 <- "no_matches"
    }

    #If skill matches a term in the description, then it appends that term to new vector
    else if (grepl(terms_list[a_term], corpus[desc])) {
      d1 <- c(d1, paste0(category_list[desc]))
    }
  }
  d1 <- paste(d1, collapse=" ")
  coding_results <- append(coding_results, d1)
  rm(d1)
}


coding_results <- NULL
d <- NULL
d1 <- NULL
for (doc in 1:length(corpus)) {
  for (terms in 1:length(terms_list)) {
    list1 <- unlist(strsplit(terms_list[terms], ", "))
    for (a_term in 1:length(list1)) {
      if (grepl(list1[a_term], corpus[doc])) {
        d <- category_list[doc]
        break
      }
      else {
        d <- "no_matches"
      }
    }
  d1 <- append(d1, d)
  }
  d <- paste(d, collapse=" ")
  coding_results <- append(coding_results, d)
  d <- NULL
  d1 <- NULL
}
