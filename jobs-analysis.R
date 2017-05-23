# TM Text Mining Package for R
library(tm)
library(Rgraphviz)

# Read in the Job Description data into R
d <- read.csv("jobs-data.csv")
d <- d$text

# Data Cleaning: Make docs lower case, remove numbers,
# remove stopwords, remove punctuation, remove excess
# spaces between words
d <- tolower(d)
d <- removeNumbers(d)
d <- removeWords(d, stopwords(kind="en"))
d <- removePunctuation(d)
d <- stripWhitespace(d)

# Save cleaned description data as new spreadsheet
write.csv(d, file="data-clean.csv")

############
# Analysis #
############

# Read in the clean Job Description data into R
d <- read.csv("data-clean.csv")

# Transform data to Corpus for TM Package
d <- Corpus(VectorSource(d))

# Transform data to Document Term Matrix
dtm <- DocumentTermMatrix(d)

# Top Word Frequencies
freq <- colSums(as.matrix(dtm))
freq <- head(sort(freq, decreasing=TRUE), 50)
freq <- data.frame(names(freq), as.numeric(freq))
colnames(freq) <- c("Word", "Freq")
write.csv(freq, "freq.csv")

# Correlation Graph
num <- 15
freq <- colSums(as.matrix(dtm))
freq <- head(sort(freq, decreasing=TRUE), num)
freq <- data.frame(names(freq), as.numeric(freq))
colnames(freq) <- c("Word", "Freq")
lowNum <- tail(freq$Freq, 1)
highNum <- head(freq$Freq, 1)
png("corGraph.png", 900, 700)
defAttrs <- getDefaultAttrs()
plot(dtm, terms=findFreqTerms(dtm, lowfreq=lowNum, highfreq=highNum),
       corThreshold=0.95, attrs=list(node=list(shape = "ellipse", fixedsize = FALSE,
                                              fillcolor="lightblue", height="2.6", width="10.5",
                                              fontsize="14")))
dev.off()
