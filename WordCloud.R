library('tm')
library('SnowballC')
library('wordcloud')
library('RColorBrewer')

setwd('~/Development/word-cloud')
text <- read.delim('transcript.txt')
docs <- Corpus(VectorSource(text))

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove custom stop word
#
# Specify stopwords as a character vector
docs <- tm_map(docs, removeWords, c("unicorn", "unicorns")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=144, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(12, "Paired"))

# frequent terms
findFreqTerms(dtm, lowfreq = 10)

# frequent words associated with a given word/phrase
# A score of 1 means that two words always appear together
# A score approaching 0 means the terms seldom appear 
findAssocs(dtm, terms = "like", corlimit = 0.25)
