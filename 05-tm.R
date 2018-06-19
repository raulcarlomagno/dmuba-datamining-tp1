library(tm)
library(SnowballC)



df <- read.csv("tweets.csv",encoding = "UTF-8", stringsAsFactors = FALSE)
corpus <- SimpleCorpus(VectorSource(enc2utf8(df$text)), control = list(language = "es"))

#convert to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

#remove emojis not working
#corpus2 <- tm_map(corpus, content_transformer(gsub), pattern="\\W",replace=" ")

#remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))

#remove anything other than spanish letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeNumPunct))

#remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

#remove extra whitespace
corpus <- tm_map(corpus, stripWhitespace)

#Remove numbers
corpus <- tm_map(corpus, removeNumbers)

#Remove punctuations
corpus <- tm_map(corpus, removePunctuation)

#Stemming
corpus <- tm_map(corpus, stemDocument)

#create a term matrix and store it as dtm
dtm <- TermDocumentMatrix(corpus)

inspect(dtm)


m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(term = names(v),frec=v)
top50 = head(d, 50)
rownames(top50) <- NULL


library(RColorBrewer) 
library(wordcloud2)
wordcloud2(top50, color = "random-dark",size = 1, shape = "circle", backgroundColor = "white")

#https://rstudio-pubs-static.s3.amazonaws.com/265713_cbef910aee7642dc8b62996e38d2825d.html