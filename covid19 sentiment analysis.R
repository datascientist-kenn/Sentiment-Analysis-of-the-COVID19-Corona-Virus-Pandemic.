library(rtweet)
library(dplyr)
library(graphics)

# Downloading tweets
tweets <- search_tweets("#Covid19", "#Covid-19", n=20000, include_rts = TRUE)
covid19_tweets = tweets
names(covid19_tweets)

covid19_Eng <- covid19_tweets[ which(covid19_tweets$lang== 'en'),]

covid19_Eng <- covid19_Eng %>%
  select(user_id, status_id, created_at, screen_name, text, favorite_count, source, is_quote, is_retweet, retweet_text, retweet_created_at)
prev <- read.csv('merged.csv')


df <-merge(covid19_Eng, prev, by=intersect(names(covid19_Eng), names(prev)), by.covid19_Eng ="user_id", prev ="user_id", all=TRUE, all.covid19_Eng=all, all.prev=all, sort=TRUE, suffixes=c(".covid19_Eng", ".prev"), no.dups=TRUE, incomparables = NULL)

write.csv(df, file = "merged.csv")

# Cleaning the data with twitter corpus
library(purrr)
library(syuzhet)
library(corpus)

library(tm)
mycorpus <- Corpus(VectorSource(covid19_Eng$text))
mycorpus <- tm_map(mycorpus, removeWords, stopwords())

# Removing RedHat word like COVID which I am expecting to appear the most as it is the basis for the search hashtag

myStopWords <- "covid"
mycorpus <- tm_map(mycorpus, removeWords, myStopWords)

# Removing urls
remove_urls <- function(x) gsub("http[^[:space:]]*","",x)
mycorpus <- tm_map(mycorpus, content_transformer(remove_urls))

# Remove anything that is not english letters, words and space
rnp <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
mycorpus <- tm_map(mycorpus, content_transformer(rnp))
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, stemDocument)

dtm <- DocumentTermMatrix(mycorpus)

# WordCloud
library(wordcloud)
wc <- wordcloud(mycorpus, min.freq = 10)
wc <- wordcloud(mycorpus, min.freq = 10, random.order = FALSE)

# Removing alphanumeric characters
covid19_Eng$text <- gsub("[^0-9A-Za-z///' ]","", covid19_Eng$text)

# Removing web links
covid19_Eng$text <- gsub("http:\\w+", "", covid19_Eng$text)

# Removing the "rt" text
covid19_Eng$text <- gsub("rt", "", covid19_Eng$text)

covid19_Eng$text <- tolower(covid19_Eng$text)


# The Sentiment Analysis
library(sentimentr)
SA <- sentiment(covid19_Eng$text)

covid19_Eng$sentiment <- SA$sentiment

covid19Sentiment <- subset(covid19_Eng, select=c(screen_name, text, source, sentiment))
covid19Sentiment <- as.data.frame(covid19Sentiment)

sum(covid19Sentiment$sentiment)
mean(covid19Sentiment$sentiment)
?average_weighted_mixed_sentiment
?average_downweighted_zero
average_weighted_mixed_sentiment(covid19Sentiment$sentiment)
average_downweighted_zero(covid19Sentiment$sentiment)
