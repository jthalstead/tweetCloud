library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)

# remove web strings?
rmNonAlphabet = function(str) {
  words = unlist(strsplit(str, " "))
  in.alphabet = grep(words, pattern = "[a-z|0-9]", ignore.case = T)
  nice.str = paste(words[in.alphabet], collapse = " ")
  nice.str
}

setwd("~/Mega/cloudy")
source("creds.R")
setup_twitter_oauth(api.key, api.secret, token, token.secret)

# tweets = searchTwitter("Obamacare OR ACA OR 'Affordable Care Act' OR #ACA", n = 100, lang = "en", since = "2016-01-01")
tweets = searchTwitter("#feelthebern", n = 2500, lang = "en")
tweets = sapply(tweets, function(x) x$getText())
tweets = sapply(tweets, function(x) rmNonAlphabet(x))
tweetCorpus = Corpus(VectorSource(tweets))
tweetCorpus = tm_map(tweetCorpus, PlainTextDocument)
tweetCorpus = tm_map(tweetCorpus, removePunctuation)
tweetCorpus = tm_map(tweetCorpus, removeNumbers)
tweetCorpus = tm_map(tweetCorpus, removeWords, c('RT', 'rt', stopwords('english')))
wordcloud(tweetCorpus, min.freq = 50, random.order = F, colors = c(brewer.pal(9, "Set1")))


