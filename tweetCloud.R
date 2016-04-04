library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)

rmNonAlphabet = function(str) {
  words = unlist(strsplit(str, " "))
  in.alphabet = grep(words, pattern = "[a-z|0-9]", ignore.case = T)
  nice.str = paste(words[in.alphabet], collapse = " ")
  nice.str
}

removeURL = function(x) gsub("http*", "", x)

setwd("~/Mega/cloudy")
source("creds.R")
setup_twitter_oauth(api.key, api.secret, token, token.secret)

tweets = searchTwitter("Obamacare OR ACA OR 'Affordable Care Act' OR #ACA", n = 500, lang = "en")
tweet.df = twListToDF(tweets)
tweetCorpus = Corpus(VectorSource(tweet.df$text))
tweetCorpus = tm_map(tweetCorpus, content_transformer(tolower))
tweetCorpus = tm_map(tweetCorpus, removePunctuation)
tweetCorpus = tm_map(tweetCorpus, removeNumbers)
tweetCorpus = tm_map(tweetCorpus, content_transformer(rmNonAlphabet))
tweetCorpus = tm_map(tweetCorpus, content_transformer(removeURL))
tweetCorpus = tm_map(tweetCorpus, removeWords, c('rt', stopwords('english')))
# tweetCorpus = tm_map(tweetCorpus, stemDocument)

m = as.matrix(TermDocumentMatrix(tweetCorpus))
word.freq = sort(rowSums(m), decreasing = T)
dm = data.frame(word = names(word.freq), freq = word.freq)

wordcloud(dm$word, dm$freq, min.freq = 5, random.order = F, colors = brewer.pal(8, "Dark2"))
