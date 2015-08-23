#VISUALIZING TEXT DATA USING WORD CLOUDS

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
library(tm)
library(SnowballC)
#create corpus

#convert to lowercase

#remove punctuation


#remove stop words


tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))
ncol(allTweets)


#PROBLEM 2.1 - BUILDING A WORD CLOUD

install.packages("wordcloud")
library(wordcloud)

?wordcloud

vecTweets = colSums(allTweets)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))


# Deleting the most frequentword
allTweets$apple = NULL
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))



# Playing with a color
install.packages("RColorBrewer")
library(RColorBrewer)
brewer.pal() 
display.brewer.all() 


library(wordcloud)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues"))
brewer.pal(9, "Blues")
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[c(-5, -6, -7, -8, -9)])
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[c(1, 2, 3, 4)])







