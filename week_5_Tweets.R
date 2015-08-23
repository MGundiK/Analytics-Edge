# Unit 5: Turning tweets into knowledge
# VIDEO 5: PRE-PROCESSING IN R

tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)

str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)

install.packages("tm")

library(tm)

install.packages("SnowballC")

library(SnowballC)

#create corpus
corpus = Corpus(VectorSource(tweets$Tweet))

corpus
corpus[[1]]$content

#convert to lowercase
corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)
#corpus = tm_map(corpus, content_transformer(tolower))
corpus[[1]]

#remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]
corpus[[1]]$content

#remove stop words
stopwords("english")[1:10]

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
#removes the word apple and all of the english stop words

corpus[[1]]
corpus[[1]]$content

#stem document
corpus = tm_map(corpus, stemDocument)
corpus[[1]]
corpus[[1]]$content


# VIDEO 6: BAG OF WORDS IN R

# extract word frequencies

frequencies = DocumentTermMatrix(corpus)

frequencies

inspect(frequencies[1000:1005, 505:515])

findFreqTerms(frequencies, lowfreq=20)

sparse = removeSparseTerms(frequencies, 0.995)
sparse

# convert sparse matrix into a data frame
tweetsSparse = as.data.frame(as.matrix(sparse))

#remove names that are numbers
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

#add dependant variables to data set

tweetsSparse$Negative = tweets$Negative

# split data

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio=0.7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)

# Which of the following words appear at least 100 times?
findFreqTerms(frequencies, lowfreq=100)

#VIDEO 7: PREDICTING SENTIMENT
# build CART model

library(rpart)
library(rpart.plot)


tweetCART = rpart(Negative ~., data = trainSparse, method="class")
prp(tweetCART)

#Our tree says that if the word "freak" is in the tweet,
#then predict TRUE, or negative sentiment.
#If the word "freak" is not in the tweet,
#but the word "hate" is, again predict TRUE.
#If neither of these two words are in the tweet,
#but the word "wtf" is, also predict TRUE, or negative
#sentiment.
#If none of these three words are in the tweet,
#then predict FALSE, or non-negative sentiment.

predictCART = predict(tweetCART, newdata=testSparse, type="class")

conmat = table(testSparse$Negative, predictCART)
conmat

#accuracy

acc = sum(diag(conmat))/nrow(testSparse)
acc

#Let's compare this to a simple baseline model
#that always predicts non-negative.

#To compute the accuracy of the baseline model,let's make a table of just the outcome variable Negative.
#So we'll type table, and then in parentheses,testSparse$Negative.
#This tells us that in our test set we have 300 observations with non-negative sentiment
#and 55 observations with negative sentiment. So the accuracy of a baseline model

table(testSparse$Negative)
300/(300+55)

# Random forrest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data = trainSparse)

predictRF = predict(tweetRF, newdata=testSparse)

conmatRF = table(testSparse$Negative, predictRF)
conmatRF

#accuracy

accRF = sum(diag(conmatRF))/nrow(testSparse)
accRF

# ogistic regression

tweetLog = glm(Negative ~.,  data = trainSparse, family = binomial)

predictions = predict(tweetLog, newdata=testSparse, type="response")

conmat_glm = table(testSparse$Negative, predictions >= 0.5)
conmat_glm

accu_glm = sum(diag(conmat_glm))/nrow(testSparse)
accu_glm










# RECITATION - THE STORY OF ENRON

emails = read.csv("energy_bids.csv", stringsAsFactors = FALSE)

str(emails)

emails$email[1]

emails$responsive[1]

emails$email[2]

emails$responsive[2]

table(emails$responsive)

#PRE-PROCESSING
library(tm)

corpus = Corpus(VectorSource(emails$email))

corpus[[1]]$content

#convert to lower case

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)


#remove punctuation

corpus = tm_map(corpus, removePunctuation)

#remove stop words

corpus = tm_map(corpus, removeWords, stopwords("english"))

#stem the document

corpus = tm_map(corpus, stemDocument)

corpus[[1]]$content

length(stopwords("english")) 

#BAG OF WORDS

#build document term matrix

dtm = DocumentTermMatrix(corpus)

#summary
dtm

#remove terms that is not found in at least 3% of documents
dtm = removeSparseTerms(dtm, 0.97)

dtm

#build data frame
labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive  = emails$responsive

str(labeledTerms)


#BUILDING MODELS

library(caTools)

set.seed(144)

spl = sample.split(labeledTerms$responsive, 0.7)

train = subset(labeledTerms, spl == TRUE)

test = subset(labeledTerms, spl == FALSE)

#build CART model

library(rpart)
library(rpart.plot)

emailCART = rpart(responsive~., data = train, method="class")

prp(emailCART)

#EVALUATING THE MODEL

#evaluate model on a test set

pred = predict(emailCART, newdata=test)

#select first 10 rows and all the collumns

pred[1:10,]

#we want to extract
#the predicted probability of the document being responsive.
#So we're looking for the rightmost column.
#So we'll create an object called pred.prob.and select 2nd column

pred.prob = pred[,2]

#accuracy

am = table(test$responsive, pred.prob >= 0.5)

acc = sum(diag(am))/nrow(test)
acc

#compare accuracy to base line model (predicts always that the document is non responsive)
table(test$responsive)

215/(215+42)
# just a small improvement in accuracy using the CART model


#THE ROC CURVE

library(ROCR)

prdROCR = prediction(pred.prob, test$responsive)

#plot ROCR curve (performance)

perfROCR = performance(prdROCR, "tpr", "fpr")

plot(perfROCR, colorize = TRUE)

performance(prdROCR, "auc")@y.values

#AUC in the test set of 79.4%, which
#means that our model can differentiate
#between a randomly selected responsive and non-responsive
#document about 80% of the time.













