trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(trials)
str(trials)
#The nchar() function counts the number of characters in a piece of text. 
#Using the nchar() function on the variables in the data frame, 
#answer the following questions:

#How many characters are there in the longest abstract? 
#(Longest here is defined as the abstract with the largest number of characters.)

max(nchar(trials$abstract))

#or

summary(nchar(trials$abstract)) 

# How many search results provided no abstract? 
str(subset(trials, nchar(trials$abstract) == 0))

#alternatively
table(nchar(trials$abstract) == 0)

#or

sum(nchar(trials$abstract) == 0)

#Find the observation with the minimum number of characters in the title 
#(the variable "title") out of all of the observations in this dataset. 
#What is the text of the title of this article? 
#Include capitalization and punctuation in your response, but don't include the quotes.

min(nchar(trials$title))
which.min(nchar(trials$title))
trials$title[which.min(nchar(trials$title))]

#PREPARING THE CORPUS

#Because we have both title and abstract information for trials, 
#we need to build two corpera instead of one. Name them corpusTitle and corpusAbstract.

#1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.

#2) Convert corpusTitle and corpusAbstract to lowercase. After performing this step, remember to run the lines:
  
#  corpusTitle = tm_map(corpusTitle, PlainTextDocument)

#corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

#3) Remove the punctuation in corpusTitle and corpusAbstract.

#4) Remove the English language stop words from corpusTitle and corpusAbstract.

#5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).

#6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.

#7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).

#8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).


library(tm)

# Preprocessing title variable
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)


# Preprocessing abstract variable
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95%
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# How many terms remain in dtmTitle and dtmAbstract after removing sparse terms
dtmTitle
dtmAbstract

# What is the most frequent word stem across all the abstracts? 
# Hint: you can use colSums() to compute the frequency of a word across all 
# the abstracts.

dtmAbstractMatrix = as.data.frame(as.matrix(dtmAbstract))
which.max(colSums(dtmAbstractMatrix))

#alternatively
csAbstract = colSums(dtmAbstract)
which.max(csAbstract)

# I does not work since we converted to dataframe

# PROBLEM 3 - BUILDING A MODEL
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)

dtmAbstractMatrix= as.data.frame(as.matrix(dtmAbstract))
dtmTitleMatrix = as.data.frame(as.matrix(dtmTitle))
dtm = cbind(dtmTitleMatrix, dtmAbstractMatrix)
dtm$trial = trials$trial
# How many columns are in this combined data frame?
str(dtm)

# Splitting data
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
# What is the accuracy of the baseline model on the training set?
table(train$trial)

# Building a CART model
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~., data=train, method="class")
# What is the name of the first variable the model split on?
prp(trialCART, tweak=1.9)

# What is the maximum predicted probability for any result?
predTrain = predict(trialCART, newdata=train, method="class")
predTrain = predTrain[, 2]
max(predTrain)


# What is the training set accuracy of the CART model?
table(train$trial, predTrain >= 0.5)
# 0.8233487
# Sensitivity (TPR) and Specificity (SPC)
TPR = 441/(131+441)
SPC = 631/(631+99)

TPR
SPC

# Evaluate the CART model on the testing set
# What is the testing set accuracy, assuming a probability threshold of 0.5
predTest = predict(trialCART, newdata=test, method="class")
predTest = predTest[, 2]
table(test$trial, predTest >= 0.5)
(261+162)/(261+52+83+162)

# Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)
predROCR = prediction(predTest, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
#plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values










