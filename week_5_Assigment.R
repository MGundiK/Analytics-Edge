#DETECTING VANDALISM ON WIKIPEDIA

wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)
summary(wiki)
nvandal = subset(wiki,Vandal == 1)
nrow(nvandal)

#simpler solution
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

wiki$Added[1]

#PRE-PROCESSING
library(tm)

corpus = Corpus(VectorSource(wiki$Added))

corpus[[1]]$content

#The text already is lowercase and stripped of punctuation.

#1) Create the corpus for the Added column, and call it "corpusAdded".

corpusAdded = Corpus(VectorSource(wiki$Added))

corpusAdded[[1]]

corpusAdded[[1]]$content

#2) Remove the English-language stopwords.

#remove stop words

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

#3) Stem the words.

#stem the document

corpusAdded = tm_map(corpusAdded, stemDocument)

corpusAdded[[1]]$content

length(stopwords("english")) 

#4) Build the DocumentTermMatrix, and call it dtmAdded.

#build document term matrix

dtmAdded = DocumentTermMatrix(corpusAdded)

#summary
dtmAdded

#Filter out sparse terms by keeping only terms that appear in 0.3% or more of 
#the revisions, and call the new matrix sparseAdded. 
#How many terms appear in sparseAdded?

#remove terms that is not found in at least 3% of documents
sparseAdded = removeSparseTerms(dtmAdded, 0.997)

sparseAdded

#Convert sparseAdded to a data frame called wordsAdded,

#build data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))
wordsAdded$Added  = wiki$Added

str(wordsAdded)


#and then prepend all the words with the letter A, by using the command:

colnames(wordsAdded) = paste("A", colnames(wordsAdded))

str(wordsAdded)

#Now repeat all of the steps we've done so far (create a corpus, remove stop words, 
#stem the document, create a sparse document term matrix, and convert it to a data 
#frame) to create a Removed bag-of-words dataframe, called wordsRemoved, except this 
#time, prepend all of the words with the letter R:

#colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#1) Create the corpus for the Added column, and call it "corpusRemoved".

corpusRemoved = Corpus(VectorSource(wiki$Removed))

corpusRemoved[[1]]

corpusRemoved[[1]]$content

#2) Remove the English-language stopwords.

#remove stop words

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

#3) Stem the words.

#stem the document

corpusRemoved = tm_map(corpusRemoved, stemDocument)

corpusRemoved[[1]]$content

length(stopwords("english")) 

#4) Build the DocumentTermMatrix, and call it dtmRemoved.

#build document term matrix

dtmRemoved = DocumentTermMatrix(corpusRemoved)

#summary
dtmRemoved

#Filter out sparse terms by keeping only terms that appear in 0.3% or more of 
#the revisions, and call the new matrix sparseRemoved. 


#remove terms that is not found in at least 3% of documents
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)

sparseRemoved

#Convert sparseAdded to a data frame called wordsRemoved,

#build data frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
wordsRemoved$Removed  = wiki$Removed

str(wordsRemoved)

#and then prepend all the words with the letter R, by using the command:

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

str(wordsRemoved)

#How many words are in the wordsRemoved data frame?

# Creating wordsRemoved dataset
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(dtmRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
# How many words are in the wordsRemoved data frame?
str(wordsRemoved)



#Combine the two data frames into a data frame called wikiWords
#with the following line of code:

wikiWords = cbind(wordsAdded, wordsRemoved)

wikiWords$Vandal = wiki$Vandal

table(wikiWords$Vandal)

# Creating training and test sets
library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)
table(test$Vandal)

#baseline accuracy

618/(618+545)

#Build a CART model to predict Vandal, using all of the other variables 
#as independent variables.
#Use the training set to build the model and the default parameters 
#(don't set values for minbucket or cp).

#build CART model

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal~., data=train, method="class")
pred = predict(wikiCART, newdata=test, method="class")
# What is the accuracy of the model on the test set, using a threshold of 0.5?
pred.prob = pred[,2]
table(test$Vandal, pred.prob >= 0.5)
# Plot the CART tree. How many word stems does the CART model use?
prp(wikiCART)



















