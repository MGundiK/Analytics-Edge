#PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT
#load data
stocks = read.csv("StocksCluster.csv")
str(stocks)

nrow(stocks)
#What proportion of the observations have positive returns in December?
table(stocks$PositiveDec)
6324/11580
#or simply
mean(stocks$PositiveDec)

#What is the maximum correlation between any two return variables in the dataset? 
cor(stocks)
#0.19167279 ReturnNov - ReturnOct

#Which month (from January through November) has the largest mean return across all observations in the dataset?
summary(stocks)
#April
#Which month (from January through November) has the smallest mean return across all observations in the dataset?
#September

#INITIAL LOGISTIC REGRESSION MODEL
library(caTools)
set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)

#train the model
StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)

#compute predictions on the training set 
PredictTrain = predict(StocksModel, type="response")

#construct a classification matrix
table(stocksTrain$PositiveDec, PredictTrain > 0.5)
#overall accuracy of the model
(990 + 3640)/(990 + 2689 + 787 + 3640)
#or
a=table(stocksTrain$PositiveDec, PredictTrain > 0.5)
sum(diag(a))/nrow(stocksTrain)


#INITIAL LOGISTIC REGRESSION MODEL

#compute predictions on the test set

PredictTest = predict(StocksModel, newdata=stocksTest, type="response")
#compute the classification matrix on the test set
b=table(stocksTest$PositiveDec, PredictTest > 0.5)
#overall accuracy of the model on the test set
sum(diag(b))/nrow(stocksTest)

#What is the accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)?
table(stocksTest$PositiveDec)
#baseline model would get all of the PositiveDec = 1 cases correct, 
#and all of the PositiveDec = 0 cases wrong
1897/(1577 + 1897) 

#CLUSTERING STOCKS
#remove the dependent variable
limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

#normalize by the mean and standard deviation of the variables in the training set

library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)
#What is the mean of the ReturnJan variable in normTrain?
mean(normTrain$ReturnJan)
#What is the mean of the ReturnJan variable in normTest?
mean(normTest$ReturnJan)

#Why is the mean ReturnJan variable much closer to 0 in normTrain than in normTest?
# The distribution of the ReturnJan variable is different in the training and testing set 

#From mean(stocksTrain$ReturnJan) and mean(stocksTest$ReturnJan),
#we see that the average return in January is slightly higher in the training set
#than in the testing set. 
#Since normTest was constructed by subtracting by the mean ReturnJan value
#from the training set, this explains why the mean value of ReturnJan is slightly
#negative in normTest.


set.seed(144)
#clustering with 3 clusters on normTrain
km = kmeans(normTrain, centers = 3)

#Which cluster has the largest number of observations?
table(km$cluster)


library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

#How many test-set observations were assigned to Cluster 2?
table(clusterTest)

#build data frames stocksTrain1, stocksTrain2, and stocksTrain3,
#containing the elements in the stocksTrain data frame assigned
#to clusters 1, 2, and 3, respectively
#take subsets of stocksTrain, not of normTrain

#Similarly build stocksTest1, stocksTest2, and stocksTest3 from 
#the stocksTest data frame.

#Which training set data frame has the highest average value of the dependent variable?
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)

stocksTrain2 = subset(stocksTrain, clusterTrain == 2)

stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)

stocksTest2 = subset(stocksTest, clusterTest == 2)

stocksTest3 = subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
#stocksTrain1 has the observations with the highest average value of the dependent variable

#Build logistic regression models StocksModel1, StocksModel2, 
#and StocksModel3, which predict PositiveDec using all the other variables 
#as independent variables. StocksModel1 should be trained on stocksTrain1,
#StocksModel2 should be trained on stocksTrain2, and StocksModel3 should be
#trained on stocksTrain3.

#Which variables have a positive sign for the coefficient in at least one 
#of StocksModel1, StocksModel2, and StocksModel3 and a negative sign 
#for the coefficient in at least one of StocksModel1, StocksModel2, and
#StocksModel3?

StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)

StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)

StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)
#ReturnJan, ReturnFeb, ReturnMar, ReturnJune, ReturnAug, and ReturnOct

#CLUSTER-SPECIFIC PREDICTIONS
#Using StocksModel1, make test-set predictions called PredictTest1 on 
#the data frame stocksTest1. 
#Using StocksModel2, make test-set predictions called PredictTest2 on 
#the data frame stocksTest2. 
#Using StocksModel3, make test-set predictions called PredictTest3 on 
#the data frame stocksTest3.

#What is the overall accuracy of StocksModel1 on the test set stocksTest1,
#using a threshold of 0.5?
#What is the overall accuracy of StocksModel2 on the test set stocksTest2,
#using a threshold of 0.5?
#What is the overall accuracy of StocksModel3 on the test set stocksTest3,
#using a threshold of 0.5?

#predictions
PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type="response")

PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type="response")

PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type="response")

#classification matrices 

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
#accuracy of StocksModel1 on test set
(30 + 774)/(30 + 471 + 23 + 774)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
#accuracy of StocksModel2 on test set
(388 + 757)/(388 + 626 + 309 + 757)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
#accuracy of StocksModel3 on test set
(49 + 13)/(49 + 13 + 21 + 13)

#compute the overall test-set accuracy of the cluster-then-predict approach
#combine all the test-set predictions into a single vector 
#and all the true outcomes into a single vector

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

#What is the overall test-set accuracy of the cluster-then-predict approach, again using a threshold of 0.5?
table(AllOutcomes, AllPredictions > 0.5)
a3=table(AllOutcomes, AllPredictions > 0.5)
sum(diag(a3))/sum(a3)
#modest improvement over the original logistic regression model

