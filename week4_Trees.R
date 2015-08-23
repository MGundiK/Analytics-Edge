stevens <- read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)
train = subset(stevens, spl == TRUE)
test = subset(stevens, spl == FALSE)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 25)
prp(StevensTree)
predictCart = predict(StevensTree, newdata = test, type ="class")
table(test$Reverse, predictCart)
#accuracy
(41+71)/(41+36+22+71)
library(ROCR)
predictROCR = predict(StevensTree, newdata = test)
predictROCR
pred = prediction(predictROCR[ ,2], test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

# Quiz

StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 5)
prp(StevensTree2)
predictCart = predict(StevensTree2, newdata = test, type ="class")
table(test$Reverse, predictCart)


StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 100)
prp(StevensTree3)
predictCart = predict(StevensTree2, newdata = test, type ="class")
table(test$Reverse, predictCart)


################################################################################

# RANDOM FORESTS

install.packages("randomForest")
library(randomForest)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
PredictForest = predict(StevensForest, newdata = test)
table(test$Reverse, PredictForest)

# accuracy
(40+74)/(40+37+19+74)

# Quiz
# Let's see what happens if we set the seed to two different values and create two different
# random forest models.

#First, set the seed to 100, and the re-build the random forest model, exactly like we did in 
# the previous video (Video 5). Then make predictions on the test set. 
# What is the accuracy of the model on the test set?

#Ans
#You can create the models and compute the accurracies with the following commands in R:

set.seed(100)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)
PredictForest = predict(StevensForest, newdata = test)
table(test$Reverse, PredictForest)
#accuracy
(74+43)/(43+34+19+74)

# Now, set the seed to 200, and then re-build the random forest model, 
# exactly like we did in the previous video (Video 5). Then make predictions on the test set.
#What is the accuracy of this model on the test set?

#Ans
set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)
PredictForest = predict(StevensForest, newdata = test)
table(test$Reverse, PredictForest)
#accuracy
(44+76)/(44+11+17+76)


install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
numfolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "rpart", trControl = numfolds, tuneGrid = cpGrid)

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", cp = 0.19)
PredictCV = predict(StevensTreeCV, newdata = test, type = "class")
table(test$Reverse, PredictCV)
#accuracy
(59+64)/(59+18+29+64)

# Quiz
# Plot the tree that we created using cross-validation. How many splits does it have?
prp(StevensTreeCV)
#Ans 1






#################################################################

#################################################################




# VIDEO 6: CLAIMS DATA IN R

Claims = read.csv("ClaimsData.csv")
str(Claims)
table(Claims$bucket2008, Claims$bucket2009)

table(Claims$bucket2008)/ nrow(Claims)
library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio=0.6)
ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest = subset(Claims, spl == FALSE)

# Quiz
# What is the average age of patients in the training set, ClaimsTrain?
summary(ClaimsTrain)

# What proportion of people in the training set (ClaimsTrain) had 
# at least one diagnosis code for diabetes?
a = subset(Claims, Claims$diabetes == 1)
nrow(a)
nrow(a)/nrow(Claims)
# EndQuiz

#VIDEO 7: BASELINE METHOD AND PENALTY MATRIX

table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
#baseline accuracy
(110138+10721+2774+1539 +104)/nrow(ClaimsTest)
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
PenaltyMatrix
#Outcomes = left column
#Predicted (Outcomes) = top row

#So now to compute the penalty error of the baseline method,
#we can multiply our classification matrix
#by the penalty matrix




#And we're going to surround the entire table function
#by as.matrix to convert it to a matrix
#so that we can multiply it by our penalty matrix.

#multiply matrix
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix

#So what this does is it takes each number
#in the classification matrix and multiplies it
#by the corresponding number in the penalty matrix.

#So now to compute the penalty error,
#we just need to sum it up and divide
#by the number of observations in our test set.


sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)

#So the penalty error for the baseline method is 0.74.
#In the next video, our goal will be
#to create a CART model that has an accuracy higher than 68%
#and a penalty error lower than 0.74.

# Quiz
# Suppose that instead of the baseline method discussed in the previous video, 
# we used the baseline method of predicting the most frequent outcome for all observations.
# This new baseline method would predict cost bucket 1 for everyone.

# What would the accuracy of this baseline method be on the test set?


table(Claims$bucket2009)/nrow(Claims)

round(100.0*table(Claims$bucket2009)/nrow(Claims), 2)
#The vast majority of patients in this data set have low cost.


hist(log10(Claims$reimbursement2008), xlab = "log10(2008 reimbursements)", ylab = "Frequency", main = "Distribution of 2008 Reimbursements", col = "orange")

hist(Claims$reimbursement2008, xlab = "2008 reimbursements", ylab = "Frequency", main = "Distribution of 2008 Reimbursements", col = "orange")

#We can check the total cost by bucket:

tot2008 <- sum(Claims$reimbursement2008)
#group_by(Claims, bucket2008) %>% summarise(cost2008 = sum(reimbursement2008), frac2008 = cost2008/tot2008)

library("magrittr")
library("ggplot2")

cmat_base <- table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
base_accu <- sum(diag(cmat_base))/nrow(ClaimsTest)
base_accu
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix
base_penalty_error <- sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)
base_penalty_error

cmat_all1 <- matrix(c(rowSums(cmat_base), rep(0, 20)), byrow = FALSE, nrow = 5)
cmat_all1
?rep
cmat_base
rowSums(cmat_base)
110138+7787+3427+1452+174
#Accuracy
sum(cmat_all1[,1])
accu_base_all_1 <- cmat_all1[1,1]/nrow(ClaimsTest)
accu_base_all_1 
cmat_all1[1,1]
nrow(ClaimsTest)

#Penalty error:
# as.matrix(cmat_all1*PenaltyMatrix)
# sum(as.matrix(cmat_all1*PenaltyMatrix))
altbase_penalty_error <- sum(as.matrix(cmat_all1*PenaltyMatrix))/nrow(ClaimsTest)
altbase_penalty_error

###################################################################

#EXPLANATION

#To compute the accuracy, you can create a table of the variable 
# ClaimsTest$bucket2009:

table(ClaimsTest$bucket2009)

# According to the table output, this baseline method would get 122978 
# observations correct, and all other observations wrong. So the accuracy 
# of this baseline method is 122978/nrow(ClaimsTest) = 0.67127.

# For the penalty error, since this baseline method predicts 1 for 
# all observations, it would have a penalty error of:
  
(0*122978 + 2*34840 + 4*16390 + 6*7937 + 8*1057)/nrow(ClaimsTest) 
# = 1.044301

########################################################################

# VIDEO 8: PREDICTING HEALTHCARE COSTS IN R


library(rpart)
library(rpart.plot)
str(Claims)
ClaimsTree = rpart(bucket2009 ~ age + arthritis+alzheimers+cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", cp = 0.00005 )
prp(ClaimsTree)
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
classmatrix = table(ClaimsTest$bucket2009, PredictTest)
classmatrix
#accuracy
acc8 = sum(diag(classmatrix))/nrow(ClaimsTest)
acc8
#penalty

penalty8 = sum(as.matrix(classmatrix*PenaltyMatrix))/nrow(ClaimsTest)
penalty8

ClaimsTree = rpart(bucket2009 ~ age + arthritis+alzheimers+cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", cp = 0.00005, parms = list(loss = PenaltyMatrix) )
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
classmatrix = table(ClaimsTest$bucket2009, PredictTest)
classmatrix
sum(diag(classmatrix))/nrow(ClaimsTest)
penalty8 = sum(as.matrix(classmatrix*PenaltyMatrix))/nrow(ClaimsTest)
penalty8





















