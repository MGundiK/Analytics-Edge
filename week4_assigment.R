#Assigment 2

letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
str(letters)
library(caTools)
set.seed(1000)
spl <- sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)
table(letters$isB)
#baseline accuracy
2350/(2350+766)
str(letters)
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)
predictCARTb = predict(CARTb, newdata = test, type ="class")
accutab = table(test$isB, predictCARTb)
accutab
accu_cartb = sum(diag(accutab))/nrow(test)
accu_cartb
(1118+340)/(1118+57+43+340)




#random forrest
library(randomForest)

lforrest = randomForest(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, data = train )

PredictForest = predict(lforrest, newdata = test)
accutab = table(test$isB, PredictForest)
accutab
accu = sum(diag(accutab))/nrow(test)
accu

letters$letter = as.factor( letters$letter )

set.seed(2000)
spl2 <- sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, spl == TRUE)
test2 = subset(letters, spl == FALSE)
table(test$letter)
#baseline accuracy
396/(nrow(test2))


396/(385+383+396+394)

letters$letter <- as.factor(letters$letter)
set.seed(2000)
spl <- sample.split(letters$letter, SplitRatio = 0.5)
train <- subset(letters, spl == TRUE)
test <- subset(letters, spl == FALSE)
table(test$letter)



CARTLetter <- rpart(letter ~ . - isB, data = train, method = "class")

prp(CARTLetter)
predictCARTLetter = predict(CARTLetter, newdata = test, type ="class")
accutab = table(test$letter, predictCARTLetter)
accutab
acculet = sum(diag(accutab))/nrow(test)
acculet

library(randomForest)
set.seed(1000)
letterforrest <- randomForest(letter ~ . - isB, data = train)

PredictForest = predict(letterforrest, newdata = test)
accutab = table(test$letter, PredictForest)
accutab
accu = sum(diag(accutab))/nrow(test)
accu


#Assigment 1


gerber = read.csv("gerber.csv")
str(gerber)
voted = subset(gerber, voting == 1)
nrow(voted)
nrow(voted)/nrow(gerber)

#alternativly
table(gerber$voting)
?tapply
tapply(gerber$voting, gerber$civicduty, mean)

tapply(gerber$voting, gerber$hawthorne, mean)

tapply(gerber$voting, gerber$self, mean)

tapply(gerber$voting, gerber$neighbors, mean)

votinglm = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)
summary(votinglm)
predictvote = predict(votinglm, type ="response")
table(gerber$voting, predictvote >= 0.3)
#accuracy of the logistic regression model
(134513+51966)/(134513 + 100875 + 56730 + 51966)

table(gerber$voting, predictvote >= 0.5)
#accuracy
235388/nrow(gerber)
?nrow

235388/nrow(gerber)
library(ROCR)
predictTest = predict(votinglm, type="response")
ROCRpredTest = prediction(predictTest, gerber$voting)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
#weak model, AUC = 0.5 -> random guessing


# We will now try out trees. Build a CART tree for voting using all data and 
# the same four treatment variables we used before. 
# Don't set the option method="class" - we are actually going to create 
# a regression tree here.

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)



CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#Using only the CART tree plot, determine what fraction (a number between 0 and 1)
# of "Civic Duty" people voted:


# ou can find this answer by reading the tree - 
# the people in the civic duty group correspond to the bottom right split,
# which has value 0.31 in the leaf.



CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

#In the control group, which gender is more likely to vote? For the control group,
#which corresponds to the bottom left, sex = 0 (male) corresponds 
# to a higher voting percentage.

#In the "Civic Duty" group, which gender is more likely to vote?

#For the civic duty group, which corresponds to the bottom right, sex = 0 (male)
# corresponds to a higher voting percentage.
str(gerber)




CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
summary(CARTmodel4)
abs(0.2966383 - 0.3400004)

CARTmodel5 = rpart(voting ~ control+sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

#Now, using the second tree (with control and sex), determine who 
#is affected more by NOT being in the control group 
#(being in any of the four treatment groups):
#
#You can plot the second tree using the command:
  
#  prp(CARTsex, digits=6)

#The first split says that if control = 1, go left. Then, if sex = 1 (female) 
#predict 0.290456, and if sex = 0 (male) predict 0.302795.
#On the other side of the tree, where control = 0, if sex = 1 (female)
#predict 0.334176, and if sex = 0 (male) predict 0.345818. 
#So for women, not being in the control group increases the fraction voting
#by 0.04372. For men, not being in the control group increases 
#the fraction voting by 0.04302. So men and women are affected about the same.



#Going back to logistic regression now, create a model using “sex” and “control”

ControlSex_lr <- glm(voting ~ sex + control, data = gerber, family = binomial)
summary(ControlSex_lr)
#If you look at the summary of the model, you can see that the coefficient 
#for the "sex" variable is -0.055791. This means that women are less likely 
#to vote, since women have a larger value in the sex variable, and a negative 
#coefficient means that larger values are predictive of 0.

# PROBLEM 3.4 - INTERACTION TERMS  

#The regression tree calculated the percentage voting exactly for every one of the four
#possibilities (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control).
#Logistic regression has attempted to do the same, although it wasn't able to do as well 
#because it can't consider exactly the joint possibility of being a women and in the control
#group.

#We can quantify this precisely. Create the following dataframe (this contains all of the
#possible values of sex and control), and evaluate your logistic regression using the 
#predict function (where "LogModelSex" is the name of your logistic regression model 
#that uses both control and sex):


Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(ControlSex_lr, newdata=Possibilities, type="response")
#The four values in the results correspond to the four possibilities in the order
#they are stated above 
#( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) ). 

#What is the absolute difference between the tree and the logistic regression 
#for the (Woman, Control) case? Give an answer with five numbers after the decimal point.

#(Woman, Control) = 0.2908065 in logistic regression (ControlSex_lr)

#(woman, control) = 0.290456 in tree (CARTmodel5)
abs(0.2908065-0.290456)


#PROBLEM 3.5 - INTERACTION TERMS
#So the difference is not too big for this dataset, but it is there. 
#We're going to add a new term to our logistic regression now, 
#that is the combination of the "sex" and "control" variables - 
#so if this new variable is 1, that means the person is a woman AND in the control group.
#We can do that with the following command:

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
#How do you interpret the coefficient for the new variable in isolation? 
#That is, how does it relate to the dependent variable?


#Ans
#If a person is a woman and in the control group, the chance that she voted goes down.
#- correct

predict(LogModel2, newdata=Possibilities, type="response")

#(Woman, Control) = 0.2904558 in logistic regression (LogModel2)
#(woman, control) = 0.290456  in tree (CARTmodel5)
abs(0.2908065 - 0.290456)
ans = 0.290456-0.2904558
ans
0.0000002

#The logistic regression model now predicts 0.2904558 for the (Woman, Control) case,
#so there is now a very small difference (practically zero) between CART and 
#logistic regression.


# Assigment 3 
#PREDICTING EARNINGS FROM CENSUS DATA

census = read.csv("census.csv")
str(census)
set.seed(2000)
library(caTools)
split = sample.split(census$over50k, SplitRatio=0.6)
train = subset(census, split==TRUE)
test = subset(census, split == FALSE)

logReg1 = glm(over50k ~., data = train, family = binomial)
summary(logReg1)
pred1 = predict(logReg1, type="response", newdata=test)
t1 = table(test$over50k, pred1 >= 0.5)
#accuracy of the logistic regression model
t1
acct1 = sum(diag(t1))/nrow(test)
acct1

#baseline accuracy 
table(test$over50k)
# <50k more frequent
9713/nrow(test)
#What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)
ROCRpred1 = prediction(pred1, test$over50k)
as.numeric(performance(ROCRpred1, "auc")@y.values)

# Build a regression tree model
library(rpart)
library(rpart.plot)

CART1 <- rpart(over50k ~ ., data=train, method="class")
prp(CART1, tweak=1.9)

#PROBLEM 2.4 - A CART MODEL

# Predict making over 50k
pred2 <- predict(CART1, newdata=test, type="class")
# Accuracy
t2 <- table(test$over50k, pred2)
(t2[1,1] + t2[2,2]) / nrow(test)
# This highlights a very regular phenomenon when comparing CART and logistic regression.
#CART often performs a little worse than logistic regression in out-of-sample accuracy. 
#However, as is the case here, the CART model is often much simpler to describe and 
#understand. 

# Performance of the model
PredictROC = predict(CART1, newdata = test)
pred = prediction(PredictROC[,2], test$over50k)
perf = performance(pred, "tpr", "fpr")  #tpr: true positive rate. 
plot(perf, colorize = TRUE)
# Compute the AUC
as.numeric(performance(pred, "auc")@y.values)

set.seed(1)
library(caTools)
split = sample.split(census$over50k, SplitRatio=0.6)
train = subset(census, split==TRUE)
test = subset(census, split == FALSE)

trainSmall = train[sample(nrow(train), 2000), ]
library(randomForest)
CensusForest = randomForest(over50k ~ .- nativecountry, data = trainSmall)
pred3 = predict(CensusForest, newdata=test)
# Accuracy
t3 <- table(test$over50k, pred3)
(t3[1,1] + t3[2,2]) / nrow(test)
# metric
vu = varUsed(CensusForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(CensusForest$forest$xlevels[vusorted$ix]))
# impurity metric

varImpPlot(CensusForest)

# PROBLEM 4.1 - SELECTING CP BY CROSS-VALIDATION
library(caret)
library(e1071)
set.seed(2)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 )

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 

tr = train(over50k ~ ., data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
tr
# Build CART model with cp=0.02

CART4 = rpart(over50k ~ ., data=train, cp=0.002)
prp(CART4)

# Accuracy

pred6 = predict(CART4, newdata = test, type="class")
t4 = table(test$over50k, pred6)
(t4[1,1] + t4[2,2])/nrow(test)

prp(CART4)
text(CART4)
summary(CART4)

model = rpart(over50k~., data=train, method="class", cp=0.002)
prp(model)
summary(model)
print(model)



# Use only Area as the predictor
set.seed(111)
tr1 <- train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
CART5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.01)
prp(CART5)
# SSE
pred7 <- predict(CART5)
sum((pred7 - statedata$Life.Exp)^2)



CARTmodel_final <- rpart(over50k ~ ., data = train, method = "class",cp=0.002)
prediction <- predict(CARTmodel_final, newdata = test, type = "class")
table(test$over50k, prediction)
prp(CARTmodel_final)




















