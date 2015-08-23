framingham <- read.csv("framingham.csv")
str(framingham)
#split data
library(caTools)
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
# 65% of data in the training set

train <- subset(framingham, split == TRUE)
test <- subset(framingham, split == FALSE)
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)
# now test our model on test set
predictTest = predict(framinghamLog, type = "response", newdata = test)
#create a confussion matrix with given treshold
table(test$TenYearCHD, predictTest > 0.5)
# accuracy of the model
(1081 + 16)/(1081+13+176+16)
#baseline accuracy
(1081 + 13)/(1081+13+176+16)
# our model barely beats the baseline model prediction
#compute out of sample AUC (Area Under the Curve) by varying the treshold
library(ROCR)
#prediction
ROCRpred <- prediction(predictTest, test$TenYearCHD)
#predictTest = our predictions
#test$TenYearCHD = true outcome
as.numeric(performance(ROCRpred, "auc")@y.values)

# What is the sensitivity of our logistic regression model on the test set,
# using a threshold of 0.5?

# sensitivity = True Positives (TP)/(True Positives (TP) + False Negatives (FN))
11/(11+187)

# What is the specificity of our logistic regression model on the test set,
# using a threshold of 0.5?
# specificity = True Negatives (TN)/(True Negatives (TN) + False Positives (FP))
1069/(1069+6)


