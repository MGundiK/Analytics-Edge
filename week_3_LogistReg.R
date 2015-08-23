-1.5 + 3*1  + -0.5*5
exp(-1)
1/(1 + exp(1))


quality <- read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
#baseline model accuracy:
98/131

install.packages("caTools")

library(caTools)

#randomly split data in to training set and testing set
set.seed(88)
#75% of data goes in to training set and 25% in to testing set
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
split

# TRUE - put observation in training set,
# FALSE - put observation in testing set
qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

# build model
# glm - generalised linear model
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)

# prefered model is the one with minimal AIC

predictTrain <- predict(QualityLog, type = "response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)


model10 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
# StartedOnCombination is a binary variable, which equals 1 if the patient is 
# started on a combination of drugs to treat their diabetes, and equals 0 if the patient 
# is not started on a combination of drugs. All else being equal, 
# does this model imply that starting a patient on a combination of drugs is indicative of 
# poor care, or good care?
summary(model10)

# The coefficient value is positive, meaning that positive values of the variable 
# make the outcome of 1 more likely. This corresponds to Poor Care.

# VIDEO 5: THRESHOLDING


table(qualityTrain$PoorCare, predictTrain > 0.5)

# sensetivity - true positive rate
10/25

# specificity - true negative rate
70/74

table(qualityTrain$PoorCare, predictTrain > 0.7)
# sensetivity 
8/25
# specificity
73/74

table(qualityTrain$PoorCare, predictTrain > 0.2)
# sensetivity 
16/25
# specificity
54/74

#increase treshold -> sensetivity increases???

# VIDEO 6: ROC CURVES

install.packages("ROCR")
library(ROCR)
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))


predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
