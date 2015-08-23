#VIDEO 4: LINEAR REGRESSION IN R
wine <- read.csv("wine.csv")
str(wine)
summary(wine)
model1 <- lm(Price ~ AGST, data = wine)
summary(model1)
model1$residuals
#Sum of Squared Errors
SSE <- sum(model1$residuals^2)
SSE
model2 <- lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
model2$residuals
SSE <- sum(model2$residuals^2)
SSE

model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
SSE <- sum(model3$residuals^2)
SSE
model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)

#Quiz
#Use the dataset wine.csv to create a linear regression model to predict Price
#using HarvestRain and WinterRain as independent variables, like you did in the previous
#quick question. Using the summary output of this model, answer the following questions:
#Is the coefficient for HarvestRain significant?

model5 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model5)

#HarvestRain is significant, while WinterRain is not

#VIDEO 6: CORRELATION AND MULTICOLLINEARITY

cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

model6 <- lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(model6)

wineTest <- read.csv("wine_test.csv")
str(wineTest)
predictTest <- predict(model4, newdata = wineTest)
predictTest
SSE <- sum((wineTest$Price - predictTest)^2)
SST <- sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST


#VIDEO 2: MAKING IT TO THE PLAYOFFS

baseball <- read.csv("baseball.csv")
str(baseball)
moneyball <- subset(baseball, Year < 2002)
str(moneyball)
#run difference
moneyball$RD = moneyball$RS - moneyball$RA
moneyball
str(moneyball)
plot(moneyball$RD, moneyball$W)

WinsReg <- lm(W ~ RD, data = moneyball)
summary(WinsReg)

# W = 80.881375 + 0.105766 *(RD)
# W >= 95
# 80.881375 + 0.105766 *(RD) >= 95
# RD >= (95 - 80.881375)/0.105766
(95 - 80.881375)/0.105766

# RD >= 133.4893 ~ 134 

# Quiz
# If a baseball team scores 713 runs and allows 614 runs,
# how many games do we expect the team to win?
# Using the linear regression model constructed during the lecture,
# enter the number of games we expect the team to win:
a <- 713 - 614 #run difference
80.881375 + 0.105766 *(a) # number of wins

# VIDEO 3: PREDICTING RUNS

str(moneyball)

RunsReg <- lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg)

RunsReg2 <- lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg2)

# Quiz
# If a baseball team's OBP is 0.311 and SLG is 0.405,
# how many runs do we expect the team to score?

# Runs = -804.63 + 2737.77 * (OBP) + 1584.91*(SLG)
-804.63 + 2737.77 * (0.311) + 1584.91*(0.405)
# Ans 688.705


# If a baseball team's opponents OBP (OOBP) is 0.297 
# and oppenents SLG (OSLG) is 0.370, how many runs do we expect the team to allow?
# -837.38 + 2913.60*(OOBP) + 1514.29*(OSLG) 
-837.38 + 2913.60*(0.297) + 1514.29*(0.370) 



# Quiz

teamRank = c(1,2,3,3,4,4,4,4,5,5)

wins2012 = c(94,88,95,88,93,94,98,97,93,94)

wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank, wins2012)

cor(teamRank, wins2013)

# Recitation

NBA <- read.csv("NBA_train.csv")
str(NBA)

#VIDEO 2: PLAYOFFS AND WINS

table(NBA$W, NBA$Playoffs)

NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)

WinsReg <- lm(W ~ PTSdiff, data = NBA)
summary(WinsReg)
# W = 4.100e+01 + 3.259e-02 * (PTSdiff)
# w >= 42
# PTSdiff = (42 - 4.100e+01)/3.259e-02
# 4.100e+01 = 41
# 3.259e-02 = 0.03259
(42 - 41)/0.03259

# VIDEO 3: POINTS SCORED

PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)

PointsReg$residuals
SSE <- sum(PointsReg$residuals^2)
SSE
RMSE <- sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)

PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB +  STL + BLK, data = NBA)
summary(PointsReg2)

PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB +   STL + BLK, data = NBA)
summary(PointsReg3)



PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)

SSE4 <- sum(PointsReg4$residuals^2)
RMSE4 <- sqrt(SSE4/nrow(NBA))
SSE4
RMSE4

#VIDEO 4: MAKING PREDICTIONS

NBA_test <- read.csv("NBA_test.csv")
PointsPrediction <- predict(PointsReg4, newdata = NBA_test)
SSE <- sum((PointsPrediction - NBA_test$PTS)^2)
SSE
SST <- sum((mean(NBA$PTS) - NBA_test$PTS)^2)
SST
R2 <- 1 - SSE/SST
R2

RMSE <- sqrt(SSE/nrow(NBA_test))
RMSE






