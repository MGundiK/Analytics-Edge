# ASSIGMENT 3
# POPULARITY OF MUSIC RECORDS

songs <- read.csv("songs.csv")
summary(songs)
y2010 <- subset(songs, year == 2010)
nrow(y2010)
names(songs)
michael <- subset(songs, artistname == "Michael Jackson")
nrow(michael)
michaeltop <- subset(songs, artistname == "Michael Jackson" & Top10 == 1)
nrow(michaeltop)
View(michaeltop)
summary(songs$timesignature)
table(songs$timesignature)
summary(songs$tempo)
fastest <- subset(songs, tempo == 244.30)
fastest
which.max(songs$tempo)
songs$songtitle[6206]

SongsTrain <- subset(songs, year <= 2009)
str(SongsTrain)
SongsTest <- subset(songs, year == 2010)
str(SongsTest)
library(caTools)

# Model 1
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)
cor(SongsTrain$loudness, SongsTrain$energy)
# Model 2
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

# Model 3
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

predictTrain <- predict(SongsLog3, type = "response")
summary(predictTrain)
tapply(predictTrain, SongsTrain$Top10, mean)
table(SongsTrain$Top10, predictTrain > 0.45)

library(ROCR)
(5953 + 248)/(nrow(SongsTrain))

predictTest = predict(SongsLog3, type="response", newdata=SongsTest)
ROCRpredTest = prediction(predictTest, SongsTest$Top10)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

summary(ROCRpredTest)





# PREDICTING PAROLE VIOLATORS

parole <- read.csv("parole.csv")
summary(parole)
str(parole)
violator <- subset(parole, violator == 1)
nrow(violator)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
str(parole)
names(parole)
summary(parole$state) 
summary(parole$crime) 
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model10 <- glm(violator ~ male + race + age + state + time.served + max.sentence + multiple.offenses + crime, data = train, family = binomial)
summary(model10)


#PREDICTING LOAN REPAYMENT

loans <- read.csv("loans.csv")
str(loans)
summary(loans)
notPaid <- subset(loans, not.fully.paid == 1)
nrow(notPaid)
1533/9578
which(is.na(loans$Two))





