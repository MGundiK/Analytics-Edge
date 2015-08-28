sd(c(5,8,12))
which.min(c(4,1,6))
c(2,3,5,8, 13)
Country <- c("Brazil","China","India", "Switzerland", "USA")
LifeExpectancy <- c(74,76,65,83,79)
Country
LifeExpectancy
seq(0,100,2)
CountryData <- data.frame(Country, LifeExpectancy)
CountryData
CountryData$Population <- c(199000, 1390000,1240000, 7997,318000)
CountryData
Country <- c("Australia", "Greece")
LifeExpectancy <- c(82, 81)
Population <- c(23050, 11125)
NewCountryData <- data.frame(Country, LifeExpectancy, Population)
NewCountryData
AllCountryData <- rbind(CountryData, NewCountryData)
AllCountryData
WHO <- read.csv("WHO.csv")
str(WHO) #data structure
summary(WHO)
WHO_Europe <- subset(WHO, Region == "Europe")
str(WHO_Europe)
write.csv(WHO_Europe, "WHO_Europe.csv")
ls()
rm(WHO_Europe) #remove from list
ls()
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]
plot(WHO$GNI, WHO$FertilityRate)
Outliers <- subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country", "GNI", "FertilityRate")]

#Quiz
#What is the mean value of the "Over60" variable?
a <- mean(WHO$Over60)
a
#Which country has the smallest percentage of the population over 60?
which.min(WHO$Over60)
WHO$Country[183]
#Which country has the largest literacy rate?
which.max(WHO$LiteracyRate)
WHO$Country[44]
#End Quiz

#VIDEO 6: DATA ANALYSIS - PLOTS AND SUMMARY TABLES

hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy", main = "Life Expectancy of Countryes by Region")
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, mean)
#This splits the observations by Region
#and then computes the mean of the variable Over60.
#So tapply splits the data by the second argument you give,
#and then applies the third argument function
#to the variable given as the first argument.
#This result tells us that the average percentage
#of the population over 60 in 
#African countries is about 5%,
#while the average percentage of the population over 60
#in European countries is about 20%.

tapply(WHO$LiteracyRate, WHO$Region, min)

tapply(WHO$LiteracyRate, WHO$Region, min, na.rm = TRUE)

#Quiz
#Use the tapply function to find the average child mortality rate of countries in each region.

#Which region has the lowest average child mortality rate across all countries in that region?

tapply(WHO$ChildMortality, WHO$Region, min)
#Answer Europe
#End Quiz


#VIDEO 2: WORKING WITH DATA IN R

USDA <- read.csv("USDA.csv")
str(USDA)
summary(USDA)
USDA$Sodium
which.max(USDA$Sodium)
USDA$Sodium[265]
names(USDA)
USDA$Description[265]
HighSodium <- subset(USDA, Sodium > 10000)
HighSodium
nrow(HighSodium)
HighSodium$Description
match("CAVIAR", USDA$Description)
USDA$Sodium[4154]
#Or we could do it like
USDA$Sodium[match("CAVIAR", USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium)
sd(USDA$Sodium, na.rm = TRUE)

#VIDEO 4: CREATING PLOTS IN R

plot(USDA$Protein, USDA$TotalFat)
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab="Fat", main="Protein vs Fat", col = "red")
hist(USDA$VitaminC, xlab ="Vitamin C (mg)", main="Histogram of Vitamin C Levels")
hist(USDA$VitaminC, xlab ="Vitamin C (mg)", main="Histogram of Vitamin C Levels", xlim = c(0,100))
hist(USDA$VitaminC, xlab ="Vitamin C (mg)", main="Histogram of Vitamin C Levels", xlim = c(0,100), breaks = 100)
hist(USDA$VitaminC, xlab ="Vitamin C (mg)", main="Histogram of Vitamin C Levels", xlim = c(0,100), breaks = 2000)
boxplot(USDA$Sugar, ylab = "Sugar (g)", main = "Boxplot of Sugar Levels")

#VIDEO 5: ADDING VARIABLES

USDA$Sodium[1] > mean(USDA$Sodium, na.rm = TRUE)
USDA$Sodium[50] > mean(USDA$Sodium, na.rm = TRUE)
HighSodium <- USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE)
str(HighSodium)

#change from logical to numeric tipe

HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
str(HighSodium)
USDA$HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
str(USDA)

USDA$HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighFat <- as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbs <- as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))
str(USDA)

#VIDEO 6: SUMMARY TABLES

table(USDA$HighSodium)

table(USDA$HighSodium, USDA$HighFat)
#The rows belong to the first input, which is HighSodium,
#and the columns correspond to the second input,
#which is HighFat.
#So from the table we see that we have
#3,529 foods with low sodium and low fat,
#1,355 foods with low sodium and high fat, 1,378
#foods with high sodium but low fat,
#and finally 712 foods with both high sodium and high fat.

#Now, what if we want to compute the average amount of iron sorted by high and low protein?
#Well, to do this we can use the tapply function.

#The tapply function takes three arguments, and groups the first argument 
#according to the second argument, and then applies argument three.
#For instance, we wanted to compute the average amount of iron sorted by high and low protein.
#In this case, the first argument is whatever we are trying to analyze,
#which is the Iron vector, and we are sorting it according to the vector HighProtein, which
#is our second argument.

tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)

#Now how about the maximum level of vitamin C in foods with high and low carbs?

tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm = TRUE)

tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm = TRUE)
#We obtain the following two sets of information.
#The first set corresponds to the foods with low carb content,
#and the second set of information corresponds
#to foods with higher carb content than average.



