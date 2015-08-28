#DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES
CPS = read.csv("CPSData.csv")
str(CPS)
#PROBLEM 1.2 - LOADING AND SUMMARIZING THE DATASET 
#Among the interviewees with a value reported for the Industry variable,
#what is the most common industry of employment? 
summary(CPS) 
table(CPS$Industry)
#PROBLEM 1.3 - LOADING AND SUMMARIZING THE DATASET
#Which state has the fewest interviewees?
#Which state has the largest number of interviewees?
sort(table(CPS$State))

#PROBLEM 1.4 - LOADING AND SUMMARIZING THE DATASET
#What proportion of interviewees are citizens of the United States?
View(table(CPS$Citizenship))
123712/131302

#PROBLEM 1.5 - LOADING AND SUMMARIZING THE DATASET
#For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?

table(CPS$Race, CPS$Hispanic)
#PROBLEM 2.1 - EVALUATING MISSING VALUES  
#Which variables have at least one interviewee with a missing (NA) value?
View(summary(CPS))

#PROBLEM 2.2 - EVALUATING MISSING VALUES
#test the relationship between  variable values and whether the Married variable is missing 
table(CPS$Region, is.na(CPS$Married))

table(CPS$Sex, is.na(CPS$Married))

table(CPS$Age, is.na(CPS$Married))

table(CPS$Citizenship, is.na(CPS$Married))

#PROBLEM 2.3 - EVALUATING MISSING VALUES
#How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)?
#For this question, treat the District of Columbia as a state (even though it is not technically a state).
table(CPS$State, is.na(CPS$MetroAreaCode))

#How many states had all interviewees living in a metropolitan area? 
#Again, treat the District of Columbia as a state.

#PROBLEM 2.4 - EVALUATING MISSING VALUES
#Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))

#PROBLEM 2.5 - EVALUATING MISSING VALUES  
#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
#Wisconsin
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))
#Which state has the largest proportion of non-metropolitan interviewees, 
#ignoring states where all interviewees were non-metropolitan?
#Montana

#PROBLEM 3.1 - INTEGRATING METROPOLITAN AREA DATA
#import data
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
#number of observations
str(MetroAreaMap)
str(CountryMap)

#PROBLEM 3.2 - INTEGRATING METROPOLITAN AREA DATA
#connect the field MetroAreaCode from the CPS data frame with the field Code in MetroAreaMap
#merge the two data frames on these columns, overwriting the CPS data frame with the result
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

#by.x="MetroAreaCode" means we're matching on the MetroAreaCode variable from the "x" data frame (CPS)
#by.y="Code" means we're matching on the Code variable from the "y" data frame (MetroAreaMap)
#all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), 
#even if some of the rows' MetroAreaCode doesn't match any codes in MetroAreaMap

#What is the name of the variable that was added to the data frame by the merge() operation?
summary(CPS)
#MetroArea
#How many interviewees have a missing value for the new metropolitan area variable? 
#34238

#PROBLEM 3.3 - INTEGRATING METROPOLITAN AREA DATA
#Which of the following metropolitan areas has the largest number of interviewees?

table(CPS$MetroArea)

#PROBLEM 3.4 - INTEGRATING METROPOLITAN AREA DATA
#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
tapply(CPS$Hispanic, CPS$MetroArea, mean)

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

#PROBLEM 3.5 - INTEGRATING METROPOLITAN AREA DATA
#determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

#PROBLEM 3.6 - INTEGRATING METROPOLITAN AREA DATA
#obtain the sorted list of proportions by metropolitan area

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

#We can see that Iowa City, IA had 2.9% of interviewees not finish high school, the smallest value of any metropolitan area.

#PROBLEM 4.1 - INTEGRATING COUNTRY OF BIRTH DATA
#merge in the country of birth information from the CountryMap data frame, replacing the CPS data frame with the result
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
summary(CPS)

#PROBLEM 4.2 - INTEGRATING COUNTRY OF BIRTH DATA 
#Among all interviewees born outside of North America,
#which country was the most common place of birth?

sort(table(CPS$Country))

#839, for the Philippines

#PROBLEM 4.3 - INTEGRATING COUNTRY OF BIRTH DATA (2 points possible)
#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area
#have a country of birth that is not the United States? 
#For this computation, don't include people from this metropolitan area who have a missing country of birth.
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
1668/(1668+3736)

#PROBLEM 4.4 - INTEGRATING COUNTRY OF BIRTH DATA (3 points possible)
#Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India?
sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))























