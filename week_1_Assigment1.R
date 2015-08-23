mvt = read.csv("mvtWeek1.csv")
#PROBLEM 1.1 - LOADING THE DATA
#How many rows of data (observations) are in this dataset?
#PROBLEM 1.2 - LOADING THE DATA
#How many variables are in this dataset?
str(mvt) 
#PROBLEM 1.3 - LOADING THE DATA  
#Using the "max" function, what is the maximum value of the variable "ID"?
max(mvt$ID)
#PROBLEM 1.4 - LOADING THE DATA
#What is the minimum value of the variable "Beat"?
min(mvt$Beat)
#PROBLEM 1.5 - LOADING THE DATA
#How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
summary(mvt)
#PROBLEM 1.6 - LOADING THE DATA
#How many observations have a LocationDescription value of ALLEY?
table(mvt$LocationDescription)
#PROBLEM 2.1 - UNDERSTANDING DATES IN R
#In what format are the entries in the variable Date?
mvt$Date[1]
#PROBLEM 2.2 - UNDERSTANDING DATES IN R
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
#What is the month and year of the median date in our dataset? 
summary(DateConvert)
#PROBLEM 2.3 - UNDERSTANDING DATES IN R
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
#replace the old Date variable with DateConvert
mvt$Date = DateConvert
#In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)
#PROBLEM 2.4 - UNDERSTANDING DATES IN R
#On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)
#PROBLEM 2.5 - UNDERSTANDING DATES IN R
#Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Arrest,mvt$Month)
#PROBLEM 3.1 - VISUALIZING CRIME TRENDS
hist(mvt$Date, breaks=100)
#In general, does it look like crime increases or decreases from 2002 - 2012?
#Decreases

#In general, does it look like crime increases or decreases from 2005 - 2008?
#Decreases

#In general, does it look like crime increases or decreases from 2009 - 2011?
#Increases 

#PROBLEM 3.2 - VISUALIZING CRIME TRENDS 
#Create a boxplot of the variable "Date", sorted by the variable "Arrest"
#Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period?
boxplot(mvt$Date ~ mvt$Arrest)
# First half

#PROBLEM 3.3 - VISUALIZING CRIME TRENDS
#For what proportion of motor vehicle thefts in 2001 was an arrest made?
table(mvt$Arrest, mvt$Year)
2152/(18517+2152)

#PROBLEM 3.4 - VISUALIZING CRIME TRENDS
#For what proportion of motor vehicle thefts in 2007 was an arrest made?
table(mvt$Arrest, mvt$Year)
1212/(1212+13068)
#PROBLEM 3.5 - VISUALIZING CRIME TRENDS
#For what proportion of motor vehicle thefts in 2012 was an arrest made?
table(mvt$Arrest, mvt$Year)
550/(550+13542)

#PROBLEM 4.1 - POPULAR LOCATIONS  
#If they want to increase the number of arrests that are made for motor vehicle thefts, where should they focus their efforts?
sort(table(mvt$LocationDescription))

#PROBLEM 4.2 - POPULAR LOCATIONS
#Create a subset of your data, only taking observations for which the theft happened in one of these five locations
#How many observations are in Top5?

Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")
str(Top5)

#or simply
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)
str(Top5)

#PROBLEM 4.3 - POPULAR LOCATIONS  
#One of the locations has a much higher arrest rate than the other locations. Which is it? 
Top5$LocationDescription = factor(Top5$LocationDescription)

table(Top5$LocationDescription, Top5$Arrest)
439/(439+1672)

#PROBLEM 4.4 - POPULAR LOCATIONS
#On which day of the week do the most motor vehicle thefts at gas stations happen?
table(Top5$LocationDescription, Top5$Weekday)
#PROBLEM 4.5 - POPULAR LOCATIONS  
#On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
table(Top5$LocationDescription, Top5$Weekday)
