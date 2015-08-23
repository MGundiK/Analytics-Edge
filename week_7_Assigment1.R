library(ggplot2)
install.packages("maps")
library(maps)
install.packages("ggmap")
library(ggmap)
Sys.setlocale("LC_ALL", "C")
statesMap = map_data("state")
str(statesMap)
summary(statesMap$group)
table(statesMap$group)
length(table(statesMap$group))
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
polling = read.csv("PollingImputed.csv")
str(polling)
table(polling$Year)
#Load the data using the read.csv function, and call it "polling".
#Then split the data using the subset function into a training set called "Train" 
#that has observations from 2004 and 2008, and a testing set called "Test" 
#that has observations from 2012.

Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
str(Train)
str(Test)
#Note that we only have 45 states in our testing set, since we are missing observations
#for Alaska, Delaware, Alabama, Wyoming, and Vermont, so these states will not appear
#colored in our map.


#Then, create a logistic regression model and make predictions
#on the test set using the following commands:

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")

#TestPrediction gives the predicted probabilities for each state, 
#but let's also create a vector of Republican/Democrat predictions by using
#the following command:

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

#Now, put the predictions and state labels in a data.frame so that we can use ggplot:

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

#For how many states is our binary prediction 1 (for 2012), corresponding to Republican?
table(TestPredictionBinary)
sum(predictionDataFrame$TestPredictionBinary == 1)


#What is the average predicted probability of our model (on the Test set, for 2012)?
mean(TestPrediction)

#PROBLEM 2.2 - COLORING THE STATES BY PREDICTIONS

#Now, we need to merge "predictionDataFrame" with the map data "statesMap",
#like we did in lecture. Before doing so, we need to convert the Test.State variable 
#to lowercase, so that it matches the region variable in statesMap. 
#Do this by typing the following in your R console:

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

#Now, merge the two data frames using the following command:

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

#Lastly, we need to make sure the observations are in order so that the map is drawn
#properly, by typing the following:

predictionMap = predictionMap[order(predictionMap$order),]

#How many observations are there in predictionMap?

str(predictionMap)
nrow(predictionMap)


#How many observations are there in statesMap?
nrow(statesMap)


#PROBLEM 2.3 - COLORING THE STATES BY PREDICTIONS

#When we merged the data in the previous problem, it caused the number 
#of observations to change. Why? Check out the help page for merge by typing ?merge
#to help you answer this question.

#Ans
#Because we only make predictions for 45 states, we no longer have observations 
#for some of the states. These observations were removed in the merging process.

#Comment
#PROBLEM 2.4 - COLORING THE STATES BY PREDICTIONS
#Now we are ready to color the US map with our predictions! You can color the states
#according to our binary predictions by typing the following in your R console:

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

#The states appear light blue and dark blue in this map.
#Which color represents a Republican prediction?

#Light Blue

#EXPLANATION

#Our logistic regression model assigned 1 to Republican and 0 to Democrat. 
#As we can see from the legend, 1 corresponds to a light blue color on the map 
#and 0 corresponds to a dark blue color on the map.

#PROBLEM 2.5 - COLORING THE STATES BY PREDICTIONS

#We see that the legend displays a blue gradient for outcomes between 0 and 1. 
#However, when plotting the binary predictions there are only two possible outcomes: 0 
#or 1. 
#Let's replot the map with discrete outcomes. 
#We can also change the color scheme to blue and red, to match the blue color 
#associated with the Democratic Party in the US and the red color associated with 
#the Republican Party in the US. This can be done with the following command:

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#Alternatively, we could plot the probabilities instead of the binary predictions.
#Change the plot command above to instead color the states by the variable TestPrediction. 

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012") + scale_color_brewer(palette="Dark2")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", name = "Prediction 2012")

#The only state that appears purple (the color between red and blue) is the state 
#of Iowa, so the maps look very similar. If you take a look at TestPrediction, 
#you can see that most of our predicted probabilities are very close to 0 or 
#very close to 1. In fact, we don't have a single predicted probability between 0.065 and 0.93.

#PROBLEM 3.1 - UNDERSTANDING THE PREDICTIONS

#In the 2012 election, the state of Florida ended up being a very close race.
#It was ultimately won by the Democratic party. 
#Did we predict this state correctly or incorrectly?


#In our prediction map, the state of Florida is colored red, meaning that we predicted Republican.
#So we incorrectly predicted this state.

#PROBLEM 3.2 - UNDERSTANDING THE PREDICTIONS

#What was our predicted probability for the state of Florida?

predFlorida = subset(predictionMap, predictionMap$Test.State=="Florida")
mean(predFlorida$TestPrediction)

#or

predictionDataFrame[which(predictionDataFrame$Test.State == "Florida"), ]


#What does this imply?

#Our prediction model did not do a very good job of correctly predicting 
#the state of Florida, and we were very confident in our incorrect prediction.

#PROBLEM 4 - PARAMETER SETTINGS
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#The first plot can be generated by setting the parameter linetype = 3:
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#The second plot can be generated by setting the parameter size = 3:

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#Plot (3) can be created by changing the alpha parameter:

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#The "alpha" parameter controls the transparency or darkness of the color.
#A smaller value of alpha will make the colors lighter.






