#STOCK DYNAMICS
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing =read.csv("BoeingStock.csv")
#PROBLEM 1.1 - SUMMARY STATISTICS
#convert the dates
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
#How many observations are there in each data set?
str(IBM)
str(GE)
str(ProcterGamble)
str(CocaCola)
str(Boeing)
#PROBLEM 1.2 - SUMMARY STATISTICS  
#What is the earliest year in our datasets?
summary(IBM)
summary(GE)
summary(ProcterGamble)
summary(CocaCola)
summary(Boeing)
#PROBLEM 1.3 - SUMMARY STATISTICS
#What is the latest year in our datasets?
max(IBM$Date)
max(GE$Date)
max(ProcterGamble$Date)
max(CocaCola$Date)
max(Boeing$Date)
#PROBLEM 1.4 - SUMMARY STATISTICS
#What is the mean stock price of IBM over this time period?
summary(IBM)

#PROBLEM 1.5 - SUMMARY STATISTICS
#What is the minimum stock price of General Electric (GE) over this time period?
summary(GE)

#PROBLEM 1.6 - SUMMARY STATISTICS  
#What is the maximum stock price of Coca-Cola over this time period?
summary(CocaCola)

#PROBLEM 1.7 - SUMMARY STATISTICS  
#What is the median stock price of Boeing over this time period?
summary(Boeing)

#PROBLEM 1.8 - SUMMARY STATISTICS  
#What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)

#PROBLEM 2.1 - VISUALIZING STOCK DYNAMICS
#plot the Date on the x-axis and the StockPrice on the y-axis, for Coca-Cola.

plot(CocaCola$Date, CocaCola$StockPrice, type="l")

#PROBLEM 2.2 - VISUALIZING STOCK DYNAMICS
#add the line for Procter & Gamble
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")


#PROBLEM 3.1 - VISUALIZING STOCK DYNAMICS 1995-2005
#how have the stock prices changed from 1995-2005 for all five companies
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
#ylim=c(0,210), makes the y-axis range from 0 to 210

#add the rest
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")

lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")

lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")

lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
#add a vertical line to the plot at March 2000
abline(v=as.Date(c("2000-03-01")), lwd=2)

#PROBLEM 3.3 - VISUALIZING STOCK DYNAMICS 1995-2005
#In October of 1997, there was a global stock market crash that was caused
#by an economic crisis in Asia. Comparing September 1997 to November 1997,
#which companies saw a decreasing trend in their stock price? 

#create vertical lines at September 1997 and November 1997
abline(v=as.Date(c("1997-09-01")), lwd=2)

abline(v=as.Date(c("1997-11-01")), lwd=2)

#PROBLEM 4.1 - MONTHLY TRENDS
#calculate the mean stock price of IBM, sorted by months

mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean)

#rest of the companies
tapply(GE$StockPrice, months(GE$Date), mean)

tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)

#PROBLEM 4.3 - MONTHLY TRENDS
#For the months of December and January, every company's average stock is higher in one month and lower in the other.
#In which month are the stock prices lower?
tapply(IBM$StockPrice, months(IBM$Date), mean)





