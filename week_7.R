WHO = read.csv("WHO.csv")
str(WHO)
plot(WHO$GNI, WHO$FertilityRate)
install.packages("ggplot2")
library(ggplot2)
scaterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))
scaterplot + geom_point()
scaterplot + geom_line()
scaterplot + geom_point(color = "blue", size = 3, shape = 17)
scaterplot + geom_point(color = "darkred", size = 3, shape = 8)
scaterplot + geom_point(color = "darkred", size = 3, shape = 8) + ggtitle("Fertility Rate vs Gross National Income")
fertillityGNIplot = scaterplot + geom_point(color = "darkred", size = 3, shape = 8) + ggtitle("Fertility Rate vs Gross National Income")
pdf("MyPlot.pdf")
print(fertillityGNIplot)
dev.off()
scaterplot + geom_point(color = "blue", size = 3, shape = 15)
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()
model = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(model)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm")
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", level = 0.99)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", se = FALSE)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", se = FALSE, color = "orange")
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", level = 0.99)



ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point()




#predictive policing
#A LINE PLOT
Sys.setlocale("LC_ALL", "C")
mvt = read.csv("mvt.csv", stringsAsFactors = FALSE)
str(mvt)
#We want to first convert the Date variable to a format
#that R will recognize so that we can extract the day of the week

#Here, we can see in the output from the str function
#that our format is the month slash the day slash the year,
#and then the hour colon minutes.

mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")

mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

table(mvt$Weekday)
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)
library(ggplot2)
ggplot(WeekdayCounts, aes(x = Var1, y=Freq)) + geom_line(aes(group = 1))
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(WeekdayCounts, aes(x = Var1, y=Freq)) + geom_line(aes(group = 1))

ggplot(WeekdayCounts, aes(x = Var1, y=Freq)) + geom_line(aes(group = 1)) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

#quit
ggplot(WeekdayCounts, aes(x = Var1, y=Freq)) + geom_line(aes(group=1), linetype=2) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")
ggplot(WeekdayCounts, aes(x = Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")


#VIDEO 4: A HEATMAP

table(mvt$Weekday, mvt$Hour)

DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
ggplot(DayHourCounts,aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1))

ggplot(DayHourCounts,aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1, color = Var1), size=2)

#heatmap
DayHourCounts$Var1=factor(DayHourCounts$Var1,ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") )
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y=element_blank())

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts", low = "white", high = "red") + theme(axis.title.y=element_blank())

#quiz
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts", low = "white", high = "black") + theme(axis.title.y=element_blank())


#VIDEO 5: A GEOGRAPHICAL HOT SPOT MAP

library(maps)
library(ggmap)

chicago = get_map(location="chicago", zoom=11)

ggmap(chicago)
  
ggmap(chicago) + geom_point(data=mvt[1:100,],aes(x=Longitude, y=Latitude)) 
  
LatLonCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(LatLonCounts) 
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq))

ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq)) + scale_color_gradient(low="yellow", high="red")
  
ggmap(chicago) + geom_tile(data= LatLonCounts, aes(x=Long, y=Lat, alpha=Freq), fill="red")  

#quiz
LatLonCounts2=subset(LatLonCounts, Freq > 0)
ggmap(chicago) + geom_tile(data= LatLonCounts2, aes(x=Long, y=Lat, alpha=Freq), fill="red")  
str(LatLonCounts2)
str(LatLonCounts)
1638-686
nrow(LatLonCounts) - nrow(LatLonCounts2)

#VIDEO 6: A HEATMAP ON THE UNITED STATES

murders=read.csv("murders.csv")

str(murders)

statesMap=map_data("state")
str(statesMap)
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black")

murders$region = tolower(murders$State)

murderMap = merge(statesMap, murders, by="region")
str(murderMap)

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high = "red")
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

murderMap$MurderRate = murderMap$Murders/murderMap$Population*100000
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))

#quiz

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")
