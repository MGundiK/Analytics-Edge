#MARKET SEGMENTATION FOR AIRLINES
airlines = read.csv("AirlinesCluster.csv")

summary(airlines)
#normalize the data
library(caret)
#pre-process the data
preproc = preProcess(airlines)
#normalization
airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)
#compute the distances
distances = dist(airlinesNorm, method="euclidean")
#hierarchical clustering
hierClust = hclust(distances, method="ward.D")
#dendrogram
plot(hierClust)
#divide the data points into 5 clusters
clusterGroups = cutree(hierClust, k = 5)
table(clusterGroups)
#compare the average values in each of the variables for the 5 clusters 
#(the centroids of the clusters)
#compute the average values of the unnormalized data
#compute the average values for all variables in each of the clusters
tapply(airlines$Balance, clusterGroups, mean)

tapply(airlines$QualMiles, clusterGroups, mean)

tapply(airlines$BonusMiles, clusterGroups, mean)

tapply(airlines$BonusTrans, clusterGroups, mean)

tapply(airlines$FlightMiles, clusterGroups, mean)

tapply(airlines$FlightTrans, clusterGroups, mean)

tapply(airlines$DaysSinceEnroll, clusterGroups, mean)

#In just one line

lapply(split(airlines, clusterGroups), colMeans)

# K-MEANS CLUSTERING
set.seed(88)

kmeansClust = kmeans(airlinesNorm, centers=5, iter.max=1000)
table(kmeansClust$cluster)
