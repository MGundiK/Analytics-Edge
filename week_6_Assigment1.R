#DOCUMENT CLUSTERING WITH DAILY KOS
#building a hierarchical clustering model
#import data
dailykos = read.csv("dailykos.csv")
#compute the distances
kosDist = dist(dailykos, method="euclidean")
#build the hierarchical clustering model
kosHierClust = hclust(kosDist, method="ward.D")

#Plot the dendrogram of  hierarchical clustering model
plot(kosHierClust)

#pick 7 clusters
hierGroups = cutree(kosHierClust, k = 7)
#split the data into  7 clusters
HierCluster1 = subset(dailykos, hierGroups == 1)

HierCluster2 = subset(dailykos, hierGroups == 2)

HierCluster3 = subset(dailykos, hierGroups == 3)

HierCluster4 = subset(dailykos, hierGroups == 4)

HierCluster5 = subset(dailykos, hierGroups == 5)

HierCluster6 = subset(dailykos, hierGroups == 6)

HierCluster7 = subset(dailykos, hierGroups == 7)

table(hierGroups)

#top 6 words in each cluster
tail(sort(colMeans(HierCluster1)))

tail(sort(colMeans(HierCluster2)))

tail(sort(colMeans(HierCluster3)))

tail(sort(colMeans(HierCluster4)))

tail(sort(colMeans(HierCluster5)))

tail(sort(colMeans(HierCluster6)))

tail(sort(colMeans(HierCluster7)))

#k-means clustering, setting the seed to 1000
set.seed(1000)

KmeansCluster = kmeans(dailykos, centers=7)

#subset  data into the 7 clusters
KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)

KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)

KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)

KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)

KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)

KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)

KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)

table(KmeansCluster$cluster)

#Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
table(hierGroups, KmeansCluster$cluster)
#116 (80.6%) of the observations in K-Means Cluster 2 also fall in Hierarchical Cluster 7

#Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
#171 (61.7%) of the observations in K-Means Cluster 3 also fall in Hierarchical Cluster 5

#Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
#no more than 123 (39.9%) of the observations in K-Means Cluster 7 fall in any hierarchical cluster

#Which Hierarchical Cluster best corresponds to K-Means Cluster 6?
#320 (97.3%) of observations in K-Means Cluster 6 fall in Hierarchical Cluster 2




