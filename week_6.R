a = c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
b = c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)
net = sqrt(sum((a-b)^2))
net
#import data
movies = read.table("movielens.txt", header = FALSE, sep = "|", quote= "\"")
str(movies)
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Advanture", "Animation", "Childrens", "Comedy", "Crimes", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror","Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)
#remove data
movies$ID = NULL
movies$ReleaseDate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB= NULL
movies = unique(movies)
str(movies)
table(movies$Comedy)
table(movies$Western)
table(movies$Romance & movies$Drama)
#or
table(movies$Romance, movies$Drama)

#HIERARCHICAL CLUSTERING IN R

distances = dist(movies[2:20], method="euclidean")

clusterMovies = hclust(distances, method="ward.D")

plot(clusterMovies)

clusterGroups = cutree(clusterMovies, k = 10)

tapply(movies$Action, clusterGroups, mean)

tapply(movies$Romance, clusterGroups, mean)

subset(movies, Title == "Men in Black (1997)")

clusterGroups[257]

cluster2 = subset(movies, clusterGroups == 2)

cluster2$Title[1:10]

#An alternative approach is to use the colMeans function. With this approach,
#you only have one command for each cluster instead of one command for each variable.
#If you run the following command in your R console, 
#you can get all of the column (variable) means for cluster 1:

colMeans(subset(movies[2:20], clusterGroups == 1))

#You can repeat this for each cluster by changing the clusterGroups number.
#However, if you also have a lot of clusters, this approach is not that much more 
#efficient than just using the tapply function.

#A more advanced approach uses the "split" and "lapply" functions. 
#The following command will split the data into subsets based on the clusters:

spl = split(movies[2:20], clusterGroups)

#Then you can use spl to access the different clusters, because

spl[[1]]

#is the same as

subset(movies[2:20], clusterGroups == 1)

#so colMeans(spl[[1]]) will output the centroid of cluster 1. 
#But an even easier approach uses the lapply function. 
#The following command will output the cluster centroids for all clusters:

lapply(spl, colMeans)

#The lapply function runs the second argument (colMeans) on each element 
#of the first argument (each cluster subset in spl). 
#So instead of using 19 tapply commands, or 10 colMeans commands, 
#we can output our centroids with just two commands: one to define spl,
#and then the lapply command.

#Note that if you have a variable called "split" in your current R session, 
#you will need to remove it with rm(split) so that you can use the split function.


#make new clustering with k = 2
clusterGroups2 = cutree(clusterMovies, k = 2)

str(clusterGroups2)
summary(clusterGroups2)
plot(clusterGroups2)

#which movie genres are in cluster 2
colMeans(subset(movies[2:20], clusterGroups2 == 2))


