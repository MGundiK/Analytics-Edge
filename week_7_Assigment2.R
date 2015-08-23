edges = read.csv("edges.csv")
users = read.csv("users.csv")

#PROBLEM 1.1 - SUMMARIZING THE DATA
#How many Facebook users are there in our dataset?
str(edges)
str(users)
nrow(users)
#In our dataset, what is the average number of friends per user? 
#Hint: this question is tricky, and it might help to start by thinking about a small example with two users who are friends.
summary(users)
#From str(edges) or nrow(edges), we see that there are 146 pairs of users 
#in our dataset who are Facebook friends. However, each pair (A, B) must be counted twice,
#because B is a friend of A and A is a friend of B. 
#To think of this in simpler terms, consider a network with just new people, A and B, and a single edge (A, B). 
#Even though there are two vertices and one edge, each user has on average one friend.

## Download and install the package
install.packages("igraph")

## Load package
library(igraph)

g = graph.data.frame(edges, F, users)
# Plot the network. There are 4 connected components and 7 users with no
# friends in the network
plot(g, vertex.size = 5, vertex.label = NA, vertex.shape = "sphere")

# How many users are friends with 10 or more other Facebook users in this
# network?
sum(degree(g) >= 10)

## What is the average number of friends per user?
mean(degree(g))



#Out of all the students who listed a school, what was the most common locale?
table(users$locale, users$school)
#From table(users$locale, users$school), we read that all students listed 
#at schools A and B listed their locale as B.
#alternatevly
userSchool = subset(users, school != "NA")
table(userSchool$locale)

#Is it possible that either school A or B is an all-girls or all-boys school?
table(users$school, users$gender)
#We see from table(users$gender, users$school) that both genders A and B have attended schools A and B.

#PROBLEM 2.1 - CREATING A NETWORK

g = graph.data.frame(edges, F, users)

#PROBLEM 2.2 - CREATING A NETWORK
#Use the correct command from Problem 2.1 to load the graph g.

#Now, we want to plot our graph. By default, the vertices are large and have text labels
#of a user's identifier. Because this would clutter the output, 
#we will plot with no text labels and smaller vertices:

plot(g, vertex.size=5, vertex.label=NA)

#In this graph, there are a number of groups of nodes where all the nodes in each group
#are connected but the groups are disjoint from one another, 
#forming "islands" in the graph. Such groups are called "connected components," 
#or "components" for short. 
#How many connected components with at least 2 nodes are there in the graph?
plot(g, vertex.size=5, vertex.label=NA)

#Answer 4
#In addition to the large connected component, 
#there is a 4-node component and two 2-node components.


#How many users are there with no friends in the network?
sum(degree(g) == 0)

#PROBLEM 2.3 - CREATING A NETWORK

#In our graph, the "degree" of a node is its number of friends.
#We have already seen that some nodes in our graph have degree 0 (these are the nodes with no friends),
#while others have much higher degree. We can use degree(g) to compute the degree of all the nodes in our graph g.

#How many users are friends with 10 or more other Facebook users in this network?

sum(degree(g) >= 10)


#PROBLEM 2.4 - CREATING A NETWORK

#In a network, it's often visually useful to draw attention to "important" nodes in 
#the network. While this might mean different things in different contexts, 
#in a social network we might consider a user with a large number of friends to be 
#an important user. From the previous problem, we know this is the same as saying 
#that nodes with a high degree are important users.

#To visually draw attention to these nodes, we will change the size of the vertices
#so the vertices with high degrees are larger. 
#To do this, we will change the "size" attribute of the vertices of our graph
#to be an increasing function of their degrees:

V(g)$size = degree(g)/2+2

#Now that we have specified the vertex size of each vertex, we will no longer use 
#the vertex.size parameter when we plot our graph:

plot(g, vertex.label=NA)

#What is the largest size we assigned to any node in our graph?

max(V(g)$size)

#What is the smallest size we assigned to any node in our graph?

min(V(g)$size)

#PROBLEM 3.1 - COLORING VERTICES

#Thus far, we have changed the "size" attributes of our vertices. However,
#we can also change the colors of vertices to capture additional information about
#the Facebook users we are depicting.

#When changing the size of nodes, we first obtained the vertices of our graph with V(g) 
#and then accessed the the size attribute with V(g)$size. 
#To change the color, we will update the attribute V(g)$color.

#To color the vertices based on the gender of the user, we will need access to that 
#variable. When we created our graph g, we provided it with the data frame users, 
#which had variables gender, school, and locale. 
#These are now stored as attributes V(g)$gender, V(g)$school, and V(g)$locale.

#We can update the colors by setting the color to black for all vertices,
#than setting it to red for the vertices with gender A and setting it to gray for 
#the vertices with gender B:

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

#Plot the resulting graph. 
#What is the gender of the users with the highest degree in the graph?

plot(g, vertex.label=NA)

#PROBLEM 3.2 - COLORING VERTICES

#Now, color the vertices based on the school that each user in our network attended.

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)



#Are the two users who attended both schools A and B Facebook friends with each other?

#yes

#What best describes the users with highest degree?

#Some, but not all, of the high-degree users attended school A 

#The two students who attended schools A and B are colored gray; 
#we can see from the graph that they are Facebook friends
#(aka they are connected by an edge). The high-degree users 
#(depicted by the large nodes) are a mixture of red and black color, 
#meaning some of these users attended school A and other did not.

#PROBLEM 3.3 - COLORING VERTICES  

#Now, color the vertices based on the locale of the user.

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

#The large connected component is most associated with which locale?

#B

#The 4-user connected component is most associated with which locale?

#A

#Nearly all of the vertices from the large connected component are colored gray,
#indicating users from Locale B. 
#Meanwhile, all the vertices in the 4-user connected component are colored red,
#indicating users from Locale A.

#PROBLEM 4 - OTHER PLOTTING OPTIONS

#Answer the following questions with the help of ?igraph.plotting 
#and experimentation in your R console.
?igraph.plotting 

#Which igraph plotting function would enable us to plot our graph in 3-D?

#rgplot

#What parameter to the plot() function would we use to change the edge width when plotting g?
plot(g, vertex.label=NA, edge.width=3)

#edge.width





