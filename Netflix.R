# Clustering for Netflix Example

# Reading the data file
movies = read.table("movieLens.txt", header=FALSE, sep="|",quote="\"")
str(movies)
# Adding column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data
str(movies)
head(movies)
# Compute distances
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering using Ward distance
clusterMovies = hclust(distances, method = "ward.D2") #THE RIGHT METHOD
clusterMoviesb = hclust(distances, method = "ward.D") 

# Plot the dendrogram
plot(clusterMovies)
plot(clusterMoviesb)
rect.hclust(clusterMovies, k=4, border = "red")
# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 4)


clusterGroupsb = cutree(clusterMoviesb, k = 4)

# What did change in changing the Ward method from the wrong Ward.D (formerly Ward) to the correct Ward.D2?
table(clusterGroups)
table(clusterGroupsb)

cluster1 = subset(movies, clusterGroups==1)
cluster1b = subset(movies, clusterGroupsb==1)

# Look at the first 10 titles in this cluster:
cluster1$Title[1:10]
cluster1b$Title[1:10]

# Enough with Ward.D vs Ward.D2
# Let's stick to the correct Ward.D2
# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)

# Cluster analysis

# Computing the percentage of movies of a given genre in each cluster

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)
tapply(movies$Documentary, clusterGroups, mean)
tapply(movies$Drama, clusterGroups, mean)
tapply(movies$FilmNoir, clusterGroups, mean)

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods

# Find which cluster Men in Black and Stars Wars are in.
subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

subset(movies, Title=="Star Wars (1977)")
clusterGroups[50]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]

# Create a new data set with just the movies from cluster 1
cluster1 = subset(movies, clusterGroups==1)

# Look at the first 10 titles in this cluster:
cluster1$Title[1:10]


clusterGroups = cutree(clusterMovies, k = 4)
clusterGroups
movies$clusternb = as.factor(clusterGroups)
library(rpart)
library(rpart.plot)
expl = rpart(clusternb ~ Unknown + Action + Adventure + Animation + Childrens + Comedy + Crime + Documentary + Drama + Fantasy + FilmNoir + Horror + Musical + Mystery + Romance + SciFi + Thriller + War + Western, data=movies)
summary(expl)
rpart.plot(expl)
rpart.plot(expl, tweak = 2)
