# Clustering in R
# Hierarchical and K-means
# Updated October 2018

WHO = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Cluster/WHO.csv")
WHOCluster = WHO[c("Population", "Under15", "Over60", "LifeExpectancy")]
str(WHO)
str(WHOCluster)
# Normalization
#install.packages("caret") #say yes to restarting R 
library(caret) 
library(lattice)
library(ggplot2)
preproc = preProcess(WHOCluster) #preparation of the transformation of the data (scaling, centering)
WHOClusterNorm = predict(preproc, WHOCluster) #applying the transformation to a data set
str(WHOClusterNorm)

# Hierarchical Clustering
distances = dist(WHOClusterNorm, method="euclidean")
HierClustering = hclust(distances, method="ward.D2")
plot(HierClustering)

clusterGroups = cutree(HierClustering, k = 4)
rect.hclust(HierClustering,k=4, border="red")
table(clusterGroups)
#clusterGroups
#1  2  3  4 
#40 97 55  2 

tapply(WHOCluster$Population, clusterGroups, mean)
tapply(WHOCluster$Under15, clusterGroups, mean)
tapply(WHOCluster$Over60, clusterGroups, mean)
tapply(WHOCluster$LifeExpectancy, clusterGroups, mean)

cluster4 = subset(WHOCluster,clusterGroups==4)
cluster4
WHO[36,]
WHO[78,]

clusternb=clusterGroups
WHO$clusternb = as.factor(clusterGroups)
#if you forget as.factor your tree will make predictions like 2.6 in rpart.plot
library(rpart)
explanation = rpart(clusternb ~ Region + Population + Under15 + Over60 + FertilityRate + LifeExpectancy + CellularSubscribers + LiteracyRate + GNI, data = WHO)
#leave out country because it would "explain" everything but gives no reason for it
library(rpart.plot)
rpart.plot(explanation)

# Kmeans Clustering
KmeansClustering = kmeans(WHOClusterNorm, centers = 4)
table(KmeansClustering$cluster)
#1  2  3  4 
#52 57  2 83 
#not the same as with hclust
KmeansClustering$centers

### How to pick the best number of clusters
## with fviz_nbclust in package factoextra
#install.packages("cluster")
#install.packages("factoextra")
library(cluster)
library(factoextra) 
kmax <- 20
fviz_nbclust(WHOClusterNorm, FUN = hcut, method = "wss", k.max=kmax) # default k.max is 10
# k=4
fviz_nbclust(WHOClusterNorm, FUN = hcut, method = "silhouette", k.max=kmax)
# k=3

fviz_nbclust(WHOClusterNorm, FUN = kmeans, method = "wss", k.max=kmax)
# clearly a randomness issue around k=4 on my graph
fviz_nbclust(WHOClusterNorm, FUN = kmeans, method = "wss", k.max=kmax, nstart=5) 
# to smooth out the randomness in kmeans we do it 5 times
# k=4
fviz_nbclust(WHOClusterNorm, FUN = kmeans, method = "silhouette", k.max=kmax, nstart=5)
# k=4

## with package gap_stat
gap_stat <- clusGap(WHOClusterNorm, FUN = hcut, nstart = 25, K.max = kmax, B = 10) # B=50 better
fviz_gap_stat(gap_stat)
# k= 2

gap_stat <- clusGap(WHOClusterNorm, FUN = kmeans, nstart = 25, K.max = kmax, B = 10) # B=50 better
fviz_gap_stat(gap_stat)
# k= 2

## with package NbClust
#install.packages("NbClust")
library(NbClust)
nc <- NbClust(WHOClusterNorm, distance="euclidean", min.nc=2, max.nc=kmax, method="ward.D2")
length(unique(nc$Best.partition)) # returns optimal number of clusters as selected by majority


### trying to understand fviz_nbclust a little better
# replicating fviz_nbclust for kmeans and wss
fviz_nbclust(WHOClusterNorm, FUN = kmeans, method = "wss", k.max=kmax, nstart=5)
wss <- (nrow(WHOClusterNorm)-1)*sum(apply(WHOClusterNorm,2,var))
for (i in 2:kmax) wss[i] <- sum(kmeans(WHOClusterNorm,
    centers=i, nstart=5)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# replicating fviz_nbclust for kmeans and silhouette
fviz_nbclust(WHOClusterNorm, FUN = kmeans, method = "silhouette", k.max=kmax, nstart=5)
sil <- rep(0, kmax)
# Compute the average silhouette width for 
for(i in 2:kmax){
  km.res <- kmeans(WHOClusterNorm, centers = i, nstart = 5)
  ss <- silhouette(km.res$cluster, dist(WHOClusterNorm))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
split.screen(c(1,1))
plot(1:kmax, sil, type = "b", pch = 19, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)


