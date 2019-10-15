Hub0 = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Cluster/HubwayTrips.csv", header = TRUE)
str(Hub0)
summary(Hub0)
library(caret)
library(ggplot2)
library(lattice)
# transformation
prepos = preProcess(Hub0)
Hub = predict(prepos, Hub0)
summary(Hub)
#sd(Hub$Weekend)

k <- 4
set.seed(1000)

#K-Mean Clustering
KMC_Hub = kmeans(Hub, centers = k)
table(KMC_Hub$cluster)  
KMC_Hub$size  
KMC_Hub$centers 

mu = prepos$mean  
stdev = prepos$std
mat = KMC_Hub$centers

unscaled = matrix(0, nrow = k, ncol = length(mu))
colnames(unscaled, do.NULL = FALSE)
colnames(unscaled) <- names(Hub0)
for(i in c(1:k)){
  for(j in c(1:length(mu))){
    unscaled[i,j] = mu[j] + stdev[j]*mat[i,j]
  }
}
options(scipen = 999)
round(unscaled, digits = 4)

library(cluster)
library(factoextra)
kmax <- 20
fviz_nbclust(Hub, FUN = kmeans, method = "wss", k.max = kmax, nstart = 5)
wss <- (nrow(Hub)-1)*sum(apply(Hub,2,var))
for (i in 2:kmax) {
  wss[i] <- sum(kmeans(Hub,centers=i, nstart=5)$withinss)}
plot(1:kmax, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")


fviz_nbclust(Hub, FUN = kmeans, method = "silhouette", k.max = kmax, nstart = 5)

Hub2 <- Hub[,c(-5,-7)]
nsim <- 1000
Hubids <- sample(1:nrow(Hub2), nsim, replace = FALSE)
Hub3 = Hub2[Hubids, ]

sil <- rep(0, kmax)
# Compute the average silhouette width for 
for(i in 2:kmax){
  km.res <- kmeans(Hub3, centers = i, nstart = 5)
  ss <- silhouette(km.res$cluster, dist(Hub3))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
split.screen(c(1,1))
plot(1:kmax, sil, type = "b", pch = 19, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)
library(NbClust)





str(Hub3)
nc <- NbClust(Hub3, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2")

