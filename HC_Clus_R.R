
# IRIS DATA
x.dist1=dist(iris[,-5],method="euclidean")
x.dist2=dist(iris[,-5],method="manhattan")
x.dist3=dist(iris[,-5],method="minkowski")
# Distances
hc.single1=hclust(x.dist1,method="single",members=NULL)
hc.single2=hclust(x.dist2,method="single",members=NULL)
hc.single3=hclust(x.dist3,method="single",members=NULL)
hc.ward.D2=hclust(x.dist1,method="ward",members=NULL)
# Dendrograms
plot(hc.single1)
plot(hc.single2)
plot(hc.single3)
plot(hc.ward)
#
hicluste1=cutree(hc.single1,k=3)
hicluste2=cutree(hc.single2,k=3)
hicluste3=cutree(hc.single3,k=3)
hiward=cutree(hc.ward,k=3)
#

plot(iris[,-5], col=hicluste1,main="Single Linkage - Euclidean distance")
plot(iris[,-5], col=hicluste2,main="Single Linkage - Manhattan distance")
plot(iris[,-5], col=hicluste3,main="Single Linkage - Minkowski distance")
plot(iris[,-5], col=hiward,main="Ward's minimum variance - Euclidean distance")
#
table(hicluste1,iris[,5])
table(hicluste2,iris[,5])
table(hicluste3,iris[,5])
table(hiward,iris[,5])
#
k.iris <- kmeans(iris[,-5],centers=3,nstart = 20,iter.max = 100)
plot(iris[,-5],col=k.iris$cluster)
table(iris[,5],k.iris$cluster)
#
#
# FOOD DATA
food = fooddat[, -1]                         # delete the first column (types of families)
f = data.frame(scale(food))                           # standardize variables
rownames(f) = c("MA2", "EM2", "CA2", "MA3", "EM3", "CA3", "MA4", "EM4", "CA4", 
                "MA5", "EM5", "CA5")                        # define types of families
x.dist1=dist(f,method="euclidean")
x.dist2=dist(f,method="manhattan")
x.dist3=dist(f,method="minkowski")
# Distances
hc.single1=hclust(x.dist1,method="single",members=NULL)
hc.single2=hclust(x.dist2,method="single",members=NULL)
hc.single3=hclust(x.dist3,method="single",members=NULL)
hc.single4=hclust(x.dist1,method="ward",members=NULL)
# Dendrograms
plot(hc.single1)
plot(hc.single2)
plot(hc.single3)
plot(hc.single4)
#
hicluste1=cutree(hc.single1,k=2)
hicluste2=cutree(hc.single2,k=2)
hicluste3=cutree(hc.single3,k=2)
hicluste4=cutree(hc.single4,k=2)
#
plot(f, col=hicluste1,main="Single Linkage - Euclidean distance")
plot(f, col=hicluste2,main="Single Linkage - Manhattan distance")
plot(f, col=hicluste3,main="Single Linkage - Minkowski distance")
plot(f, col=hicluste4,main="Ward's minimum variance - Euclidean distance")
#
table(hicluste1,fooddat[,1])
table(hicluste2,fooddat[,1])
table(hicluste3,fooddat[,1])
table(hicluste4,fooddat[,1])
#
k.food <- kmeans(f,centers=2,nstart = 10)
plot(f,col=k.food$cluster)
table(fooddat[,1],k.food$cluster)
#
distance <- c("euclidean","manhattan","minkowski")
methods <- c("single","complete","average","ward")
k <- 0
results <- list()
for (i in 1:length(distance))
{
  dist <- dist(banknote[,-1],method = distance[i])
  for(j in 1:length(methods))
  { 
    k <- k+1
    results[[k]] <- hclust(dist,method = methods[j])
  }
}
clustering <- matrix(NA,nrow=200,ncol=12)
for(j in 1:12)
{
  clustering[,j] <- cutree(results[[j]],k=2)
}
table(clustering[,1],banknote[,1])
k.bank <- kmeans(banknote[,-1],centers=2,nstart = 20)
plot(banknote[,-1],col=k.bank$cluster)
table(banknote[,1],k.bank$cluster)

