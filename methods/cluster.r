# 1. Carreguem les dades
# DIR <- "C:\\Users\\andreu.huguet\\Downloads\\wine-quality-ad-master\\"
DIR <- '~/wine-quality-ad-master/'
setwd(DIR)
dd <- read.csv2("./data/winequality-red.csv")
head(dd)

# 2. Creem la matriu X de variables
X <- data.matrix(dd[,1:(ncol(dd) - 1)])
quality <- dd$quality > 5

# 3. Boxplots per detectar anomalies
boxplot(X)
Xn <- X[-which(dd$total.sulfur.dioxide > 200),]
quality <- quality[-which(dd$total.sulfur.dioxide > 200)]
boxplot(Xn)

# 4. Estandaritzar les dades
Xs <- scale(Xn)
summary(Xs)

# 4.1. Treiem una altra anomalia
boxplot(Xs)
Xsn <- Xs[-which(Xs[,5] > 10),] 
quality <- quality[-which(Xs[,5] > 10)]
boxplot(Xsn)

# 5. Matriu de distàncies euclidianes
D <- dist(Xsn, method = "euclidean")
summary(D)

# 6. Agrupament jeràrquic
closest.neigh <- hclust(D, method = "single")
plot(closest.neigh)
farthest.neigh <- hclust(D, method = "complete")
plot(farthest.neigh)
average.neigh <- hclust(D, method = "average")
plot(average.neigh)
ward.neigh <- hclust(D, method = "ward.D2")
plot(ward.neigh)

# 7. Tall de l'arbre de jerarquia
clusters.closest <- cutree(closest.neigh, 1)
table(clusters.closest)
clusters.farthest <- cutree(farthest.neigh, 4)
table(clusters.farthest)
clusters.average <- cutree(average.neigh, 3)
table(clusters.average)
clusters.ward <- cutree(ward.neigh, 7)
table(clusters.ward)

# 8. Relació amb la qualitat (Ward)
(compar.ward <- table(clusters.ward, quality))
ward.maxs <- apply(compar.ward, 1, which.max)

# 8.1. Ward error
ward.misclass <- 0
for (k in 1:7) ward.misclass <- ward.misclass + sum(compar.ward[k,-ward.maxs[k]])
(ward.err <- ward.misclass/sum(compar.ward))

# 9. K-means clustering vs. Ward
K.kmeans <- 7
clusters.kmeans <- kmeans(D, K.kmeans)
(compar.kmeans <- table(clusters.kmeans$cluster, quality))
kmeans.maxs <- apply(compar.kmeans, 1, which.max)

# 9.1. K-means error
kmeans.misclass <- 0
for (k in 1:K.kmeans) kmeans.misclass <- kmeans.misclass + sum(compar.kmeans[k,-kmeans.maxs[k]])
(kmeans.err <- kmeans.misclass/sum(compar.kmeans))

# 10. Mixture of models clustering
library(mclust)
cluster.mix <- Mclust(Xsn)
plot(cluster.mix, what = "classification")
(compar.mix <- table(apply(cluster.mix$z, 1, which.max), quality))
mix.maxs <- apply(compar.mix, 1, which.max)

# 10.1. Mixture of models error
mix.misclass <- 0
for (k in 1:7) mix.misclass <- mix.misclass + sum(compar.mix[k,-mix.maxs[k]])
(mix.err <- mix.misclass/sum(compar.mix))

# 11. Comparison of the three methods
data.frame(wardERR = ward.err,
           kmeansERR = kmeans.err,
           mixERR = mix.err)
