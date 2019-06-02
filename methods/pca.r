# 1. Carreguem les dades
DIR <- "C:\\Users\\andreu.huguet\\Downloads\\wine-quality-ad-master\\"
setwd(DIR)
dd <- read.csv2("./data/winequality-red.csv")
head(dd)

# 1.1. Creem la matriu X de variables
X <- data.matrix(dd[,1:(ncol(dd) - 1)])
X[,4] <- log(X[,4])
cor(X)

# 2. Anàlisi de Components Principals
library(car)
pca.out <- princomp(X, cor = T)
summary(pca.out)
screeplot(pca.out)

# 3. Biplot
biplot(pca.out,
       xlabs = rep(".", nrow(X)))

# 3.1. Fora anomalies
X <- X[-which(X[,4] > 2.4),]

# 4. Repetim l'anàlisi
pca.out <- princomp(X, cor = T)
biplot(pca.out,
       xlabs = rep(".", nrow(X)))

# 5. Plot dels dos components principals
plot(pca.out$scores[,1], pca.out$scores[,2],
     col = as.factor(dd$quality > 5))
