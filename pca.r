library(car)
library(FactoMineR)
library(ade4)

pca.out <- princomp(df, cor = T)
summary(pca.out)
biplot(pca.out,
       xlabs = rep(".", nrow(X)))
screeplot(pca.out)

plot(pca.out$scores[,1], pca.out$scores[,2],
     col = as.factor(dd$quality))
