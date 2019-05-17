library(MASS)
lda.out <- lda(dd$quality.section ~ X)

lda.out

transf <- X %*% lda.out$scaling[,1:2]
plot(transf, col = dd$quality.section + 1)

lda.out
plot(lda.out)

lda.pred <- predict(lda.out)
(lda.confusiontable <- table(dd$quality.section, lda.pred$class))

sum(diag(lda.confusiontable))/sum(lda.confusiontable)

lda.perc.confusiontable <- matrix(nrow = 3, ncol = 3)
lda.perc.confusiontable[,1] <- lda.confusiontable[,1]/sum(lda.confusiontable[,1])
lda.perc.confusiontable[,2] <- lda.confusiontable[,2]/sum(lda.confusiontable[,2])
lda.perc.confusiontable[,3] <- lda.confusiontable[,3]/sum(lda.confusiontable[,3])

lda.perc.confusiontable

# Dels misclassified, volem veure de quan ens hem equivocat
low.misclass <- which(dd$quality.section == 0 & lda.pred$class == 1)
sum(dd[low.misclass,]$quality == 4)/length(low.misclass)

hist(dd[low.misclass,]$quality,
     main = "Misclassified low-class wines' real scores",
     xlim = range(1:4),
     breaks = 2)

high.misclass  <- which(dd$quality.section == 2 & lda.pred$class == 1)
sum(dd[high.misclass,]$quality == 7)/length(high.misclass)

hist(dd[high.misclass,]$quality,
     main = "Misclassified high-class wines' real scores",
     xlim = range(7:10),
     breaks = 2)

summary(as.factor(dd$quality))

low.class <- which(dd$quality.section == 0 & lda.pred$class == 0)
dd[low.class,]$quality
dd[low.misclass,]$quality
