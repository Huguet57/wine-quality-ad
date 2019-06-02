# 1. Carreguem les dades
DIR <- "C:\\Users\\andreu.huguet\\Downloads\\wine-quality-ad-master\\"
setwd(DIR)
dd <- read.csv2("./data/winequality-red.csv")
head(dd)

# 1.1. Creem la matriu X de variables
X <- data.matrix(dd[,1:(ncol(dd) - 1)])
quality <- dd$quality

# 1.2 Funció definida per agafar els vins de quality == K

get.quality <- function(level) {
  level.ids <- which(quality == level)
  quality[level.ids]
}

# 2. Mirem si hi ha dades mancants

sum(is.na(X)) == 0
sum(is.na(quality)) == 0

# 3. Estandarització de les variables

# 3.1. Transformacions pre-estandaritzat

X[,8] <- scale(X[,8])
X[,4] <- log(X[,4])
X[,5] <- log(X[,5])

Xs <- scale(X, center = T, scale = T)
sum(is.na(Xs)) == 0
summary(Xs)

# 4. Fem un LDA i donem els coeficients

library(MASS)
bin <- quality > 5
lda.out <- lda(bin ~ Xs)
lda.out$scaling

# 5. Taula de confusió
lda.pred <- predict(lda.out)

(lda.ct <- table(bin, lda.pred$class))
(err.lda <- sum(diag(lda.ct))/sum(lda.ct))

# 6. QDA
qda.out <- qda(bin ~ Xs)
qda.pred <- predict(qda.out)

(qda.ct <- table(bin, qda.pred$class))
(err.qda <- sum(diag(qda.ct))/sum(qda.ct))

# 7. Jack-and-knife

# 8. Logistic regression
mod <- glm(bin ~ Xs,
           family = "binomial")

pred <- predict(mod, type = "response")
(log.ct <- table(bin, pred > 0.5))
(err.log <- sum(diag(log.ct))/sum(log.ct))

# 9. Taula de comparació
comp.table <- data.frame(ERR.lda = err.lda,
                         ERR.qda = err.qda,
                         ERR.log = err.log)
comp.table
