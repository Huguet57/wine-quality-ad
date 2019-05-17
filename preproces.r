dd.link <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
dd <- read.csv2(dd.link)

D <- ncol(dd) - 1
X <- data.matrix(dd[,1:D])
df <- data.frame(X)

## Feature extraction
# df$log.res.sug <- log(as.numeric(dd$residual.sugar))
# df$log.chlorides <- log(as.numeric(dd$chlorides))
quality.low <- which(dd$quality <= 4)
quality.med <- which(4 < dd$quality & dd$quality < 7)
quality.high <- which(7 <= dd$quality)

dd$quality.section <- rep(NA, N)
dd$quality.section[quality.low] <- 0
dd$quality.section[quality.med] <- 1
dd$quality.section[quality.high] <- 2

D <- ncol(df)
N <- nrow(dd)

head(df)