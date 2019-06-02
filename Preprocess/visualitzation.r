for (k in 1:D) {
  var <- as.numeric(df[,k])
  varname <- colnames(df)[k]
  hist(var, main = varname)
}

library(corrplot)
corrplot(cor(df),
         type="upper")
