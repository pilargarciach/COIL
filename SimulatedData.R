library(readr)
coildata <- coildata <- read_csv("coildata.csv")

library(MVN)
Skewness <- mvn(coildata)$Descriptives[9]
Kurtosis <- mvn(coildata)$Descriptives[10]

 <- mvn(coildata)


library(covsim)
sigma.target  <- cov(MASS::mvrnorm(5, rep(0,3), diag(3)))
res  <- covsim::rPLSIM(10^5, sigma.target, skewness=rep(1,3), excesskurtosis=rep(4,3))
my.sample  <- res[[1]][[1]]

res$model
