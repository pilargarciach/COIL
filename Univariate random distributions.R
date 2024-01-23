library(stats)
dchisq(1, df = 1:3)
hist(pchisq(1, df =  5, ncp = 0:5))

Z0 <- rchisq(100, df = 0, ncp = 3)
# Non-normal chi-square distribution
Z1 <- rchisq(100, df = 1, ncp = 3)
Z2 <- dcauchy(1:65)
pave <- density(Z2)
pave1 <- density(Z1/4)
plot(pave)
plot(pave1)

# install.packages("MASS")
library(MASS)
x <- rchisq(100, df = 1, ncp = 3)
y <- dcauchy(1:100)
z <- kde2d(x, y, n = 100)
d <- data.frame(x, y, z)

library(MASS)
DENS <- kde2d(d$x,d$y)
contour(DENS)



library(covsim)
sigma.target <- matrix(c(1, 0.5, 0.5, 1), 2)
set.seed(1)
vita_clayton <- vita(list(list(distr = "norm"), list(distr = "norm")), sigma.target, family_set = "clayton")
vita_joe <- vita(list(list(distr = "norm"), list(distr = "norm")), sigma.target, family_set = "joe")
library(rvinecopulib)
clayton.disc <- apply(rvine(10^3, vita_clayton), 2, cut, breaks = c(-Inf, 0, 1, Inf), labels = FALSE)
summary(vita_clayton)
