library(readr)
coildata <- coildata <- read_csv("coildata.csv")

library(MVN)
mvn(coildata)
Skewness <- mvn(coildata)$Descriptives[9]
Skewness <- round(Skewness$Skew, digits = 3) 

Kurtosis <- mvn(coildata)$Descriptives[10]
Kurtosis <- round(Kurtosis$Kurtosis, digits = 3)

library(lavaan)
modelData <- coildata 
model <- '
   SP=~SP__SP1*SP1
   SP=~SP__SP2*SP2
   SP=~SP__SP3*SP3
   SP=~SP__SP4*SP4
   SP=~SP__SP5*SP5
   CR=~CR__CR1*CR1
   CR=~CR__CR2*CR2
   CR=~CR__CR3*CR3
   CR=~CR__CR4*CR4
   CR=~CR__CR5*CR5
   CD=~CD__CD1*CD1
   CD=~CD__CD2*CD2
   CD=~CD__CD3*CD3
   CD=~CD__CD4*CD4
   CD=~CD__CD5*CD5
   SP1 ~~ VAR_SP1*SP1
   SP2 ~~ VAR_SP2*SP2
   SP3 ~~ VAR_SP3*SP3
   SP4 ~~ VAR_SP4*SP4
   SP5 ~~ VAR_SP5*SP5
   CR1 ~~ VAR_CR1*CR1
   CR2 ~~ VAR_CR2*CR2
   CR3 ~~ VAR_CR3*CR3
   CR4 ~~ VAR_CR4*CR4
   CR5 ~~ VAR_CR5*CR5
   CD1 ~~ VAR_CD1*CD1
   CD2 ~~ VAR_CD2*CD2
   CD3 ~~ VAR_CD3*CD3
   CD4 ~~ VAR_CD4*CD4
   CD5 ~~ VAR_CD5*CD5
   SP ~~ 1.0*SP
   CR ~~ 1.0*CR
   CD ~~ 1.0*CD
   SP ~~ COV_SP_CR*CR
   CR ~~ COV_CR_CD*CD
   SP ~~ COV_SP_CD*CD
   SP1~1;
   SP2~1;
   SP3~1;
   SP4~1;
   SP5~1;
   CR1~1;
   CR2~1;
   CR3~1;
   CR4~1;
   CR5~1;
   CD1~1;
   CD2~1;
   CD3~1;
   CD4~1;
   CD5~1;'

fit  <- sem(model, data=modelData)
population.sigma <- lavInspect(fit, "sigma.hat")
population.sigma <- as.matrix(population.sigma)

library(covsim)
set.seed(1234)
res  <- covsim::rPLSIM(10^3, population.sigma, skewness=Skewness, excesskurtosis=Kurtosis)
S1 <- data.frame(res$samples[1])
mvn(S1)

coildata_long <- reshape2::melt(coildata)
library(ggplot2)
ggplot(coildata_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_minimal() +  # Optional: Customize the theme
  labs(title = "Statistical distributions of observed variables")

coildata_long2 <- reshape2::melt(S1)
ggplot(coildata_long2, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_minimal() +  # Optional: Customize the theme
  labs(title = "Statistical distributions of observed variables")

mvn(S1)
mvn(coildata)

colnames(S1)[1:15] <- variable.names(coildata)
variable.names(S1)

fit2  <- cfa(model, S1)

fit
fit2

summary(fit2, fit.measures=TRUE)
summary(fit, fit.measures=TRUE)


library(semTools)
mi <- modificationIndices(fit)
mi <- mi
hist(mi$mi)
mi$epc
MODTEST <- miPowerFit(fit, stdLoad = 0.4, cor = 0.1, stdBeta = 0.1, intcept = 0.2, cilevel = 0.95)
table(MODTEST$decision.pow)
table(MODTEST$decision.ci)


mi2 <- modificationindices(fit2)
mi2 <- mi2
hist(mi2$mi)
mi2$epc
MODTEST2 <- miPowerFit(fit2, stdLoad = 0.4, cor = 0.1, stdBeta = 0.1, intcept = 0.2, cilevel = 0.95)

table(MODTEST2$decision.pow)
table(MODTEST2$decision.ci)
