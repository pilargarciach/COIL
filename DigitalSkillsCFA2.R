library(readr)
coil <- read_delim("coil.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

DS <- data.frame(coil[c(51,54,55)])
variable.names(DS)
IN <- data.frame(coil[1:6])
ST <- data.frame(coil[7:17])
EX <- data.frame(coil[18:22])

library(MVN)
MVN::mvn(DS)

library(lavaan);
modelData <- DS;
model<-"
! regressions 
   DS=~DS__IN*IN
   DS=~DS__EX*EX
   DS=~DS__ST*ST
! residuals, variances and covariances
   IN ~~ VAR_IN*IN
   EX ~~ VAR_EX*EX
   ST ~~ VAR_ST*ST
   DS ~~ 1.0*DS
! observed means
   IN~1;
   EX~1;
   ST~1;
";
result1A<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="ML", std.ov=TRUE);
result2A<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
m1a <- lavInspect(result2A, what = "vcov.std.all")
eigen(m1a)
chol(m1a)

summary(result1, fit.measures=TRUE);
summary(result2, fit.measures=TRUE);
