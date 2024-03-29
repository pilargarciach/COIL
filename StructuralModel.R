library(readr)
coil <- read_delim("coil.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(lavaan);
modelData <- coil
model<-"
! regressions 
   CO=~CO__SP*SP
   CO=~CO__CR*CR
   CO=~CO__CD*CD
   DS=~DS__IN*IN
   DS=~DS__PR*PR
   DS=~DS__AP*AP
   DS=~DS__EX*EX
   DS=~DS__CO*CO
! residuals, variances and covariances
   SP ~~ VAR_SP*SP
   CR ~~ VAR_CR*CR
   CD ~~ VAR_CD*CD
   CO ~~ 1.0*CO
   DS ~~ 1.0*DS
   IN ~~ VAR_IN*IN
   PR ~~ VAR_PR*PR
   AP ~~ VAR_AP*AP
   EX ~~ VAR_EX*EX
! observed means
   SP~1;
   CR~1;
   CD~1;
   IN~1;
   PR~1;
   AP~1;
   EX~1;
";

result5<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="ML", std.ov=TRUE);
result6<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
m6 <- lavInspect(result6, what = "vcov.std.all")
eigen(m6)
chol(m6)

summary(result5, fit.measures=TRUE);
summary(result6, fit.measures=TRUE);

library(semPlot)
set.seed(1234)
semPaths(result6, whatLabels = "std", layout = "groups", color = list(
  lat = rgb(124, 12, 199, maxColorValue = 255),
  man = rgb(155, 253, 175, maxColorValue = 255)),
  edge.color = "black",
  edge.label.cex = 1,
  edge.width = 1.5,
  label.cex = 2,
  node.width = 1,
  node.height = 1,
  mar = c(10, 5, 10, 5), intercepts = FALSE, residuls = FALSE, nCharNodes = 0)

