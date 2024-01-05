library(readr)
coil <- read_delim("coil.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

DS <- data.frame(coil[27:48])
variable.names(DS)
IN <- data.frame(DS[1:6])
PR <- data.frame(DS[7:13])
AP <- data.frame(DS[14:17])
EX <- data.frame(DS[18:22])

library(MVN)
MVN::mvn(DS)

library(lavaan);
modelData <- coil ;
model<-"
! regressions 
   DS=~x2__Instrumental*IN
   DS=~x2__Stra_Privilege*PR
   DS=~x2__Stra_Appropriation*AP
   DS=~x2__Expansive*EX
! residuals, variances and covariances
   IN ~~ VAR_Instrumental*IN
   PR ~~ VAR_Stra_Privilege*PR
   AP ~~ VAR_Stra_Appropriation*AP
   EX ~~ VAR_Expansive*EX
   DS ~~ 1.0*DS
! observed means
   IN~1;
   PR~1;
   AP~1;
   EX~1;
";

result1<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="ML", std.ov=TRUE);
result2<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
m1 <- lavInspect(result2, what = "vcov.std.all")
eigen(m1)
chol(m1)

summary(result1, fit.measures=TRUE);
summary(result2, fit.measures=TRUE);

library(semPlot)
semPaths(result2, whatLabels = "std", layout = "tree", color = list(
  lat = rgb(124, 12, 199, maxColorValue = 255),
  man = rgb(155, 253, 175, maxColorValue = 255)),
  edge.color = "black",
  edge.label.cex = 1,
  edge.width = 1.5,
  label.cex = 2,
  node.width = 2,
  node.height = 2,
  mar = c(10, 5, 10, 5), intercepts = FALSE, residuls = FALSE, nCharNodes = 0)

library(semTable)
semTable(list("Model 1" = result1 , "Model 2" = result2),
         fits = c("chisq", "rmsea", "srmr", "cfi", "tli"),
         columns = c("estsestars", "rsquare" ,"p"),
         paramSets = c("loadings" , "slopes", "latentcovariances"),
         type = "latex", table.float = TRUE , longtable = FALSE,
         caption = "Confirmatory Factor Analysis for the Digital Skill Psychometric Structure",
         label = "t3")
