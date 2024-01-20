library(readr)
coildata <- coildata <- read_csv("coildata.csv")
coil2 <- replicate(5, coildata, simplify = FALSE) 
coil2  <-  do.call(rbind, coil2)

summary(coildata) == summary(coil2)

library(lavaan)
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


result1 <- lavaan(model, data=coildata, fixed.x=FALSE, estimator="ML", std.ov=TRUE);
result2 <- lavaan(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE)
result3 <- lavaan(model, data=coil2, fixed.x=FALSE, estimator = "ML", std.ov=TRUE)
result4 <- lavaan(model, data=coil2, fixed.x=FALSE, estimator = "MLM", std.ov=TRUE)


lavTestLRT(result1)
lavTestLRT(result2)
lavTestLRT(result3)
lavTestLRT(result4)





m1 <- lavInspect(result2, what = "vcov.std.all")
chol(m1)
eigen(m1)

m2 <- lavInspect(result4, what = "vcov.std.all")
chol(m2)
eigen(m2)




library(semTable)
semTable(list("Model A" = result1, "Model B" = result2),
         columns = c("estsestars", "rsquare" ,"p"), 
         fits = c("chisq", "rmsea", "srmr", "cfi", "tli"),
         paramSets = c("loadings", "latentcovariances"),
         table.float = TRUE, 
         longtable = FALSE, 
         caption = "Statistical Estimated Parameters for Collaboration",
         label = "t1")



library(semPlot)
semPaths(result2, whatLabels = "std", layout = "tree", color = list(
  lat = rgb(124, 12, 199, maxColorValue = 255),
  man = rgb(155, 253, 175, maxColorValue = 255)),
  edge.color = "black",
  edge.label.cex = 1,
  edge.width = 1.5,
  label.cex = 1,
  node.width = 1,
  node.height = 1,
  mar = c(3, 1, 3, 1), intercepts = FALSE, residuls = FALSE, nCharNodes = 0)

fit1 <- summary(result1, fit.measures=TRUE)
fit2 <- summary(result2, fit.measures=TRUE)
fit3 <- summary(result3, fit.measures=TRUE)
fit4 <- summary(result4, fit.measures=TRUE)