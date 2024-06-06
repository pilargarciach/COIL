library(readr)
coildata <- coildata <- read_csv("coildata.csv")

library(lavaan);
modelData <- coildata
model<-"
! regressions 
   SP=~SP__SP1*SP1
   SP=~SP__SP2*SP2
   SP=~SP__SP3*SP3
   SP=~SP__SP4*SP4
   SP=~SP__SP5*SP5
! residuals, variances and covariances
   SP1 ~~ VAR_SP1*SP1
   SP2 ~~ VAR_SP2*SP2
   SP3 ~~ VAR_SP3*SP3
   SP4 ~~ VAR_SP4*SP4
   SP5 ~~ VAR_SP5*SP5
   SP ~~ 1.0*SP
! observed means
   SP1~1;
   SP2~1;
   SP3~1;
   SP4~1;
   SP5~1;
";

result2 <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE);
summary(result2, fit.measures=TRUE);

SP <- summary(result2, fit.measures=TRUE)
ParametersSP <- data.frame(SP$pe)
round(ParametersSP$est, digits = 2)

SP <- data.frame(SP$fit)
SP$Index <- rownames(SP)


modelSP <-"
! regressions 
   SP=~SP__SP1*SP1
   SP=~SP__SP3*SP3
   SP=~SP__SP4*SP4
   SP=~SP__SP5*SP5
! residuals, variances and covariances
   SP1 ~~ VAR_SP1*SP1
   SP3 ~~ VAR_SP3*SP3
   SP4 ~~ VAR_SP4*SP4
   SP5 ~~ VAR_SP5*SP5
   SP ~~ 1.0*SP
! observed means
   SP1~1;
   SP3~1;
   SP4~1;
   SP5~1;
"
result2 <- cfa(modelSP, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE)

SP2 <- summary(result2, fit.measures=TRUE)
SP2 <- data.frame(SP2$fit)
SP2

library(semPlot)
png("F4A.png", width = 15, height = 10, units = 'in', res = 300)
semPaths(result2, whatLabels = "std", 
         layout = "tree", 
         color = list(lat = rgb(0, 204, 0, maxColorValue = 255),
                      man = rgb(155, 253, 175, maxColorValue = 255)),
         edge.color = "black",
         shapeMan = "rectangle",
         edge.label.cex = 1.5,
         edge.width = 1.5,
         label.cex = 1.5,
         node.width = 2,
         node.height = 2,
         mar = c(6, 1.2, 9, 1.2), title = FALSE, intercepts = FALSE, residuals = TRUE, nCharNodes = 0)
dev.off()

modelData <- coildata
model<-"
! regressions 
   CR=~CR__CR1*CR1
   CR=~CR__CR2*CR2
   CR=~CR__CR3*CR3
   CR=~CR__CR4*CR4
   CR=~CR__CR5*CR5
! residuals, variances and covariances
   CR1 ~~ VAR_CR1*CR1
   CR2 ~~ VAR_CR2*CR2
   CR3 ~~ VAR_CR3*CR3
   CR4 ~~ VAR_CR4*CR4
   CR5 ~~ VAR_CR5*CR5
   CR ~~ 1.0*CR
! observed means
   CR1~1;
   CR2~1;
   CR3~1;
   CR4~1;
   CR5~1;
";

result2 <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
summary(result2, fit.measures=TRUE);

CR <- summary(result2, fit.measures=TRUE)
CR$test
CR <- data.frame(CR$fit)
CR$Index <- rownames(CR)


modelData <- coildata
model<-"
! regressions 
   CR=~CR__CR1*CR1
   CR=~CR__CR2*CR2
   CR=~CR__CR3*CR3
! residuals, variances and covariances
   CR1 ~~ VAR_CR1*CR1
   CR2 ~~ VAR_CR2*CR2
   CR3 ~~ VAR_CR3*CR3
   CR ~~ 1.0*CR
! observed means
   CR1~1;
   CR2~1;
   CR3~1;
";

result4 <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE);

CR2 <- summary(result4, fit.measures=TRUE)
CR2 <- data.frame(CR2$fit)
CR2

library(semPlot)
png("F4B.png", width = 15, height = 10, units = 'in', res = 300)
semPaths(result4, 
         whatLabels = "std", 
         layout = "tree", 
         color = list(lat = rgb(255, 255, 0, maxColorValue = 255),
                      man = rgb(255, 255, 153, maxColorValue = 255)),
         edge.color = "black",
         shapeMan = "rectangle",
         edge.label.cex = 1.5,
         edge.width = 1.5,
         label.cex = 1.5,
         node.width = 2,
         node.height = 2,
         mar = c(6, 1.2, 9, 1.2), title = FALSE, intercepts = FALSE, residuals = TRUE, nCharNodes = 0)
dev.off()

modelData <- coildata
modelCD<-"
! regressions 
   CD=~CD__CD1*CD1
   CD=~CD__CD2*CD2
   CD=~CD__CD4*CD4
! residuals, variances and covariances
   CD1 ~~ VAR_CD1*CD1
   CD2 ~~ VAR_CD2*CD2
   CD4 ~~ VAR_CD4*CD4
   CD ~~ 1.0*CD
! observed means
   CD1~1;
   CD2~1;
   CD4~1;
";



result6 <- cfa(modelCD, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE);

CD2 <- summary(result6, fit.measures=TRUE)
CD2 <- data.frame(CD2$fit)
CD2

library(semPlot)
png("F4C.png", width = 15, height = 10, units = 'in', res = 300)
semPaths(result6, 
         whatLabels = "std", 
         layout = "tree", 
         color = list(lat = rgb(255, 0, 0, maxColorValue = 255),
                      man = rgb(255, 102, 102, maxColorValue = 255)),
         edge.color = "black",
         shapeMan = "rectangle",
         edge.label.cex = 1.5,
         edge.width = 1.5,
         label.cex = 1.5,
         node.width = 2,
         node.height = 2,
         mar = c(6, 1.2, 9, 1.2), title = FALSE, intercepts = FALSE, residuals = TRUE, nCharNodes = 0)
dev.off()
