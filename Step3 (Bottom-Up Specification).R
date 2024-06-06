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

