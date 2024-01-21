library(readr)
coildata <- coildata <- read_csv("coildata.csv")
coildata_long <- reshape2::melt(coildata)

library(ggplot2)
png("F2.png", width = 15, height = 7, units = 'in', res = 300)
ggplot(coildata_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_minimal() +  # Optional: Customize the theme
  labs(title = "Statistical distributions of observed variables")
dev.off()

coil2 <- replicate(5, coildata, simplify = FALSE) 
coil2  <-  do.call(rbind, coil2)

summary(coildata) == summary(coil2)



library(MVN)
mvn(coildata)
mvn(coil2)

coildata_long2 <- reshape2::melt(coil2)

ggplot(coildata_long2, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_minimal() +  # Optional: Customize the theme
  labs(title = "Statistical distributions of observed variables")



library(lavaan);
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

result0 <- lavaan.2stage(model=model, modelData, se = "standard")
result1 <- lavaan(model, data=modelData, fixed.x=FALSE, estimator="ML", std.ov=TRUE);
result2 <- lavaan(model, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE)
result3 <- lavaan(model, data=coil2, fixed.x=FALSE, estimator = "ML", std.ov=TRUE)
result4 <- lavaan(model, data=coil2, fixed.x=FALSE, estimator = "MLM", std.ov=TRUE)


lavTestLRT(result1)
lavTestLRT(result2)
lavTestLRT(result3)
lavTestLRT(result4)





m1 <- lavInspect(result2, what = "vcov.std.all")
chol(m1)
eigen(m1)

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



library(semTools)
mi <- modificationIndices(result1)
mi <- mi
hist(mi$mi)
mi$epc
MODTEST <- miPowerFit(result0, stdLoad = 0.4, cor = 0.1, stdBeta = 0.1, intcept = 0.2, cilevel = 0.95)
table(MODTEST$decision.pow)
table(MODTEST$decision.ci)


Residual <- lavTest(result2, test = "browne.residual.adf")
lavTest(result2, test = "browne.residual.nt")
Residual$stat.group

fit2$data[2]

ModelStatistics <- c("Chi2", "CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Sample.Size")
ModelA <- c(fit1$fit[3], fit1$fit[9],  fit1$fit[10], fit1$fit[17], fit1$fit[25], fit1$fit[13], fit1$fit[14], fit1$data[2])
ModelB <- c(fit2$fit[6], fit2$fit[21], fit2$fit[22], fit2$fit[42], fit2$fit[47], fit2$fit[25], fit2$fit[26], fit2$data[2])
ModelC <- c(fit3$fit[3], fit3$fit[9],  fit3$fit[10], fit3$fit[17], fit3$fit[25], fit3$fit[13], fit3$fit[14], fit3$data[2])
ModelD <- c(fit4$fit[3], fit4$fit[9],  fit4$fit[10], fit4$fit[17], fit4$fit[25], fit4$fit[13], fit4$fit[14], fit4$data[2])
results <- list(c(ModelA, ModelB, ModelC, ModelD))
results <- do.call("rbind", list(ModelA, ModelB, ModelC, ModelD))
Results <- data.frame(ModelStatistics, ModelA, ModelB, ModelC, ModelD)
Results$ModelStatistics <- c("Chi2", "CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Sample.Size")
Results <- Results[, c(3, 1, 2)]
Results <- round(Results[2:3], 3)
fit1$fit[14]
fit2$fit[26]

fit1$fit





# Penalized Model1

library(lslx)
PenalizedModel <- '
   SP =~ free() * SP1 + pen() * SP2 + free() * SP3 + free() * SP4 + free() * SP5
   CR =~ free() * CR1 + free() * CR2 + free() * CR3 + pen() * CR4 + pen() * CR5
   CD =~ free() * CD1 + free() * CD2 + pen() * CD3 + free() * CD4 + pen() * CD5
   SP <=> fix(1) * SP
   CR <=> fix(1) * CR
   CD <=> fix(1) * CD
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

lslx_fa <- lslx$new(model = PenalizedModel, data = coildata)

lslx_fa$fit(penalty_method = "mcp", lambda_grid = seq(0.01, 0.60, 0.01), delta_grid = c(1.5, 3.0, Inf))
lslx_fa$summarize(selector = "bic", interval = FALSE)
