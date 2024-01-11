library(readr)
coildata <- coildata <- read_csv("coildata.csv")
library(lavaan);
modelData <- coildata 
model1 <- '
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

model1_fit <- sem(model = model1, data = coildata, meanstructure = TRUE)
model1_params <- parameterEstimates(model1_fit) 
write.csv(model1_params, "model1_params.csv")

library(MIIVsem)
miivs(model1)
model1_miiv <- miive(model = model1, data = coildata)
Model1_miiv <- estimatesTable(model1_miiv)
write.csv(Model1_miiv, "Model1_miiv.csv")

ML.lhs <- model1_params$lhs
ML.rhs <- model1_params$rhs
ML.es <- model1_params$est 
ML.se <- model1_params$se
ML.p <- model1_params$pvalue
lava1 <- data.frame(ML.lhs, ML.rhs, ML.es, ML.se, ML.p)
colnames(lava1)[1] <- "lhs"
colnames(lava1)[2] <- "rhs"

MIIVE.lhs <- Model1_miiv$lhs
MIIVE.rhs <- Model1_miiv$rhs
MIIVE.es <- Model1_miiv$est
MIIVE.se <- Model1_miiv$se
MIIVE.p <- Model1_miiv$pvalue
miiv1 <- data.frame(MIIVE.lhs, MIIVE.rhs, MIIVE.es, MIIVE.se, MIIVE.p)
colnames(miiv1)[1] <- "lhs"
colnames(miiv1)[2] <- "rhs"

## merge lava1 and miiv1
MethodsComparison <- merge(lava1, miiv1, by = c("lhs", "rhs"), all.x = TRUE, all.y = TRUE)


## extract lavaan fit estimates
chi              <- round(fitMeasures(model1_fit)["chisq"], 2)
chidf            <- fitMeasures(model1_fit)["df"]
chip             <- round(fitMeasures(model1_fit)["pvalue"], 2)

## extract MIIVsem fit estimates
sarg             <- round(model1_miiv[12, "Sargan"],2)
sargdf           <- model1_miiv[19, "df"]
sargp            <- round(model1_miiv[19, "P(Chi)"], 2)

## create comparison table for display
likchi_lavaan    <- paste0(chi, ", df = ", chidf, ", p = ", chip)
eq1sarg_lavaan   <- "---------------------"
eq2sarg_lavaan   <- "---------------------"
likchi_miiv      <- "---------------------"
eq1sarg_miiv     <- paste0("0 (exactly identified)")
eq2sarg_miiv     <- paste0(sarg, ", df = ", sargdf, ", p = ", sargp)
col1             <- rbind(likchi_lavaan, eq1sarg_lavaan, eq2sarg_lavaan)
col2             <- rbind(likchi_miiv, eq1sarg_miiv, eq2sarg_miiv)
fit0             <- cbind(col1, col2)
rownames(fit0)  <- c("Model 1", "Model 1: educ equation", "Model 1: prestg80 equation")
colnames(fit0)  <- c("Likelihood Chi Square", "Sargan Chi Square")

print(mod1)
print(fit0, quote = FALSE, right = FALSE, row.names = TRUE)
