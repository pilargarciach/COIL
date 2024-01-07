library(readr)
coildata <- read_csv("coildata.csv")

library(lavaan)
modelData <- coildata
model<-"
! regressions 
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
   IN=~IN__IN1*IN1
   IN=~IN__IN2*IN2
   IN=~IN__IN3*IN3
   IN=~IN__IN4*IN4
   IN=~IN__IN5*IN5
   IN=~IN__IN6*IN6
   PR=~PR__PR1*PR1
   PR=~PR__PR2*PR2
   PR=~PR__PR3*PR3
   PR=~PR__PR4*PR4
   PR=~PR__PR5*PR5
   PR=~PR__PR6*PR6
   PR=~PR__PR7*PR7
   AP=~AP__AP1*AP1
   AP=~AP__AP2*AP2
   AP=~AP__AP3*AP3
   AP=~AP__AP4*AP4
   EX=~EX__EX1*EX1
   EX=~EX__EX2*EX2
   EX=~EX__EX3*EX3
   EX=~EX__EX4*EX4
   EX=~EX__EX5*EX5
   TMS=~TMS__SP*SP
   TMS=~TMS__CR*CR
   TMS=~TMS__CD*CD
   DS=~DS__IN*IN
   DS=~DS__PR*PR
   DS=~DS__AP*AP
   DS=~DS__EX*EX
   DS=~DS__TMS*TMS
! residuals, variances and covariances
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
   TMS ~~ 1.0*TMS
   IN1 ~~ VAR_IN1*IN1
   IN2 ~~ VAR_IN2*IN2
   IN3 ~~ VAR_IN3*IN3
   IN4 ~~ VAR_IN4*IN4
   IN5 ~~ VAR_IN5*IN5
   IN6 ~~ VAR_IN6*IN6
   PR1 ~~ VAR_PR1*PR1
   PR2 ~~ VAR_PR2*PR2
   PR3 ~~ VAR_PR3*PR3
   PR4 ~~ VAR_PR4*PR4
   PR5 ~~ VAR_PR5*PR5
   PR6 ~~ VAR_PR6*PR6
   PR7 ~~ VAR_PR7*PR7
   AP1 ~~ VAR_AP1*AP1
   AP2 ~~ VAR_AP2*AP2
   AP3 ~~ VAR_AP3*AP3
   AP4 ~~ VAR_AP4*AP4
   EX1 ~~ VAR_EX1*EX1
   EX2 ~~ VAR_EX2*EX2
   EX3 ~~ VAR_EX3*EX3
   EX4 ~~ VAR_EX4*EX4
   EX5 ~~ VAR_EX5*EX5
   IN ~~ 1.0*IN
   PR ~~ 1.0*PR
   AP ~~ 1.0*AP
   EX ~~ 1.0*EX
   DS ~~ 1.0*DS
   SP ~~ 0.0*CR
   SP ~~ 0.0*CD
   SP ~~ 0.0*IN
   SP ~~ 0.0*PR
   SP ~~ 0.0*AP
   SP ~~ 0.0*EX
   SP ~~ 0.0*DS
   CR ~~ 0.0*CD
   CR ~~ 0.0*IN
   CR ~~ 0.0*PR
   CR ~~ 0.0*AP
   CR ~~ 0.0*EX
   CR ~~ 0.0*DS
   CD ~~ 0.0*IN
   CD ~~ 0.0*PR
   CD ~~ 0.0*AP
   CD ~~ 0.0*EX
   CD ~~ 0.0*DS
   TMS ~~ 0.0*IN
   TMS ~~ 0.0*PR
   TMS ~~ 0.0*AP
   TMS ~~ 0.0*EX
   IN ~~ 0.0*PR
   IN ~~ 0.0*AP
   IN ~~ 0.0*EX
   PR ~~ 0.0*AP
   PR ~~ 0.0*EX
   AP ~~ 0.0*EX
! means
   SP1~0*1;
   SP2~0*1;
   SP3~0*1;
   SP4~0*1;
   SP5~0*1;
   CR1~0*1;
   CR2~0*1;
   CR3~0*1;
   CR4~0*1;
   CR5~0*1;
   CD1~0*1;
   CD2~0*1;
   CD3~0*1;
   CD4~0*1;
   CD5~0*1;
   SP~0*1;
   CR~0*1;
   CD~0*1;
   TMS~0*1;
   IN1~0*1;
   IN2~0*1;
   IN3~0*1;
   IN4~0*1;
   IN5~0*1;
   IN6~0*1;
   PR1~0*1;
   PR2~0*1;
   PR3~0*1;
   PR4~0*1;
   PR5~0*1;
   PR6~0*1;
   PR7~0*1;
   AP1~0*1;
   AP2~0*1;
   AP3~0*1;
   AP4~0*1;
   EX1~0*1;
   EX2~0*1;
   EX3~0*1;
   EX4~0*1;
   EX5~0*1;
   IN~0*1;
   PR~0*1;
   AP~0*1;
   EX~0*1;
   DS~0*1;
";


result1<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="ML", std.ov=TRUE);
result2<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
# To inspect the standardized estimates for factor loadings
lavInspect(result2, what = "std.all")

m1 <- lavInspect(result1, what = "vcov.std.all")
eigen(m1)
chol(m1)

library(MIIVsem)
miivs(model)
summary(miivs(model), eq.info = TRUE)
fit <- miive(model, modelData, sarg.adjust = "none")
summary(fit, restrict.tests = TRUE)

summary(miive(model, modelData, estimator = "2SLS", var.cov = TRUE))
pave <- fitmeasures(eje, c("cfi"))
estimatesTable(eje)
print(eje)
