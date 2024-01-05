library(readr)
coil <- read_delim("coil.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
coil <- coil[51:54]
library(lslx);
modelData <- coil ;
model<- "Digital_Skills :~> IN + PR + AP + EX
   Digital_Skills ~~ 1.0*Digital_Skills
   IN ~~ VAR_IN*IN
   PR ~~ VAR_PR*PR
   AP ~~ VAR_AP*AP
   EX ~~ VAR_EX*EX
   IN~1;
   PR~1; 
   AP~1;
   EX~1"


IN <- c(0.985, 0.72, 0.616, 0.477)
PR <- c(0.72, 0.985, 0.748, 0.58)
AP <- c(0.616, 0.748, 0.985, 0.496)
EX <- c(0.477, 0.588, 0.496, 0.985)
samplecov <- rbind(IN, PR, AP, EX)
colnames(samplecov) <- c("IN", "PR", "AP", "EX")

lslxCFA <- lslx$new(model = model, sample_cov = samplecov, sample_size = 65)
vcov(lslxCFA, selector = "aic")
moment_jacobian <- lslxCFA$extract_moment_jacobian(selector = "bic", type = "effective")

# Following the suggestions of Po-Hsien Huang (2020),
# we used the lambda matrix in confirmatory factor 
# analysis estimations, to define the values of 
# the lambda_grid argument required by lslx syntax

lslxCFA$fit(penalty_method = "mcp", lambda_grid = seq(0.81, 0.62), delta_grid = seq(0.110,0.600))


result<-lavaan(model, data=modelData, fixed.x=FALSE, missing="FIML");
summary(result, fit.measures=TRUE);