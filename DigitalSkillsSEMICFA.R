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

lslxCFA <- lslx$new(model = model, data = modelData)
lslxCFA$fit(penalty_method = "lasso", lambda_grid = "default", delta_grid = "default")


result<-lavaan(model, data=modelData, fixed.x=FALSE, missing="FIML");
summary(result, fit.measures=TRUE);