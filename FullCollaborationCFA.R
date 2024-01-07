library(readr)
coil <- read_delim("coil.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
coil <- coil[1:48]
coil <- coil[c(9:13,15:19,21:25,27:48)]
library(lavaan);
modelData <- coil
model<-"
! regressions 
   SP=~SP__Espe1*Espe1
   SP=~SP__Espe2*Espe2
   SP=~SP__Espe3*Espe3
   SP=~SP__Espe4*Espe4
   SP=~SP__Espe5*Espe5
   CR=~CR__Credib1*Credib1
   CR=~CR__Credib2*Credib2
   CR=~CR__Credib3*Credib3
   CR=~CR__Credib4*Credib4
   CR=~CR__Credib5*Credib5
   CD=~CD__Coord1*Coord1
   CD=~CD__Coord2*Coord2
   CD=~CD__Coord3*Coord3
   CD=~CD__Coord4*Coord4
   CD=~CD__Coord5*Coord5
   TMS=~TMS__SP*SP
   TMS=~TMS__CR*CR
   TMS=~TMS__CD*CD
! residuals, variances and covariances
   Espe1 ~~ VAR_Espe1*Espe1
   Espe2 ~~ VAR_Espe2*Espe2
   Espe3 ~~ VAR_Espe3*Espe3
   Espe4 ~~ VAR_Espe4*Espe4
   Espe5 ~~ VAR_Espe5*Espe5
   Credib1 ~~ VAR_Credib1*Credib1
   Credib2 ~~ VAR_Credib2*Credib2
   Credib3 ~~ VAR_Credib3*Credib3
   Credib4 ~~ VAR_Credib4*Credib4
   Credib5 ~~ VAR_Credib5*Credib5
   Coord1 ~~ VAR_Coord1*Coord1
   Coord2 ~~ VAR_Coord2*Coord2
   Coord3 ~~ VAR_Coord3*Coord3
   Coord4 ~~ VAR_Coord4*Coord4
   Coord5 ~~ VAR_Coord5*Coord5
   SP ~~ 1.0*SP
   CR ~~ 1.0*CR
   CD ~~ 1.0*CD
   TMS ~~ 1.0*TMS
   SP ~~ 0.0*CR
   SP ~~ 0.0*CD
   CR ~~ 0.0*CD
! means
   Espe1~0*1;
   Espe2~0*1;
   Espe3~0*1;
   Espe4~0*1;
   Espe5~0*1;
   Credib1~0*1;
   Credib2~0*1;
   Credib3~0*1;
   Credib4~0*1;
   Credib5~0*1;
   Coord1~0*1;
   Coord2~0*1;
   Coord3~0*1;
   Coord4~0*1;
   Coord5~0*1;
   SP~0*1;
   CR~0*1;
   CD~0*1;
   TMS~0*1;
";
result1<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="ML", std.ov=TRUE);
result2<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);

lavInspect(result2, what = "std.all")

m1 <- lavInspect(result2, what = "vcov.std.all")
eigen(m1)
chol(m1)
