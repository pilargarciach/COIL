library(readr)
coil <- read_delim("coil.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
coil <- coil[1:48]
coil <- coil[c(9:13,15:19,21:25,27:48)]
variable.names(coil)
colnames(coil)[1] <- "SP1"
colnames(coil)[2] <- "SP2"
colnames(coil)[3] <- "SP3"
colnames(coil)[4] <- "SP4"
colnames(coil)[5] <- "SP5"
colnames(coil)[6] <- "CR1"
colnames(coil)[7] <- "CR2"
colnames(coil)[8] <- "CR3"
colnames(coil)[9] <- "CR4"
colnames(coil)[10] <- "CR5"
colnames(coil)[11] <- "CD1"
colnames(coil)[12] <- "CD2"
colnames(coil)[13] <- "CD3"
colnames(coil)[14] <- "CD4"
colnames(coil)[15] <- "CD5"
colnames(coil)[16] <- "IN1"
colnames(coil)[17] <- "IN2"
colnames(coil)[18] <- "IN3"
colnames(coil)[19] <- "IN4"
colnames(coil)[20] <- "IN5"
colnames(coil)[21] <- "IN6"
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-
colnames(coil)[] <-