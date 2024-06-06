load("~/Documents/GitHub/coil/Project/Results/lslx_Estimates.RData")
load("~/Documents/GitHub/coil/Project/Results/MIIV_Estimates.RData")
load("~/Documents/GitHub/coil/Project/Results/ML_Estimates.RData")
EstimatesLSLX$Parameter <- rownames(EstimatesLSLX)
library(dplyr)
EstimatesLSLX <- EstimatesLSLX %>% slice((15 + 1):30)
EstimatesMIIV <- EstimatesMIIV %>% slice(1:15)
EstimatesML <- EstimatesML %>% slice(1:15)

colnames(EstimatesLSLX)[1] <- "est"
colnames(EstimatesLSLX)[2] <- "se"
colnames(EstimatesLSLX)[3] <- "z"
colnames(EstimatesLSLX)[4] <- "pvalue"

EstimatesLSLX$Method <- "PL"
EstimatesMIIV$Method <- "MIIVs"
EstimatesML$Method <- "ML"
EstimatesLSLX$rhs <- substr(EstimatesLSLX$Parameter , start = 1, stop = 3)

lslx <- EstimatesLSLX[c(8:9,1:4)]
ml <- EstimatesML[c(10,3,6:9)]
MIIVS <- EstimatesMIIV[c(8,3:7)]

AllEstimations <- list(lslx, ml, MIIVS)
Estimations <- do.call("rbind", AllEstimations)
rownames(Estimations) <- 1:45
Estimations <- Estimations[!grepl("SP1|CR1|CD1", Estimations$rhs), ]
rm(list=setdiff(ls(), "Estimations"))

library(psych)
describe.by(Estimations$se, group = Estimations$Method, mat = TRUE, digits = 3)


library(ggplot2)
png("F5A.png", width = 9, height = 6, units = 'in', res = 300)
ggplot(Estimations, aes(x=est, y=se, color =Method)) +
  geom_point() + # Show dots
  geom_text(
    label=Estimations$rhs, 
    nudge_x = 0.01, nudge_y = 0.01, 
    check_overlap = F
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 20, color = "black"), 
    axis.text = element_text(size = 20, color = "black") 
  ) +
  scale_color_manual(values = c("green4", "#E7B800", "#7c0cc7")) +
  xlab("Statistical Estimation") + 
  ylab("Standard Error")

dev.off()

library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
png("F5B.png", width = 9, height = 6, units = 'in', res = 300)
ggplot(Estimations, aes(x = se, y = Method)) +
  geom_density_ridges(alpha = 0.7, aes(fill = Method)) +
  scale_fill_manual(values = c("green4", "#E7B800", "#7c0cc7")) +
  theme_minimal() +
  theme(
    legend.position = "none", # Removing the legend
    axis.title = element_text(size = 20, colour = "black"), 
    axis.text = element_text(size = 20, color = "black") 
  ) + 
  xlab("Standard error of estimation")
dev.off()



