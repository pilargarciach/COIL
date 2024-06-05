library(readr)
coildata <- coildata <- read_csv("coildata.csv")
library(MVN)
mvn(coildata)

library(ggplot2)

coildata_long <- reshape2::melt(coildata)

ggplot(coildata_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_minimal() +  # Optional: Customize the theme
  labs(title = "Statistical distributions of observed variables")
