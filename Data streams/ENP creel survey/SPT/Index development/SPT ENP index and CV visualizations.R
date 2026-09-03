#########################################
#########################################
#########################################
### ENP creel spotted seatrout index standardization
### Manuel Coffill-Rivera
### started 9/3/26
#########################################
#########################################
#########################################


########### load libraries
library(here)
library(readxl)
library(ggplot2)
library(dplyr)
library(writexl)
library(mgcv)
library(MuMIn)
library(gratia)
library(car)
library(DHARMa)

########### upload filtered dataset 
index <- read_excel(
  here("SPTENPIndexYearPreds.xlsx"),
  guess_max = Inf
)




################################################################################
################################################################################
################################################################################
############### CV
ggplot(index, aes(x = years, y = CV)) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Coefficient of Variation (CV)"
  )

# ggsave("SPTENPIndexCV.png", width = 10, height = 8, dpi = 1000)



################################################################################
################################################################################
################################################################################
############### index
colnames(index)

library(ggplot2)

ggplot(index, aes(x = years, y = pred.mean)) +
  geom_ribbon(
    aes(ymin = `2.5%`, ymax = `97.5%`),
    alpha = 0.2
  ) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  theme_bw() +
  labs(
    x = "Year",
    y = "SPT Standardized CPUE Index"
  )
ggsave("SPTENPIndex.png", width = 10, height = 8, dpi = 1000)



################################################################################
################################################################################
################################################################################
### index sclaed to the mean
# Calculate time-series mean of predicted CPUE
index.mean <- mean(index$pred.mean, na.rm = TRUE)

# Scale predictions and 95% CIs to the time-series mean
index$index <- index$pred.mean / index.mean
index$index.LCL <- index$`2.5%` / index.mean
index$index.UCL <- index$`97.5%` / index.mean

summary(index$index)
mean(index$index)

ggplot(index, aes(x = years, y = index)) +
  geom_ribbon(
    aes(ymin = index.LCL, ymax = index.UCL),
    alpha = 0.2
  ) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  theme_bw() +
  labs(
    x = "Year",
    y = "SPT Standardized CPUE Index (Normalized)"
  )
ggsave("SPTENPIndexNormalized.png", width = 10, height = 8, dpi = 1000)













