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

########### upload year preds 
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
# ggsave("SPTENPIndex.png", width = 10, height = 8, dpi = 1000)



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
# ggsave("SPTENPIndexNormalized.png", width = 10, height = 8, dpi = 1000)




################################################################################
################################################################################
################################################################################
## calculate nominal CPUE
spt <- read_excel(
  here("ENPCreelSpottedSeatroutFilteredForIndex.xlsx"),
  guess_max = Inf
)
# need to do this so it doesnt assume that cols with no values in the first
# however many rows make it think that the whole column is blank throughout

nrow(spt)
names(spt)

nominal_cpue <- spt %>%
  group_by(Year) %>%
  summarise(
    nominal_CPUE = mean(CPUE, na.rm = TRUE),
    n = sum(!is.na(CPUE)),
    .groups = "drop"
  )

nominal_cpue


# Scale nominal CPUE to its time-series mean
nominal_mean <- mean(nominal_cpue$nominal_CPUE, na.rm = TRUE)

nominal_cpue <- nominal_cpue %>%
  mutate(
    nominal_index = nominal_CPUE / nominal_mean
  )



# Make sure year columns are numeric
index$years <- as.numeric(as.character(index$years))
nominal_cpue$Year <- as.numeric(as.character(nominal_cpue$Year))

# Combine standardized GAM index and nominal index
index_combined <- index %>%
  left_join(
    nominal_cpue %>%
      select(Year, nominal_CPUE, nominal_index),
    by = c("years" = "Year")
  )

# Plot nominal vs. standardized index with 95% CI
ggplot(index_combined, aes(x = years)) +
  
  # 95% CI for standardized GAM index
  geom_ribbon(
    aes(ymin = index.LCL, ymax = index.UCL),
    fill = "black",
    alpha = 0.15
  ) +
  
  # Nominal index
  geom_line(
    aes(y = nominal_index, color = "Nominal"),
    linewidth = 2
  ) +
  geom_point(
    aes(y = nominal_index, color = "Nominal"),
    size = 3
  ) +
  
  # Standardized GAM index
  geom_line(
    aes(y = index, color = "Standardized"),
    linewidth = 2
  ) +
  geom_point(
    aes(y = index, color = "Standardized"),
    size = 3
  ) +
  
  # Colors
  scale_color_manual(
    values = c(
      "Nominal" = "red",
      "Standardized" = "black"
    )
  ) +
  
  # Time-series mean
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.85)
  ) +
  
  labs(
    x = "Year",
    y = "SPT CPUE Index (Normalized)",
    color = NULL
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.85),
    legend.background = element_rect(
      colour = "black",
      fill = "white",
      linewidth = 0.5
    )
  )

# ggsave("SPTENPIndexNormalizedWithNominal.png", width = 10, height = 8, dpi = 1000)

















