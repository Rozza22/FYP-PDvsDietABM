# bmi distribution
# Descriptive analysis
rm(list = ls()) # clear environment

library(haven)
library(tidyverse)
library(ggplot2)
library(dplyr)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
setwd(wd)

source("Data imputation/ProcessingData/1_HSE_process_functions.R")

combined <- read.csv("Data imputation/Data/Descriptive Analysis/preImputationCombined.csv")

NDNS <- readRDS("Data imputation/Data/NDNS/NDNS_full.RDS")
HSE <- readRDS("Data imputation/Data/HSE/HSE_Full.RDS")

colOrder <- c("surveyYear", "age", "sex", "bmival", "bmiok")
HSE <- HSE[, colOrder]

NDNS <- cbind(subset(NDNS, select = c(SurveyYear, age, Sex, bmival, bmiok)))
NDNS <- subset(NDNS, age >= 16)

HSE$bmival <- ifelse(HSE$bmival < 0, NA, HSE$bmival)
NDNS$bmival <- ifelse(NDNS$bmival < 0, NA, NDNS$bmival)

HSEbmi <- HSE$bmival
NDNSbmi <- NDNS$bmival

# Create data frames for density plot
density_data_NDNS <- data.frame(bmi = NDNSbmi)
density_data_HSE <- data.frame(bmi = HSEbmi)

# Plot the density distributions
ggplot() +
  geom_density(data = density_data_NDNS, aes(x = bmi, color = "NDNS"), size = 1.5) +
  geom_density(data = density_data_HSE, aes(x = bmi, color = "HSE"), size = 1.5) +
  scale_color_manual(values = c("NDNS" = "skyblue", "HSE" = "lightgreen")) +
  labs(title = "Distribution of BMI in NDNS and HSE",
       x = "BMI",
       y = "Density") +
  theme_minimal()

# ------------when in categories-------------------------------
# # HSE <- BMIquintiles(HSE)
# # NDNS <- BMIquintiles(NDNS)
# 
# # remove rows with age below 16
# NDNS <- subset(NDNS, age >= 16)
# 
# # statistics are not how they appear in final form due to having recoded them
# 
# # Extract bmi columns from both data frames
# HSE$bmival <- ifelse(HSE$bmival < 0, NA, HSE$bmival)
# NDNS$bmival <- ifelse(NDNS$bmival < 0, NA, NDNS$bmival)
# 
# bmiHse <- HSE$bmival
# bmiNdns <- NDNS$bmival
# 
# bmiCategories <- c(1,2,3,4,5)
# 
# bmiHse <- as.data.frame(bmiHse)
# bmiHse <- na.omit(bmiHse)
# 
# bmiNdns <- as.data.frame(bmiNdns)
# bmiNdns <- na.omit(bmiNdns)
# 
# column_nameHse <- colnames(bmiHse)[1]
# column_nameNdns <- colnames(bmiNdns)[1]
# 
# names(bmiHse)[names(bmiHse) == column_nameHse] <- "bmi"
# names(bmiNdns)[names(bmiNdns) == column_nameNdns] <- "bmi"
# 
# freqTableHSEbmi <- table(bmiHse)
# bmiFreqHSE <- freqTableHSEbmi[match(bmiCategories, names(freqTableHSEbmi))]
# 
# freqTableNDNSbmi <- table(bmiNdns)
# bmiFreqNDNS <- freqTableNDNSbmi[match(bmiCategories, names(freqTableNDNSbmi))]
# 
# # Plot the distributions side by side
# 
# # Combine the frequencies into a single data frame
# totalHSE <- sum(as.vector(bmiFreqHSE))
# totalNDNS <- sum(as.vector(bmiFreqNDNS))
# 
# bmiFreqNDNS <- bmiFreqNDNS/totalNDNS
# bmiFreqHSE <- bmiFreqHSE/totalHSE
# 
# # Combine frequency data into a single data frame
# combined_data <- data.frame(bmi = bmiCategories, HSE = bmiFreqHSE, NDNS = bmiFreqNDNS)
# combined_data <- combined_data[, -c(2,4)]
# names(combined_data)[names(combined_data) == "NDNS.Freq"] <- "NDNS"
# names(combined_data)[names(combined_data) == "HSE.Freq"] <- "HSE"
# 
# bmi_mapping <- c("Underweight", "Normal", "Overweight", "Obese", "Extremely Obese")
# 
# combined_data$bmi <- bmi_mapping[combined_data$bmi]
# 
# # combined_data$bmi <- factor(combined_data$bmi, levels = bmi_mapping)
# 
# # Reshape the data from wide to long format
# combined_data_long <- tidyr::pivot_longer(combined_data, cols = c("HSE", "NDNS"), names_to = "Dataset")
# 
# # Plot the data
# ggplot(combined_data_long, aes(x = bmi, y = value, fill = Dataset)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
#   labs(title = "Distribution of bmi in HSE and NDNS",
#        x = "bmi",
#        y = "Frequency") +
#   scale_fill_manual(values = c("HSE" = "skyblue", "NDNS" = "lightgreen")) +
#   theme_minimal()
# 
# # Calculate key statistics
statsHse <- summary(HSEbmi)
statsNdns <- summary(NDNSbmi)

statsHse
statsNdns

# Print key statistics
cat("HSE bmi Statistics:\n", statsHse, "\n")
cat("NDNS bmi Statistics:\n", statsNdns, "\n")
# 

density_data_HSE <- na.omit(density_data_HSE)
density_data_NDNS <- na.omit(density_data_NDNS)

HSEbmiVariance <- var(density_data_HSE$bmi)
NDNSbmiVariance <- var(density_data_NDNS$bmi)
