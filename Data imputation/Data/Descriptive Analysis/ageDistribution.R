# Descriptive analysis
rm(list = ls()) # clear environment

library(haven)
library(tidyverse)
library(ggplot2)
library(dplyr)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
setwd(wd)

source("Data imputation/ProcessingData/1_NDNS_process_Functions - Copy.R")

NDNS <- read.csv("Data imputation/Data/Descriptive Analysis/NDNS.csv")
HSE <- read.csv("Data imputation/Data/Descriptive Analysis/HSE.csv")
combined <- read.csv("Data imputation/Data/Descriptive Analysis/preImputationCombined.csv")

NDNS <- readRDS("Data imputation/Data/NDNS/NDNS_full.RDS")
HSE <- readRDS("Data imputation/Data/HSE/HSE_Full.RDS")

colOrder <- c("surveyYear", "age", "sex", "bmival", "bmiok")
HSE <- HSE[, colOrder]

NDNS <- cbind(subset(NDNS, select = c(SurveyYear, age, Sex, bmival, bmiok)))
NDNS <- subset(NDNS, age >= 16)
NDNS <- AgeBands(NDNS)

# statistics are not how they appear in final form due to having recoded them

# Extract age columns from both data frames
ageHse <- HSE$age
ageNdns <- NDNS$age

ageCategories <- c(1,2,3,4,5,6,7)

ageHse <- as.data.frame(ageHse)
ageHse <- as.data.frame(ageHse[!is.na(ageHse), ])

ageNdns <- as.data.frame(ageNdns)
ageNdns <- as.data.frame(ageNdns[!is.na(ageNdns), ])

column_name<- colnames(ageHse)[1]

names(ageHse)[names(ageHse) == column_name] <- "age"
names(ageNdns)[names(ageNdns) == column_name] <- "age"

freqTableHSEage <- table(ageHse)
ageFreqHSE <- freqTableHSEage[match(ageCategories, names(freqTableHSEage))]

freqTableNDNSage <- table(ageNdns)
ageFreqNDNS <- freqTableNDNSage[match(ageCategories, names(freqTableNDNSage))]

# Plot the distributions side by side

# Combine the frequencies into a single data frame
totalHSE <- sum(as.vector(ageFreqHSE))
totalNDNS <- sum(as.vector(ageFreqNDNS))

ageFreqNDNS <- ageFreqNDNS/totalNDNS
ageFreqHSE <- ageFreqHSE/totalHSE

# Combine frequency data into a single data frame
combined_data <- data.frame(Age = ageCategories, HSE = ageFreqHSE, NDNS = ageFreqNDNS)
combined_data <- combined_data[, -c(2,4)]
names(combined_data)[names(combined_data) == "NDNS.Freq"] <- "NDNS"
names(combined_data)[names(combined_data) == "HSE.Freq"] <- "HSE"

# Reshape the data from wide to long format
combined_data_long <- tidyr::pivot_longer(combined_data, cols = c("HSE", "NDNS"), names_to = "Dataset")

# Plot the data
ggplot(combined_data_long, aes(x = Age, y = value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Distribution of ages in HSE and NDNS",
       x = "Age",
       y = "Frequency") +
  scale_fill_manual(values = c("HSE" = "skyblue", "NDNS" = "lightgreen")) +
  theme_minimal()

# Calculate key statistics
statsHse <- summary(ageHse)
statsNdns <- summary(ageNdns)

# Print key statistics
cat("HSE Age Statistics:\n", statsHse, "\n")
cat("NDNS Age Statistics:\n", statsNdns, "\n")

HSEageVariance <- var(ageHse)
NDNSageVariance <- var(ageNdns)
