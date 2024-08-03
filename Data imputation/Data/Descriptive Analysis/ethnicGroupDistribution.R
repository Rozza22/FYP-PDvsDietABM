# ethGrp distribution
# Descriptive analysis
rm(list = ls()) # clear environment

library(haven)
library(tidyverse)
library(ggplot2)
library(dplyr)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
setwd(wd)

NDNS <- read.csv("Data imputation/Data/Descriptive Analysis/NDNS.csv")
HSE <- read.csv("Data imputation/Data/Descriptive Analysis/HSE.csv")
combined <- read.csv("Data imputation/Data/Descriptive Analysis/preImputationCombined.csv")


# statistics are not how they appear in final form due to having recoded them
# Replace negative values in column 'x' with NA
HSE$ethGrp <- ifelse(HSE$ethGrp < 0, NA, HSE$ethGrp)
NDNS$ethGrp <- ifelse(NDNS$ethGrp < 0, NA, NDNS$ethGrp)
# Extract ethGrp columns from both data frames
ethGrpHse <- HSE$ethGrp
ethGrpNdns <- NDNS$ethGrp

# ethnicity_labels <- c("White", "Mixed Race", "Black", "Asian", "Other")
ethGrpCategories <- c("White","Mixed Race","Black","Asian","Other")

# Replace numeric values in ethGrp column with ethnicity labels
# ethGrpHse$ethGrp <- ethGrpCategories[ethGrpHse$ethGrp]
# ethGrpNdns$ethGrp <- ethGrpCategories[ethGrpNdns$ethGrp]

ethGrpCategories <- c(1,2,3,4,5)
# 

ethGrpHse <- as.data.frame(ethGrpHse)
# ethGrpHse <- as.data.frame(ethGrpHse[!is.na(ethGrpHse), ])

ethGrpNdns <- as.data.frame(ethGrpNdns)
# ethGrpNdns <- as.data.frame(ethGrpNdns[!is.na(ethGrpNdns), ])

column_name<- colnames(ethGrpHse)[1]

names(ethGrpHse)[names(ethGrpHse) == column_name] <- "ethGrp"
names(ethGrpNdns)[names(ethGrpNdns) == column_name] <- "ethGrp"

freqTableHSEethGrp <- table(ethGrpHse)
ethGrpFreqHSE <- freqTableHSEethGrp[match(ethGrpCategories, names(freqTableHSEethGrp))]

freqTableNDNSethGrp <- table(ethGrpNdns)
ethGrpFreqNDNS <- freqTableNDNSethGrp[match(ethGrpCategories, names(freqTableNDNSethGrp))]

totalHSE <- sum(as.vector(ethGrpFreqHSE))
totalNDNS <- sum(as.vector(ethGrpFreqNDNS))

ethGrpFreqNDNS <- ethGrpFreqNDNS/totalNDNS
ethGrpFreqHSE <- ethGrpFreqHSE/totalHSE

par(mar = c(5, 5, 2, 1))  # Adjust the values as needed

# HSEethGrpDistr <- barplot(ethGrpFreqHSE, 
#                        names.arg = ethGrpCategories, 
#                        xlab = "ethGrp", 
#                        ylab = "Frequency",  
#                        main = "Distribution of ethGrp in HSE", 
#                        col = "skyblue", 
#                        border = "black")
# 
# NDNSethGrpDistr <- barplot(ethGrpFreqNDNS, 
#                         names.arg = ethGrpCategories, 
#                         xlab = "ethGrp", 
#                         ylab = "Frequency",  
#                         main = "Distribution of ethGrp in NDNS", 
#                         col = "skyblue", 
#                         border = "black")

# Combine frequency data into a single data frame
combined_data <- data.frame(ethGrp = ethGrpCategories, HSE = ethGrpFreqHSE, NDNS = ethGrpFreqNDNS)
combined_data <- combined_data[, -c(2,4)]
names(combined_data)[names(combined_data) == "NDNS.Freq"] <- "NDNS"
names(combined_data)[names(combined_data) == "HSE.Freq"] <- "HSE"

ethnicity_mapping <- c("White", "Mixed Race", "Black", "Asian", "Other")

combined_data$ethGrp <- ethnicity_mapping[combined_data$ethGrp]

# Reshape the data from wide to long format
combined_data_long <- tidyr::pivot_longer(combined_data, cols = c("HSE", "NDNS"), names_to = "Dataset")


# Plot the data
ggplot(combined_data_long, aes(x = ethGrp, y = value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Distribution of ethGrp in HSE and NDNS",
       x = "ethGrp",
       y = "Frequency") +
  scale_fill_manual(values = c("HSE" = "skyblue", "NDNS" = "lightgreen")) +
  theme_minimal()

# Calculate key statistics
statsHse <- summary(ethGrpHse)
statsNdns <- summary(ethGrpNdns)

# Print key statistics
cat("HSE ethGrp Statistics:\n", statsHse, "\n")
cat("NDNS ethGrp Statistics:\n", statsNdns, "\n")

HSEethGrpVariance <- var(ethGrpHse)
NDNSethGrpVariance <- var(ethGrpNdns)
