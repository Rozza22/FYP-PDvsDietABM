# ethnicity distribution - final population

# ethGrp distribution
# Descriptive analysis
rm(list = ls()) # clear environment

library(haven)
library(tidyverse)
library(ggplot2)
library(dplyr)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
setwd(wd)

data <- read.csv("ABM_code - with PD/props/GLondon_basepop_1000.csv")


# statistics are not how they appear in final form due to having recoded them
# Replace negative values in column 'x' with NA
# data$ethGrp <- ifelse(data$ethGrp < 0, NA, data$ethGrp)
# Extract ethGrp columns from both data frames
ethGrpdata <- data$ethgrp5

ethGrpCategories <- c("white","mixed","black","asian","other")

# Replace numeric values in ethGrp column with ethnicity labels
# ethGrpHse$ethGrp <- ethGrpCategories[ethGrpHse$ethGrp]
# ethGrpdata$ethGrp <- ethGrpCategories[ethGrpdata$ethGrp]

ethGrpdata <- as.data.frame(ethGrpdata)
# ethGrpdata <- as.data.frame(ethGrpdata[!is.na(ethGrpdata), ])

column_name<- colnames(ethGrpdata)[1]

names(ethGrpdata)[names(ethGrpdata) == column_name] <- "ethGrp"

freqTabledataethGrp <- table(ethGrpdata)
ethGrpFreqdata <- freqTabledataethGrp[match(ethGrpCategories, names(freqTabledataethGrp))]

ethGrpFreqdata <- ethGrpFreqdata/1000

par(mar = c(5, 5, 2, 1))  # Adjust the values as needed


dataethGrpDistr <- barplot(ethGrpFreqdata,
                        names.arg = ethGrpCategories,
                        xlab = "ethGrp",
                        ylab = "Frequency",
                        main = "Distribution of ethGrp in imputed population",
                        col = "skyblue",
                        border = "black")


ethnicity_mapping <- c("White", "Mixed Race", "Black", "Asian", "Other")

combined_data$ethGrp <- ethnicity_mapping[combined_data$ethGrp]

# Reshape the data from wide to long format
combined_data_long <- tidyr::pivot_longer(combined_data, cols = c("HSE", "data"), names_to = "Dataset")


# Plot the data
ggplot(combined_data_long, aes(x = ethGrp, y = value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Distribution of ethGrp in HSE and data",
       x = "ethGrp",
       y = "Frequency") +
  scale_fill_manual(values = c("HSE" = "skyblue", "data" = "lightgreen")) +
  theme_minimal()

# Calculate key statistics
statsdata <- summary(ethGrpdata)

# Print key statistics
cat("data ethGrp Statistics:\n", statsdata, "\n")

dataethGrpVariance <- var(ethGrpdata)
