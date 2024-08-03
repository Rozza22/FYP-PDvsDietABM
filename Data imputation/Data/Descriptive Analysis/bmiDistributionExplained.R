# bmiDistribution Explanation
rm(list = ls()) # clear environment

library(foreign)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(hablar)
library(haven)
library(tidyverse)
library(ggplot2)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
# data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

####read in the joined up data files 
NDNS <- readRDS("Data imputation/Data/NDNS/NDNS_full.RDS")
HSE <- readRDS("Data imputation/Data/HSE/HSE_Full.RDS")

colOrder <- c("surveyYear", "age", "sex", "bmival", "bmiok")
HSE <- HSE[, colOrder]

NDNS <- cbind(subset(NDNS, select = c(SurveyYear, age, Sex, bmival, bmiok)))

# NDNSbmiRel <- cor(NDNS$bmiok, NDNS$bmival)
# HSEbmiRel <- cor(HSE$bmiok, HSE$bmival)

# ------------------------------
# Extract bmi columns from both data frames
HSE$bmiok <- ifelse(HSE$bmiok < 0, NA, HSE$bmiok)
NDNS$bmiok <- ifelse(NDNS$bmiok < 0, NA, NDNS$bmiok)

bmiokHse <- HSE$bmiok
bmiokNdns <- NDNS$bmiok

bmiokCategories <- c(1,2,3,4,5)

bmiokHse <- as.data.frame(bmiokHse)
bmiokHse <- na.omit(bmiokHse$bmiokHse)

bmiokNdns <- as.data.frame(bmiokNdns)
bmiokNdns <- na.omit(bmiokNdns$bmiokNdns)

column_name<- colnames(bmiokHse)[1]

names(bmiokHse)[names(bmiokHse) == column_name] <- "bmiok"
names(bmiokNdns)[names(bmiokNdns) == column_name] <- "bmiok"

freqTableHSEbmiok <- table(bmiokHse)
bmiokFreqHSE <- freqTableHSEbmiok[match(bmiokCategories, names(freqTableHSEbmiok))]

freqTableNDNSbmiok <- table(bmiokNdns)
bmiokFreqNDNS <- freqTableNDNSbmiok[match(bmiokCategories, names(freqTableNDNSbmiok))]

# Plot the distributions side by side

# Combine the frequencies into a single data frame
totalHSE <- sum(as.vector(bmiokFreqHSE))
totalNDNS <- sum(as.vector(bmiokFreqNDNS))

bmiokFreqNDNS <- bmiokFreqNDNS/totalNDNS
bmiokFreqHSE <- bmiokFreqHSE/totalHSE

# Combine frequency data into a single data frame
combined_data <- data.frame(bmiok = bmiokCategories, HSE = bmiokFreqHSE, NDNS = bmiokFreqNDNS)
combined_data <- combined_data[, -c(2,4)]
names(combined_data)[names(combined_data) == "NDNS.Freq"] <- "NDNS"
names(combined_data)[names(combined_data) == "HSE.Freq"] <- "HSE"

bmiok_mapping <- c("Valid", "height/weight not usable", "height/weight refused", "height/weight attempted", "height/weight not attempted")

combined_data$bmiok <- bmiok_mapping[combined_data$bmiok]

# combined_data$bmiok <- factor(combined_data$bmiok, levels = bmiok_mapping)

# Reshape the data from wide to long format
combined_data_long <- tidyr::pivot_longer(combined_data, cols = c("HSE", "NDNS"), names_to = "Dataset")

# Plot the data
ggplot(combined_data_long, aes(x = bmiok, y = value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Distribution of bmiok in HSE and NDNS",
       x = "bmiok",
       y = "Frequency") +
  scale_fill_manual(values = c("HSE" = "skyblue", "NDNS" = "lightgreen")) +
  theme_minimal()

# NDNSbmiRel <- cor(bmiokNdns, NDNS$bmival)
# HSEbmiRel <- cor(bmiokHse, HSE$bmival)
