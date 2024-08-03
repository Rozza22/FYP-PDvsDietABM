# Plot PD and BMI data from optimal model compared to intervention

rm(list = ls()) # clear environment

# setting up files for calibration for PHASE project 
library(dplyr)
library(lhs)
library(truncnorm)
library(yaml)
library(tidyverse)
library(ggplot2)
library(patchwork)

setwd("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")

source("Data imputation/ProcessingData/1_NDNS_process_Functions - Copy.R")

data <- read.csv("Data imputation/Data/NDNS/NDNS_combined.csv")
modelData <- read.csv("ABM_code - with PD/outputs/annual_data_props_model_yaml_15_123.csv") # Optimal model 11 years
# modelData <- read.csv("ABM_code - with PD/outputs/interventionVariation/annual_data_props_interventionVariation_model_10_yaml_1_123.csv")

data <- cbind(subset(data, select = c(SurveyYear, age, Sex, bmival))) # Have taken "WrkStat" out
data[data < 0] <- NA #sets all negative values to NA
data = data %>% na.omit() #omits all rows with NA in them
names(data)[names(data) == "SurveyYear"] <- "year"
data <- subset(data, age >= 16)

# comparing BMI categories
BMIquintiles <- BMIquintiles(data)
BMIquintiles$bmival <- ifelse(BMIquintiles$bmival == 5, 4, BMIquintiles$bmival)

BMIquintiles <- pivot_wider(data = BMIquintiles, names_from = bmival, values_from = bmival, values_fn = length, values_fill = 0)
BMIquintiles <- BMIquintiles %>%
  select(year, Sex, `1`, `2`, `3`, `4`)
BMIquintiles$year <- BMIquintiles$year - 1

names(BMIquintiles)[names(BMIquintiles) == "1"] <- "underweight"
names(BMIquintiles)[names(BMIquintiles) == "2"] <- "normalWeight"
names(BMIquintiles)[names(BMIquintiles) == "3"] <- "overweight"
names(BMIquintiles)[names(BMIquintiles) == "4"] <- "obese"

maleTargetBMIquint <- BMIquintiles[BMIquintiles$Sex == 1, ]
maleTargetBMIquint <- subset(maleTargetBMIquint, select = -Sex)
maleTargetBMIquint <- maleTargetBMIquint %>%
  group_by(year) %>%
  summarize(
    underweight_count = sum(underweight),
    normalWeight_count = sum(normalWeight),
    overweight_count = sum(overweight),
    obese_count = sum(obese)
  )
# Calculate total count of people for each year
maleTargetBMIquint <- maleTargetBMIquint %>%
  mutate(total_count = underweight_count + normalWeight_count + overweight_count + obese_count)

# Convert counts to proportions
maleTargetBMIquint <- maleTargetBMIquint %>%
  mutate(
    underweight_propTarget = underweight_count / total_count,
    normalWeight_propTarget = normalWeight_count / total_count,
    overweight_propTarget = overweight_count / total_count,
    obese_propTarget = obese_count / total_count
  ) %>%
  select(-total_count, -underweight_count, -normalWeight_count, -overweight_count, -obese_count) # Remove the total count column if you no longer need it


# maleTargetBMI <- head(maleTargetBMI, 6)
femaleTargetBMIquint <- BMIquintiles[BMIquintiles$Sex == 2, ]
femaleTargetBMIquint <- subset(femaleTargetBMIquint, select = -Sex)
femaleTargetBMIquint <- femaleTargetBMIquint %>%
  group_by(year) %>%
  summarize(
    underweight_count = sum(underweight),
    normalWeight_count = sum(normalWeight),
    overweight_count = sum(overweight),
    obese_count = sum(obese)
  )
# Calculate total count of people for each year
femaleTargetBMIquint <- femaleTargetBMIquint %>%
  mutate(total_count = underweight_count + normalWeight_count + overweight_count + obese_count)

# Convert counts to proportions
femaleTargetBMIquint <- femaleTargetBMIquint %>%
  mutate(
    underweight_propTarget = underweight_count / total_count,
    normalWeight_propTarget = normalWeight_count / total_count,
    overweight_propTarget = overweight_count / total_count,
    obese_propTarget = obese_count / total_count
  ) %>%
  select(-total_count, -underweight_count, -normalWeight_count, -overweight_count, -obese_count)  # Remove the total count column if you no longer need it

# Work out categories from model
maleBMIquintsModel <- cbind(modelData$tick, modelData$bmiGrp1Male, modelData$bmiGrp2Male, modelData$bmiGrp3Male, modelData$bmiGrp4Male)
maleBMIquintsModel <- as.data.frame(maleBMIquintsModel)
maleBMIquintsModel$V1 <- maleBMIquintsModel$V1 / 365
names(maleBMIquintsModel)[names(maleBMIquintsModel) == "V1"] <- "year"
names(maleBMIquintsModel)[names(maleBMIquintsModel) == "V2"] <- "underweight"
names(maleBMIquintsModel)[names(maleBMIquintsModel) == "V3"] <- "normalWeight"
names(maleBMIquintsModel)[names(maleBMIquintsModel) == "V4"] <- "overweight"
names(maleBMIquintsModel)[names(maleBMIquintsModel) == "V5"] <- "obese"

# Calculate total count of people for each year
maleBMIquintsModel <- maleBMIquintsModel %>%
  mutate(total_count = underweight + normalWeight + overweight + obese)

# Convert counts to proportions
maleBMIquintsModel <- maleBMIquintsModel %>%
  mutate(
    underweight_propModel = underweight / total_count,
    normalWeight_propModel = normalWeight / total_count,
    overweight_propModel = overweight / total_count,
    obese_propModel = obese / total_count
  ) %>%
  select(-total_count, -underweight, -normalWeight, -overweight, -obese)  # Remove the total count column if you no longer need it

# now for female output
femaleBMIquintsModel <- cbind(modelData$tick, modelData$bmiGrp1Female, modelData$bmiGrp2Female, modelData$bmiGrp3Female, modelData$bmiGrp4Female)
femaleBMIquintsModel <- as.data.frame(femaleBMIquintsModel)
femaleBMIquintsModel$V1 <- femaleBMIquintsModel$V1 / 365
names(femaleBMIquintsModel)[names(femaleBMIquintsModel) == "V1"] <- "year"
names(femaleBMIquintsModel)[names(femaleBMIquintsModel) == "V2"] <- "underweight"
names(femaleBMIquintsModel)[names(femaleBMIquintsModel) == "V3"] <- "normalWeight"
names(femaleBMIquintsModel)[names(femaleBMIquintsModel) == "V4"] <- "overweight"
names(femaleBMIquintsModel)[names(femaleBMIquintsModel) == "V5"] <- "obese"

# Calculate total count of people for each year
femaleBMIquintsModel <- femaleBMIquintsModel %>%
  mutate(total_count = underweight + normalWeight + overweight + obese)

# Convert counts to proportions
femaleBMIquintsModel <- femaleBMIquintsModel %>%
  mutate(
    underweight_propModel = underweight / total_count,
    normalWeight_propModel = normalWeight / total_count,
    overweight_propModel = overweight / total_count,
    obese_propModel = obese / total_count
  ) %>%
  select(-total_count, -underweight, -normalWeight, -overweight, -obese)  # Remove the total count column if you no longer need it

# Join all male data together
selectedModelMale <- maleBMIquintsModel[, c("underweight_propModel", "normalWeight_propModel", "overweight_propModel", "obese_propModel")]
combinedMaleData <- cbind(maleTargetBMIquint, selectedModelMale)

# Join all female data together
selectedModelFemale <- femaleBMIquintsModel[, c("underweight_propModel", "normalWeight_propModel", "overweight_propModel", "obese_propModel")]
combinedFemaleData <- cbind(femaleTargetBMIquint, selectedModelFemale)


# # plot data -----------------------
# Plot male data with legend
malePlot <- ggplot(data = combinedMaleData, aes(x = year)) +
  geom_line(aes(y = underweight_propTarget, color = "Underweight Target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = underweight_propModel, color = "Underweight Model")) +   # Second line (BMI2)
  geom_line(aes(y = normalWeight_propTarget, color = "Normal Weight Target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = normalWeight_propModel, color = "Normal Weight Model")) +   # Second line (BMI2)
  geom_line(aes(y = overweight_propTarget, color = "Overweight Target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = overweight_propModel, color = "Overweight Model")) +   # Second line (BMI2)
  geom_line(aes(y = obese_propTarget, color = "Obese Target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = obese_propModel, color = "Obese Model")) +   # Second line (BMI2)
  geom_vline(xintercept = 8, linetype = "dotted", color = "blue") +
  labs(x = "Year", y = "Proportion of population") +  # Add labels for x and y axes
  ggtitle("Male populations BMI") +  # Add a title to the plot
  scale_x_continuous(breaks = 1:11, labels = 2008:2018) +
  scale_color_manual(values = c("Underweight Target" = "red", "Underweight Model" = "red",
                                "Normal Weight Target" = "green", "Normal Weight Model" = "green",
                                "Overweight Target" = "blue", "Overweight Model" = "blue",
                                "Obese Target" = "black", "Obese Model" = "black")) +
  theme(axis.text.x = element_text(size = 12), legend.position = "right")  # Adjust the font size of x-axis labels and legend position

# Plot female data with legend
femalePlot <- ggplot(data = combinedFemaleData, aes(x = year)) +
  geom_line(aes(y = underweight_propTarget, color = "Underweight Target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = underweight_propModel, color = "Underweight Model")) +   # Second line (BMI2)
  geom_line(aes(y = normalWeight_propTarget, color = "Normal Weight Target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = normalWeight_propModel, color = "Normal Weight Model")) +   # Second line (BMI2)
  geom_line(aes(y = overweight_propTarget, color = "Overweight Target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = overweight_propModel, color = "Overweight Model")) +   # Second line (BMI2)
  geom_line(aes(y = obese_propTarget, color = "Obese Target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = obese_propModel, color = "Obese Model")) +   # Second line (BMI2)
  geom_vline(xintercept = 8, linetype = "dotted", color = "blue") +
  labs(x = "Year", y = "Proportion of population") +  # Add labels for x and y axes
  ggtitle("Female populations BMI") +  # Add a title to the plot
  scale_x_continuous(breaks = 1:11, labels = 2008:2018) +
  scale_color_manual(values = c("Underweight Target" = "red", "Underweight Model" = "red",
                                "Normal Weight Target" = "green", "Normal Weight Model" = "green",
                                "Overweight Target" = "blue", "Overweight Model" = "blue",
                                "Obese Target" = "black", "Obese Model" = "black")) +
  theme(axis.text.x = element_text(size = 12), legend.position = "right")  # Adjust the font size of x-axis labels and legend position

combined_plots <- malePlot / femalePlot

combined_plots