# Plot PD and BMI data from optimal model

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
# modelData <- read.csv("ABM_code - with PD/outputs/annual_data_props_model_yaml_15_123.csv") # Optimal model 11 years
modelData <- read.csv("ABM_code - with PD/outputs/interventionVariation/annual_data_props_interventionVariation_model_2_yaml_1_123.csv")

modelData <- cbind(subset(modelData, select = c(tick, meanPDmale,meanPDfemale,PDyesMale,PDyesFemale,PDnoMale,PDnoFemale)))

modelData$tick <- modelData$tick/365 
names(modelData)[names(modelData) == "tick"] <- "year"

data <- cbind(subset(data, select = c(SurveyYear, age, Sex, ghq12scr))) # Have taken "WrkStat" out
data[data < 0] <- NA #sets all negative values to NA
data = data %>% na.omit() #omits all rows with NA in them
names(data)[names(data) == "SurveyYear"] <- "year"
data <- subset(data, age >= 16)
data <- subset(data, select = -age)

# Assuming 'df' is your data frame and 'column' is the column with values from 0-12
data$ghq12scr <- ifelse(data$ghq12scr < 4, 0, 1)

PDdata <- pivot_wider(data = data, names_from = ghq12scr, values_from = ghq12scr, values_fn = length, values_fill = 0)
PDdata <- PDdata %>%
  select(year, Sex, `0`, `1`)
PDdata$year <- PDdata$year - 1

names(PDdata)[names(PDdata) == "0"] <- "PDnoTarget"
names(PDdata)[names(PDdata) == "1"] <- "PDyesTarget"

# Target data men
maleTargetPD <- PDdata[PDdata$Sex == 1, ]
maleTargetPD <- subset(maleTargetPD, select = -Sex)
maleTargetPD <- maleTargetPD %>%
  group_by(year) %>%
  summarize(
    PDno_count = sum(PDnoTarget),
    PDyes_count = sum(PDyesTarget)
  )
# Calculate total count of people for each year
maleTargetPD <- maleTargetPD %>%
  mutate(total_count = PDno_count + PDyes_count)

# Convert counts to proportions
maleTargetPD <- maleTargetPD %>%
  mutate(
    PDno_propTarget = PDno_count / total_count,
    PDyes_propTarget = PDyes_count / total_count
  ) %>%
  select(-total_count, -PDno_count, -PDyes_count) # Remove the total count column if you no longer need it


# Target data women
femaleTargetPD <- PDdata[PDdata$Sex == 2, ]
femaleTargetPD <- subset(femaleTargetPD, select = -Sex)
femaleTargetPD <- femaleTargetPD %>%
  group_by(year) %>%
  summarize(
    PDno_count = sum(PDnoTarget),
    PDyes_count = sum(PDyesTarget)
  )
# Calculate total count of people for each year
femaleTargetPD <- femaleTargetPD %>%
  mutate(total_count = PDno_count + PDyes_count)

# Convert counts to proportions
femaleTargetPD <- femaleTargetPD %>%
  mutate(
    PDno_propTarget = PDno_count / total_count,
    PDyes_propTarget = PDyes_count / total_count
  ) %>%
  select(-total_count, -PDno_count, -PDyes_count) # Remove the total count column if you no longer need it

# Model data men
maleModelPD <- modelData[, c("year", "PDyesMale", "PDnoMale")]
names(maleModelPD)[names(maleModelPD) == "PDnoMale"] <- "PDnoModel"
names(maleModelPD)[names(maleModelPD) == "PDyesMale"] <- "PDyesModel"

# Model data men
maleModelPD <- maleModelPD %>%
  group_by(year) %>%
  summarize(
    PDno_count = sum(PDnoModel),
    PDyes_count = sum(PDyesModel)
  )
# Calculate total count of people for each year
maleModelPD <- maleModelPD %>%
  mutate(total_count = PDno_count + PDyes_count)

# Convert counts to proportions
maleModelPD <- maleModelPD %>%
  mutate(
    PDno_propModel = PDno_count / total_count,
    PDyes_propModel = PDyes_count / total_count
  ) %>%
  select(-total_count, -PDno_count, -PDyes_count) # Remove the total count column if you no longer need it

# Model data women
femaleModelPD <- modelData[, c("year", "PDyesFemale", "PDnoFemale")]
names(femaleModelPD)[names(femaleModelPD) == "PDnoFemale"] <- "PDnoModel"
names(femaleModelPD)[names(femaleModelPD) == "PDyesFemale"] <- "PDyesModel"

# Model data women
femaleModelPD <- femaleModelPD %>%
  group_by(year) %>%
  summarize(
    PDno_count = sum(PDnoModel),
    PDyes_count = sum(PDyesModel)
  )
# Calculate total count of people for each year
femaleModelPD <- femaleModelPD %>%
  mutate(total_count = PDno_count + PDyes_count)

# Convert counts to proportions
femaleModelPD <- femaleModelPD %>%
  mutate(
    PDno_propModel = PDno_count / total_count,
    PDyes_propModel = PDyes_count / total_count
  ) %>%
  select(-total_count, -PDno_count, -PDyes_count) # Remove the total count column if you no longer need it

# combine sexes and save  
selectedModelFemale <- femaleModelPD[, c("PDno_propModel", "PDyes_propModel")]
combinedModelPDdata <- cbind(maleModelPD, selectedModelFemale)

names(combinedModelPDdata)[2] <- "PDno_optimalMale"
names(combinedModelPDdata)[3] <- "PDyes_optimalMale"
names(combinedModelPDdata)[4] <- "PDno_optimalFemale"
names(combinedModelPDdata)[5] <- "PDyes_optimalFemale"

# write.csv(combinedModelPDdata, "ABM_code - with PD/calibration/optimalModelPD.csv", row.names = FALSE)

optimalModelPDdata <- read.csv("ABM_code - with PD/calibration/optimalModelPD.csv")
optimalModelPDdata <- optimalModelPDdata %>%
  select(-year)

# Combine datasets
# Join all male data together
selectedModelMale <- maleModelPD[, c("PDno_propModel", "PDyes_propModel")]
optimalModelMale <- optimalModelPDdata[, c("PDno_optimalMale", "PDyes_optimalMale")]
combinedMaleData <- cbind(maleTargetPD, selectedModelMale, optimalModelMale)

# Join all female data together
selectedModelFemale <- femaleModelPD[, c("PDno_propModel", "PDyes_propModel")]
optimalModelFemale <- optimalModelPDdata[, c("PDno_optimalFemale", "PDyes_optimalFemale")]
combinedFemaleData <- cbind(femaleTargetPD, selectedModelFemale, optimalModelFemale)

# plot data -------------------------
# plot female data
femaleData <- ggplot(data = combinedFemaleData, aes(x = year)) +
  # geom_line(aes(y = PDyes_propTarget, color = "PD yes - target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = PDyes_propModel, color = "PD yes - intervention")) +   # Second line (BMI2)
  # geom_line(aes(y = PDno_propTarget, color = "PD no - target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = PDno_propModel, color = "PD no - intervention")) +   # Second line (BMI2)
  geom_line(aes(y = PDno_optimalFemale, color = "PD no - optimal")) +  # First line (BMI1)
  geom_line(aes(y = PDyes_optimalFemale, color = "PD yes - optimal")) +   # Second line (BMI2)
  labs(x = "Year", y = "Proportion of population") +  # Add labels for x and y axes
  ggtitle("Female populations PD") +  # Add a title to the plot
  scale_x_continuous(breaks = 1:11, labels = 2008:2018) +
  geom_vline(xintercept = 8, linetype = "dotted", color = "blue") +
  scale_color_manual(values = c("PD yes - target" = "red", "PD yes - intervention" = "red",
                                "PD no - target" = "green", "PD no - intervention" = "green",
                                "PD no - optimal" = "green4", "PD yes - optimal" = "red4")) +
  theme(axis.text.x = element_text(size = 12), legend.position = "right")  # Adjust the font size of x-axis labels and legend position


# male data
maleData <- ggplot(data = combinedMaleData, aes(x = year)) +
  # geom_line(aes(y = PDyes_propTarget, color = "PD yes - target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = PDyes_propModel, color = "PD yes - intervention")) +   # Second line (BMI2)
  # geom_line(aes(y = PDno_propTarget, color = "PD no - target"), size = 1.5) +  # First line (BMI1)
  geom_line(aes(y = PDno_propModel, color = "PD no - intervention")) +   # Second line (BMI2)
  geom_line(aes(y = PDno_optimalMale, color = "PD no - optimal")) +  # First line (BMI1)
  geom_line(aes(y = PDyes_optimalMale, color = "PD yes - optimal")) +   # Second line (BMI2)
  labs(x = "Year", y = "Proportion of population") +  # Add labels for x and y axes
  ggtitle("Male populations PD") +  # Add a title to the plot
  scale_x_continuous(breaks = 1:11, labels = 2008:2018) +
  geom_vline(xintercept = 8, linetype = "dotted", color = "blue") +
  scale_color_manual(values = c("PD yes - target" = "red", "PD yes - intervention" = "red",
                                "PD no - target" = "green", "PD no - intervention" = "green",
                                "PD no - optimal" = "green4", "PD yes - optimal" = "red4")) +
  theme(axis.text.x = element_text(size = 12), legend.position = "right")  # Adjust the font size of x-axis labels and legend position

combinedData <- maleData / femaleData

combinedData

