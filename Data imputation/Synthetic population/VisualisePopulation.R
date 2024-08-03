# visualise spread of PD to verify imputation worked
# checking that the history generator works 
library(haven)
library(tidyverse)
library(ggplot2)

# wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
# data <- ("Data/NDNS/Individual Data/")

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
# data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

source

preImputationData <- read.csv("Data imputation/Data/preImputationCombined.csv")
pop <- read.csv("Data imputation/Data/NDNS/GLondon_basepop_1000.csv")

# Values to find
PDvalues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

# original table -----------------
# remove rows with data missing in PD column
preImputationPD <- as.data.frame(preImputationData$ghq12scr)
preImputationPD <- as.data.frame(preImputationPD[!is.na(preImputationPD), ])

column_name<- colnames(preImputationPD)[1]

names(preImputationPD)[names(preImputationPD) == column_name] <- "PD"

frequency_table_preIMP <- table(preImputationPD$PD)
frequencies_preImp <- frequency_table_preIMP[match(PDvalues, names(frequency_table_preIMP))]

# create bar chart to show distribution of BMIs in data
weightCategories <- c("underweight", "healthy", "overweight", "obese", "severely obese")

# plotting 
# plot margins
par(mfrow = c(2, 1))  # 2 rows, 1 column (, mar = c(4, 4, 2, 1))

preImptutationPDdistribution <- barplot(frequencies_preImp, 
                                        names.arg = PDvalues, 
                                        xlab = "GHQ-12-Score", 
                                        ylab = "Frequency",  
                                        main = "pre-Imptutation PD distribution", 
                                        col = "skyblue", 
                                        border = "black")

# png("Data/Plots/Imputation Validation/preImptutationPDdistribution.png")
# dev.off()

# Calculate frequency table for imputed table -----------------
frequency_table <- table(pop$PD)
frequencies <- frequency_table[match(PDvalues/12, names(frequency_table))]

# create bar chart to show distribution of BMIs in data
weightCategories <- c("underweight", "healthy", "overweight", "obese", "severely obese")

ImputedPDdistribution <- barplot(frequencies, 
                                 names.arg = PDvalues, 
                                 xlab = "GHQ-12-Score", 
                                 ylab = "Frequency", 
                                 main = "Imputed PD distribution", 
                                 col = "skyblue", 
                                 border = "black")
