# PD and diet
rm(list = ls()) # clear environment

library(haven)
library(tidyverse)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
# data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

pop <- read.csv("Data imputation/Data/NDNS/GLondon_basepop_1000.csv")

focusData <- as.data.frame(pop$history_0.200 * 0 + pop$history_201.500 * 200 + pop$history_501.750 * 500 
                                 + pop$history_751.1000 * 750 + pop$history_1001.* 1000)
colnames(focusData)[1] <- "historyHFSS"

focusData <- cbind(focusData, pop$PD*12)
colnames(focusData)[2] <- "PD"

Desire <- as.data.frame(pop$desire_0.200 * 0 + pop$desire_201.500 * 200 + pop$desire_501.750 * 500 
                        + pop$desire_751.1000 * 750 + pop$desire_1001.* 1000)
colnames(Desire)[1] <- "desireHFSS"

focusData <- cbind(focusData, Desire$desireHFSS, pop$bmival)
colnames(focusData)[3] <- "desireHFSS"
colnames(focusData)[4] <- "bmi"

# plots for PD ----------------------------

ggplot(focusData, aes(x = historyHFSS, y = PD)) +
  geom_point() +  # Add points for the scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add a line of best fit using linear regression
  labs(x = "Historical HFSS intake", y = "Psychological Distress", title = "Historical HFSS impact on PD")


ggplot(focusData, aes(x = PD, y = desireHFSS)) +
  geom_point() +  # Add points for the scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add a line of best fit using linear regression
  labs(x = "Psychological Distress", y = "Desire HFSS intake", title = "PD impact on Desire HFSS")

# plots for BMI ----------------------------

ggplot(focusData, aes(x = historyHFSS, y = bmi)) +
  geom_point() +  # Add points for the scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add a line of best fit using linear regression
  labs(x = "Historical HFSS intake", y = "BMI", title = "Historical HFSS impact on BMI")


ggplot(focusData, aes(x = bmi, y = desireHFSS)) +
  geom_point() +  # Add points for the scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add a line of best fit using linear regression
  labs(x = "BMI", y = "Desire HFSS intake", title = "BMI impact on Desire HFSS")



# statistical tests on PD --------------------
contingencyTableHFSSPD <- table(focusData$PD, focusData$historyHFSS)
chiSquareTestHFSSPD <- chisq.test(contingencyTableHFSSPD)
print(chiSquareTestHFSSPD)
corHFSSPD <- cor(focusData$PD, focusData$historyHFSS)

contingencyTableHFSSdesirePD <- table(focusData$PD, focusData$desireHFSS)
chiSquareTestHFSSdesirePD <- chisq.test(contingencyTableHFSSdesirePD)
print(chiSquareTestHFSSdesirePD)
corHFSSdesirePD <- cor(focusData$PD, focusData$desireHFSS)

# statistical tests on BMI --------------------
contingencyTableHFSSdesireBMI <- table(focusData$bmi, focusData$desireHFSS)
chiSquareTestHFSSdesireBMI <- chisq.test(contingencyTableHFSSdesireBMI)
print(chiSquareTestHFSSdesireBMI)
corHFSSdesireBMI <- cor(focusData$bmi, focusData$desireHFSS)

contingencyTableHFSShistoryBMI <- table(focusData$historyHFSS, focusData$bmi)
chiSquareTestHFSShistoryBMI <- chisq.test(contingencyTableHFSShistoryBMI)
print(chiSquareTestHFSShistoryBMI)
corHFSShistoryBMI <- cor(focusData$historyHFSS, focusData$bmi)


# Making sure the weighted way was OK ---------------------------

allDesire <- cbind(pop$desire_0.200, pop$desire_201.500, pop$desire_501.750, pop$desire_751.1000, 
                   pop$desire_1001., focusData$desireHFSS)
allDesire <- as.data.frame(allDesire)

names(allDesire)[names(allDesire) == "V1"] <- "VeryLow"
names(allDesire)[names(allDesire) == "V2"] <- "Low"
names(allDesire)[names(allDesire) == "V3"] <- "Med"
names(allDesire)[names(allDesire) == "V4"] <- "High"
names(allDesire)[names(allDesire) == "V5"] <- "VeryHigh"
names(allDesire)[names(allDesire) == "V6"] <- "WeightedSum"

corVeryHighHFSS <- cor(allDesire$VeryHigh, allDesire$WeightedSum)
corVeryLowHFSS <- cor(allDesire$VeryLow, allDesire$WeightedSum)
corMedHFSS <- cor(allDesire$Med, allDesire$WeightedSum)
corHighHFSS <- cor(allDesire$High, allDesire$WeightedSum)
