# HSE analysis
rm(list = ls()) # clear environment

library(foreign)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(hablar)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")

setwd(wd)

HSE <- readRDS("Data imputation/Data/HSE/HSE_Full.RDS")

colOrder <- c("surveyYear", "age", "sex", "origin", "topQual", "WrkStat", "eqv3", "ghq12scr", "bmival", "sayDiet", "sayWgt")
HSE <- HSE[, colOrder]

HSE <- HSE[HSE$ghq12scr >= 0, ]
HSE <- HSE[HSE$bmival >= 0, ]

contingencyTableGHQBmi <- table(HSE$ghq12scr, HSE$bmival)
chiSquareTestGHQBmi <- chisq.test(contingencyTableGHQBmi)
print(chiSquareTestGHQBmi)

corGHQBMI <- cor(HSE$ghq12scr, HSE$bmival)

# plot ------------------
# Scatter plot of historical diet and PD
ggplot(HSE, aes(x = ghq12scr, y = bmival)) +
  geom_point() +  # Add points for the scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add a line of best fit using linear regression
  labs(x = "ghq12scr", y = "bmival", title = "PD vs BMI")
