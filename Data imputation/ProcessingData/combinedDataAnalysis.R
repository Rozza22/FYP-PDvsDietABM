# analysis of imputed data
# Combine HSE and NDNS then fill out empty variables
rm(list = ls()) # clear environment

library(foreign)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(missForest)
library(mice)
library(lattice)
library(VIM)
library(e1071)
library(caTools)
library(class)
# library(ggplot2)
# library(GGally)
library(scatterplot3d)
library(fastDummies)

# wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
# # data <- ("Data/HSE/Individual Data/")
wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
# data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

data <- read.csv("Data imputation/Data/NDNS/NDNS_combined.csv")

data <- cbind(subset(data, select = c(SurveyYear, seriali, age, Sex, ethgrp5, qual7, eqvinc, eqv3, bmival, ghq12scr))) # Have taken "WrkStat" out

contingencyTableGHQBmi <- table(data$ghq12scr, data$bmival)
chiSquareTestGHQBmi <- chisq.test(contingencyTableGHQBmi)
print(chiSquareTestGHQBmi)
