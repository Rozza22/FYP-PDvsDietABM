# Analysing NDNS
# script to process the NDNS data
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

# NDNS <- readRDS("Data imputation/Data/NDNS/NDNS_full.RDS")
# NDNS <- cbind(subset(NDNS, select = c(SurveyYear, seriali, age, Sex, ethgrp5, qual7, eqvinc, eqv3, bmival)))
# NDNS <- subset(NDNS, age >= 16)
# NDNS$bmival <- ifelse(NDNS$bmival < 0, NA, NDNS$bmival)

NDNS <- read.csv("Data imputation/Data/NDNS/NDNS_combined.csv")

NDNS <- cbind(subset(NDNS, select = c(SurveyYear, seriali, age, Sex, ethgrp5, qual7, eqvinc, eqv3, bmival, ghq12scr))) # Have taken "WrkStat" out


contingencyTableEthBmi <- table(NDNS$ethgrp5, NDNS$bmival)
chiSquareTestEthBmi <- chisq.test(contingencyTableEthBmi)
print(chiSquareTestEthBmi)
corEthnicBMI <- cor(NDNS$ethgrp5, NDNS$bmival)

contingencyTableAgeBmi <- table(NDNS$age, NDNS$bmival)
chiSquareTestAgeBmi <- chisq.test(contingencyTableAgeBmi)
print(chiSquareTestAgeBmi)
