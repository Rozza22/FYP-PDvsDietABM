# script to process the HSE data
rm(list = ls()) # clear environment

library(foreign)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(missForest)
library(mice)
library(VIM)


# CB laptop directory
# wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
# data <- ("Data/HSE/Individual Data/")
wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

####read in the joined up data files 
HSE <- readRDS("Data imputation/Data/HSE/HSE_Full.RDS")
gc()

# Read in R script of functions
source("Data imputation/ProcessingData/1_HSE_process_functions.R")

# Order in same way where we can as the NDNS dataset
# colOrder <- c("surveyYear", "age", "sex", "origin", "topQual", "WrkStat", "eqv3", "ghq12scr", "qimd", "bmival", "sayDiet", "sayWgt")
colOrder <- c("surveyYear", "age", "sex", "origin", "topQual", "WrkStat", "eqv3", "ghq12scr", "bmival", "sayDiet", "sayWgt")
HSE <- HSE[, colOrder]

# set all negative values to NA, this will be needed when looking at imputation and also other areas
HSE[HSE < 0] <- NA #sets all negative values to NA

# make sex 0 and 1 instead of 1 and 2
HSE <- binarySex(HSE)

# Split BMI into quintiles - would've used the already available data for this but a lot of it was missing in comparison
# HSE <- BMIquintiles(HSE)

# Recode ethnicity to be consistent within HSE and also with NDNS equivalent
HSE <- recodeEthnicity(HSE)
names(HSE)[names(HSE) == "origin"] <- "ethGrp"

# Recode the econact to be compatible with the NDNS equivalent
# HSE <- econStatus(HSE) # Taking this out all together as it is subject to too much variation
HSE <- subset(HSE, select = -c(WrkStat)) # removes redundant "WrkStat"


# names(HSE)[names(HSE) == "WrkStat"] <- "wrkStat"

# Check if  columns with lots of missing data are missing at random ("sayDiet" and "sayWgt") --------------------
# To check if your data are MAR, take each column with missingness and recode it as one if it is missing 
# and zero otherwise. Then regress each of the the other variables onto it using a logistic regression. 
# A significant p-value indicates an association between the regressor and missingness, meaning your data are MAR
# DietMAR <- prepMARtest(HSE)

# names(summary(fit))# Checks how many data points we'd be left with if we omitted all rows with NA and negative values
HSEna1 <- HSE # initialising this dataset for "Complete Case Analysis" - where NA values are just taken out
HSEna1[HSEna1 < 0] <- NA #sets all negative values to NA
HSEna1 <- HSEna1 %>% na.omit() #omits all rows with NA in them

names(HSE)[names(HSE) == "surveyYear"] <- "time"

saveRDS(HSE, file = "//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/Data imputation/Data/HSE/HSEcca.RDS")

# Checking correlations in this part of the data
cor(HSE$bmival, HSE$ghq12scr, use="complete.obs")

# Imputing just HSE -------------------------------------------------------------------------
# md.pattern(HSE)
# 
# imp <- mice(HSE, maxit=10, print=F)
# fit <- with(imp, lm(ghq12scr ~ age + eqv3 + sex + topQual + wrkStat), print=F)
# AllImputedData <- pool(fit)
# summary(AllImputedData)
# AllImputedData # looking out for lambda and fmi to be low, below 0.1 each
