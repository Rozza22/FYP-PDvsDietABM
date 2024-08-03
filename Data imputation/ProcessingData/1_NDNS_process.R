# script to process the NDNS data
rm(list = ls()) # clear environment

library(foreign)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(hablar)

# CB laptop directory
# wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
# data <- ("Data/NDNS/Individual Data/")

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
# data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

####read in the joined up data files 
NDNSoriginal <- readRDS("Data imputation/Data/NDNS/NDNS_full.RDS")
NDNS_HFSS <- readRDS("Data imputation/Data/NDNS/NDNS_HFSS.RDS")
gc()

NDNSoriginal$ben # - variable for economic status in NDNS

# Read in R script of functions
source("Data imputation/ProcessingData/1_NDNS_process_Functions - Copy.R")

# NDNSoriginal$EIMD_2015_quintile # England - is the IMD value which is a good socio-economic measure
# NDNSoriginal$SIMD_2016_quintile # Scotland - have a look at what this is?
# NDNSoriginal$NIMD_2017_quintile # Northern Ireland
# NDNSoriginal$WIMD_2014_quintile # Wales

# Remove values from HFSS data which we aren't using
NDNS_HFSS <- cbind(subset(NDNS_HFSS, select = c(SurveyYear, seriali, DayNo, Energykcal, HFSS))) # might be worth adding in some others but will need to do research to see if needed

NDNS_demo <- cbind(subset(NDNSoriginal, select = c(SurveyYear, seriali, age, Sex, ethgrp5, qual7, eqvinc, eqv3, bmival))) # Have taken "WrkStat" out
qimd <- cbind(subset(NDNSoriginal, select = c(SurveyYear, EIMD_2015_quintile, SIMD_2016_quintile, NIMD_2017_quintile, WIMD_2014_quintile)))
# rm(NDNSoriginal)

# get rid of years with no GHQ12 in HSE data
# NDNS_demo <- OnlyGHQyrs(NDNS_demo) # applies function - doing it here for both to cut length of time taken for functions
# NDNS_HFSS <- OnlyGHQyrs(NDNS_HFSS)
# NDNSoriginal <- OnlyGHQyrs(NDNSoriginal)
qimd <- OnlyGHQyrs(qimd)
seriali <- NDNS_demo$seriali
serialh <- NDNS_demo$serialh

# Combine the IMDs into one column
qimd <- subset(qimd, select = -c(SurveyYear)) # removes redundant eqvinc
qimd <- qimd %>% mutate(qimd = coalesce(EIMD_2015_quintile,SIMD_2016_quintile,NIMD_2017_quintile,WIMD_2014_quintile)) %>%
  select(qimd)

# group ages into 10 bands - same as the HSE data ---------------------------------
origAge <- NDNS_demo$age
NDNS_demo <- AgeBands(NDNS_demo)

# calculate eqv3 for years 1, 2 and 3 ----------------------------- fix this to band limits in HSE
NDNS_demo <- IncomeTertiles(NDNS_demo)

# Change sex values from 1 and 2 to 0 and 1
NDNS_demo <- binarySex(NDNS_demo)

# recoding "WrkStat"
# NDNS_demo <- recodeEconStat(NDNS_demo)

# Splitting BMI into 5 quintiles
NDNS_demo <- BMIquintiles(NDNS_demo)

# Putting participants into 5 bands based on calories per day from HFSS foods and joining demo+HFSSscore
NDNS <- HFSSbands(NDNS_HFSS, NDNS_demo)
# NDNS <- cbind(NDNS, qimd)

rm(NDNS_demo)
rm(NDNS_HFSS)
# rm(qimd)

# Join the two data frames together ------------------------
# NDNS <- DuplicateForJoining(NDNS_HFSS, NDNS_demo) # this will be adding 'HFSS' variable at all meals recorded


# rename columns to be the same as HSE or make them better--------------------------------
names(NDNS)[names(NDNS) == "Age"] <- "age"
names(NDNS)[names(NDNS) == "Sex"] <- "sex"
names(NDNS)[names(NDNS) == "qual7"] <- "topQual"
names(NDNS)[names(NDNS) == "SurveyYear"] <- "time"
# names(NDNS)[names(NDNS) == "WrkStat"] <- "wrkStat"
names(NDNS)[names(NDNS) == "ethgrp5"] <- "ethGrp"

# check how many rows we are left with when removing all rows with NA or negative values
NDNSna <- NDNS
NDNSna[NDNSna < 0] <- NA #sets all negative values to NA
NDNSna = NDNSna %>% na.omit() #omits all rows with NA in them

saveRDS(NDNS, file = "//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/Data imputation/Data/NDNS/NDNScca.RDS")
saveRDS(origAge, file = "//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/Data imputation/Data/NDNS/origAge.RDS")
saveRDS(seriali, file = "//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/Data imputation/Data/NDNS/seriali.RDS")
saveRDS(serialh, file = "//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/Data imputation/Data/NDNS/serialh.RDS")
write.csv(NDNSoriginal, paste0("Data imputation/Data/NDNS/NDNS_combined.csv"), row.names=F)

#check correlations in this part of the data
cor(NDNS$bmival, NDNS$QuintHFSScalsHist, use="complete.obs")

# summary(NDNS$bmival)
