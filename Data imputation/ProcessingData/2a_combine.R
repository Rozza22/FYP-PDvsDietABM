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

####read in the joined up data files 
HSE <- readRDS("Data imputation/Data/HSE/HSEcca.RDS")
NDNS <- readRDS("Data imputation/Data/NDNS/NDNScca.RDS")
NDNSoriginal <- read.csv("Data imputation/Data/NDNS/NDNS_combined.csv")
origAge <- readRDS("Data imputation/Data/NDNS/origAge.RDS")
seriali <- readRDS("Data imputation/Data/NDNS/seriali.RDS")
gc()

HSE$QuintHFSScalsHist <- NA # introduce new columns to make them compatible
NDNS$ghq12scr <- NA
NDNS$sayDiet <- NA
NDNS$sayWgt <- NA

NDNS <- NDNS %>% relocate("ghq12scr", .after = "eqv3") # change column order
NDNS <- NDNS %>% relocate("sayDiet", .after = "bmival")
NDNS <- NDNS %>% relocate("sayWgt", .after = "sayDiet")
# NDNS <- NDNS %>% relocate("qimd", .after = "ghq12scr")

# Looking at correlations within each dataset to see if imputation has led to decrease in correlation


#Imputing ----------------------------------------------
# There are ways we can adjust how these imputated predictions are made, we can take some values out altogether
# It will definitely be good to take "ghq12scr" out of predicting "HFSS" and vice versa

# # Compare datasets
# summary(NDNS)
# summary(HSE)

# combine data and include original age of NDNS data
# Need to make a an empty column in HSE
emptyVector <- rep(NA, nrow(HSE))
origAge <- c(origAge, emptyVector)

CombinedData <- rbind(NDNS, HSE)
# write to Descriptive analysis folder
write.csv(NDNS, "Data imputation/Data/Descriptive Analysis/NDNS.csv", row.names=F)
write.csv(HSE, "Data imputation/Data/Descriptive Analysis/HSE.csv", row.names=F)
write.csv(CombinedData, "Data imputation/Data/Descriptive Analysis/preImputationCombined.csv", row.names=F)

write.csv(CombinedData, "Data imputation/Data/preImputationCombined.csv", row.names=F)

# remove any remaining negative values ------------------------
for (i in seq_along(CombinedData)) {
  CombinedData[[i]][CombinedData[[i]] < 0] <- NA  # Replace negative values with NA0
}

# change ethnic groups to match hfss_schema file
CombinedData$ethGrp[CombinedData$ethGrp == 3]<-9 # where 9 is an arbitrary temporary value
CombinedData$ethGrp[CombinedData$ethGrp == 4]<-3
CombinedData$ethGrp[CombinedData$ethGrp == 9]<-4

assignedYear <- subset(CombinedData, select = c(time))

for (i in 1:nrow(assignedYear)){
  assignedYear$time[i] =assignedYear$time[i] + 2007
}
names(assignedYear)[names(assignedYear) == "time"] <- "year"
CombinedData <- cbind(CombinedData, assignedYear)
CombinedData <- CombinedData %>% relocate("year", .before = "time")

CombinedData <- subset(CombinedData, select = -c(topQual)) # removes redundant eqvinc

# Dummy variable creation for categorical values
data <- CombinedData
data <- dummy_cols(data,select_columns = "sex", ignore_na = TRUE) # ignore_NA means no NA column
data <- dummy_cols(data,select_columns = "ethGrp", ignore_na = TRUE)
# data <- dummy_cols(data,select_columns = "wrkStat", ignore_na = TRUE)
data <- dummy_cols(data,select_columns = "sayDiet", ignore_na = TRUE)
data <- dummy_cols(data,select_columns = "sayWgt", ignore_na = TRUE)

# Dummy variables created for ordinal variables
data$time <- factor(data$time, ordered = TRUE, levels = c("1", "2", "3", "5", "7", "9", "11")) # edit to fit ------------------------
data <- dummy_cols(data, select_columns = "age", ignore_na = TRUE)
data$age <- factor(data$age, ordered = TRUE, levels = c("0", "1", "2", "3", "4", "5", "6", "7")) # edit to fit ------------------------
data <- dummy_cols(data, select_columns = "age", ignore_na = TRUE)
data$eqv3 <- factor(data$eqv3, ordered = TRUE, levels = c("1", "2", "3")) # edit to fit ------------------------
data <- dummy_cols(data, select_columns = "eqv3", ignore_na = TRUE)
# data$ghq12scr <- factor(data$ghq12scr, ordered = TRUE, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) # edit to fit ------------------------
# data <- dummy_cols(data, select_columns = "ghq12scr", ignore_na = TRUE)
# data$qimd <- factor(data$qimd, ordered = TRUE, levels = c("1", "2", "3", "4", "5")) # edit to fit ------------------------
# data <- dummy_cols(data, select_columns = "qimd", ignore_na = TRUE)

# data <- subset(data, select = -c(sex, ethGrp, sayDiet, sayWgt, age, eqv3, qimd, year, time)) # removes redundant categorical variables
data <- subset(data, select = -c(sex, ethGrp, sayDiet, sayWgt, age, eqv3, year, time)) # removes redundant categorical variables

data <- data %>% relocate("ghq12scr", .after = "eqv3_3")
data <- data %>% relocate("QuintHFSScalsHist", .after = "ghq12scr")

rm(HSE)
rm(assignedYear)

iniImp <- mice(data, maxit=0, m=1)
imputedData1 <- complete(iniImp, "long")
cor(imputedData1$bmival, imputedData1$QuintHFSScalsHist)

# pred <- iniImp$pred # making predictorMatrix stand alone variable to begin editing
# # Column variables used to impute the row variable if there is a '1'
# 
# predDF = as.data.frame(pred)
# write.csv(predDF, file='C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/RScripts/Using/CombPredictorMat.csv')
# # Edit the csv file in excel at this stage - This will be used in the actual project to exclude HFSS and GHQ impacting each other
# predDF <- read.csv(file='C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/RScripts/Using/CombPredictorMatEdited.csv')
# rownames(predDF) <- c(predDF[, 1]) # Assigned row names to first column instead of heading - fixing this
# predDF = select(predDF, -X)
# predDF <- as.matrix(predDF) # Edited predictor matrix which means that HFSS and Mental Health variables are not used for imputation of each other

impPredEdit <- mice(data, maxit = 0, m=1 , method = 
                      c("","","","","","","","","","","","","","","","","","","", # currently 27 columns including last two
                        "","","","","","","pmm",""), visitSequence = 1:nrow(data)) # Imputation with only two columns being imputed

# meth <- impPredEdit$method
# meth
# impPredEdit$defaultMethod[names(impPredEdit$method) %in% c("ghq12scr", "QuintHFSScalsHist")] <- "pmm"
# meth
# meth <- as_character(meth)
# impPredEdit$method <- meth
# impPredEdit$method
# impPredEdit <- mice(data, maxit = 10, m=1) # Don't think I need this anymore because first way works now

imputedData <- complete(impPredEdit, "long")

# Retrieve and update NDNS data and put serial number back in
NDNSimputed <- imputedData[1:nrow(NDNS), ]
# First replace ages with "origAge"
origAge <- origAge[1:nrow(NDNS)]
NDNS$age <- origAge
# Next thing is to just replace ghq12scr and HFSSSchema score
NDNSoriginal$ghq12scr <- NDNSimputed$ghq12scr
# NDNS$QuintHFSScalsHist <- NDNSimputed$QuintHFSScalsHist


names(NDNS)[names(NDNS) == "sex"] <- "Sex"
names(NDNS)[names(NDNS) == "ethGrp"] <- "ethgrp5"

#CombinedData$origAge <- origAge

write.csv(NDNSoriginal, "Data imputation/Data/NDNS/NDNS_combined.csv", row.names=F)
# saveRDS(NDNS, file = "C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/Data/imputedNDNS.RDS")

cor(imputedData$bmival, imputedData$QuintHFSScalsHist, use="complete.obs")

# testing stuff
# fit <- with(impPredEdit, lm(QuintHFSScalsHist ~ ghq12scr), print=F)
# AllImputedData <- pool(fit)
# summary(AllImputedData)
# AllImputedData # looking out for lambda and fmi to be low, below 0.1 each
# 
# fit1 <- with(impPredEdit, lm(QuintHFSScalsHist ~ ghq12scr + sayWgt + sayDiet), print=F)
# AllImputedData1 <- pool(fit1)
# summary(AllImputedData1)
# AllImputedData1 # looking out for lambda and fmi to be low, below 0.1 each
# 
# plot(impPredEdit)
# # stripplot(impPredEdit) # This caused a crash because there was too much
# 
# c.long <- complete(impPredEdit, "long")  
# cor(CombinedData$sayWgt, CombinedData$sayDiet, na.rm=T) # is there any link?

# summary(CombinedData) # Seeing brief overview of what each column shows us
# md.pattern(CombinedData) # Overview of where missing values are


