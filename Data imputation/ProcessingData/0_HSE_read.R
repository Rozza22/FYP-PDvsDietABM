rm(list = ls()) # clear environment

library(haven)
library(tidyr)
library(foreign)
#library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
# setwd("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/ABM_code - with PD")
wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

gc()

#read in all the data files downloaded from data bank online
#I renamed them initially after downloading before realizing this was easier
#names <- Sys.glob(c("*.dta",
#                    "*.DTA"))
#dataFiles < lapply(names, read.dta)
#names(dataFiles) <- names

#rename the files - only leaving year of data

#names(dataFiles) <- gsub("hse", "", names(dataFiles))
#names(dataFiles) <- gsub("ai.dta", "", names(dataFiles))
#as.numeric(names(dataFiles))

#names(dataFiles) <- parse_number(names(dataFiles))
#rm(list=setdiff(ls(), c("dataFiles")))

#dataFiles <- dataFiles[order(names(dataFiles))]
#save rds of full data list

#saveRDS(dataFiles, file="C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/Data/HSE_Full.RDS")

hse08 <- read_dta(paste0(data, "hse08ai.dta"))  # year 1
hse09 <- read_dta(paste0(data, "hse09ai.dta"))
hse10 <- read_dta(paste0(data, "hse10ai.dta"))
# hse11 <- read_dta(paste0(data, "hse11ai.dta")) # commented this and the others out as they don't include the psychological stress measure (GHQ12)
hse12 <- read_dta(paste0(data, "hse12ai.dta")) # year 5
# hse13 <- read_dta(paste0(data, "hse13ai.dta"))
hse14 <- read_dta(paste0(data, "hse14ai.dta"))
# hse15 <- read_dta(paste0(data, "hse15ai.dta"))
hse16 <- read_dta(paste0(data, "hse16ai.dta")) # year 9 
# hse17 <- read_dta(paste0(data, "hse17ai.dta"))
hse18 <- read_dta(paste0(data, "hse18ai.dta"))
# hse19 <- read_dta(paste0(data, "hse19ai.dta"))

# hse15compare <- subset(hse15, select = c(Activb2,HRPactiv2))

#names <- (hse08,hse09,hse10,hse11,hse12,hse13,hse14,hse15,hse16,hse17,hse18,hse19)
#dataFiles <-lapply(names, read.dta)

Yr1Col <- replicate(nrow(hse08),1) # make a column for the year, done like this to match with how its done in NDNS survey
Yr2Col <- replicate(nrow(hse09),2)
Yr3Col <- replicate(nrow(hse10),3)
Yr4Col <- replicate(nrow(hse12),5)
Yr5Col <- replicate(nrow(hse14),7)
Yr6Col <- replicate(nrow(hse16),9)
Yr7Col <- replicate(nrow(hse18),11)

hse08$eqv3 = as.numeric(as.character(hse08$eqv3)) 
hse09$eqv3 = as.numeric(as.character(hse09$eqv3)) 
hse10$eqv3 = as.numeric(as.character(hse10$eqv3)) 
hse12$eqv3 = as.numeric(as.character(hse12$eqv3)) 
hse14$eqv3 = as.numeric(as.character(hse14$eqv3)) 
hse16$eqv3 = as.numeric(as.character(hse16$eqv3)) 
hse18$eqv3 = as.numeric(as.character(hse18$eqv3)) 

sub1 <- cbind(subset(hse08, select = c(ag16g10, sex, origin, topqual2, econact, # changed the measure of age as will be easier to align with NDNS data
                                 ghq12scr, bmival, qimd, saywgt, saydiet, bmiok)), Yr1Col, hse08$eqv3)
sub2 <- cbind(subset(hse09, select = c(ag16g10, sex, origin, topqual2, econact,
                                 ghq12scr, bmival, IMD2007, saywgt, saydiet, bmiok)), Yr2Col, hse09$eqv3)
sub3 <- cbind(subset(hse10, select = c(ag16g10, sex, origin, topqual2, econact,
                                 ghq12scr, bmival, imd2007, saywgt, saydiet, bmiok)), Yr3Col, hse10$eqv3)
sub4 <- cbind(subset(hse12, select = c(ag16g10, Sex, Origin, topqual2, econact,
                                 ghq12scr, bmival, qimd, SayWgt, SayDiet, bmiok)), Yr4Col, hse12$eqv3)
sub5 <- cbind(subset(hse14, select = c(ag16g10, Sex, origin2, topqual2, econact,
                                 ghq12scr, BMIval, qimd, SayWgt, SayDiet, BMIok)), Yr5Col, hse14$eqv3)
sub6 <- cbind(subset(hse16, select = c(ag16g10, Sex, Origin2, topqual2, StWork, # was going to add StWork to the last two but there is very poor response rate
                                 ghq12scr, BMIval, qimd, SayWgt, SayDiet, BMIok)), Yr6Col, hse16$eqv3)                        
sub7 <- cbind(subset(hse18, select = c(ag16g10, Sex, origin2, topqual2, StWork, GHQ12Scr, BMIVal, qimd, BMIOK)), Yr7Col, hse18$eqv3)

sub7$sayWgt <- NA
sub7$sayDiet <- NA

names(sub6)[names(sub6) == "StWork"] <- "econact"
names(sub7)[names(sub7) == "StWork"] <- "econact"

sub7 <- sub7 %>% relocate("sayWgt", .after = "BMIVal") # change column order
sub7 <- sub7 %>% relocate("sayDiet", .after = "sayWgt") # change column order

rm(hse08)
rm(hse09)
rm(hse10)
rm(hse12)
rm(hse14)
rm(hse16)
rm(hse18)

# eqv3Column = rbind(hse08$eqv3, hse09$eqv3, hse10$eqv3, hse12$eqv3, hse14$eqv3, hse16$eqv3, hse18$eqv3)


#rename columns to all be the same
sub1 <- sub1 %>%
  rename("age" = "ag16g10", "topQual" = "topqual2", "surveyYear" = "Yr1Col", "eqv3" = "hse08$eqv3",
         "sayWgt" = "saywgt", "sayDiet" = "saydiet")
sub2 <- sub2 %>%
  rename("age" = "ag16g10", "topQual" = "topqual2", "surveyYear" = "Yr2Col", "eqv3" = "hse09$eqv3",
         "sayWgt" = "saywgt", "sayDiet" = "saydiet", "qimd" = "IMD2007")
sub3 <- sub3 %>%
  rename("age" = "ag16g10", "topQual" = "topqual2", "surveyYear" = "Yr3Col", "eqv3" = "hse10$eqv3",
         "sayWgt" = "saywgt", "sayDiet" = "saydiet", "qimd" = "imd2007")
sub4 <- sub4 %>%
  rename("age" = "ag16g10", "sex" = "Sex", "topQual" = "topqual2", "surveyYear" = "Yr4Col",
         "sayWgt" = "SayWgt", "sayDiet" = "SayDiet", "eqv3" = "hse12$eqv3", "origin" = "Origin")
sub5 <- sub5 %>%
  rename("age" = "ag16g10", "sex" = "Sex", "topQual" = "topqual2", "surveyYear" = "Yr5Col", "origin" = "origin2",
         "bmival" = "BMIval", "sayWgt" = "SayWgt", "sayDiet" = "SayDiet", "eqv3" = "hse14$eqv3", "bmiok" = "BMIok")
sub6 <- sub6 %>%
  rename("age" = "ag16g10", "sex" = "Sex", "topQual" = "topqual2", "surveyYear" = "Yr6Col", "origin" = "Origin2",
         "bmival" = "BMIval", "sayWgt" = "SayWgt", "sayDiet" = "SayDiet", "eqv3" = "hse16$eqv3", "bmiok" = "BMIok")
sub7 <- sub7 %>%
  rename("age" = "ag16g10", "sex" = "Sex", "topQual" = "topqual2", "ghq12scr" = "GHQ12Scr", "surveyYear" = "Yr7Col", 
         "eqv3" = "hse18$eqv3",  "bmival" = "BMIVal", "origin" = "origin2", "bmiok" = "BMIOK")

sub1 <- as.matrix(sub1, rownames.force = NA) # Original form not correct and wouldn't bind
sub1 <- as.data.frame(sub1)
sub2 <- as.matrix(sub2, rownames.force = NA)
sub2 <- as.data.frame(sub2)
sub3 <- as.matrix(sub3, rownames.force = NA)
sub3 <- as.data.frame(sub3)
sub4 <- as.matrix(sub4, rownames.force = NA)
sub4 <- as.data.frame(sub4)
sub5 <- as.matrix(sub5, rownames.force = NA)
sub5 <- as.data.frame(sub5)
sub6 <- as.matrix(sub6, rownames.force = NA)
sub6 <- as.data.frame(sub6)
sub7 <- as.matrix(sub7, rownames.force = NA)
sub7 <- as.data.frame(sub7)

#adding all together
Data <- rbind(sub1, sub2, sub3, sub4, sub5, sub6, sub7)

Data <- Data %>%
  rename("WrkStat" = "econact")

rm(sub1)
rm(sub2)
rm(sub3)
rm(sub4)
rm(sub5)
rm(sub6)
rm(sub7)

write.csv(Data, paste0("Data imputation/Data/HSE/HSE_full.csv"), row.names=F)

saveRDS(Data, file="Data imputation/Data/HSE/HSE_Full.RDS")
