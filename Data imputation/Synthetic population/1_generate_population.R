rm(list = ls()) # clear environment
library(haven)
library(tidyverse)
library(readxl)
library(ipfp)

# wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
# data <- ("Data/NDNS/Individual Data/")

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
# data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

source("Data imputation/Synthetic population/IPF_functions.R")

NDNS <- read_csv("Data imputation/Data/NDNS_outputs/NDNS_schema_HFSS.csv") %>% 
  mutate(nssec8 = ifelse(age<16, 0,
                         ifelse(nssec8==99, 9,
                         ifelse(nssec8<0, NA,nssec8))),
    nssec8 = factor(nssec8,
                         levels = c(0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9),
                         labels = c("0" ,"1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "l15")),
         ageband = cut(age, breaks = c(0, 15 , 24, 34, 49, 64, Inf),
                       labels = c("_16" , "16_24", "25_34", "35_49", "50_64", "65_")),
    # Sex = ifelse(Sex==1, "m","f"),
    ethgrp5 = ifelse(ethgrp5 <0, NA, ethgrp5),
    ethgrp5 = recode(ethgrp5, "1"="white","2"="mixed", "3"="asian", "4"="black","5"="other")
    ) %>% drop_na(nssec8,ageband,Sex,ethgrp5)

missing <- NDNS %>% 
  filter(Sex=="m" & ageband=="65_" & nssec8=="l15") %>%
  mutate(SurveyYear = 1)

NDNS <- rbind(NDNS, missing) %>% filter(SurveyYear<=2)

census <- read_excel("Data imputation/Data/Census_nssec_0802 noqual.xlsx") %>%
  dplyr::select(sex, age, nssec, label, Greater_London, Sheffield, Manchester) %>%
  filter(age!="_16")

cons <- ipf_consprep(census)

ind <- ipf_ndnsprep(NDNS)

# weights < ipf_run(NDNS, cons, ind)
# 
# # save the weights for each individual
# weights <- data.frame(weights)
# names(weights) <- c("GLondon","Sheffield","Manchester")
# write.csv(weights, "Data/NDNS/pop_weights.csv", row.names=F)

weights <- read.csv("Data imputation/Data/NDNS/pop_weights.csv")
int_glondon <- int_trs(weights[,1])
int_sheffield <- int_trs(weights[,2])
int_manchester <- int_trs(weights[,3])

ID <- 1:nrow(NDNS)

# bind with the other NDNS data
NDNS <- cbind(ID, NDNS, int_glondon, int_sheffield, int_manchester)


# select variables of interest for the baseline population 
population <- NDNS %>% dplyr::select(ID, age, Sex, nssec8, SES, ethgrp5, eth_new, bmival, ghq12scr, TotalKcal_M_0,
                                     TotalKcal_SD_0, TotalKcal_M_1, TotalKcal_SD_1,
                                     `history_0-200`, `history_201-500`, `history_501-750`, `history_750-1000`,
                                     `history_1001+`,
                                     `desire_0-200`, `desire_201-500`, `desire_501-750`, `desire_751-1000`,
                                     `desire_1001-`,
                                     int_glondon, int_sheffield, int_manchester)

# create a population for Greater London 
library(splitstackshape)
GLondon <- population %>% dplyr::select(-c(int_sheffield, int_manchester)) %>% expandRows(., "int_glondon") %>% 
  sample_n(1000) %>%
  mutate(ID = 1:nrow(.),
         location="GLondon") 

Sheffield <- population %>% dplyr::select(-c(int_glondon, int_manchester)) %>% expandRows(., "int_sheffield") %>% 
  sample_n(1000) %>%
  mutate(ID = 1:nrow(.),
         location="Sheffield") 

Manchester <- population %>% dplyr::select(-c(int_glondon, int_sheffield)) %>% expandRows(., "int_manchester") %>% 
  sample_n(1000) %>%
  mutate(ID = 1:nrow(.),
         location="Manchester") 

compare <- rbind(GLondon, Sheffield, Manchester) %>% group_by(location, Sex) %>% 
  summarise(mean(age),
            mean(bmival),
            mean(TotalKcal_M_1))

# save populations 
write.csv(GLondon, "Data imputation/Data/NDNS/GLondon_basepop_1000.csv", row.names=F)
write.csv(Sheffield, "Data imputation/Data/NDNS/Sheffield_basepop.csv", row.names=F)
write.csv(Manchester, "Data imputation/Data/NDNS/Manchester_basepop.csv", row.names=F)

