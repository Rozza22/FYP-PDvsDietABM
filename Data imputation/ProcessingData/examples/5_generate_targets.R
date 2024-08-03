# script for generating PHASE targets 
rm(list = ls()) # clear environment
library(tidyverse)
library(plotrix)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
setwd(wd)

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
# Target no.1 - mean number of calories coming from HFSS in each year for each Sex / SES subgroup 

Target1 <- NDNS %>% ungroup() %>% group_by(SurveyYear, SES) %>% 
  # filter(HFSS==1) %>% 
  summarise(meanHFSS= mean(TotalKcal_M_1),
            seHFSS = std.error(TotalKcal_M_1)) %>% 
  mutate(SES = factor(SES, levels=c("low","medium","high")),
         SurveyYear = SurveyYear+2007)

write.csv(Target1, "ABM_code - with PD/calibration/Target_data_HFSS_SES.csv", row.names=F)

Target1 <- NDNS %>% group_by(SurveyYear, Sex, seriali) %>%
  # filter(HFSS==1) %>%
  mutate(HFSS_per_person= mean(TotalKcal_M_1),
         SurveyYear = SurveyYear+2007) %>%
  ungroup() %>%
  group_by(SurveyYear, Sex) %>%
  summarise(meanHFSS = mean(HFSS_per_person),
            seHFSS = std.error(HFSS_per_person)) %>% distinct()

# Target1 <- NDNS %>% group_by(SES,Sex) %>%
#   arrange(SurveyYear) %>%
#   mutate(lag1 = lag(meanHFSS),
#          lag2 = lag(meanHFSS, 2)) %>%
#   group_by(SurveyYear, SES, Sex) %>%
#   summarise(meanHFSS = ifelse(is.na(lag1), meanHFSS,
#                               ifelse(is.na(lag2), lag1,
#                                      lag1+lag2/2)))
#                               

# generating data for 2019 - to simulate the intervention

# options - 1) take the percentage reduction from the overall paper -  6.7% (95% CI 3.2% to 10.1%) (yau et al, 2022)
# 2) rescale the percentage reduction according to the absolute values reported in the paper for SES groups
# High (n=464) [reference group]	Middle (n=1164)	Low (n=342)
# Total HFSS	-575.1 (-1,576.5 to 426.3)	-581.4 (-1,807.2 to 644.4)	-329.5 (-2,171.9 to 1512.8)
# randomly sample an amount to reduce by for each household 
# calculate mean household purchasing for the NDNS and then apply these values/ 7 

policy <- NDNS %>% group_by(SurveyYear, seriali) %>% 
 # filter(HFSS==1) %>% 
  mutate(meanHFSS= mean(TotalKcal_M_1),
         SurveyYear = SurveyYear+2007) %>% 
  filter(SurveyYear==2018) %>% 
  mutate(SES = factor(SES, levels=c("low","medium","high"))) %>% 
  mutate(reduction = ifelse(SES=="high", 575.1/7,
                            ifelse(SES=="medium", -581.4/7,
                                   ifelse(SES=="low", -329.5/7)))) %>% 
  ungroup() %>% group_by(SES, Sex) %>% 
  dplyr::summarise(meanHFSS = mean(meanHFSS) - mean(meanHFSS)*0.067) %>% mutate(SurveyYear=2019)


# relative differences for high, middle and low 
Target1 <- rbind(Target1, policy)

write.csv(Target1, "ABM_code - with PD/calibration/Target_data_HFSS_sex.csv", row.names=F)

Target1 <- Target1 %>% 
  mutate(SES = paste(SES, "SES"),
         SES = factor(SES, levels=c("low SES","medium SES","high SES")),
         Sex = ifelse(Sex=="m","Men","Women"))

ggplot(data=Target1, aes(x=SurveyYear, y=meanHFSS)) + geom_line() + 
  facet_grid(cols=vars(SES), rows=vars(Sex)) + ylim(0,NA) + geom_vline(xintercept=2018, linetype="dashed") + theme_bw() +
  geom_smooth()
