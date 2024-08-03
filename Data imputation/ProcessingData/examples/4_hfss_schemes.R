# setting up schemas of HFSS foods for PHASE project 
library(haven)
library(tidyverse)
library(readxl)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/Data imputation")
setwd(wd)

# data <- read_csv("Data/NDNS/processed_fooddiary.csv")
data <- read_csv("Data/NDNS/processed_fooddiaryNew.csv")
# clusters <- read_csv("Data/NDNS/final_clusters.csv")
clusters <- read_csv("Data/NDNS/final_clustersNew.csv")
data <- left_join(data, clusters)

# NDNS <- read_csv("Data/NDNS/NDNS_combinedOriginal.csv") %>% 
NDNS <- read_csv("Data/NDNS/NDNS_combined.csv") %>% 
  mutate(nssec8 = ifelse(age<16, 0,
                         ifelse(nssec8==99, 9,
                                ifelse(nssec8<0, NA,nssec8))),
         # nssec8 = factor(nssec8,
         #                 levels = c(0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9),
         #                 labels = c("0" ,"1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "l15")),
         ageband = cut(age, breaks = c(0, 15 , 24, 34, 49, 64, Inf),
                       labels = c("_16" , "16_24", "25_34", "35_49", "50_64", "65_")),
         Sex = ifelse(Sex==1, "m","f"),
         ethgrp5 = ifelse(ethgrp5 <0, NA, ethgrp5),
         ethgrp5 = recode(ethgrp5, "1"="white","2"="mixed", "3"="asian", "4"="black","5"="other"),
         eth_new = ifelse(ethgrp5=="mixed" | ethgrp5=="other", "mixed/other", ethgrp5),
         bmival = ifelse(bmival==-1, NA, bmival),
         SES = ifelse(nssec8=="1" | nssec8=="2", "high",
                      ifelse(nssec8=="3" | nssec8=="4" | nssec8=="5" | nssec8=="6", "medium",
                             ifelse(nssec8=="7"| nssec8=="8" | nssec8=="9", "low", NA)))
  ) %>% drop_na(SES,ageband,Sex,ethgrp5, bmival) %>% 
  dplyr::select(seriali, serialh, age, Sex, ethgrp5, eth_new, nssec8, SES, bmival)

summary(as.factor(NDNS$SES))

# join with individual properties from NDNS 
data <- left_join(data, NDNS) %>% drop_na()

# for each individual, quantify how many calories from each cluster
clusters <- data %>% 
  # mutate(HFSS = ifelse(description=="biscuits, chocolate, crisps, puddings", 1,0)) %>% 
  group_by(SurveyYear, DayNo, seriali, description) %>% 
  mutate(TotalKcal = sum(Energykcal)) %>% 
  dplyr::select(SurveyYear, DayNo, seriali, age, Sex, eth_new, nssec8, bmival,
                description, TotalKcal) %>% 
  distinct()

summary <- clusters %>% ungroup() %>% 
  mutate(agecat = cut(age, breaks=c(0,16,24,34,44,54,64,100),
                      labels=c("0-15","16-24","25-34","35-44","45-54",
                               "55-64","65+"))) %>% 
  group_by(SurveyYear, agecat, Sex, description) %>% 
  summarise(TotalKcal = sum(TotalKcal)) %>% 
  ungroup() %>% 
  group_by(SurveyYear, agecat, Sex) %>% 
  mutate(proportion=TotalKcal/sum(TotalKcal))

ggplot(data=summary, aes(x=SurveyYear, y=proportion, colour=description)) + 
  geom_point() + facet_grid(cols=vars(Sex), rows=vars(agecat)) + 
  geom_line()
# looks fairly stable over time for all categories, justify collapsing all years data

# now plot distributions of calories from HFSS category
HFSS <- data %>% mutate(HFSS = ifelse(description=="biscuits, chocolate, crisps, puddings", 1,0)) %>% 
  mutate(agecat = cut(age, breaks=c(0,15,24,34,44,54,64,100),
                      labels=c("0-15","16-24","25-34","35-44","45-54",
                               "55-64","65+"))) %>% 
  group_by(seriali, serialh, SurveyYear, DayNo, age, agecat, Sex, ethgrp5, eth_new,  bmival, nssec8, SES, HFSS) %>% 
  summarise(TotalKcal = sum(Energykcal))

totalkcalpergroup <- HFSS %>% filter(HFSS==1) %>% group_by(agecat, Sex) %>% 
  summarise(TotalKcal = sum(TotalKcal)) %>% 
  ungroup() %>% mutate(proportion = TotalKcal / sum(TotalKcal))
  
  
meansperday <- HFSS %>% group_by(seriali, HFSS, agecat, Sex) %>% 
  summarise(meanperday = mean(TotalKcal)) %>% 
  group_by(HFSS, agecat, Sex) %>% 
  summarise(mean=mean(meanperday)) %>% filter(HFSS==1)

min(meansperday$mean)
max(meansperday$mean)
mean(meansperday$mean)
median(meansperday$mean)

HFSS <- HFSS %>% group_by(Sex, agecat, eth_new) %>% 
  mutate(median = median(TotalKcal))
  # filter(agecat!="0-17")

ggplot(data=subset(HFSS, HFSS==1), aes(x=TotalKcal)) +
  geom_density(aes(y=..scaled.., color=as.factor(Sex), fill=as.factor(Sex)), alpha=0.6) +
  geom_vline(aes(xintercept=median, color=as.factor(Sex)), linetype="dashed") + 
  facet_grid(rows=vars(eth_new), cols=vars(agecat), scales="free") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) +
  xlim(0,2000) + xlab("total kcal per day from HFSS category")

ggsave("Data/NDNS_outputs/plot_distribution_HFSS_kcal_race.png", dpi=300, width=33,
       height=19, units="cm")

HFSS <- HFSS %>% group_by(Sex, eth_new, SES) %>% 
  mutate(median = median(TotalKcal)) %>% 
  # filter(agecat!="0-17") %>% 
  mutate(SES = factor(SES, 
                      levels=c("low","medium","high")))

ggplot(data=subset(HFSS, HFSS==1), aes(x=TotalKcal)) +
  geom_density(aes(y=..scaled.., color=as.factor(Sex), fill=as.factor(Sex)), alpha=0.6) +
  geom_vline(aes(xintercept=median, color=as.factor(Sex)), linetype="dashed") + 
  facet_grid(rows=vars(eth_new), cols=vars(SES), scales="free") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) +
  xlim(0,2000) + xlab("total kcal per day from HFSS category")

ggsave("Data/NDNS_outputs/plot_distribution_HFSS_kcal_SES.png", dpi=300, width=33,
       height=19, units="cm")

HFSS <- HFSS %>% group_by(Sex, agecat, SES) %>% 
  mutate(median = median(TotalKcal)) %>% 
  # filter(agecat!="0-17") %>% 
  mutate(SES = factor(SES, 
                      levels=c("low","medium","high")))

ggplot(data=subset(HFSS, HFSS==1), aes(x=TotalKcal)) +
  geom_density(aes(y=..scaled.., color=as.factor(Sex), fill=as.factor(Sex)), alpha=0.6) +
  geom_vline(aes(xintercept=median, color=as.factor(Sex)), linetype="dashed") + 
  facet_grid(rows=vars(agecat), cols=vars(SES), scales="free") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) +
  xlim(0,2000) + xlab("total kcal per day from HFSS category")

ggsave("Data/NDNS_outputs/plot_distribution_HFSS_kcal_age_SES.png", dpi=300, width=33,
       height=19, units="cm")

write.csv(HFSS, "Data/NDNS_outputs/NDNS_HFSS_food_diaries.csv", row.names=F)

individualdata <- HFSS %>% #proportion each individual gets from each category - for initialisation of model 
  filter(HFSS==1) %>% 
  mutate(KcalBin = cut(TotalKcal,
                       breaks=c(-1,200,500,750,1000,7000),
                       labels=c("0-200","201-500","501-750","750-1000","1001+"))) %>% 
  group_by(seriali, serialh, SurveyYear, age, agecat, Sex,ethgrp5, eth_new,nssec8, SES, bmival, HFSS, KcalBin) %>% 
  summarise(TotalKcal = sum(TotalKcal)) %>% ungroup() %>% 
  group_by(seriali, serialh, SurveyYear, age, agecat, Sex, ethgrp5, eth_new, nssec8, SES, bmival, HFSS) %>% 
  mutate(proportion = TotalKcal / sum(TotalKcal)) %>% 
  dplyr::select(-TotalKcal) %>% 
  pivot_wider(names_from=KcalBin, values_from=proportion, names_prefix="history_")


# now get "desire" - the proportion of foods from each category for the household
desire <- HFSS %>% #proportion each individual gets from each category - for initialisation of model 
  filter(HFSS==1) %>% 
  group_by(serialh, seriali, SurveyYear, DayNo, Sex) %>% 
  summarise(TotalKcal = sum(TotalKcal)) %>% ungroup() %>% 
  group_by(serialh, SurveyYear) %>% 
  summarise(TotalKcal_M_desire = mean(TotalKcal),
            TotalKcal_SD_desire = sd(TotalKcal),
            TotalKcal_SD_desire = ifelse(is.na(TotalKcal_SD_desire), 0, TotalKcal_SD_desire))

generate_calories <- function(data){
  history <- rtruncnorm(365,a=0, b=Inf, data$TotalKcal_M_desire, data$TotalKcal_SD_desire)
  newdata <- data.frame(serialh=data$serialh, calories = history)
  return(newdata)
}

history <- desire %>% group_by(serialh) %>% do(generate_calories(.)) %>% 
  mutate(calories = ifelse(calories>1250, 1250, calories))

history$history <- cut(history$calories,
                       breaks=c(0,200,500,750,1000,10000),
                       labels=c("0.200","201.500","501.750","751.1000","1001."))

history <- history %>% 
  group_by(serialh, history) %>% tally() %>% ungroup() %>% 
  group_by(serialh) %>% 
  mutate(proportion=n/sum(n)) %>% 
  dplyr::select(-n) %>% 
  pivot_wider(names_from=history, values_from=proportion)

names(history)[2:6] <- paste0("desire_", names(history[2:6]))

history[is.na(history)] <- 0 

individualdata <- left_join(individualdata, history)

individualdata[is.na(individualdata)] <- 0


# explore how many people have the same desire = history 
test <- individualdata %>% 
  mutate(same = ifelse(`desire_0.200`==`history_0-200`,1,0)) %>% filter(same==0)


# distribution of other foods for individuals - shape of distribution will be difficult because only 4 days observed for people 
# just assume normal distribution?
otherfoods <- HFSS %>% 
  # filter(HFSS==0) %>% 
  group_by(seriali, DayNo, HFSS, age, Sex, eth_new, ethgrp5, nssec8, SES, bmival) %>% 
  summarise(TotalKcal = sum(TotalKcal)) %>% 
  ungroup() %>%
  group_by(seriali, HFSS, age, Sex, eth_new, ethgrp5, nssec8, SES, bmival) %>%
  summarise(TotalKcal_M = mean(TotalKcal),
            TotalKcal_SD = sd(TotalKcal)) %>% 
  ungroup() %>% 
  dplyr::select(seriali, HFSS, TotalKcal_M,
                TotalKcal_SD) %>% 
  pivot_wider(names_from=HFSS, values_from=c(TotalKcal_M, TotalKcal_SD))

individualdata <- left_join(individualdata, otherfoods) %>% 
  ungroup() %>% 
  dplyr::select(-HFSS)

write.csv(individualdata, "Data/NDNS_outputs/NDNS_schema_HFSS.csv", row.names=F)

# ind <- read.csv("Data/NDNS_outputs/NDNS_schema_HFSS.csv")
# estimate social norms by group
socialnorms <-  HFSS %>% mutate(KcalBin = cut(TotalKcal,
                                     breaks=c(-1,200,500,750,1000,7000),
                                     labels=c("0-200","201-500","501-750","750-1000","1001+")),
                                agecat = cut(age, 
                                    breaks=c(-1,15,34,54,64,100),
                                    labels=c("0-15","16-34","35-44","45-64","65+"))) %>% 
  filter(HFSS==1) %>% 
  group_by(seriali, DayNo, agecat, Sex, SES, KcalBin) %>% 
  summarise(Meancalsperday = mean(TotalKcal)) %>% 
  group_by(agecat, Sex, SES, KcalBin) %>% 
  summarise(TotalKcal = sum(Meancalsperday)) %>% ungroup() %>% 
  group_by(agecat, SES, Sex) %>% 
  mutate(proportion = round(TotalKcal / sum(TotalKcal), digits=2)) %>% 
  dplyr::select(-TotalKcal)

# age and sex is important - lots of variation there
ggplot(data=socialnorms, aes(x=SES, y=proportion, fill=Sex)) + geom_bar(stat="identity",position="dodge") +
  facet_grid(cols=vars(KcalBin), rows=vars(agecat))

socialnorms <- socialnorms %>% pivot_wider(names_from=KcalBin, values_from=proportion) %>% 
  pivot_wider(names_from=Sex, values_from=c(`0-200`:`1001+`))

write.csv(socialnorms, "Data/NDNS_outputs/NDNS_descriptive_norms.csv", row.names=F)

# target data - mean calories per day from HFSS foods
targets <- data %>% 
  group_by(seriali, DayNo, SurveyYear, Sex, SES) %>% 
  mutate(HFSS = ifelse(description=="biscuits, chocolate, crisps, puddings", 1,0)) %>% 
  filter(HFSS == 1) %>% 
  summarise(Kcalperday = sum(Energykcal)) %>% ungroup() %>% 
  group_by(seriali, SurveyYear, Sex, SES) %>% 
  summarise(Kcalperday = mean(Kcalperday)) %>% ungroup() %>% 
  group_by(SurveyYear, Sex, SES) %>% 
  summarise(original = mean(Kcalperday)) %>% 
  mutate(policyeffect = ifelse(SurveyYear==7, 
                               original - (original*0.067),
                               original)) %>% 
  pivot_longer(cols=original:policyeffect) %>% 
  mutate(SES = factor(SES, levels=c("low","medium","high")),
         Sex = ifelse(Sex=="f","Women","Men"))

ggplot(data=targets, aes(x=SurveyYear, y=value, linetype=name)) + 
  geom_line() + facet_grid(cols=vars(Sex), rows=vars(SES)) + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom") + ylim(0,NA)

ggsave("Data/NDNS_outputs/targets_Sex_SES.png", dpi=300, width=33,
       height=19, units="cm")
