# exploring nutrient profile models 

rm(list = ls()) # clear environment

library(haven)
# library(tidyverse)
library(dplyr)
library(readxl)
library(ipfp)
library(readr)
library(tidyr)

# wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
setwd(wd)

data <- ("Data imputation/Data/NDNS/UKDA-6533-stata/stata/stata13_se/")

selectvars <- function(data){
  data <- data %>% 
    mutate(Age = ifelse(SurveyYear=="NDNS Year 10" | SurveyYear=="NDNS Year 11" |
                          SurveyYear=="NDNS Year 9", AgeR, Age)) %>% 
    dplyr::select(SurveyYear, DayNo, seriali, Age, MealTimeDescription, SubFoodGroupDesc, SubFoodGroupCode, MainFoodGroupDesc, FoodName, FoodNumber, Energykcal,
                  TotalGrams, WhoWith, Where, WatchingTV, Table)
  return(data)
}

files <- (Sys.glob(paste0(data, "*foodleveldietarydata*.dta")))
profilefiles <- (Sys.glob(paste0(data, "*nutrientdatabank*.dta")))
files
list <- lapply(files, function(x) read_dta(x)) 

for(i in 1:length(list)){
  list[[i]] <- selectvars(list[[i]])
}

data <- do.call(rbind, list)
rm(list)
gc()
nutrientprofile <- lapply(profilefiles, function(x) read_dta(x)) 
profilefiles
names <- c(10,11,1:9)

for(i in 1:length(nutrientprofile)){
  nutrientprofile[[i]]$SurveyYear <- names[i]
  nutrientprofile[[i]] <- nutrientprofile[[i]] %>% 
    mutate(ENGFIB = ifelse(SurveyYear>=10, NA, ENGFIB),
           FruitVegNut = Fruit + DriedFruit + FruitJuice + SmoothieFruit + Tomatoes + TomatoPuree +
             Brassicaceae + YellowRedGreen + Beans + Nuts + OtherVeg) %>% 
    dplyr::select(SurveyYear, FoodName, FoodNumber, WATER, STAR, KCALS, KJ, PROT, FAT, CHO, CHOL, TOTSUG,
                  GLUC, FRUCT, MALT, LACT, SUCR, OSUG, CMON, TRANS,
                  SATFA, `NA`, ENGFIB, FruitJuice, FruitVegNut)
}

nutrientprofile <- do.call(rbind, nutrientprofile)

# investigate similarity of nutrient profiles over years 
similarity <- nutrientprofile %>% 
  group_by(FoodName) %>% 
  summarise(diffKcal=abs(min(KCALS)-max(KCALS)),
            diffprot = abs(min(PROT)-max(PROT)),
            diffsug = abs(min(TOTSUG)-max(TOTSUG)),
            diffsatfat = abs(min(SATFA)-max(SATFA)),
            diffsod = abs(min(`NA`)-max(`NA`)),
            difffibre = abs(min(ENGFIB,na.rm=T)-max(ENGFIB, na.rm=T)))
# some differences - use the year specific data 

# bind with the food level diary data 
data$SurveyYear <- parse_number(data$SurveyYear)
data <- left_join(data, nutrientprofile)

# fill in any missing nutrients from other nutrient profile years 
# remove any non food or drink items
# classify whether items are food or drink 
data <- data %>% group_by(FoodName) %>% 
  fill(c(TOTSUG, SATFA,ENGFIB, `NA`, FruitJuice, FruitVegNut), .direction=c("downup")) %>% 
  mutate(drop = ifelse(SubFoodGroupCode == "54A" | SubFoodGroupCode == "54B" |SubFoodGroupCode == "54C" |
                                SubFoodGroupCode == "54D"  |SubFoodGroupCode == "54E"  |SubFoodGroupCode == "54F"  |
                                SubFoodGroupCode == "54G"  |SubFoodGroupCode == "54H"  |SubFoodGroupCode == "54I"  |
                                SubFoodGroupCode == "54J"  |SubFoodGroupCode == "54K"  |SubFoodGroupCode == "54L"  |
                                SubFoodGroupCode == "54M"  |SubFoodGroupCode == "54N"  |SubFoodGroupCode == "54P"  |
                                SubFoodGroupCode == "54R", 1,0),
         foodordrink = ifelse(SubFoodGroupCode=="10R" | SubFoodGroupCode=="11R" | SubFoodGroupCode=="60R" |
                                SubFoodGroupCode=="12R" | SubFoodGroupCode=="13A" | SubFoodGroupCode=="13B" |
                                SubFoodGroupCode=="13R" | SubFoodGroupCode=="45R" | SubFoodGroupCode=="61R" | 
                                SubFoodGroupCode=="57A" | SubFoodGroupCode=="57B" | SubFoodGroupCode=="57C" | 
                                SubFoodGroupCode=="58A" | SubFoodGroupCode=="58B" | SubFoodGroupCode=="58C" | 
                                SubFoodGroupCode=="51A" | SubFoodGroupCode=="51B" | SubFoodGroupCode=="51C" | 
                                SubFoodGroupCode=="51D" | SubFoodGroupCode=="51R" | SubFoodGroupCode=="47A" | 
                                SubFoodGroupCode=="47B" | SubFoodGroupCode=="48A" | SubFoodGroupCode=="48B" | 
                                SubFoodGroupCode=="48C" | SubFoodGroupCode=="49A" | SubFoodGroupCode=="49B" | 
                                SubFoodGroupCode=="49C" | SubFoodGroupCode=="49D" | SubFoodGroupCode=="49E" | 
                                SubFoodGroupCode=="52A", 0, 1),
         FruitVegNut = ifelse(SubFoodGroupCode=="45R" & FruitJuice<=40, FruitVegNut-FruitJuice,
                              FruitVegNut)) %>% 
  filter(drop==0) %>% 
  filter(MainFoodGroupDesc!="COMMERCIAL TODDLERS FOODS AND DRINKS" & 
           MainFoodGroupDesc!="BEER LAGER CIDER & PERRY" & 
           MainFoodGroupDesc!="WINE" &
           MainFoodGroupDesc!="SPIRITS AND LIQUEURS" & 
           MainFoodGroupDesc!="TEA COFFEE AND WATER" & 
           MainFoodGroupDesc!="ARTIFICIAL SWEETENERS" & 
           MainFoodGroupDesc!="SOFT DRINKS LOW CALORIE")
  

# calculate nutrient profile score
data <- data %>% 
  mutate(NutProf_A_Energy = as.numeric(cut(KJ,
                                breaks=c(-1,335,670,1005,1340,1675, 2010,2345,2680,3015,3350,Inf),
                                labels=c(0,1,2,3,4,5,6,7,8,9,10))),
         NutProf_A_SatFat = as.numeric(cut(SATFA,
                                breaks=c(-1,1,2,3,4,5,6,7,8,9,10,Inf),
                                labels=c(0,1,2,3,4,5,6,7,8,9,10))),
         NutProf_A_Sug = as.numeric(cut(TOTSUG,
                             breaks=c(-1,4.5,9,13.5,18,22.5,27,31,36,40,45,Inf),
                             labels=c(0,1,2,3,4,5,6,7,8,9,10))),
         NutProf_A_Sod = as.numeric(cut(`NA`,
                             breaks=c(-1,90,180,270,360,450,540,630,720,810,900,Inf),
                             labels=c(0,1,2,3,4,5,6,7,8,9,10))),
         NutProf_C_fruit = ifelse(FruitVegNut<=40, 0,
                                  ifelse(FruitVegNut>40 & FruitVegNut<=60, 1,
                                         ifelse(FruitVegNut>60 & FruitVegNut<=80, 2,
                                                ifelse(FruitVegNut>80, 5, NA)))),
         NutProf_C_fibre = as.numeric(cut(ENGFIB,
                               breaks=c(-1,0.7,1.4,2.1,2.8,3.5,Inf),
                               labels=c(0,1,2,3,4,5))),
         NutProf_C_Prot = as.numeric(cut(PROT,
                              breaks=c(-1,1.6,3.2,4.8,6.4,8.0,Inf),
                              labels=c(0,1,2,3,4,5))),
         Total_A = NutProf_A_Energy + NutProf_A_SatFat + NutProf_A_Sug + NutProf_A_Sod,
         Total_C = NutProf_C_fruit + NutProf_C_fibre + NutProf_C_Prot,
         Total_NP_score = ifelse(Total_A <11,
                                 Total_A - Total_C,
                                 ifelse(Total_A >=11 & NutProf_C_fruit==5, 
                                        Total_A - Total_C, 
                                        Total_A - (NutProf_C_fibre + NutProf_C_fruit))),
         HFSS = ifelse(Total_NP_score>=4 & foodordrink==1, 1,
                       ifelse(Total_NP_score>=1 & foodordrink==0, 1,0)),
         Mealtime = ifelse(MealTimeDescription=="6am to 8:59am", 1,
                           ifelse(MealTimeDescription=="9am to 11:59am", 2,
                                  ifelse(MealTimeDescription=="12 noon to 1:59pm",3,
                                         ifelse(MealTimeDescription=="2pm to 4:59pm", 4,
                                                ifelse(MealTimeDescription=="5pm to 7:59pm", 5,
                                                       ifelse(MealTimeDescription=="8pm to 9:59pm", 6, 7))))))
         ) %>% drop_na(Total_NP_score)



write.csv(data, "Data imputation/Data/NDNS/processed_fooddiary.csv", row.names=F)
# saveRDS(data, file="Data/NDNS/NDNS_HFSS.RDS")
