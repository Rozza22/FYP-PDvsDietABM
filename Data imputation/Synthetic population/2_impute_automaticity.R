# impute values of external eating ("automaticity") and restraint ("PBC") and emotional eating 
rm(list = ls()) # clear environment
library(haven)
library(tidyverse)
library(readxl)
library(ipfp)
library(rFSA)
library(mice)

# wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
# data <- ("Data/NDNS/Individual Data/")

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
# data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

GLondon <- read.csv("Data imputation/Data/NDNS/GLondon_basepop_1000.csv")

# read in NDNS 2000 DEBQ data 
DEBQ <- read.csv("Data imputation/Data/NDNS/NDNS_DEBQ.csv") %>% 
  rename(age=respage, Sex=respsex, SES=ses, eth_new=ethnic,
         bmival = bmi) %>% 
  mutate(SES=ifelse(SES=="med","medium",SES),
         agecat = cut(age, 
                      breaks=c(0,24,34,54,64,100)),
         ID=10001:(nrow(.)+10000))

imputing_GLondon <- GLondon %>% 
  mutate(meankcal=TotalKcal_M_0 + TotalKcal_M_1) %>% 
  dplyr::select(ID, age, Sex, SES, eth_new, bmival, ghq12scr, meankcal) %>% 
  mutate(external=NA, restraint=NA, emotional=NA)

imputing_DEBQ <- DEBQ %>% 
  dplyr::select(ID, age, Sex, SES, eth_new, bmival, meankcal, external, restraint, emotional)

# Add ghq12scr into imputing_DEBQ
emptyVector <- rep(NA, nrow(imputing_DEBQ))
ghq12scr <- c(emptyVector)
imputing_DEBQ$ghq12scr <- ghq12scr
imputing_DEBQ <- imputing_DEBQ %>% relocate("ghq12scr", .after = "bmival") # change column order and this puts ghq12scr to column 6


imputing <- rbind(imputing_GLondon, imputing_DEBQ) %>% 
  mutate(Sex = as.factor(Sex), SES = as.factor(SES), eth_new=as.factor(eth_new))

predictors <- quickpred(imputing, exclude="ID")
predictors[2:10,2:10] <- 1

model <- mice(imputing, m=1, predictorMatrix = predictors, 
              method=c("","","","","","","pmm","","pmm","pmm","pmm"))

summary(model)
imputed <- complete(model) %>% dplyr::select(ID, external, restraint, emotional)

GLondon <- left_join(GLondon, imputed)

normalise <- function(x){
  x <- 0.1 + 0.8*(x-min(x))/(max(x)-min(x))
  return(x)
}


GLondon$automaticity <- normalise(GLondon$external)
GLondon$restraint <- normalise(GLondon$restraint)
GLondon$emotional <- normalise(GLondon$emotional)

GLondon$pbc <- GLondon$restraint
GLondon <- GLondon %>% relocate("pbc", .after = "automaticity") # change column order and this puts ghq12scr to column 6


summary(GLondon$automaticity)
summary(GLondon$pbc)

ggplot(data=GLondon, aes(x=automaticity, colour=Sex)) + geom_density() +
  facet_grid(cols=vars(SES))

ggplot(data=GLondon, aes(x=pbc, colour=Sex)) + geom_density() +
  facet_grid(cols=vars(SES))


# save GLondon with automaticity 
write.csv(GLondon, "Data imputation/Data/NDNS/GLondon_basepop_imputedvars.csv", row.names=F)

