# impute values of external eating ("automaticity") and restraint ("PBC") and emotional eating 

rm(list = ls()) # clear environment
library(haven)
library(tidyverse)
library(readxl)
library(ipfp)
library(rFSA)
library(mice)
library(splitstackshape)
library(truncnorm)
library(nnet)

# wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
# data <- ("Data/NDNS/Individual Data/")

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
# data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

GLondon <- read.csv("Data imputation/Data/NDNS/GLondon_basepop_imputedvars.csv")

formodel <- GLondon %>% pivot_longer(desire_0.200:desire_1001.) %>% 
  mutate(value=round(value*10)) %>% expandRows(., "value")
  

# now specify a multinomial logistic regression
# model <- multinom(name ~ age + Sex + SES + eth_new + bmival + TotalKcal_M_0 + TotalKcal_M_1 +
#                     age*Sex*SES*bmival,
#                   data = formodel)
# summary(model)
# 
# nnet.mod.loglik <- nnet:::logLik.multinom(model)
# nnet.mod0 <- multinom(name ~ 1, data=formodel)
# nnet.mod0.loglik <- nnet:::logLik.multinom(nnet.mod0)
# (nnet.mod.mfr2 <- as.numeric(1 - nnet.mod.loglik/nnet.mod0.loglik))
# 
# predicted <- data.frame(predict(model, formodel, "probs"))
# predicted$ID <- formodel$ID
# predicted <- distinct(predicted)
# names(predicted)[1:5] <- paste0("imputed_", names(predicted)[1:5])
# 
# GLondon <- left_join(GLondon, predicted)
# 
# cor.test(GLondon$desire_0.200, GLondon$imputed_desire_0.200)
# cor.test(GLondon$desire_201.500, GLondon$imputed_desire_201.500)
# cor.test(GLondon$desire_501.750, GLondon$imputed_desire_501.750)
# cor.test(GLondon$desire_750.1000, GLondon$imputed_desire_750.1000)
# cor.test(GLondon$desire_1001., GLondon$imputed_desire_1001.)
# 
# cor.test(GLondon$history_0.200, GLondon$imputed_desire_0.200)
# cor.test(GLondon$history_201.500, GLondon$imputed_desire_201.500)
# cor.test(GLondon$history_501.750, GLondon$imputed_desire_501.750)
# cor.test(GLondon$history_750.1000, GLondon$imputed_desire_750.1000)
# cor.test(GLondon$history_1001., GLondon$imputed_desire_1001.)
# 
# GLondon <- GLondon %>% dplyr::select(-c(desire_0.200:desire_1001.))
# names(GLondon)[24:28] <- gsub("imputed_", "", names(GLondon)[24:28])

# GLondon <- GLondon 
# %>% 
#   rename(TotalKcal_M_NonHFSS = TotalKcal_M_0,
#          TotalKcal_SD_NonHFSS = TotalKcal_SD_0,
#          TotalKcal_M_HFSS = TotalKcal_M_1, 
#          TotalKcal_SD_HFSS = TotalKcal_SD_1)

GLondon$microsim.init.eatingstatus <- 1
GLondon$habit.update.interval <- round(rtruncnorm(nrow(GLondon), mean=99.5, sd=44.2,
                                            a=30, b=Inf),digits=0)

GLondon[is.na(GLondon)] <- 0

GLondon <- GLondon %>% 
  group_by(Sex) %>% 
  mutate(autonomy = ifelse(Sex=="f", rbeta(nrow(.), 1.37, 4.94),
                           rbeta(nrow(.), 3.03, 0.67)))

# save GLondon with automaticity 
write.csv(GLondon, "Data imputation/Data/NDNS/GLondon_basepop_1000.csv", row.names=F)

GLondon %>% group_by(Sex) %>% 
  summarise(mean=mean(TotalKcal_M_1))

cor.test(GLondon$history_0.200, GLondon$desire_0.200)

cor.test(GLondon$history_201.500, GLondon$desire_201.500)

cor.test(GLondon$history_501.750, GLondon$desire_501.750)

cor.test(GLondon$history_750.1000, GLondon$desire_751.1000)
cor.test(GLondon$history_1001., GLondon$desire_1001.)




            