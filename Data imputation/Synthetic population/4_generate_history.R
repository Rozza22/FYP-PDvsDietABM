# checking that the history generator works 
rm(list = ls()) # clear environment
library(haven)
library(tidyverse)
# wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
# data <- ("Data/NDNS/Individual Data/")

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")
# data <- ("Data imputation/Data/HSE/Individual Data/")

setwd(wd)

pop <- read.csv("Data imputation/Data/NDNS/GLondon_basepop_1000.csv")

ranges <- data.frame(schema = c(1:5),
                     lower = c(0, 201, 501, 751, 1001),
                     upper = c(200, 500, 750, 1000, 1500))

historygenerator <- function(data){
  proportions <- as.numeric(data %>% dplyr::select(history_0.200:history_1001.))
  history <- sample(1:5, 365, replace=T, prob=proportions)
  newdata <- data.frame(ID=data$ID, sex = data$Sex, schema=history)
  newdata <- left_join(newdata, ranges, by=c("schema"))
  return(newdata)
}

history <- pop %>% group_by(ID) %>% do(historygenerator(.))

sample_fun <- function(row){
  sample <- runif(1, min=as.numeric(row["lower"]), max=as.numeric(row["upper"]))
  return(sample)
}

# apply the function to each row of the data frame
history$calories <- apply(history, MARGIN=1, FUN=sample_fun)

means <- history %>% group_by(sex) %>% summarise(mean=mean(calories))

othermeans <- pop %>% group_by(Sex) %>% summarise(mean=mean(TotalKcal_M_1))

alternative_generator <- function(data){
  history <- rtruncnorm(365,a=0, b=Inf, data$TotalKcal_M_1, data$TotalKcal_SD_1)
  newdata <- data.frame(ID=data$ID, sex=data$Sex, calories = history)
  return(newdata)
}

history <- pop %>% group_by(ID) %>% do(alternative_generator(.)) %>% 
  mutate(calories = ifelse(calories>1250, 1250, calories))

alternativemethod <- history %>% group_by(sex) %>% summarise(mean=mean(calories))

# now categorise the alternative method output into schemas 

history$history <- cut(history$calories,
                         breaks=c(0,200,500,750,1000,10000),
                         labels=c("0.200","201.500","501.750","751.1000","1001."))

history <- history %>% 
  group_by(ID, history) %>% tally() %>% ungroup() %>% 
  group_by(ID) %>% 
  mutate(proportion=n/sum(n)) %>% 
  dplyr::select(-n) %>% 
  pivot_wider(names_from=history, values_from=proportion)

names(history)[2:6] <- paste0("history_", names(history[2:6]))

history[is.na(history)] <- 0 

summary(history)


# cor.test(pop$history_0.200, pop$newhistory_0_200)
# cor.test(pop$history_201.500, pop$newhistory_201_500)
# cor.test(pop$history_501.750, pop$newhistory_501_750)
# cor.test(pop$history_750.1000, pop$newhistory_751_1000)
# cor.test(pop$history_1001., pop$newhistory_1001.)


pop <- pop %>% dplyr::select(-c("history_0.200":"history_1001."))
pop <- left_join(pop, history)

# adjust ghq12scr/PD to fit our needs
# standardise it 
pop <- pop %>% relocate("ghq12scr", .after = "history_1001.") # change column order
names(pop)[names(pop) == "ghq12scr"] <- "PD"
pop$PD <- pop$PD/12 # normalise here and it will always be 12
pop$bmival <- pop$bmival # this will be normalised in the model (specifically model.py)

write.csv(pop, "Data imputation/Data/NDNS/GLondon_basepop_1000.csv",row.names=F)

write.csv(pop, "ABM_code - with PD/props/GLondon_basepop_1000_trial.csv",row.names=F)
