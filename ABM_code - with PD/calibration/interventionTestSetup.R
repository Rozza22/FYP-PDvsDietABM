rm(list = ls()) # clear environment

# setting up files for calibration for PHASE project 
library(dplyr)
library(lhs)
library(truncnorm)
library(yaml)

setwd("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/ABM_code - with PD")

source("calibration/0_sample_lhs_PHASE.R")

# read in base population and read in the 
basepop <- read.csv("props/GLondon_basepop_1000.csv")

basepop$habit.update.interval <- NULL
basepop$autonomy <- NULL

# sample latin hypercube samples
n_samples <- 10
lhsSamples <- sample_lhs(n_samples)

write.csv(lhsSamples, "calibration/lhs.csv", row.names=F)

basepoplist <- list()

for(i in 1:n_samples){
  basepoplist[[i]] <- basepop %>% 
    group_by(Sex) %>% 
    mutate(autonomy = ifelse(Sex=="m", rbeta(nrow(.), lhsSamples[i,]$BAUTONOMY1_SEXM,
                                             lhsSamples[i,]$BAUTONOMY2_SEXM),
                             rbeta(nrow(.), lhsSamples[i,]$BAUTONOMY1_SEXF,
                                   lhsSamples[i,]$BAUTONOMY2_SEXF))) %>% 
    ungroup() %>% 
    mutate(habit.update.interval = round(rtruncnorm(nrow(.), mean = lhsSamples[i,]$HABIT_UPDATE_MEAN,
                                                    sd = lhsSamples[i,]$HABIT_UPDATE_SD,
                                                    a = 18, b=254)))
}

names <- paste0("GLondon_basepop_", 1:n_samples, ".csv")

# now save to props/calibration 
dir.create("props/calibration")
for(i in 1:n_samples){
  write.csv(basepoplist[[i]], paste0("props/interventionVariation/",names[i]))
}

# now write multiple different versions of the props/model.yaml file 
# first read in the file 
props <- read_yaml("props/calibration/best_hyperparameters.yaml")

interventionScale <- seq(0.1, 1, by = 0.1)

propslist <- list()
# now assign different props files 
for(i in 1:n_samples){
  propslist[[i]] <- props
  # propslist[[i]]$beta.interventionEffect <- lhsSamples[i,]$BETA_INTERVENTIONEFFECT # added in
  propslist[[i]]$beta.interventionScale <- interventionScale[i] # added in
  propslist[[i]]$file.rank0 <- paste0("./props/calibration/",names[i])
  # propslist[[i]]$random.seed <- sample(1:100, 1)
  propslist[[i]]$random.seed <- as.integer(123)
  propslist[[i]]$stop.at <- as.integer(3650)
  propslist[[i]]$count.of.agents <- as.integer(1000) # was 1000 initially, changed for testing temporarily
}

# now save the different props files 
namesprops <- paste0("model_", 1:n_samples, ".yaml")

for(i in 1:n_samples){
  write_yaml(propslist[[i]], paste0("props/interventionVariation/",namesprops[i]))
}

for(i in 1:n_samples){
  print(i)
  print(min(basepoplist[[i]]$automaticity))
  print(min(propslist[[i]]$bias.factor))
}
