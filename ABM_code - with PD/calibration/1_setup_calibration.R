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
n_samples <- 30 # was 1000 initially, changed for testing temporarily
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
dir.create("props/calibration2")
for(i in 1:n_samples){
  write.csv(basepoplist[[i]], paste0("props/calibration2/",names[i]))
}

# now write multiple different versions of the props/model.yaml file 
# first read in the file 
props <- read_yaml("props/model.yaml")

propslist <- list()
# now assign different props files 
for(i in 1:n_samples){
  propslist[[i]] <- props
  propslist[[i]]$norms.n.days.descriptive <- lhsSamples[i,]$DESCRIPTIVE_CALC_N
  propslist[[i]]$transformational.interval.descriptive.norm <- lhsSamples[i,]$DESCRIPTIVE_TRANS_N
  propslist[[i]]$bias.factor <- lhsSamples[i,]$BIAS_FACTOR
  propslist[[i]]$beta.attitude <- lhsSamples[i,]$BETA_ATTITUDE
  propslist[[i]]$beta.norm <- lhsSamples[i,]$BETA_NORM
  propslist[[i]]$beta.pbc <- lhsSamples[i,]$BETA_PBC
  propslist[[i]]$beta.restraint <- lhsSamples[i,]$BETA_RESTRAINT
  propslist[[i]]$beta.emotional <- lhsSamples[i,]$BETA_EMOTIONAL
  propslist[[i]]$beta.PD <- lhsSamples[i,]$BETA_PD # added in
  propslist[[i]]$beta.HFSSbmival <- lhsSamples[i,]$BETA_HFSSBMIVAL # added in
  propslist[[i]]$beta.bmivalPD <- lhsSamples[i,]$BETA_BMIVALPD # added in
  propslist[[i]]$beta.bmivalEmo <- lhsSamples[i,]$BETA_BMIVALEMO # added in
  propslist[[i]]$beta.interventionEffect <- lhsSamples[i,]$BETA_INTERVENTIONEFFECT # added in
  propslist[[i]]$beta.interventionScale <- lhsSamples[i,]$BETA_INTERVENTIONSCALE # added in
  propslist[[i]]$file.rank0 <- paste0("./props/calibration/",names[i])
  # propslist[[i]]$random.seed <- sample(1:100, 1)
  propslist[[i]]$random.seed <- as.integer(123)
  propslist[[i]]$stop.at <- as.integer(1825)
  propslist[[i]]$count.of.agents <- as.integer(100) # was 1000 initially, changed for testing temporarily
}

# now save the different props files 
namesprops <- paste0("model_", 1:n_samples, ".yaml")

for(i in 1:n_samples){
  write_yaml(propslist[[i]], paste0("props/calibration2/",namesprops[i]))
}

for(i in 1:n_samples){
  print(i)
  print(min(basepoplist[[i]]$automaticity))
  print(min(propslist[[i]]$bias.factor))
}
