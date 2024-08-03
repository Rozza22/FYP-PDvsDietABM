rm(list = ls()) # clear environment

# setting up files for experiment with social norms mechanisms 
library(dplyr)
library(lhs)
library(truncnorm)
library(yaml)

setwd("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/ABM_code - with PD")

compare <- read.csv("calibration/implausibility.csv")

best <- compare %>% ungroup() %>% 
  mutate(rank = ntile(implausibility, nrow(.))) %>% 
  filter(rank<=1)

ids <- best$sampleno

# look at the values with the best implausibility 
lhsSamples <- read.csv("calibration/lhs.csv") %>% filter(SampleNum %in% ids)

# find the original calibration files - to keep the population and seed the same initially 
basepops <- paste0("props/calibration/GLondon_basepop_", ids, ".csv")
props <- paste0("props/calibration/model_", ids, ".yaml")

basepops <- lapply(basepops, read.csv)
props <- lapply(props, read_yaml)

interventionIterations <- 10

basepops <- rep(basepops, times = interventionIterations+1)
props <- rep(props, times = interventionIterations+1)

BETA_INTERVENTION <- seq(0,1,by=1/interventionIterations) # varying effectiveness of intervention

n_samples <- length(BETA_INTERVENTION)
names <- paste0("GLondon_basepop_", 1:n_samples, ".csv")

propslist <- props
for(i in 1:n_samples){
  # propslist[[i]] <- props
  propslist[[i]]$beta.intervention <- BETA_INTERVENTION[i] # added in
  propslist[[i]]$file.rank0 <- paste0("./props/interventionVariation/",names[i])
}

# n_samples <- length(props)
# n_samples <- as.numeric(length(table(props)))

# alter the props to run the model until 2018
# dir.create("props/validation")
for(i in 1:n_samples){
  propslist[[i]]$stop.at <- 3650
  write_yaml(propslist[[i]], paste0("props/interventionVariation/model_",i,".yaml"))
  write.csv(basepops[[i]], paste0("props/interventionVariation/GLondon_basepop_1000_",i,".csv"), row.names=F)
}
