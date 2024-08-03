# setting up files for experiment with social norms mechanisms 
library(dplyr)
library(lhs)
library(truncnorm)
library(yaml)
library(stringr)

setwd("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/ABM_code - with PD")

# pick best fitting model and read in those props 
implausibility <- read.csv("calibration/implausibility.csv")

best <- implausibility %>% ungroup() %>% 
  mutate(rank = ntile(implausibility, nrow(.))) %>% 
  filter(rank<=1)

ids <- best$samplenum

# look at the values with the best implausibility 
lhsSamples <- read.csv("calibration/lhs.csv") %>% filter(SampleNum %in% ids)

# find the original calibration files - to keep the population and seed the same initially 
basepops <- paste0("props/calibration/GLondon_basepop_", ids, ".csv")
props <- paste0("props/calibration/model_", ids, ".yaml")

basepops <- lapply(basepops, read.csv)
props <- lapply(props, read_yaml)

# basepops <- read.csv("props/GLondon_basepop_1000.csv")
# props <- read_yaml("props/model.yaml") # %>% filter(SampleNum %in% ids)

# splitProps <- separate(props, X....Properties.in.yaml.format, into = c("variable name", "value"), sep = ": ")

normvalues <- seq(0,1, by=0.1) # initially by=0.05

# normvalues <- runif(20, min=0, max=1)
normvalues

propslist <- list()

normprops <- expand.grid(normvalues=normvalues, modelids=ids)

names(props) <- ids
names(basepops) <- ids

# now write multiple different versions of the props/model.yaml file 
# first read in the file 
# now assign different props files 
for(i in 1:nrow(normprops)){
  modelid <- as.numeric(normprops$modelids[i])
  propslist[[i]] <- props[[paste(modelid)]]
  propslist[[i]]$advertising.policy.enabled <- TRUE
  propslist[[i]]$advertising.policy.scaling.factor <- normprops$normvalues[i]
  propslist[[i]]$stop.at <- as.integer(4380)
  propslist[[i]]$advertising.policy.scaling.enable.at.tick <- as.integer(3650)
}

normsexperiment <- normprops 
normsexperiment$sampleno <- 1:nrow(normprops)

write.csv(normsexperiment, "calibration/normsexperimentvalues.csv", row.names=F)

# now save the different props files 
namesprops <- paste0("model_", 1:length(normvalues), "experiments.yaml")

dir.create("props/experiments")
for(i in 1:nrow(normsexperiment)){
  write_yaml(propslist[[i]], paste0("props/experiments/","experimentmodel_", normsexperiment$sampleno[i], ".yaml"))
}

# for(i in 1:length(basepops)){
#   id <- names(basepops)[[i]]
#   write.csv(basepops[[i]], paste0("props/experiments/GLondon_basepop_1000_", id, ".csv"), row.names=F)
#   
# }
