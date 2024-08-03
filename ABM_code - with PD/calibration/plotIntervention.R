rm(list = ls()) # clear environment

# setting up files for calibration for PHASE project 
library(dplyr)
library(lhs)
library(truncnorm)
library(yaml)
library(tidyverse)
library(patchwork)

setwd("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/ABM_code - with PD")

targets <- read.csv("calibration/Target_data_HFSS_sex.csv") 
targets %>% 
  mutate(upper = meanHFSS + (seHFSS*1.96),
         lower = meanHFSS - (seHFSS*1.96))
# read in the output data files 
# dir.create("outputs/outputs_newcalibration")
# dir.create("outputs/outputs_newcalibration/outputs")
names <- (Sys.glob("outputs/interventionVariation/annual_data_props_interventionVariation*.csv")) # reads in files that start with calibration and end with .csv
# names <- (Sys.glob("outputs/calibration/yaml_2*.csv"))
# names <- names[1:100]
files <- lapply(names, read.csv)

names <- gsub("/outputs/interventionVariation/annual_data_props_interventionVariation_model_", "", names)
# names <- gsub("_yaml_0", "", names)
names <- parse_number(names)

names(files) <- names

for(i in 1:length(files)){
  files[[i]]$sampleno <- names[i]
}

postprocess <- function(data){
  data <- data %>% 
    mutate(meanHFSS_m = HFSSQuantMale/Male,
           meanHFSS_f = HFSSQuantFemale/Female,
           meanHFSS_low_male = HFSSQuantSESLowMale/SESlowMale,
           meanHFSS_medium_male = HFSSQuantSESMedMale/SESmedMale,
           meanHFSS_high_male = HFSSQuantSESHighMale/SEShighMale,
           meanHFSS_low_female = HFSSQuantSESLowFemale/SESlowFemale,
           meanHFSS_medium_female = HFSSQuantSESMedFemale/SESmedFemale,
           meanHFSS_high_female = HFSSQuantSESHighFemale/SEShighFemale,
           
           SurveyYear = tick/365 + 2008) %>% 
    # dplyr::select(SurveyYear, sampleno, meanHFSS_low_male, meanHFSS_medium_male, meanHFSS_high_male,
    #               meanHFSS_low_female, meanHFSS_medium_female, meanHFSS_high_female) %>% 
    dplyr::select(SurveyYear, sampleno, meanHFSS_m, meanHFSS_f) %>% 
    pivot_longer(meanHFSS_m:meanHFSS_f) %>% 
    separate(name, into=c("meanHFSSsim","Sex")) %>% 
    dplyr::select(-meanHFSSsim) %>% 
    rename(meanHFSSsim=value)
  return(data)
}

postprocess_schema <- function(data){
  data <- data %>% 
    mutate(schema1_male = countschema1Male/Male/365,
           schema2_male = countschema2Male/Male/365,
           schema3_male = countschema3Male/Male/365,
           schema4_male = countschema4Male/Male/365,
           schema5_male = countschema5Male/Male/365,
           schema1_female = countschema1Female/Female/365,
           schema2_female = countschema2Female/Female/365,
           schema3_female = countschema3Female/Female/365,
           schema4_female = countschema4Female/Female/365,
           schema5_female = countschema5Female/Female/365,
           SurveyYear = tick/365 + 2008) %>% 
    # dplyr::select(SurveyYear, sampleno, meanHFSS_low_male, meanHFSS_medium_male, meanHFSS_high_male,
    #               meanHFSS_low_female, meanHFSS_medium_female, meanHFSS_high_female) %>% 
    dplyr::select(SurveyYear, sampleno, schema1_male, schema2_male, schema3_male, schema4_male,
                  schema5_male, schema1_female, schema2_female, schema3_female, schema4_female,
                  schema5_female) %>% 
    pivot_longer(schema1_male: schema5_female) %>% 
    separate(name, into=c("schema","Sex"))
  # dplyr::select(-meanHFSSsim) %>% 
  # rename(meanHFSSsim=value)
  return(data)
}


files <- lapply(files, postprocess)
# files <- lapply(files, postprocess_schema)

files <- do.call(rbind,files)
files$Sex <- ifelse(files$Sex=="m","Men","Women")
targets$Sex <- ifelse(targets$Sex=="m","Men","Women")


files <- left_join(files,targets)

# 
# ggplot(data=files, aes(x=SurveyYear, y=value, colour=as.factor(sampleno))) + 
#   geom_line(linewidth=1) + 
#   facet_grid(rows=vars(Sex), cols=vars(schema)) + 
#   ylim(0,NA) + 
#   theme_bw() + 
#   theme(legend.position="none",
#         legend.title=element_blank(),
#         strip.background = element_rect(fill="white"),
#         text = element_text(size=18)) + ylab("Proportion of calories from schema") +
#   xlab("Year")
# 
# ggsave("calibration/plots/PHASE_full_calibration_schemaview.png", dpi=300, width=33, height=19,
#        units="cm")

upper = quantile(files$meanHFSSsim, probs = 0.75)
lower = quantile(files$meanHFSSsim, probs = 0.25)

ggplot(data=files, aes(x=SurveyYear, y=meanHFSSsim, colour=as.factor(sampleno))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=SurveyYear, y=meanHFSS), colour="black", linewidth=1.5) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), colour=NA, fill="grey", alpha=0.3) +
  ggtitle("HFSS consumption with varied interventions") +
  facet_grid(rows=vars(Sex)) + ylim(0,NA) + 
  theme_bw() + 
  scale_color_discrete(name="Sample Number") +
  theme(#legend.position="none",
    legend.title = element_text(size = 18), # legend.title=element_blank(),
    strip.background = element_rect(fill="white"),
    text = element_text(size=18)) + 
  ylab("Mean Kcal per day HFSS") + xlab("Year")



ggsave("calibration/plots/PHASE_full_interventionVariation.png", dpi=300, width=33, height=19,
       units="cm")

compare <- files %>% 
  mutate(abserror = abs(meanHFSSsim - meanHFSS),
         denoms = sqrt(seHFSS^2),
         implausibility = abserror/denoms) %>% 
  group_by(sampleno) %>% 
  summarise(implausibility = max(implausibility))

write.csv(compare, "calibration/implausibility.csv", row.names=F)

compare <- read.csv("calibration/implausibility.csv")

best <- compare %>% ungroup() %>% 
  mutate(rank = ntile(implausibility, nrow(.))) %>% 
  filter(rank<=1) # originally set to 5, this just selects the best results from the generated data. The least implausible is the best

ids <- best$sampleno

# look at the values with the best implausibility 
lhs <- read.csv("calibration/lhs.csv") %>% filter(SampleNum %in% ids)

# now plot those best runs 

topruns <- files %>% 
  filter(sampleno %in% ids)

error <- topruns %>% mutate(error = abs(meanHFSSsim-meanHFSS)/meanHFSS) %>% 
  group_by(sampleno) %>% summarise(mean=mean(error))

# ggplot(data=topruns, aes(x=SurveyYear, y=value, colour=as.factor(sampleno))) + 
#   geom_line(linewidth=1) + 
#   facet_grid(rows=vars(Sex), cols=vars(schema)) + 
#   ylim(0,NA) + 
#   theme_bw() + 
#   theme(legend.position="none",
#         legend.title=element_blank(),
#         strip.background = element_rect(fill="white"),
#         text = element_text(size=18)) + ylab("Proportion of calories from schema") +
#   xlab("Year")
# 
# ggsave("calibration/plots/PHASE_bestruns_schemaview.png", dpi=300, width=33, height=19,
#        units="cm")

upper = quantile(topruns$meanHFSSsim, probs = 0.75)
lower = quantile(topruns$meanHFSSsim, probs = 0.25)

ggplot(data=topruns, aes(x=SurveyYear, y=meanHFSSsim, colour=as.factor(sampleno))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=SurveyYear, y=meanHFSS), colour="black", linewidth=1.5) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), colour=NA, fill="grey", alpha=0.3) + 
  facet_grid(rows=vars(Sex)) + ylim(0,NA) + 
  theme_bw() + 
  scale_color_discrete(name="Sample Number") +
  theme(#legend.position="none",
    legend.title = element_text(size = 18), # legend.title=element_blank(),
    strip.background = element_rect(fill="white"),
    text = element_text(size=18)) + ylab("Mean Kcal per day HFSS") +
  xlab("Year")

ggsave("calibration/plots/PHASE_top10_interventionVariation.png", dpi=300, width=33, height=19,
       units="cm")

