# setting up files for calibration for PHASE project 
library(dplyr)
library(lhs)
library(truncnorm)
library(yaml)
library(tidyverse)

setwd("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/ABM_code - with PD")

targets <- read.csv("calibration/Target_data_HFSS_sex.csv") %>% 
  mutate(upper = meanHFSS + (seHFSS*1.96),
         lower = meanHFSS - (seHFSS*1.96))
# read in the output data files 
names <- (Sys.glob("outputs/validation/*validation*.csv"))
files <- lapply(names, read.csv)

names <- gsub("outputs/validation/annual_data_props_valiation_model_", "", names)
names <- gsub("outputs/_yaml_0", "", names)
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

files <- lapply(files, postprocess)

files <- do.call(rbind,files)
files$Sex <- ifelse(files$Sex=="m","Men","Women")
targets$Sex <- ifelse(targets$Sex=="m","Men","Women")


files <- left_join(files,targets)


ggplot(data=files, aes(x=SurveyYear, y=meanHFSSsim, colour=as.factor(sampleno))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=SurveyYear, y=meanHFSS), colour="black", linewidth=1.5) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), colour=NA, fill="grey", alpha=0.3) + 
  facet_grid(rows=vars(Sex)) + ylim(0,NA) + 
  theme_bw() + 
  theme(legend.position="none",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) + ylab("Mean Kcal per day HFSS") +
  geom_vline(xintercept=2015) + 
  xlab("Year")

ggsave("calibration/plots/PHASE_full_validation.png", dpi=300, width=33, height=19,
       units="cm")

samplenum = files$sampleno
compare <- files %>% 
  mutate(abserror = abs(meanHFSSsim - meanHFSS),
         denoms = sqrt(seHFSS^2),
         implausibility = abserror/denoms,
         samplenum = files$sampleno) %>% # issue with use of this function, it was "parse_number(sampleno)"
  group_by(samplenum) %>% 
  summarise(implausibility = max(implausibility))

write.csv(compare, "calibration/implausibility.csv", row.names=F)

best <- compare %>% ungroup() %>% 
  mutate(rank = ntile(implausibility, nrow(.))) %>% 
  filter(rank<=2) # was 10 but for testing have reduce it to this otherwise would include all of them

ids <- best$samplenum

# look at the values with the best implausibility 
lhs <- read.csv("calibration/lhs.csv") %>% filter(SampleNum %in% ids)

# now plot those two best runs 

topruns <- files %>% mutate(sampleno=samplenum) %>% 
                              filter(sampleno %in% ids)

ggplot(data=topruns, aes(x=SurveyYear, y=meanHFSSsim, colour=as.factor(sampleno))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=SurveyYear, y=meanHFSS), colour="black", linewidth=1.5) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), colour=NA, fill="grey", alpha=0.3) + 
  facet_grid(rows=vars(Sex)) + ylim(0,NA) + 
  theme_bw() + 
  theme(legend.position="none",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) + ylab("Mean Kcal per day HFSS") +
  xlab("Year")

ggsave("calibration/plots/PHASE_top10_validation.png", dpi=300, width=33, height=19,
       units="cm")

# This plots our best lhs props combination results
