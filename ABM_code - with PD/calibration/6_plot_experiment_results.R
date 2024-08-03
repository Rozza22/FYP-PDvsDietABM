# setting up files for calibration for PHASE project 
library(dplyr)
library(lhs)
library(truncnorm)
library(yaml)
library(tidyverse)

# Have to run the model again using .yaml files generated in last step (5)

setwd("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/ABM_code - with PD")

targets <- read.csv("calibration/Target_data_HFSS_sex.csv") %>% 
  mutate(upper = meanHFSS + (seHFSS*1.96),
         lower = meanHFSS - (seHFSS*1.96))
# read in the output data files 
names <- (Sys.glob("outputs/*experiment*.csv"))
files <- lapply(names, read.csv)

names <- gsub("experimentoutputs_7June/annual_data_props_experiments_experimentmodel_", "", names)
names <- gsub("outputs/_yaml_1", "", names)
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
    # dplyr::select(SurveyYear, sampleno, meanHFSS_m, meanHFSS_f) %>% 
    dplyr::select(SurveyYear, sampleno, meanHFSS_low_male, meanHFSS_medium_male,
                  meanHFSS_high_male, meanHFSS_low_female, meanHFSS_medium_female,
                  meanHFSS_high_female) %>% 
    pivot_longer(meanHFSS_low_male:meanHFSS_high_female) %>% 
    separate(name, into=c("meanHFSSsim","SES","Sex")) %>% 
    dplyr::select(-meanHFSSsim) %>% 
    rename(meanHFSSsim=value)
  return(data)
}

files <- lapply(files, postprocess)

files <- do.call(rbind,files)
files$Sex <- ifelse(files$Sex=="male","Men","Women")
targets$Sex <- ifelse(targets$Sex=="m","Men","Women")


# files <- left_join(files,targets)

normvalues <- read.csv("calibration/normsexperimentvalues.csv")

files <- left_join(files, normvalues)

files$normvalues <- round(files$normvalues, digits=2)

compare <- read.csv("calibration/implausibility.csv")

best <- compare %>% ungroup() %>% 
  mutate(rank = ntile(implausibility, nrow(.))) %>% 
  filter(rank<=5)

ids <- best$samplenum

topruns <- files %>% filter(modelids %in% ids) %>% 
  mutate(modelids = recode(modelids, "157"="Model 1",
                           "405"="Model 2", "717"="Model 3",
                           "798"="Model 4", "889"="Model 5")) %>% 
  filter(normvalues==0.01 | normvalues==0.25 | normvalues==0.5 |
           normvalues==0.75 | normvalues==0.95) %>%
  filter(modelids=="Model 1") %>% distinct() %>% 
  mutate(SES = ifelse(SES=="low","Low SES",
                      ifelse(SES=="medium", "Medium SES",
                             "High SES")),
         SES = factor(SES, levels=c("Low SES","Medium SES","High SES")))

ggplot(data=topruns, aes(x=SurveyYear, y=meanHFSSsim, colour=as.factor(normvalues))) + 
  geom_line(linewidth=1) + 
  facet_grid(rows=vars(Sex), cols=vars(SES), scales="free") + ylim(0,NA) + 
  theme_bw() + guides(colour=guide_legend(title="Percentage norm reduction")) + 
  theme(legend.position = "bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        panel.spacing = unit(1.5, "lines")) + ylab("Mean HFSS Kcal per day") +
  geom_vline(xintercept=2018, linetype="dashed") + 
  xlab("Year") + xlim(2017,2020)

ggplot(data=subset(topruns, SES=="High SES"), aes(x=SurveyYear, y=meanHFSSsim, colour=as.factor(normvalues))) + 
  geom_line(linewidth=1) + 
  facet_grid(cols=vars(Sex), scales="free") + ylim(0,NA) + 
  theme_bw() + guides(colour=guide_legend(title="Percentage norm reduction")) + 
  theme(legend.position = "bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        panel.spacing = unit(1.5, "lines")) + ylab("Mean HFSS Kcal per day") +
  geom_vline(xintercept=2018, linetype="dashed") + 
  xlab("Year") + xlim(2017,2020)

ggsave("calibration/plots/PHASE_experiments_norms_SES.png", dpi=500, width=40, height=20,
       units="cm")
ggsave("calibration/plots/PHASE_experiments_norms_SES_example.png", dpi=500, width=40, height=20,
       units="cm")

# work out the percentage reductions between 2018 and 2019 for all versions 
compare <- files %>% filter(SurveyYear>=2018) %>% 
  dplyr::select(SurveyYear, sampleno, modelids, normvalues, Sex, SES, meanHFSSsim) %>% 
  distinct() %>% 
  pivot_wider(names_from=SurveyYear, values_from=meanHFSSsim) %>% 
  mutate(pct_2018_2019 = (abs(`2018`-`2019`)/`2018`)*100)

bestfitting <- compare %>% 
  # filter(pct_2018_2019>=6 & pct_2018_2019<=7) %>% 
  pivot_longer(`2018`:`2020`) %>% filter(modelids==157) %>% 
  filter(normvalues==0.65)




compare <- files %>% 
  mutate(abserror = abs(meanHFSSsim - meanHFSS),
         denoms = sqrt(seHFSS^2),
         implausibility = abserror/denoms,
         samplenum = parse_number(sampleno)) %>% 
  group_by(samplenum) %>% 
  summarise(implausibility = max(implausibility))

write.csv(compare, "calibration/implausibility.csv", row.names=F)

best <- compare %>% ungroup() %>% 
  mutate(rank = ntile(implausibility, nrow(.))) %>% 
  filter(rank<=10)

ids <- best$samplenum

# look at the values with the best implausibility 
lhs <- read.csv("calibration/lhs.csv") %>% filter(SampleNum %in% ids)

# now plot those two best runs 

topruns <- files %>% mutate(sampleno=parse_number(sampleno)) %>% 
                              filter(sampleno %in% ids) %>% 
  mutate()

ggplot(data=topruns, aes(x=SurveyYear, y=meanHFSSsim, colour=as.factor(sampleno))) + 
  geom_line(linewidth=1) + 
  # geom_line(aes(x=SurveyYear, y=meanHFSS), colour="black", linewidth=1.5) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), colour=NA, fill="grey", alpha=0.3) + 
  facet_grid(rows=vars(Sex)) + ylim(0,NA) + 
  theme_bw() + 
  theme(legend.position="none",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) + ylab("Mean Kcal per day HFSS") +
  xlab("Year")

ggsave("calibration/plots/PHASE_top10_calibration.png", dpi=300, width=33, height=19,
       units="cm")

