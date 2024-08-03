# setting up files for calibration for PHASE project 
library(dplyr)
library(lhs)
library(truncnorm)
library(yaml)
library(tidyverse)

setwd("~/Documents/PHASE project/PHASE_code/ABM_code")

compare <- read.csv("calibration/implausibility.csv")

best <- compare %>% ungroup() %>% 
  mutate(rank = ntile(implausibility, nrow(.))) %>% 
  filter(rank<=5)

ids <- best$sampleno

# look at the values with the best implausibility 
lhs <- read.csv("calibration/lhs.csv") %>% filter(SampleNum %in% ids) %>% 
  mutate(SampleNum = recode(SampleNum, "157"="Model 1",
                           "405"="Model 2", "717"="Model 3",
                           "798"="Model 4", "889"="Model 5"))

betas <- lhs %>% 
  dplyr::select(SampleNum, BETA_ATTITUDE, BETA_NORM, BETA_PBC, BETA_RESTRAINT, BETA_EMOTIONAL) %>% 
  pivot_longer(BETA_ATTITUDE:BETA_EMOTIONAL) %>% 
  mutate(name = recode(name, "BETA_ATTITUDE" = "B Attitude", "BETA_NORM"= "B Norm", "BETA_PBC" = "B PBC",
                       "BETA_RESTRAINT" = "B Restrained eating", "BETA_EMOTIONAL"="B Emotional eating"),
         name = factor(name, levels=c("B Attitude","B Norm","B PBC", "B Emotional eating", "B Restrained eating")))

ggplot(data=betas, aes(x=SampleNum, y=value, fill=SampleNum)) + geom_bar(stat="identity", colour="black") + 
  facet_grid(cols=vars(name)) +   theme_bw()  + 
  theme(legend.position = "bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        panel.spacing = unit(1.5, "lines"),
        axis.text.x=element_text(angle=45, vjust = 1, hjust=1)) + ylab("Beta") + xlab("") +
  scale_fill_brewer(palette="Set2")
ggsave("calibration/plots/PHASE_experiments_betas.png", dpi=500, width=40, height=20,
       units="cm")


