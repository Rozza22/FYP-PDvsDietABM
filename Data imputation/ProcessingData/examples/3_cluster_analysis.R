# clustering analysis on NDNS data for PHASE project 
rm(list = ls()) # clear environment
library(haven)
library(tidyverse)
library(readxl)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/Data imputation")
setwd(wd)

data <- read.csv("Data/NDNS/processed_fooddiaryNew.csv")

proportion <- data %>% group_by(SurveyYear, HFSS) %>% 
  summarise(totalKcal = sum(Energykcal)) %>% 
  ungroup() %>% 
  group_by(SurveyYear) %>% 
  mutate(propHFSS = totalKcal / sum(totalKcal)) %>% filter(HFSS==1)

ggplot(data=proportion, aes(x=SurveyYear, y=propHFSS)) + geom_line() + ylim(0,NA) + 
  theme_bw()

# perform clustering on non HFSS foods
means <- data %>% ungroup() %>% dplyr::select(MainFoodGroupDesc, Energykcal, TotalGrams, WATER:Mealtime) %>% 
  pivot_longer(cols=Energykcal:Mealtime) %>% 
  group_by(MainFoodGroupDesc, name) %>% 
  summarise(mean=mean(value))

maingroups <- data %>% ungroup() %>% 
  filter(MainFoodGroupDesc!="ARTIFICIAL SWEETENERS") %>% 
  filter(SubFoodGroupDesc!="NUTRITION POWDERS AND DRINKS" & 
           SubFoodGroupDesc!="BEVERAGES DRY WEIGHT") %>% 
  # filter(HFSS==0) %>% 
  dplyr::select(MainFoodGroupDesc, 
                WATER, KCALS, PROT, FAT, CHO, 
                TOTSUG,
                SATFA, `NA.`,
                ENGFIB, FruitVegNut, foodordrink, Total_NP_score) %>% 
  pivot_longer(cols=WATER:Total_NP_score) %>% 
  group_by(MainFoodGroupDesc, name) %>% 
  summarise(mean=mean(value)) %>% ungroup() %>%
  # dplyr::select(-FoodName) %>% 
  pivot_wider(names_from=name, values_from=c(mean)) %>% 
  ungroup() %>%
  distinct()

names <- maingroups$MainFoodGroupDesc
clusterdata <- as.matrix(maingroups[,-1])
row.names(clusterdata) <- names
clusterdata <- scale(clusterdata)

library(factoextra)

res.hc <- eclust(clusterdata, "hclust", nboot=500) # compute hclust

plot_clusters <- fviz_cluster(res.hc) + theme_bw() # scatter plot
plot_clusters
# fviz_dend(res.hc, rect = TRUE) # dendrogam
# fviz_silhouette(res.hc)
plot_clusters <- fviz_cluster(res.hc) + theme_bw() # scatter plot
plot_clusters
ggsave("Data/NDNS/cluster_plot_v3.png", dpi=300, width=33, height=19, units="cm")

finalclusters <- data.frame(cluster = res.hc$cluster)
finalclusters$MainFoodGroupDesc <- rownames(finalclusters)

# slightly adjust the clusters - group together chocolate confectionary and biscuits etc.
finalclusters <- finalclusters %>% 
  mutate(cluster = ifelse(cluster==4 | cluster==8, 4,
                          ifelse(MainFoodGroupDesc=="SOFT DRINKS NOT LOW CALORIE", 4, 
                                 ifelse(cluster==3, 2, 
                                        ifelse(MainFoodGroupDesc=="PUDDINGS" | MainFoodGroupDesc=="ICE CREAM", 4, 
                                               ifelse(cluster==7, 5, 
                                                      ifelse(MainFoodGroupDesc=="YOGURT FROMAGE FRAIS AND DAIRY DESSERTS", 1, cluster)))))),
         description = ifelse(cluster==1, "milk and cream",
                              ifelse(cluster==2, "meat and cheese",
                                     ifelse(cluster==4, "biscuits, chocolate, crisps, puddings",
                                            ifelse(cluster==5, "pasta, rice, bread",
                                                   ifelse(cluster==6, "butter and fats",
                                                          ifelse(cluster==9, "fruits and veg (inc juice)", "Misc")))))))

# rearrange some of the clusters

data <- left_join(data, finalclusters)

# save data with final clusters 
write.csv(finalclusters, "Data/NDNS/final_clustersNew.csv", row.names=F)

summary <- data %>% group_by(SurveyYear, DayNo, seriali,
                             description) %>% 
  summarise(TotalKcal = sum(Energykcal)) %>% ungroup() %>% 
  group_by(SurveyYear, DayNo, seriali) %>% 
  mutate(percent = TotalKcal/sum(TotalKcal))

clustersum <- data %>% ungroup() %>% dplyr::select(cluster, MainFoodGroupDesc) %>% distinct()

# data <- left_join(data, clusterlabels)
summarypop <- data %>% group_by(SurveyYear, description) %>% 
  summarise(TotalKcal = sum(Energykcal)) %>% ungroup() %>% 
  group_by(SurveyYear) %>% 
  mutate(percent = TotalKcal / sum(TotalKcal))
scaleFUN <- function(x) sprintf("%.2f", x)

summary_plot <- ggplot(data=summarypop, aes(x=SurveyYear, y=percent, colour=as.factor(description))) + geom_line(size=1) +
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom") +
  scale_y_continuous(labels=scaleFUN)

summary_plot
ggsave("Data/NDNS/cluster_prevalence_v3.png", dpi=300, width=33, height=19, units="cm")
