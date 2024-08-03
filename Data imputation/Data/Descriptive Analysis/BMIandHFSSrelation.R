#Cleaning HFSS indicators and calculating z-values with a view to seeing correlation with BMI
rm(list = ls()) # clear environment

library(plotly) 

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated")

setwd(wd)

data <- read.csv("Data imputation/Data/NDNS/NDNS_combined.csv")

HFSSindicatorsWithQuals <- data.frame(data$Totalsugarsg, data$Fatg, data$Sodiummg, data$eqvinc, data$bmival, data$Sex, data$ghq12scr) # extracting selected columns from data frame


HFSSindicatorsWithQuals[HFSSindicatorsWithQuals < 0] <- NA #sets all negative values to NA
CleanHFSSQuals = HFSSindicatorsWithQuals %>% na.omit() #omits all rows with NA in them


# z-value for Sugar
a1 <- CleanHFSSQuals$data.Totalsugarsg
MeanSugarConsumed <- mean(a1)
SDsugarConsumed <- sd(a1)
SugarZscore <- ((a1-MeanSugarConsumed)/(SDsugarConsumed))
#p2 = plot(SugarZscore, type="o", col="green")

# z-value for fat
a2 <- CleanHFSSQuals$data.Fatg
MeanFatConsumed <- mean(a2)
SDFatConsumed <- sd(a2)
FatZscore <- ((a2-MeanFatConsumed)/(SDFatConsumed))
#p1 = plot(FatZscore, type="o", col="red")

#z value for sodium
a3 <- CleanHFSSQuals$data.Sodiummg
MeanSodiumConsumed <- mean(a3)
SDSodiumConsumed <- sd(a3)
SodiumZscore <- ((a3-MeanSodiumConsumed)/(SDSodiumConsumed))

AverageZScore <- (SugarZscore+FatZscore+SodiumZscore)/3
AvgZ <- data.frame(AverageZScore)
CleanHFSSQualsAvgZ <- cbind(CleanHFSSQuals,AvgZ)

bmiAvgZ <- cbind(CleanHFSSQualsAvgZ$data.bmival, AvgZ, CleanHFSSQualsAvgZ$data.Sex)

names(bmiAvgZ)[names(bmiAvgZ) == "CleanHFSSQualsAvgZ$data.bmival"] <- "bmi"
names(bmiAvgZ)[names(bmiAvgZ) == "CleanHFSSQualsAvgZ$data.Sex"] <- "sex"

data_subset <- bmiAvgZ[bmiAvgZ$sex == 2, ]

contingencyTableHFSSbmi <- table(bmiAvgZ$bmi, bmiAvgZ$AverageZScore)
chiSquareTestHFSSbmi <- chisq.test(contingencyTableHFSSbmi)
print(chiSquareTestHFSSbmi)
corHFSSBMI <- cor(bmiAvgZ$bmi, bmiAvgZ$AverageZScore)

contingencyTableHFSSbmiWomen <- table(data_subset$bmi, data_subset$AverageZScore)
chiSquareTestHFSSbmiWomen <- chisq.test(contingencyTableHFSSbmiWomen)
print(chiSquareTestHFSSbmiWomen)
corHFSSBMIwomen <- cor(data_subset$bmi, data_subset$AverageZScore)

contingencyTableHFSSbmiWomen <- table(data_subset$bmi, data_subset$AverageZScore)
chiSquareTestHFSSbmiWomen <- chisq.test(contingencyTableHFSSbmiWomen)
print(chiSquareTestHFSSbmiWomen)
corHFSSBMIwomen <- cor(data_subset$bmi, data_subset$AverageZScore)


