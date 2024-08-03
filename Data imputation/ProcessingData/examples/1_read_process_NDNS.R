rm(list = ls()) # clear environment
library(haven)
library(tidyr)

wd <- ("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/Data imputation")
data <- ("Data/NDNS/UKDA-6533-stata/stata/stata13_se/")


setwd(wd)
### Import all the dayleveldietarydata_nutrients files ##

nutrient_year1_4<-read_dta(paste0(data, "ndns_rp_yr1-4a_dayleveldietarydata_nutrients_uk_v2.dta"))
nutrient_year5_6<-read_dta(paste0(data, "ndns_rp_yr5-6a_dayleveldietarydata_nutrients_v2.dta"))
nutrient_year7_8<-read_dta(paste0(data,"ndns_rp_yr7-8a_dayleveldietarydata_nutrients.dta"))
nutrient_year9_11<-read_dta(paste0(data,"ndns_rp_yr9-11a_dayleveldietarydata_nutrients_uk_20210831.dta"))
ind_year1_4<-read_dta(paste0(data, "ndns_rp_yr1-4a_indiv_uk.dta"))
ind_year5_6<-read_dta(paste0(data,"ndns_rp_yr5-6a_indiv.dta"))
ind_year7_8<-read_dta(paste0(data,"ndns_rp_yr7-8a_indiv.dta"))
ind_year9_11<-read_dta(paste0(data,"ndns_rp_yr9-11a_indiv_20211020.dta"))
# ind_year9<-read_dta(paste0(data,"ndns_rp_yr9a_indiv.dta"))
hhold_year1_4<-read_dta(paste0(data,"ndns_rp_yr1-4a_hhold_uk.dta"))
hhold_year5_6<-read_dta(paste0(data,"ndns_rp_yr5-6a_hhold.dta"))
hhold_year7_8<-read_dta(paste0(data, "ndns_rp_yr7-8a_hhold.dta"))
hhold_year9_11<-read_dta(paste0(data, "ndns_rp_yr9-11a_hhold_20210831.dta"))
daydiet_year1_4<-read_dta(paste0(data, "ndns_rp_yr1-4a_personleveldietarydata_uk_v2.dta"))
daydiet_year5_6<-read_dta(paste0(data, "ndns_rp_yr5-6a_personleveldietarydata_v2.dta"))
daydiet_year7_8<-read_dta(paste0(data, "ndns_rp_yr7-8a_personleveldietarydata.dta"))
daydiet_year9_11<-read_dta(paste0(data, "ndns_rp_yr9-11a_personleveldietarydata_uk_20210831.dta"))

year1_4_name<-colnames(ind_year1_4)
year5_6_name<-colnames(ind_year5_6)
year7_8_name<-colnames(ind_year7_8)
year9_11_name<-colnames(ind_year9_11)

# ## Export column names to identify gapes in the data for each wave. ## 
# write.table(year1_4_name,"NDNS Combining\\Write tables\\year1_4_name.txt")
# write.table(year5_6_name,"NDNS Combining\\Write tables\\year5_6_name.txt")
# write.table(year7_8_name,"NDNS Combining\\Write tables\\year7_8_name.txt")
# write.table(year9_11_name,"NDNS Combining\\Write tables\\year9_11_name.txt")

## Rename some variables with minor changes over time ##
names(daydiet_year5_6)[names(daydiet_year5_6) == 'Surveyyear'] <- 'SurveyYear'
names(daydiet_year1_4)[names(daydiet_year1_4) == 'variabl05'] <- 'VitaminDmg'
names(daydiet_year5_6)[names(daydiet_year5_6) == 'variabl05'] <- 'VitaminDmg'
names(daydiet_year7_8)[names(daydiet_year7_8) == 'variabl05'] <- 'VitaminDmg'
names(daydiet_year9_11)[names(daydiet_year9_11) == 'variabl05'] <- 'VitaminDmg'
names(daydiet_year1_4)[names(daydiet_year1_4) == 'Intrinsicandmilksugarsandstarch'] <- 'Intrinsicandmilksugarsandstarchg'
names(daydiet_year5_6)[names(daydiet_year5_6) == 'Intrinsicandmilksugarsandstarch'] <- 'Intrinsicandmilksugarsandstarchg'

### Only keep variables that will be used in the analyses. This can be updated if other variables are needed ##
daydiet_year1_4 = subset(daydiet_year1_4, select = c(seriali,SurveyYear,Country,TotalEMJ,FoodEMJ,EnergykJ,FoodEkJ,Energykcal,FoodEkcal,Proteing,Fatg,
                                                     Saturatedfattyacidsg,CisMonounsaturatedfattyacidsg,Cisn6fattyacidsg,Cisn3fattyacidsg,Transfattyacidsg,
                                                     Carbohydrateg,Totalsugarsg,Othersugarsg,Starchg,Glucoseg,Fructoseg,Sucroseg,Maltoseg,Lactoseg,
                                                     Nonmilkextrinsicsugarsg,Intrinsicandmilksugarsg,Intrinsicandmilksugarsandstarchg,Englystfibreg,FreeSugarsg,
                                                     AOACFibreg,VitaminDmg,VitaminCmg,Sodiummg,Calciummg,Alcoholg,ONEPERCENTMILK,BEEFVEALANDDISHES,BUTTER,
                                                     OTHERMARGARINEFATSANDOILS,OTHERMILKANDCREAM,PUFAMARGARINEOILS,SEMISKIMMEDMILK,WHOLEMILK,WHOLEMEALBREAD,
                                                     ICECREAM,WHITEFISHCOATEDORFRIED,BEERLAGERCIDERPERRY,CRISPSANDSAVOURYSNACKS,FRUITJUICE,SOFTDRINKSLOWCALORIE,
                                                     SOFTDRINKSNOTLOWCALORIE,SPIRITSANDLIQUEURS,SUGARCONFECTIONERY,TEACOFFEEANDWATER,WINE,BACONANDHAM,BISCUITS,
                                                     BUNSCAKESPASTRIESFRUITPIES,BURGERSANDKEBABS,CHEESE,CHIPSFRIEDROASTPOTATOESANDPOTATO,
                                                     CHOCOLATECONFECTIONERY,COATEDCHICKEN,EGGSANDEGGDISHES,FRUIT,HIGHFIBREBREAKFASTCEREALS,LAMBANDDISHES,
                                                     LIVERDISHES,MEATPIESANDPASTRIES,NUTSANDSEEDS,OILYFISH,OTHERBREAD,OTHERMEATANDMEATPRODUCTS,
                                                     PASTARICEANDOTHERCEREALS,PORKANDDISHES,SALADANDOTHERRAWVEGETABLES,SAUSAGES,WHITEBREAD,
                                                     YOGURTFROMAGEFRAISANDDAIRYDESSER,OTHERBREAKFASTCEREALS,OTHERPOTATOESPOTATOSALADSDISHES,
                                                     OTHERWHITEFISHSHELLFISHFISHDISHE,VEGETABLESNOTRAW,CHICKENANDTURKEYDISHES,PUDDINGS,
                                                     BROWNGRANARYANDWHEATGERMBREAD,SKIMMEDMILK,SUGARSPRESERVESANDSWEETSPREADS,DRYWEIGHTBEVERAGES,
                                                     LOWFATSPREADNOTPOLYUNSATURATED,LOWFATSPREADPOLYUNSATURATED,REDUCEDFATSPREADNOTPOLYUNSATURAT,
                                                     REDUCEDFATSPREADPOLYUNSATURATED,SAVOURYSAUCESPICKLESGRAVIESCONDI,SOUPHOMEMADEANDRETAIL,
                                                     COMMERCIALTODDLERSFOODSANDDRINKS,CHEDDARCHEESE,COTTAGECHEESE,OTHERCHEESE,Fruitg,DriedFruitg,
                                                     FruitJuiceg,FruitJuiceg100percent,SmoothieFruitg,Tomatoesg,TomatoPureeg,Brassicaceaeg,YellowRedGreeng,Beansg,
                                                     Nutsg,OtherVegg,Driedfruitx3,fruitjuicemax,smoothiefruitmax,Tompureex5,beansmax,totalfruit,totalveg,
                                                     totalfruitandveg,SmoothieFruitportions,Fruitvegportions,Fruitjuiceportions,Totfruitvegportions,Achieve5,Beefg,
                                                     Lambg,Porkg,ProcessedRedMeatg,OtherRedMeatg,Burgersg,Sausagesg,Offalg,Poultryg,ProcessedPoultryg,GameBirdsg,
                                                     WhiteFishg,OilyFishg,CannedTunag,Shellfishg,totalfish,totalredmeat,totalwhitemeat,totalmeat,CottageCheeseg,
                                                     CheddarCheeseg))
daydiet_year5_6 = subset(daydiet_year5_6, select = c(seriali,SurveyYear,Country,TotalEMJ,FoodEMJ,EnergykJ,FoodEkJ,Energykcal,FoodEkcal,Proteing,Fatg,
                                                     Saturatedfattyacidsg,CisMonounsaturatedfattyacidsg,Cisn6fattyacidsg,Cisn3fattyacidsg,Transfattyacidsg,
                                                     Carbohydrateg,Totalsugarsg,Othersugarsg,Starchg,Glucoseg,Fructoseg,Sucroseg,Maltoseg,Lactoseg,
                                                     Nonmilkextrinsicsugarsg,Intrinsicandmilksugarsg,Intrinsicandmilksugarsandstarchg,Englystfibreg,FreeSugarsg,
                                                     AOACFibreg,VitaminDmg,VitaminCmg,Sodiummg,Calciummg,Alcoholg,ONEPERCENTMILK,BEEFVEALANDDISHES,BUTTER,
                                                     OTHERMARGARINEFATSANDOILS,OTHERMILKANDCREAM,PUFAMARGARINEOILS,SEMISKIMMEDMILK,WHOLEMILK,WHOLEMEALBREAD,
                                                     ICECREAM,WHITEFISHCOATEDORFRIED,BEERLAGERCIDERPERRY,CRISPSANDSAVOURYSNACKS,FRUITJUICE,SOFTDRINKSLOWCALORIE,
                                                     SOFTDRINKSNOTLOWCALORIE,SPIRITSANDLIQUEURS,SUGARCONFECTIONERY,TEACOFFEEANDWATER,WINE,BACONANDHAM,BISCUITS,
                                                     BUNSCAKESPASTRIESFRUITPIES,BURGERSANDKEBABS,CHEESE,CHIPSFRIEDROASTPOTATOESANDPOTATO,
                                                     CHOCOLATECONFECTIONERY,COATEDCHICKEN,EGGSANDEGGDISHES,FRUIT,HIGHFIBREBREAKFASTCEREALS,LAMBANDDISHES,
                                                     LIVERDISHES,MEATPIESANDPASTRIES,NUTSANDSEEDS,OILYFISH,OTHERBREAD,OTHERMEATANDMEATPRODUCTS,
                                                     PASTARICEANDOTHERCEREALS,PORKANDDISHES,SALADANDOTHERRAWVEGETABLES,SAUSAGES,WHITEBREAD,
                                                     YOGURTFROMAGEFRAISANDDAIRYDESSER,OTHERBREAKFASTCEREALS,OTHERPOTATOESPOTATOSALADSDISHES,
                                                     OTHERWHITEFISHSHELLFISHFISHDISHE,VEGETABLESNOTRAW,CHICKENANDTURKEYDISHES,PUDDINGS,
                                                     BROWNGRANARYANDWHEATGERMBREAD,SKIMMEDMILK,SUGARSPRESERVESANDSWEETSPREADS,DRYWEIGHTBEVERAGES,
                                                     LOWFATSPREADNOTPOLYUNSATURATED,LOWFATSPREADPOLYUNSATURATED,REDUCEDFATSPREADNOTPOLYUNSATURAT,
                                                     REDUCEDFATSPREADPOLYUNSATURATED,SAVOURYSAUCESPICKLESGRAVIESCONDI,SOUPHOMEMADEANDRETAIL,
                                                     COMMERCIALTODDLERSFOODSANDDRINKS,CHEDDARCHEESE,COTTAGECHEESE,OTHERCHEESE,Fruitg,DriedFruitg,
                                                     FruitJuiceg,FruitJuiceg100percent,SmoothieFruitg,Tomatoesg,TomatoPureeg,Brassicaceaeg,YellowRedGreeng,Beansg,
                                                     Nutsg,OtherVegg,Driedfruitx3,fruitjuicemax,smoothiefruitmax,Tompureex5,beansmax,totalfruit,totalveg,
                                                     totalfruitandveg,SmoothieFruitportions,Fruitvegportions,Fruitjuiceportions,Totfruitvegportions,Achieve5,Beefg,
                                                     Lambg,Porkg,ProcessedRedMeatg,OtherRedMeatg,Burgersg,Sausagesg,Offalg,Poultryg,ProcessedPoultryg,GameBirdsg,
                                                     WhiteFishg,OilyFishg,CannedTunag,Shellfishg,totalfish,totalredmeat,totalwhitemeat,totalmeat,CottageCheeseg,
                                                     CheddarCheeseg))
daydiet_year7_8 = subset(daydiet_year7_8, select = c(seriali,SurveyYear,Country,TotalEMJ,FoodEMJ,EnergykJ,FoodEkJ,Energykcal,FoodEkcal,Proteing,Fatg,
                                                     Saturatedfattyacidsg,CisMonounsaturatedfattyacidsg,Cisn6fattyacidsg,Cisn3fattyacidsg,Transfattyacidsg,
                                                     Carbohydrateg,Totalsugarsg,Othersugarsg,Starchg,Glucoseg,Fructoseg,Sucroseg,Maltoseg,Lactoseg,
                                                     Nonmilkextrinsicsugarsg,Intrinsicandmilksugarsg,Intrinsicandmilksugarsandstarchg,Englystfibreg,FreeSugarsg,
                                                     AOACFibreg,VitaminDmg,VitaminCmg,Sodiummg,Calciummg,Alcoholg,ONEPERCENTMILK,BEEFVEALANDDISHES,BUTTER,
                                                     OTHERMARGARINEFATSANDOILS,OTHERMILKANDCREAM,PUFAMARGARINEOILS,SEMISKIMMEDMILK,WHOLEMILK,WHOLEMEALBREAD,
                                                     ICECREAM,WHITEFISHCOATEDORFRIED,BEERLAGERCIDERPERRY,CRISPSANDSAVOURYSNACKS,FRUITJUICE,SOFTDRINKSLOWCALORIE,
                                                     SOFTDRINKSNOTLOWCALORIE,SPIRITSANDLIQUEURS,SUGARCONFECTIONERY,TEACOFFEEANDWATER,WINE,BACONANDHAM,BISCUITS,
                                                     BUNSCAKESPASTRIESFRUITPIES,BURGERSANDKEBABS,CHEESE,CHIPSFRIEDROASTPOTATOESANDPOTATO,
                                                     CHOCOLATECONFECTIONERY,COATEDCHICKEN,EGGSANDEGGDISHES,FRUIT,HIGHFIBREBREAKFASTCEREALS,LAMBANDDISHES,
                                                     LIVERDISHES,MEATPIESANDPASTRIES,NUTSANDSEEDS,OILYFISH,OTHERBREAD,OTHERMEATANDMEATPRODUCTS,
                                                     PASTARICEANDOTHERCEREALS,PORKANDDISHES,SALADANDOTHERRAWVEGETABLES,SAUSAGES,WHITEBREAD,
                                                     YOGURTFROMAGEFRAISANDDAIRYDESSER,OTHERBREAKFASTCEREALS,OTHERPOTATOESPOTATOSALADSDISHES,
                                                     OTHERWHITEFISHSHELLFISHFISHDISHE,VEGETABLESNOTRAW,CHICKENANDTURKEYDISHES,PUDDINGS,
                                                     BROWNGRANARYANDWHEATGERMBREAD,SKIMMEDMILK,SUGARSPRESERVESANDSWEETSPREADS,DRYWEIGHTBEVERAGES,
                                                     LOWFATSPREADNOTPOLYUNSATURATED,LOWFATSPREADPOLYUNSATURATED,REDUCEDFATSPREADNOTPOLYUNSATURAT,
                                                     REDUCEDFATSPREADPOLYUNSATURATED,SAVOURYSAUCESPICKLESGRAVIESCONDI,SOUPHOMEMADEANDRETAIL,
                                                     COMMERCIALTODDLERSFOODSANDDRINKS,CHEDDARCHEESE,COTTAGECHEESE,OTHERCHEESE,Fruitg,DriedFruitg,
                                                     FruitJuiceg,FruitJuiceg100percent,SmoothieFruitg,Tomatoesg,TomatoPureeg,Brassicaceaeg,YellowRedGreeng,Beansg,
                                                     Nutsg,OtherVegg,Driedfruitx3,fruitjuicemax,smoothiefruitmax,Tompureex5,beansmax,totalfruit,totalveg,
                                                     totalfruitandveg,SmoothieFruitportions,Fruitvegportions,Fruitjuiceportions,Totfruitvegportions,Achieve5,Beefg,
                                                     Lambg,Porkg,ProcessedRedMeatg,OtherRedMeatg,Burgersg,Sausagesg,Offalg,Poultryg,ProcessedPoultryg,GameBirdsg,
                                                     WhiteFishg,OilyFishg,CannedTunag,Shellfishg,totalfish,totalredmeat,totalwhitemeat,totalmeat,CottageCheeseg,
                                                     CheddarCheeseg))
daydiet_year9_11 = subset(daydiet_year9_11, select = c(seriali,SurveyYear,Country,TotalEMJ,FoodEMJ,EnergykJ,FoodEkJ,Energykcal,FoodEkcal,Proteing,Fatg,
                                                 Saturatedfattyacidsg,CisMonounsaturatedfattyacidsg,Cisn6fattyacidsg,Cisn3fattyacidsg,Transfattyacidsg,
                                                 Carbohydrateg,Totalsugarsg,Othersugarsg,Starchg,Glucoseg,Fructoseg,Sucroseg,Maltoseg,Lactoseg,
                                                 Nonmilkextrinsicsugarsg,Intrinsicandmilksugarsg,Intrinsicandmilksugarsandstarchg,Englystfibreg,FreeSugarsg,
                                                 AOACFibreg,VitaminDmg,VitaminCmg,Sodiummg,Calciummg,Alcoholg,ONEPERCENTMILK,BEEFVEALANDDISHES,BUTTER,
                                                 OTHERMARGARINEFATSANDOILS,OTHERMILKANDCREAM,PUFAMARGARINEOILS,SEMISKIMMEDMILK,WHOLEMILK,WHOLEMEALBREAD,
                                                 ICECREAM,WHITEFISHCOATEDORFRIED,BEERLAGERCIDERPERRY,CRISPSANDSAVOURYSNACKS,FRUITJUICE,SOFTDRINKSLOWCALORIE,
                                                 SOFTDRINKSNOTLOWCALORIE,SPIRITSANDLIQUEURS,SUGARCONFECTIONERY,TEACOFFEEANDWATER,WINE,BACONANDHAM,BISCUITS,
                                                 BUNSCAKESPASTRIESFRUITPIES,BURGERSANDKEBABS,CHEESE,CHIPSFRIEDROASTPOTATOESANDPOTATO,
                                                 CHOCOLATECONFECTIONERY,COATEDCHICKEN,EGGSANDEGGDISHES,FRUIT,HIGHFIBREBREAKFASTCEREALS,LAMBANDDISHES,
                                                 LIVERDISHES,MEATPIESANDPASTRIES,NUTSANDSEEDS,OILYFISH,OTHERBREAD,OTHERMEATANDMEATPRODUCTS,
                                                 PASTARICEANDOTHERCEREALS,PORKANDDISHES,SALADANDOTHERRAWVEGETABLES,SAUSAGES,WHITEBREAD,
                                                 YOGURTFROMAGEFRAISANDDAIRYDESSER,OTHERBREAKFASTCEREALS,OTHERPOTATOESPOTATOSALADSDISHES,
                                                 OTHERWHITEFISHSHELLFISHFISHDISHE,VEGETABLESNOTRAW,CHICKENANDTURKEYDISHES,PUDDINGS,
                                                 BROWNGRANARYANDWHEATGERMBREAD,SKIMMEDMILK,SUGARSPRESERVESANDSWEETSPREADS,DRYWEIGHTBEVERAGES,
                                                 LOWFATSPREADNOTPOLYUNSATURATED,LOWFATSPREADPOLYUNSATURATED,REDUCEDFATSPREADNOTPOLYUNSATURAT,
                                                 REDUCEDFATSPREADPOLYUNSATURATED,SAVOURYSAUCESPICKLESGRAVIESCONDI,SOUPHOMEMADEANDRETAIL,
                                                 COMMERCIALTODDLERSFOODSANDDRINKS,CHEDDARCHEESE,COTTAGECHEESE,OTHERCHEESE,Fruitg,DriedFruitg,
                                                 FruitJuiceg,FruitJuiceg100percent,SmoothieFruitg,Tomatoesg,TomatoPureeg,Brassicaceaeg,YellowRedGreeng,Beansg,
                                                 Nutsg,OtherVegg,Driedfruitx3,fruitjuicemax,smoothiefruitmax,Tompureex5,beansmax,totalfruit,totalveg,
                                                 totalfruitandveg,SmoothieFruitportions,Fruitvegportions,Fruitjuiceportions,Totfruitvegportions,Achieve5,Beefg,
                                                 Lambg,Porkg,ProcessedRedMeatg,OtherRedMeatg,Burgersg,Sausagesg,Offalg,Poultryg,ProcessedPoultryg,GameBirdsg,
                                                 WhiteFishg,OilyFishg,CannedTunag,Shellfishg,totalfish,totalredmeat,totalwhitemeat,totalmeat,CottageCheeseg,
                                                 CheddarCheeseg))


### Check that the same number of variables are in each dataset ###
year1_4_name<-colnames(ind_year1_4)
year5_6_name<-colnames(ind_year5_6)
year7_8_name<-colnames(ind_year7_8)
year9_11_name<-colnames(ind_year9_11)

### Generate new weights for combined dataset ###
ind_year1_4$wti_1_9r <- ind_year1_4$wti_UKY1234 * 15655 / 6828 * (4/11)
ind_year5_6$wti_1_9r <- ind_year5_6$wti_Y56 * (15655) / 2546 * (2/11)
ind_year7_8$wti_1_9r <- ind_year7_8$wti_Y78 * (15655) / 2723 * (2/11)
ind_year9_11$wti_1_9r <- ind_year9_11$wti_Y911 * (15655) / 3558 * (3/11)


### Select variables for the individual dataset to combine with the personlevel dataset ###
### Ethnic 5 level grouping is named differently in year 1-4###
names(ind_year1_4)[names(ind_year1_4) == 'ethgr5'] <- 'ethgrp5'
### Generate/renames variables for IMD quintile in England, scotland, NI, and wales to align with other waves ###
names(ind_year1_4)[names(ind_year1_4) == 'qimd'] <- 'qeimd'
names(ind_year1_4)[names(ind_year1_4) == 'tnimd'] <- 'qnimd' # Cannot use this currently because tertiles not quintiles
ind_year1_4$qnimd <- -1  # Set all observations to zero until better approach can be found. 
names(ind_year1_4)[names(ind_year1_4) == 'qsimd12'] <- 'qsimd'
ind_year1_4$qwimd <- -1
### Rename variables for blood pressure/statin medication to align with other waves ###
names(ind_year1_4)[names(ind_year1_4) == 'bpmedd'] <- 'bpmedd2'
names(ind_year1_4)[names(ind_year1_4) == 'lipid'] <- 'lipid2'
### Rename GOR in year 9 dataset to align with other years ##
names(ind_year9_11)[names(ind_year9_11) == 'GOR'] <- 'gor'
names(ind_year9_11)[names(ind_year9_11) == 'MarSt2_r'] <- 'MarSt2'
names(ind_year9_11)[names(ind_year9_11) == 'AgeR'] <- 'age'
names(ind_year9_11)[names(ind_year9_11) == 'DMHSize7'] <- 'DMHSize'
names(ind_year9_11)[names(ind_year9_11) == 'NumAdult5'] <- 'NumAdult'
names(ind_year9_11)[names(ind_year9_11) == 'NumChild4'] <- 'NumChild'
names(ind_year9_11)[names(ind_year9_11) == 'qual7m'] <- 'qual7'
names(ind_year9_11)[names(ind_year9_11) == 'htval2'] <- 'htval'
names(ind_year9_11)[names(ind_year9_11) == 'wtval2'] <- 'wtval'
names(ind_year9_11)[names(ind_year9_11) == 'bmival2'] <- 'bmival'
names(ind_year9_11)[names(ind_year9_11) == 'MarSt2_r'] <- 'MarSt2'
ind_year9_11$eqvinc<-NA
ind_year1_4$eqv3<-NA
### Merge MarStat and MarSt2 in year1_4 ###
ind_year1_4[,"MarSt2"] <- replace(ind_year1_4[,"MarSt2"],ind_year1_4[,'MarStat']==1,1)
ind_year1_4[,"MarSt2"] <- replace(ind_year1_4[,"MarSt2"],ind_year1_4[,'MarStat']==2,2)
ind_year1_4[,"MarSt2"] <- replace(ind_year1_4[,"MarSt2"],ind_year1_4[,'MarStat']==3,4)
ind_year1_4[,"MarSt2"] <- replace(ind_year1_4[,"MarSt2"],ind_year1_4[,'MarStat']==4,5)
ind_year1_4[,"MarSt2"] <- replace(ind_year1_4[,"MarSt2"],ind_year1_4[,'MarStat']==5,6)
ind_year1_4[,"LiveW2"] <- replace(ind_year1_4[,"LiveW2"],ind_year1_4[,'LiveWith']==1,1)
ind_year1_4[,"LiveW2"] <- replace(ind_year1_4[,"LiveW2"],ind_year1_4[,'LiveWith']==2,2)
## Align year 1-4 with year9-11 coding
ind_year1_4[,"MarSt2"] <- replace(ind_year1_4[,'MarSt2'],ind_year1_4[,'MarSt2']==2 | ind_year1_4[,'MarSt2']==3,2)
ind_year1_4[,"MarSt2"] <- replace(ind_year1_4[,'MarSt2'],ind_year1_4[,'MarSt2']==4 | ind_year1_4[,'MarSt2']==7,3)
ind_year1_4[,"MarSt2"] <- replace(ind_year1_4[,'MarSt2'],ind_year1_4[,'MarSt2']==5 | ind_year1_4[,'MarSt2']==8,4)
ind_year1_4[,"MarSt2"] <- replace(ind_year1_4[,'MarSt2'],ind_year1_4[,'MarSt2']==6 | ind_year1_4[,'MarSt2']==9,5)
## Align year 5-6 with year 9-11 coding
ind_year5_6[,"MarSt2"] <- replace(ind_year5_6[,'MarSt2'],ind_year5_6[,'MarSt2']==2 | ind_year5_6[,'MarSt2']==3,2)
ind_year5_6[,"MarSt2"] <- replace(ind_year5_6[,'MarSt2'],ind_year5_6[,'MarSt2']==4 | ind_year5_6[,'MarSt2']==7,3)
ind_year5_6[,"MarSt2"] <- replace(ind_year5_6[,'MarSt2'],ind_year5_6[,'MarSt2']==5 | ind_year5_6[,'MarSt2']==8,4)
ind_year5_6[,"MarSt2"] <- replace(ind_year5_6[,'MarSt2'],ind_year5_6[,'MarSt2']==6 | ind_year5_6[,'MarSt2']==9,5)
## Align year 7-8 with year 9-11 coding
ind_year7_8[,"MarSt2"] <- replace(ind_year7_8[,'MarSt2'],ind_year7_8[,'MarSt2']==2 | ind_year7_8[,'MarSt2']==3,2)
ind_year7_8[,"MarSt2"] <- replace(ind_year7_8[,'MarSt2'],ind_year7_8[,'MarSt2']==4 | ind_year7_8[,'MarSt2']==7,3)
ind_year7_8[,"MarSt2"] <- replace(ind_year7_8[,'MarSt2'],ind_year7_8[,'MarSt2']==5 | ind_year7_8[,'MarSt2']==8,4)
ind_year7_8[,"MarSt2"] <- replace(ind_year7_8[,'MarSt2'],ind_year7_8[,'MarSt2']==6 | ind_year7_8[,'MarSt2']==9,5)


ind_year1_4 <- subset(ind_year1_4, select = c(age,Sex,DMHSize,NumAdult,NumChild,MarSt2,LiveW2,AdChild,qual7,WrkStat,NTypHrs,NCasHrs,nssec8,ethgrp5,eqv3,eqvinc,region,gor,EIMD_2015_quintile,WIMD_2014_quintile,SIMD_2016_quintile,NIMD_2017_quintile,GenHelf,StatinA,bpmedd2,Diabetes,lipid2,cigsta3,cigst2,htval,wtval,bmival,omsysval,Chol,HDL,A1C,seriali,serialh,wti_1_9r))
ind_year5_6 <- subset(ind_year5_6, select = c(age,Sex,DMHSize,NumAdult,NumChild,MarSt2,LiveW2,AdChild,qual7,WrkStat,NTypHrs,NCasHrs,nssec8,ethgrp5,eqv3,eqvinc,region,gor,EIMD_2015_quintile,WIMD_2014_quintile,SIMD_2016_quintile,NIMD_2017_quintile,GenHelf,StatinA,bpmedd2,Diabetes,lipid2,cigsta3,cigst2,htval,wtval,bmival,omsysval,Chol,HDL,A1C,seriali,serialh,wti_1_9r))
ind_year7_8 <- subset(ind_year7_8, select = c(age,Sex,DMHSize,NumAdult,NumChild,MarSt2,LiveW2,AdChild,qual7,WrkStat,NTypHrs,NCasHrs,nssec8,ethgrp5,eqv3,eqvinc,region,gor,EIMD_2015_quintile,WIMD_2014_quintile,SIMD_2016_quintile,NIMD_2017_quintile,GenHelf,StatinA,bpmedd2,Diabetes,lipid2,cigsta3,cigst2,htval,wtval,bmival,omsysval,Chol,HDL,A1C,seriali,serialh,wti_1_9r))
ind_year9_11 <- subset(ind_year9_11, select = c(age,Sex,DMHSize,NumAdult,NumChild,MarSt2,LiveW2,AdChild,qual7,WrkStat,NTypHrs,NCasHrs,nssec8,ethgrp5,eqv3,eqvinc,region,gor,EIMD_2015_quintile,WIMD_2014_quintile,SIMD_2016_quintile,NIMD_2017_quintile,GenHelf,StatinA,bpmedd2,Diabetes,lipid2,cigsta3,cigst2,htval,wtval,bmival,omsysval,Chol,HDL,A1C,seriali,serialh,wti_1_9r))


### Household data to get household type information for adults and children ###
### Merge MarStat and MarSt2 in year1_4 ###
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,"MarSt2"],hhold_year1_4[,'MarStat']==1,1)
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,"MarSt2"],hhold_year1_4[,'MarStat']==2,2)
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,"MarSt2"],hhold_year1_4[,'MarStat']==3,4)
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,"MarSt2"],hhold_year1_4[,'MarStat']==4,5)
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,"MarSt2"],hhold_year1_4[,'MarStat']==5,6)
hhold_year1_4[,"LiveW2"] <- replace(hhold_year1_4[,"LiveW2"],hhold_year1_4[,'LiveWith']==1,1)
hhold_year1_4[,"LiveW2"] <- replace(hhold_year1_4[,"LiveW2"],hhold_year1_4[,'LiveWith']==2,2)
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,"MarSt2"],hhold_year1_4[,'MarSt2']==-1,NA)
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,"MarSt2"],hhold_year1_4[,'MarSt2']==-4,NA)
hhold_year1_4[,"LiveW2"] <- replace(hhold_year1_4[,"LiveW2"],hhold_year1_4[,'LiveW2']==-1,NA)
hhold_year1_4[,"LiveW2"] <- replace(hhold_year1_4[,"LiveW2"],hhold_year1_4[,'LiveW2']==-4,NA)

## Align year 1-4 with year9-11 coding
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,'MarSt2'],hhold_year1_4[,'MarSt2']==2 | hhold_year1_4[,'MarSt2']==3,2)
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,'MarSt2'],hhold_year1_4[,'MarSt2']==4 | hhold_year1_4[,'MarSt2']==7,3)
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,'MarSt2'],hhold_year1_4[,'MarSt2']==5 | hhold_year1_4[,'MarSt2']==8,4)
hhold_year1_4[,"MarSt2"] <- replace(hhold_year1_4[,'MarSt2'],hhold_year1_4[,'MarSt2']==6 | hhold_year1_4[,'MarSt2']==9,5)
## Align year 5-6 with year 9-11 coding
hhold_year5_6[,"MarSt2"] <- replace(hhold_year5_6[,'MarSt2'],hhold_year5_6[,'MarSt2']==2 | hhold_year5_6[,'MarSt2']==3,2)
hhold_year5_6[,"MarSt2"] <- replace(hhold_year5_6[,'MarSt2'],hhold_year5_6[,'MarSt2']==4 | hhold_year5_6[,'MarSt2']==7,3)
hhold_year5_6[,"MarSt2"] <- replace(hhold_year5_6[,'MarSt2'],hhold_year5_6[,'MarSt2']==5 | hhold_year5_6[,'MarSt2']==8,4)
hhold_year5_6[,"MarSt2"] <- replace(hhold_year5_6[,'MarSt2'],hhold_year5_6[,'MarSt2']==6 | hhold_year5_6[,'MarSt2']==9,5)
## Align year 7-8 with year 9-11 coding
hhold_year7_8[,"MarSt2"] <- replace(hhold_year7_8[,'MarSt2'],hhold_year7_8[,'MarSt2']==2 | hhold_year7_8[,'MarSt2']==3,2)
hhold_year7_8[,"MarSt2"] <- replace(hhold_year7_8[,'MarSt2'],hhold_year7_8[,'MarSt2']==4 | hhold_year7_8[,'MarSt2']==7,3)
hhold_year7_8[,"MarSt2"] <- replace(hhold_year7_8[,'MarSt2'],hhold_year7_8[,'MarSt2']==5 | hhold_year7_8[,'MarSt2']==8,4)
hhold_year7_8[,"MarSt2"] <- replace(hhold_year7_8[,'MarSt2'],hhold_year7_8[,'MarSt2']==6 | hhold_year7_8[,'MarSt2']==9,5)
## rename yea 9-11 to
names(hhold_year9_11)[names(hhold_year9_11) == 'MarSt2_r'] <- 'MarSt2'
## Generate variable pgrid which is missing from years9-11
hhold_year9_11[,'pgrid'] <- hhold_year9_11[,'seriali']%% 10
## 

hhold_year1_4 <- subset(hhold_year1_4, select = c(MarSt2,LiveW2,serialh,pgrid,WrkStat))
hhold_year5_6 <- subset(hhold_year5_6, select = c(MarSt2,LiveW2,serialh,pgrid,WrkStat))
hhold_year7_8 <- subset(hhold_year7_8, select = c(MarSt2,LiveW2,serialh,pgrid,WrkStat))
hhold_year9_11 <- subset(hhold_year9_11, select = c(MarSt2,LiveW2,serialh,pgrid,WrkStat))

### look at 18 year old families without an older adult ## 
View(hhold_year1_4[which(hhold_year1_4$serialh==1121219),]) ## 18 year old with baby
View(hhold_year1_4[which(hhold_year1_4$serialh==4071309),]) ## 18 year old with toddler

hh1_4<-hhold_year1_4 %>% pivot_wider(names_from = pgrid, values_from = c(MarSt2, LiveW2, WrkStat))
hh5_6<-hhold_year5_6 %>% pivot_wider(names_from = pgrid, values_from = c(MarSt2, LiveW2, WrkStat))
hh7_8<-hhold_year7_8 %>% pivot_wider(names_from = pgrid, values_from = c(MarSt2, LiveW2, WrkStat))
hh9_11<-hhold_year9_11 %>% pivot_wider(names_from = pgrid, values_from = c(MarSt2, LiveW2, WrkStat))

hh1_4 <- subset(hh1_4, select = c(MarSt2_1,LiveW2_1,serialh,WrkStat_1))
hh5_6 <- subset(hh5_6, select = c(MarSt2_1,LiveW2_1,serialh,WrkStat_1))
hh7_8 <- subset(hh7_8, select = c(MarSt2_1,LiveW2_1,serialh,WrkStat_1))
hh9_11 <- subset(hh9_11, select = c(MarSt2_1,LiveW2_1,serialh,WrkStat_1))

colnames(hh1_4) <- c("MarSt_p1","LiveW_p1","serialh","WrkStat_p1")
colnames(hh5_6) <- c("MarSt_p1","LiveW_p1","serialh","WrkStat_p1")
colnames(hh7_8) <- c("MarSt_p1","LiveW_p1","serialh","WrkStat_p1")
colnames(hh9_11) <- c("MarSt_p1","LiveW_p1","serialh","WrkStat_p1")

indhhold_year1_4 <- merge(ind_year1_4,hh1_4,by="serialh")
indhhold_year5_6 <- merge(ind_year5_6,hh5_6,by="serialh")
indhhold_year7_8 <- merge(ind_year7_8,hh7_8,by="serialh")
indhhold_year9_11 <- merge(ind_year9_11,hh9_11,by="serialh")

### Problem identified in ethnicity variables over time in NDNS. Detailed ethnic grouping only available in year 1-6 ###
### Problem identified in imd variables over time in NDNS ###
### Waves 1-4 have data on confidence in cooking ###
### There is a lot of variation over waves in existing health conditions data ###
### There is variation in diuretic variable names ## 
### There are variables for energy expenditure in waves 1-4 ###

year1_4_name<-colnames(ind_year1_4)
year5_6_name<-colnames(ind_year5_6)
year7_8_name<-colnames(ind_year7_8)
year9_11_name<-colnames(ind_year9_11)

year1_4 <- merge(indhhold_year1_4,daydiet_year1_4,by="seriali")
year5_6 <- merge(indhhold_year5_6,daydiet_year5_6,by="seriali")
year7_8 <- merge(indhhold_year7_8,daydiet_year7_8,by="seriali")
year9_11 <- merge(indhhold_year9_11,daydiet_year9_11,by="seriali")

year1_4_name<-colnames(year1_4)
year5_6_name<-colnames(year5_6)
year7_8_name<-colnames(year7_8)
year9_11_name<-colnames(year9_11)

# write.table(year1_4_name,"NDNS Combining\\Write tables\\year1_4_name.txt")
# write.table(year5_6_name,"NDNS Combining\\Write tables\\year5_6_name.txt")
# write.table(year7_8_name,"NDNS Combining\\Write tables\\year7_8_name.txt")
# write.table(year9_11_name,"NDNS Combining\\Write tables\\year9_11_name.txt")

## ignore suplements only combine up to var 390
year1_4 <- year1_4 %>% zap_label() %>% zap_formats() %>% zap_labels()
year5_6 <- year5_6 %>% zap_label() %>% zap_formats() %>% zap_labels()
year7_8 <- year7_8 %>% zap_label() %>% zap_formats() %>% zap_labels()
year9_11 <- year9_11 %>% zap_label() %>% zap_formats() %>% zap_labels()

NDNS <- rbind(year1_4,year5_6,year7_8,year9_11)
write.csv(NDNS, paste0("Data/NDNS/NDNS_combinedOriginal.csv"), row.names=F)
