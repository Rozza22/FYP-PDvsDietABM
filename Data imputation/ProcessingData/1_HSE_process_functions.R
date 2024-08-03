# functions for HSE processing

binarySex <- function(data){
  for (i in 1:nrow(data)){
    if(data$sex[i] == 1){
      data$sex[i] = FALSE #man
    }
    else if(data$sex[i] == 2)
    {
      data$sex[i] = TRUE #woman 
    }
  }
  return(data)
}

econStatus <- function(data){
  for (i in 1:nrow(data)){
    if(is.na(data$WrkStat[i])){ # WrkStat is econstat originally
      data$WrkStat[i] = data$WrkStat[i]
    }
    else if(is.na(data$topQual[i])){ # WrkStat is econstat originally
      data$topQual[i] = data$topQual[i]
    }
    else if(data$WrkStat[i] == 4 && data$topQual[i] == 8){
      data$WrkStat[i] = 4
    }
    else if(data$WrkStat[i] == 4 && data$topQual[i] != 8){
      data$WrkStat[i] = 2
    }
    else if(data$WrkStat[i] == 3){
      data$WrkStat[i] = 3
    }
    else if(data$WrkStat[i] == 2){
      data$WrkStat[i] = 2
    }
    else if(data$WrkStat[i] == 1){
      data$WrkStat[i] = 1
    }
    else
      data$WrkStat[i] = data$WrkStat[i]
  }
  return(data)
}

prepMARtest <- function(data){
  HSEMARTest <- HSE
  
  for(i in 1:nrow(HSEMARTest)){
    if(is.na(HSEMARTest$sayDiet[i])){
      HSEMARTest$sayDiet[i] = 1
    }
    else if(!is.na(HSEMARTest$sayDiet[i])){
      HSEMARTest$sayDiet[i] = 0
    }
  }
  for(i in 1:nrow(HSEMARTest)){
    if(is.na(HSEMARTest$sayWgt[i])){
      HSEMARTest$sayWgt[i] = 1
    }
    else if(!is.na(HSEMARTest$sayWgt[i])){
      HSEMARTest$sayWgt[i] = 0
    }
  }
  
  # Below trying to make it simpler in a loop
  # colsMAR <- c('surveyYear', 'age', 'sex', 'topQual', 'wrkStat', 'eqv3', 'ghq12scr')
  # #colsMAR <- array(surveyYear, age, sex, topQual, wrkStat, eqv, ghq12scr)
  # TestAgainst <- HSE[, colsMAR]
  # desired_length <- length(colsMAR) # or whatever length you want
  # fitDiet <- vector(mode = "list", length = desired_length)
  # 
  # for (i in 1:length(colsMAR)){
  #   fitDiet[i] <- with(HSE, lm(sayDiet[i] ~ noquote(colsMAR[i])))
  # }
  
  fitDietSurveyYear <- with(data, lm(sayDiet ~ surveyYear))
  fitDietAge <- with(data, lm(sayDiet ~ age))
  fitDietSex <- with(data, lm(sayDiet ~ sex))
  fitDietTopQual <- with(data, lm(sayDiet ~ topQual))
  fitDietWrkStat <- with(data, lm(sayDiet ~ wrkStat))
  fitDietEqv3 <- with(data, lm(sayDiet ~ eqv3))
  fitDietGHQ12 <- with(data, lm(sayDiet ~ ghq12scr))
  
  pValSurveyYear <- anova(fitDietSurveyYear)$'Pr(>F)'[1]
  pValAge <- anova(fitDietAge)$'Pr(>F)'[1]
  pValSex <- anova(fitDietSex)$'Pr(>F)'[1]
  pValTopQual <- anova(fitDietTopQual)$'Pr(>F)'[1]
  pValWrkStat <- anova(fitDietWrkStat)$'Pr(>F)'[1]
  pValEqv3 <- anova(fitDietAge)$'Pr(>F)'[1]
  pValGHQ12 <- anova(fitDietGHQ12)$'Pr(>F)'[1]
  
  DietMAR <- c(pValSurveyYear, pValAge, pValSex, pValTopQual, pValWrkStat, pValEqv3, pValGHQ12)
  
  return(DietMAR)
}

BMIquintiles <- function(data){
  for(i in 1:nrow(data)){
    if(is.na(data$bmival[i])){
      data$bmival[i] = NA
    }
    else if(data$bmival[i] < 18.5){
      data$bmival[i] = 1
    }
    else if(data$bmival[i] >= 18.5 && data$bmival[i] < 25){
      data$bmival[i] = 2
    }
    else if(data$bmival[i] >= 25 && data$bmival[i] < 30){
      data$bmival[i] = 3
    }
    else if(data$bmival[i] >= 30 && data$bmival[i] < 40){
      data$bmival[i] = 4
    }
    else if(data$bmival[i] >= 40){
      data$bmival[i] = 5
    }
    else
      data$bmival = NA
  }
  return(data)
}

recodeEthnicity <- function(data){
  for(i in 1:nrow(data)){
    if(data$surveyYear[i] == 1 || data$surveyYear[i] == 2 || data$surveyYear[i] == 3 || data$surveyYear[i] == 5){
      if(is.na(data$origin[i])){
        data$origin[i] = NA
      }
      else if(data$origin[i] >= 1 && data$origin[i] <= 4){
        data$origin[i] = 1
      }
      else if(data$origin[i] >= 5 && data$origin[i] <= 8){
        data$origin[i] = 2
      }
      else if(data$origin[i] >= 9 && data$origin[i] <= 13){
        data$origin[i] = 4
      }
      else if(data$origin[i] >= 14 && data$origin[i] <= 16){
        data$origin[i] = 3
      }
      else if(data$origin[i] == 17 || data$origin[i] == 18){
        data$origin[i] = 5
      }
      else
        data$origin[i] = NA
    }
    else if(data$surveyYear[i] == 7 || data$surveyYear[i] == 9 || data$surveyYear[i] == 11){
      if(is.na(data$origin[i])){
        data$origin[i] = NA
      }
      else if(data$origin[i] == 2){
        data$origin[i] = 3
      }
      else if(data$origin[i] ==3){
        data$origin[i] = 4
      }
      else if(data$origin[i] == 4){
        data$origin[i] = 2
      }
      else
        data$origin[i] = NA
    }
  }
  return(data)
}