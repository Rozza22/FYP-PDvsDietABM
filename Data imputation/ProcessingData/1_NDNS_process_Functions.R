# finishing touches and recoding of NDNS - functions

OnlyGHQyrs <- function(data){ # Making the way we see what year the survey was the same as in HSE
  data <- subset(data, SurveyYear == 1 | SurveyYear == 2 | SurveyYear == 3 | SurveyYear == 5 | SurveyYear == 7 | SurveyYear == 9 | SurveyYear == 11)
}

# DuplicateForJoining <- function(HFSS, demo){
#   participantMealCount <- as.data.frame(table(HFSS$seriali)) # counts number of meals recorded by each participant 
#   listOfMealCount <- as.array(participantMealCount$Freq) # puts frequency of this in list form
#   
#   demo <- demo %>% slice(rep(1:n(), times = c(listOfMealCount))) # replicates all the data the correct amount of times
#   
#   
#   HFSS <- HFSS$HFSS
#   demo <- cbind(demo, HFSS)
#   
#   return(demo)
# }

AgeBands <- function(data){ # This function takes the exact age of each participant and puts it into 10 year bands
  for (i in 1:nrow(data)){  # Bands which are the same as those seen in HSE dataset
    # if (data$age[i] >= 16 && data$age[i] <= 24){
    #   data$age[i] = 1
    # }
    # else if (data$age[i] >= 25 && data$age[i] <= 34){
    #   data$age[i] = 2
    # }
    # else if (data$age[i] >= 35 && data$age[i] <= 44){
    #   data$age[i] = 3
    # }
    # else if (data$age[i] >= 45 && data$age[i] <= 54){
    #   data$age[i] = 4
    # }
    # else if (data$age[i] >= 55 && data$age[i] <= 64){
    #   data$age[i] = 5
    # }
    # else if (data$age[i] >= 65 && data$age[i] <= 74){
    #   data$age[i] = 6
    # }
    # else if (data$age[i] >= 75){
    #   data$age[i] = 7
    # }
    # else if (data$age[i] >= 0 && data$age[i] <= 15){ # AKA if a child
    #   data$age[i] = 0
    # }
    # else
    #   data$age[i] = data$age[i]
  }
  age_groups <- cut(data$age, 
                    breaks = c(-Inf, 0, 15, 24, 34, 44, 54, 64, 74, Inf), 
                    labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8))
  
  # Assign age group values to data$age
  data$age <- as.numeric(age_groups) - 1  # Convert factor to numeric and adjust labels
  
  # Handle NA values
  data$age[is.na(data$age)] <- NA
  return(data)
}

IncomeTertiles <- function(data) {
  # Filter data for years 1, 2, and 3
  year1 <- filter(data, SurveyYear == 1)
  year2 <- filter(data, SurveyYear == 2)
  year3 <- filter(data, SurveyYear == 3)
  
  # Separate other years from years 1, 2, and 3
  untouched <- !(data$SurveyYear %in% c(1, 2, 3))
  NDNSuntouched <- filter(data, untouched)
  
  # Calculate tertiles for each year
  tertileYr1 <- quantile(year1$eqvinc, probs = seq(0, 1, 1/3), na.rm = TRUE)
  tertileYr2 <- quantile(year2$eqvinc, probs = seq(0, 1, 1/3), na.rm = TRUE)
  tertileYr3 <- quantile(year3$eqvinc, probs = seq(0, 1, 1/3), na.rm = TRUE)
  
  # Assign incomes to each bracket
  year1$eqv3 <- cut(year1$eqvinc, breaks = tertileYr1, labels = FALSE, include.lowest = TRUE)
  year2$eqv3 <- cut(year2$eqvinc, breaks = tertileYr2, labels = FALSE, include.lowest = TRUE)
  year3$eqv3 <- cut(year3$eqvinc, breaks = tertileYr3, labels = FALSE, include.lowest = TRUE)
  
  # Combine years and untouched data
  CalcTertilesYrs <- rbind(year1, year2, year3)
  Alleqv3NDNSYears <- rbind(CalcTertilesYrs, NDNSuntouched)
  
  # Remove redundant columns
  data <- Alleqv3NDNSYears[, !(names(Alleqv3NDNSYears) %in% c("eqvinc", "seriali"))]
  
  return(data)
}

# IncomeTertiles <- function(data){
#   # calculate eqv3 for years 1, 2 and 3 -----------------------------
#   
#   year1 <- filter(data, data$SurveyYear == 1) # Splits data into these three years as each year will have different income distribution
#   year2 <- filter(data, data$SurveyYear == 2) # Necessary as inflation would play a part in how much money was worth at each moment in time
#   year3 <- filter(data, data$SurveyYear == 3) # These three years the only years in NDNS survey where eqv3 doesn't exist
#   
#   # This part of code separates the other years which aren't going to be changed in this function
#   untouched <- list(nrow(data)) # initialising list of what will be boolean values to say whether we want to separate or not
#   
#   for (i in 1:nrow(data)){
#     if (data$SurveyYear[i] == 1 || data$SurveyYear[i] == 2 || data$SurveyYear[i] == 3){ # we want to separate other years from these three
#       untouched[i] = FALSE
#     }
#     else
#       untouched[i] = TRUE
#   }
#   
#   NDNSuntouched <- filter(data, untouched == TRUE) # The separated years that we aren't editing at all here
#   
#   tertileYr1 <- unname(quantile(year1$eqvinc, probs = seq(0, 1, 1/3))) # Establish tertiles for each year
#   tertileYr2 <- unname(quantile(year2$eqvinc, probs = seq(0, 1, 1/3)))
#   tertileYr3 <- unname(quantile(year3$eqvinc, probs = seq(0, 1, 1/3)))
#   
#   for (i in 1:nrow(year1)){ # Following for loops use the tertile boundaries to assign incomes to each bracket
#     if (year1$eqvinc[i] > tertileYr1[1] && year1$eqvinc[i] <= tertileYr1[2]){ # Same coding as the other years
#       year1$eqv3[i] = 1
#     }
#     else if (year1$eqvinc[i] > tertileYr1[2] && year1$eqvinc[i] <= tertileYr1[3]){ # This could change depending on whether I should use tertiles in HSE or not
#       year1$eqv3[i] = 2
#     }
#     else if (year1$eqvinc[i] > tertileYr1[3] && year1$eqvinc[i] <= tertileYr1[4]){
#       year1$eqv3[i] = 3
#     }
#   }
#   
#   for (i in 1:nrow(year2)){
#     if (year2$eqvinc[i] > tertileYr2[1] && year2$eqvinc[i] <= tertileYr3[2]){
#       year2$eqv3[i] = 1
#     }
#     else if (year2$eqvinc[i] > tertileYr1[2] && year2$eqvinc[i] <= tertileYr1[3]){
#       year2$eqv3[i] = 2
#     }
#     else if (year2$eqvinc[i] > tertileYr1[3] && year2$eqvinc[i] <= tertileYr1[4]){
#       year2$eqv3[i] = 3
#     }
#   }
#   
#   for (i in 1:nrow(year3)){
#     if (year3$eqvinc[i] > tertileYr1[1] && year3$eqvinc[i] <= tertileYr1[2]){
#       year3$eqv3[i] = 1
#     }
#     else if (year3$eqvinc[i] > tertileYr1[2] && year3$eqvinc[i] <= tertileYr1[3]){
#       year3$eqv3[i] = 2
#     }
#     else if (year3$eqvinc[i] > tertileYr1[3] && year3$eqvinc[i] <= tertileYr1[4]){
#       year3$eqv3[i] = 3
#     }
#   }
#   
#   CalcTertilesYrs <- rbind(year1, year2, year3) # Join three years together with eqv3 column now filled out
#   Alleqv3NDNSYears <- rbind(CalcTertilesYrs, NDNSuntouched) # reconnecting the years with eqvinc measures to the ones without
#   
#   # data <- Alleqv3NDNSYears
#   
#   data <- subset(Alleqv3NDNSYears, select = -c(eqvinc, seriali)) # removes redundant eqvinc
#   
#   return(data)
# }

binarySex <- function(data){
  for (i in 1:nrow(data)){
    if(is.na(data$Sex[i])){
      data$Sex[i] = data$Sex[i]
    }
    else if(data$Sex[i] == 1){
      data$Sex[i] = FALSE #man
    }
    else if(data$Sex[i] == 2)
    {
      data$Sex[i] = TRUE #woman
    }
  }
  return(data)
}

HFSSbands <- function(data, data_demo){
  
  # data <- NDNS_HFSS # For testing purposes
  # data_demo <- NDNS_demo
  
  participantMealCount <- as.data.frame(table(data$seriali)) # counts number of meals recorded by each participant 
  listOfMealCount <- as.array(participantMealCount$Freq) # puts frequency of this in list form
  listOfAvgDayHFSScals <- array(nrow(participantMealCount))
  
  for(i in 1:nrow(participantMealCount)){ #one loop per person
    Day1T = 0
    Day2T = 0
    Day3T = 0
    Day4T = 0
    for(j in 1:participantMealCount$Freq[i]){ # loops through the persons meals
      if(data$DayNo[j] == 1 && data$HFSS[j] == 1){
        Day1T = Day1T + data$Energykcal[j]
      }
      else if(data$DayNo[j] == 2 && data$HFSS[j] == 1){
        Day2T = Day2T + data$Energykcal[i]
      }
      else if(data$DayNo[j] == 3 && data$HFSS[j] == 1){
        Day3T = Day3T + data$Energykcal[i]
      }
      else if(data$DayNo[j] == 4 && data$HFSS[i] == 1){
        Day4T = Day4T + data$Energykcal[j]
      }
    }
    listOfAvgDayHFSScals[i] = (Day1T + Day2T + Day3T + Day4T)/4
  }
  
  quintileHFSScals <- unname(quantile(listOfAvgDayHFSScals, probs = seq(0, 1, 1/5)))
  QuintHFSScalsHist <- array(length(listOfAvgDayHFSScals))
  
  for (i in 1:length(listOfAvgDayHFSScals)){ # Following for loops use the tertile boundaries to assign incomes to each bracket
    if (listOfAvgDayHFSScals[i] > quintileHFSScals[1] && listOfAvgDayHFSScals[i] <= quintileHFSScals[2]){ # Same coding as the other years
      QuintHFSScalsHist[i] = 1
    }
    else if (listOfAvgDayHFSScals[i] > quintileHFSScals[2] && listOfAvgDayHFSScals[i] <= quintileHFSScals[3]){ # This could change depending on whether I should use tertiles in HSE or not
      QuintHFSScalsHist[i] = 2
    }
    else if (listOfAvgDayHFSScals[i] > quintileHFSScals[3] && listOfAvgDayHFSScals[i] <= quintileHFSScals[4]){
      QuintHFSScalsHist[i] = 3
    }
    else if (listOfAvgDayHFSScals[i] > quintileHFSScals[4] && listOfAvgDayHFSScals[i] <= quintileHFSScals[5]){
      QuintHFSScalsHist[i] = 4
    }
    else if (listOfAvgDayHFSScals[i] > quintileHFSScals[5] && listOfAvgDayHFSScals[i] <= quintileHFSScals[6]){
      QuintHFSScalsHist[i] = 5
    }
  }
  
  dfQuintHFSScals <- as.data.frame(QuintHFSScalsHist)
  data <- cbind(data_demo, dfQuintHFSScals)
  
  return(data)
}

recodeEconStat <- function(data){
  for(i in 1:nrow(data)){
    if(is.na(data$WrkStat[i]) || data$WrkStat[i] <= 0){ # Decide where education (3) lands
      data$WrkStat[i] = NA
    }
    else if(data$WrkStat[i] == 1){ 
      data$WrkStat[i] = 4 # education
    }
    else if(data$WrkStat[i] == 2){
      data$WrkStat[i] = 1 # in employment
    }
    else if(data$WrkStat[i] == 3 && data$age[i] <= 6 && data$age[i] >= 1){
      data$WrkStat[i] = 2 # unemployed
    }
    else if(data$WrkStat[i] == 3 && data$age[i] >= 6){
      data$WrkStat[i] = 3 # retired
    }
  }
  return(data)
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