# distribution of habit update interval

rm(list = ls()) # clear environment

# To plot the output of a single model - will eventually be for the optimal model parameters

# setting up files for calibration for PHASE project 
library(dplyr)
library(lhs)
library(truncnorm)
library(yaml)
library(tidyverse)

setwd("//wsl.localhost/Ubuntu/home/ruairi/ABM_code/abmcodeupdated/ABM_code - with PD")

data <- read.csv("props/GLondon_basepop_1000.csv")

# updateInterval <- as.data.frame(data$habit.update.interval)
# names(updateInterval)[names(updateInterval) == "data$habit.update.interval"] <- "time"

interval <- data$habit.update.interval

intervalSummary <- summary(interval)
intervalMean <- mean(interval)

# Density plot of the column
plot(density(interval), main = "Density Plot of habit update intervals", xlab = "Habit update window (n days)", ylab = "Density")
abline(v = intervalMean, col = "red")
