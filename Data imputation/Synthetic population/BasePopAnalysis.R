# This script is to take a more detailed look at the base population

# impute values of external eating ("automaticity") and restraint ("PBC") and emotional eating 
library(haven)
library(tidyverse)
library(readxl)
library(ipfp)
library(rFSA)


wd <- ("C:/Users/ruair/OneDrive - sheffield.ac.uk/Fourth Year/Project/")
setwd(wd)

GLondon <- read.csv("Data/NDNS/GLondon_basepop_1000.csv")

DesireOnly <- GLondon[, c("Sex", "emotional", "external", "restraint", "automaticity", "autonomy")]

splitSex <- split(DesireOnly, DesireOnly$Sex)
female <- splitSex$f
male <- splitSex$m

female <- subset(female, select = -Sex)
male <- subset(male, select = -Sex)

femaleMeans <- colMeans(female)
maleMeans <- colMeans(male)

# Combine the arrays into a single data frame
comparison_df <- data.frame(female = femaleMeans, male = maleMeans)

# Convert row names to a column
comparison_df$Category <- rownames(comparison_df)
rownames(comparison_df) <- NULL

# Convert the data frame to long format
library(tidyr)
comparison_df_long <- pivot_longer(comparison_df, cols = c(female, male),
                                   names_to = "Array", values_to = "Value")

# Plot the comparison
library(ggplot2)
ggplot(comparison_df_long, aes(x = Category, y = Value, fill = Array)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of the sexes", x = "Variable", y = "Mean value") +
  scale_fill_manual(values = c("red", "blue")) +  # Customize fill colors
  theme_minimal()  # Optional: Customize plot theme
