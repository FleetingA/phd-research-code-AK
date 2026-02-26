
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date: 27/03/2025
-------------------------------------------------------------------------
  
# Load libraries
library(dplyr)
library(psych)
library(readr)
library(broom)
library(car) # for Anova()
library(emmeans)

# Load data
merged_extended_progressors <- read_tsv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended_progressors.tsv",
                                        show_col_types = FALSE)

### Descriptive Statistics Across Imaging Files

phenotypes_260325 <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/phenotypes_260325.csv")
View(phenotypes_260325)

# Check number of non-missing observations
sum(!is.na(phenotypes_260325$x25781_2_0)) # 45,013

sum(!is.na(phenotypes_260325$x25781_3_0)) # 4763

sum(!is.na(phenotypes_260325$x25781_2_0) & !is.na(phenotypes_260325$x25781_3_0)) # 4572

sum(!is.na(phenotypes_260325$x31_0_0)) # 502359
sum(!is.na(phenotypes_260325$x21022_0_0)) # 502359

## Create a subset with complete cases for the specified variables

# Filter subjects with non-missing values in both variables
filtered_data <- phenotypes_260325 %>%
  filter(!is.na(x25781_2_0), !is.na(x25781_3_0))

traitofinterest_cleaned <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/traitofinterest_cleaned.csv")
View(traitofinterest_cleaned)

# Step 1: Ensure userId column is consistent across both datasets

# Step 2: Filter the data
filtered_data <- phenotypes_260325 %>%
  filter(!is.na(x25781_2_0),
         !is.na(x25781_3_0),
         userId %in% traitofinterest_cleaned$userId)

# Step 3: Get descriptive statistics for all phenotype variables
summary(filtered_data)

# Confirm the number of rows matches your expected number (4329)
nrow(filtered_data) # 4329

## Run descriptives

# Age
describe(filtered_data$x21022_0_0) 
hist(filtered_data$x21022_0_0) # not normally distributed

# Computing IQR
IQR(filtered_data$x21022_0_0, na.rm = TRUE)
# Compute Q1 and Q3 manually
Q1 <- quantile(filtered_data$x21022_0_0, 0.25, na.rm = TRUE)
Q3 <- quantile(filtered_data$x21022_0_0, 0.75, na.rm = TRUE)

# Calculate IQR
Q1
Q3
IQR_age <- Q3 - Q1
IQR_age

# Count instances for that variable
sum(!is.na(filtered_data$x21022_0_0)) # 4329

# Sex
table(filtered_data$x31_0_0) 
sum(!is.na(filtered_data$x31_0_0)) # 4329 instances

# SBP, automated
describe(filtered_data$x4080_2_0) 
hist(filtered_data$x4080_2_0) # normally distributed
sum(!is.na(filtered_data$x4080_2_0)) # 3129 instances

# DBP, automated
describe(filtered_data$x4079_2_0) 
hist(filtered_data$x4079_2_0) # normally distributed
sum(!is.na(filtered_data$x4079_2_0)) # 3129 instances

# BMI
describe(filtered_data$x21001_2_0) 
hist(filtered_data$x21001_2_0) # mostly normally distributed
sum(!is.na(filtered_data$x21001_2_0)) # 4120 instances

# Current tobacco smoking
table(filtered_data$x1239_2_0) 
sum(!is.na(filtered_data$x1239_2_0)) # 4554 instances

# Alcohol intake frequency
table(filtered_data$x1558_2_0) 
sum(!is.na(filtered_data$x1558_2_0)) # 4312 instances

# Total volume of WMHs at TP1
summary(filtered_data$x25781_2_0) 
hist(filtered_data$x25781_2_0) # very skewed
# Computing IQR
IQR(filtered_data$x25781_2_0, na.rm = TRUE)
sum(!is.na(filtered_data$x25781_2_0))

# Total volume of WMHs at TP2
summary(filtered_data$x25781_3_0) 
hist(filtered_data$x25781_3_0) # very skewed as well 
# Computing IQR
IQR(filtered_data$x25781_3_0, na.rm = TRUE)
sum(!is.na(filtered_data$x25781_3_0))

# Inspect variables for which the pipeline did not run successfully
class(filtered_data$x6349_2_0)
class(filtered_data$x6350_2_0)
class(phenotypes_260325$x6349_2_0)
class(phenotypes_260325$x6350_2_0)
describe(phenotypes_260325$x6350_2_0)
table(phenotypes_260325$x6350_2_0)
table(phenotypes_260325$x20127_0_0)
describe(phenotypes_260325$x6349_2_0)
View(filtered_data$x6350_2_0)

describe(phenotypes_260325$x2443_2_0)
table(phenotypes_260325$x2443_2_0)

sum(!is.na(filtered_data$x2443_2_0))

# Save this subset 
write.csv(filtered_data, file = "filtered_data.csv", row.names = FALSE)
View(phenotypes_4572)

confounderfile_fully_adjusted <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/confounderfile_fully_adjusted.csv")
View(confounderfile_fully_adjusted)

# Exploring variables
table(confounderfile_fully_adjusted$x31_0_0)
describe(confounderfile_fully_adjusted$x4935_2_0)
table(phenotypes_260325$x4935_2_0)

