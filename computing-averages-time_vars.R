
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
 
# Author: Angelina Kancheva 
# Date: 05/05/2025
--------------------------------------------------------------------------

### Computing Averages for Time Variables

# Load libraries
library(psych)
library(readr)
library(dplyr)
  
# Load data
Date_Differences_in_Days_Only <- read.csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/UKB Data 2025/Date_Differences_in_Days_Only.csv")
View(Date_Differences_in_Days_Only)

# Inspect differences in days
describe(Date_Differences_in_Days_Only$xDays_2)
hist(Date_Differences_in_Days_Only$xDays_2)

# Mean and standard deviation in days (from your describe() output)
mean_days <- 1113.53
sd_days <- 496.47

# Convert days to years and months
mean_years <- mean_days / 365.25
sd_years <- sd_days / 365.25

mean_months <- mean_days / 30.44
sd_months <- sd_days / 30.44

# Display results, rounded
mean_years
sd_years
mean_months
sd_months

# Rounded example
cat("Mean follow-up:", round(mean_years, 2), "years (SD:", round(sd_years, 1), "years)\n")
cat("Mean follow-up:", round(mean_months, 1), "months (SD:", round(sd_months, 1), "months)\n")

# Check the range of WMH_volume
range(Date_Differences_in_Days_Only$xDays_2, na.rm = TRUE)

