
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date: 03/04/2025
-------------------------------------------------------------------------
  
### Insulin UKB Data - Transformations

# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)
library(lubridate)
library(psych)

# Load CSV files
insulin_data_UKB <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/UKB Data 2025/insulin.csv")
View(insulin_data_UKB)
Date_Differences <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/UKB Data 2025/Date_Differences_in_Days_Only.csv")
View(Date_Differences)

# Convert date columns to Date format
insulin_data_UKB$x130706_0_0 <- as.Date(insulin_data_UKB$x130706_0_0, format = "%d-%b-%y")
View(insulin_data_UKB)

## Further formatting needed
# Extract the year as numeric
years <- year(insulin_data_UKB$x130706_0_0)

# Extract the year from your date column
years <- year(insulin_data_UKB$x130706_0_0)

# Identify incorrect dates greater than 2020, explicitly handling NAs
incorrect_dates <- !is.na(years) & years > 2020

# Subtract 100 years safely from dates identified as incorrect
year(insulin_data_UKB$x130706_0_0[incorrect_dates]) <- years[incorrect_dates] - 100

# Check it has worked out
View(insulin_data_UKB)

# Moving on to the date differences file
Date_Differences$x53_2_0 <- as.Date(Date_Differences$x53_2_0, format = "%d/%m/%Y")
View(Date_Differences)

# Merge datasets by 'userId'

# ---make sure merged_data is loaded---
merged_data <- inner_join(insulin_data_UKB, Date_Differences, by = "userId")
View(merged_data)
table(merged_data$x130706_0_0)
table(merged_data$x53_2_0)

# Create new variable 'x130706new_0_0'
merged_data <- merged_data %>%
  mutate(x130706new_0_0 = if_else(x130706_0_0 < x53_2_0, 1, 0))
View(merged_data)

describe(merged_data$x130706new_0_0)
table(merged_data$x130706new_0_0)

# Save the final dataset if needed
write.csv(merged_data, "C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/UKB Data 2025/Merged_Data_with_new_variable.csv", row.names = FALSE)
write.csv(insulin_data_UKB, "C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/UKB Data 2025/insulin.csv", row.names = FALSE)
write.csv(Date_Differences, "C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/UKB Data 2025/Date_Differences.csv", row.names = FALSE)

# Double-checking ID overlap
insulin <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/UKB Data 2025/insulin.csv")
Date_Differences <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/UKB Data 2025/Date_Differences.csv")

# Find overlapping userIds
user_ids_date <- unique(Date_Differences$userId)
user_ids_insulin <- unique(insulin$userId)
common_user_ids <- intersect(user_ids_date, user_ids_insulin)
length(common_user_ids)
# Count the number of overlaps
length(common_user_ids)

# Find the common user IDs
common_user_ids <- intersect(unique(Date_Differences$userId), unique(insulin$userId))

# Filter rows for common user IDs
date_common <- Date_Differences[Date_Differences$userId %in% common_user_ids, ]
insulin_common <- insulin[insulin$userId %in% common_user_ids, ]

# Check for non-missing values
valid_date <- date_common[!is.na(date_common$x53_2_0), ]
valid_insulin <- insulin_common[!is.na(insulin_common$x130706_0_0), ]

# Get user IDs that are in both non-missing subsets
final_ids <- intersect(valid_date$userId, valid_insulin$userId)

# Count how many
length(final_ids) # only 28, too few 

