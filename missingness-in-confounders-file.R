
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date: 08/04/2025
--------------------------------------------------------------------------

### Checking missingness in confounder file 
  
# Load libraries
library(readr)
library(psych)

# Load data
confounderfile_fully_adjusted <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/confounderfile_fully_adjusted.csv")
View(confounderfile_fully_adjusted)

# Data descriptions for relevant confounders
describe(confounderfile_fully_adjusted$x2443_2_0)
table(confounderfile_fully_adjusted$x2443_2_0)

table(confounderfile_fully_adjusted$x6153_2_0)
table(confounderfile_fully_adjusted$x6177_2_0)

# Step 1: Filter Date_Differences_in_UKB_with_day_diff for non-missing values
valid_dates <- Date_Differences_in_UKB_with_day_diff[
  !is.na(Date_Differences_in_UKB_with_day_diff$x25781_2_0) &
    !is.na(Date_Differences_in_UKB_with_day_diff$x25781_3_0), 
]

# Step 2: Get the relevant user IDs
valid_user_ids <- unique(valid_dates$userId)

# Step 3: Filter confounderfile_fully_adjusted to those user IDs
confounder_filtered <- confounderfile_fully_adjusted[
  confounderfile_fully_adjusted$userId %in% valid_user_ids, 
]

# Step 4: Check distributions
table(confounder_filtered$x6153_2_0)
table(confounder_filtered$x6177_2_0)

# Optional: visualise
hist(confounder_filtered$x6153_2_0, main = "Distribution of x6153_2_0", xlab = "x6153_2_0")
hist(confounder_filtered$x6177_2_0, main = "Distribution of x6177_2_0", xlab = "x6177_2_0")

phenotypes_260325 <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/phenotypes_260325.csv")
View(phenotypes_260325)

table(phenotypes_260325$x6150_2_0)

confounderfile_fully_adjusted <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/confounderfile_fully_adjusted.csv")
table(confounderfile_fully_adjusted$x6150_2_0)