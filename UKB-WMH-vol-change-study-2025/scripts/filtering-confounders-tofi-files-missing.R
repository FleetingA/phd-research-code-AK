
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes

# Author: Angelina Kancheva 
# Date: 08/04/2025
-------------------------------------------------------------------------
  
### Filtering of Confounders and Trait of Interest files

# Load libraries
library(readr)

# Load data
confounderfile_fully_adjusted <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/confounderfile_fully_adjusted.csv")
View(confounderfile_fully_adjusted)

# Filter confounderfile_fully_adjusted for non-missing x25781_2_0
filtered_confounder <- confounderfile_fully_adjusted[
  !is.na(confounderfile_fully_adjusted$x25781_2_0) &
    confounderfile_fully_adjusted$userId %in% traitofinterest_cleaned$userId, ]

# Before transforming
table(confounderfile_fully_adjusted$x6153_2_0)

# -7       -3    -1   1     2      3     4     5 
# 23021    37   116  6424  3945    38  2253   200 

# After transforming
table(confounderfile_fully_adjusted$x6153_2_0) # 0=8915 1=3945

# Before transforming
table(confounderfile_fully_adjusted$x6177_2_0)

# -7       -3    -1     1     2     3 
# 17977    36   160  11780  3829    47 

# After transforming
table(confounderfile_fully_adjusted$x6177_2_0) # 0=30000 1=3829

# Filter traitofinterest_cleaned for non-missing x25781_3_0
valid_users <- traitofinterest_cleaned[
  !is.na(traitofinterest_cleaned$x25781_3_0), "userId"]

# Now match again using filtered valid user IDs
final_filtered <- filtered_confounder[filtered_confounder$userId %in% valid_users, ]

# Check for x2443_2_0
table(filtered_confounder$x2443_2_0) # 0=5975 1=326
table(final_filtered$x2443_2_0) # nothing!

# Check for x6150_2_0
table(filtered_confounder$x6150_2_0) # 0=53 1=241
table(final_filtered$x6150_2_0) # nothing!

# Check for x6153_2_0
table(filtered_confounder$x6153_2_0) # 0=2087 1=609 2=335 3=4 4=197 5=21
table(final_filtered$x6153_2_0) # nothing!

# Check for x31_0_0
table(filtered_confounder$x31_0_0) # 0=23754 1=21016
table(final_filtered$x31_0_0) # nothing!

# Check for x21022_0_0
table(filtered_confounder$x21022_0_0) # many
table(final_filtered$x21022_0_0) # nothing!

# Check for x6177_2_0
table(filtered_confounder$x6177_2_0) # 0=1621 1=1130 2=311 3=1 
table(final_filtered$x6177_2_0) # nothing!

# Check overlapping IDs between x25781_2_0 in the confounders file and x25781_3_0 in the outcome cleaned file

# Step 1: Filter for non-missing x25781_2_0 and x25781_3_0
confounder_filtered <- confounderfile_fully_adjusted[!is.na(confounderfile_fully_adjusted$x25781_2_0), ]
trait_filtered <- traitofinterest_cleaned[!is.na(traitofinterest_cleaned$x25781_3_0), ]

# Step 2: Get userId vectors from both
userIds_confounder <- confounder_filtered$userId
userIds_trait <- trait_filtered$userId

# Step 3: Count the number of shared userIds
length(intersect(userIds_confounder, userIds_trait)) # 4329, all good 

# Step 1: Filter confounderfile_fully_adjusted for non-missing in all 5 variables
confounder_filtered <- confounderfile_fully_adjusted[
  !is.na(confounderfile_fully_adjusted$x25781_2_0) &
    !is.na(confounderfile_fully_adjusted$x2443_2_0) &
    !is.na(confounderfile_fully_adjusted$x6153_2_0) &
    !is.na(confounderfile_fully_adjusted$x6177_2_0) &
    !is.na(confounderfile_fully_adjusted$x6150_2_0), ] # 0 overlapping

confounder_filtered <- confounderfile_fully_adjusted[
  !is.na(confounderfile_fully_adjusted$x25781_2_0) &
    !is.na(confounderfile_fully_adjusted$x2443_2_0), ] # only 637 overlapping

confounder_filtered <- confounderfile_fully_adjusted[
  !is.na(confounderfile_fully_adjusted$x25781_2_0) &
    !is.na(confounderfile_fully_adjusted$x6153_2_0) &
    !is.na(confounderfile_fully_adjusted$x6177_2_0), ] # only 267 overlapping

confounder_filtered <- confounderfile_fully_adjusted[
  !is.na(confounderfile_fully_adjusted$x25781_2_0) & 
    !is.na(confounderfile_fully_adjusted$x6150_2_0), ] # only 26 overlapping

# Step 2: Filter traitofinterest_cleaned for non-missing x25781_3_0
trait_filtered <- traitofinterest_cleaned[!is.na(traitofinterest_cleaned$x25781_3_0), ]

# Step 3: Get userId vectors
userIds_confounder <- confounder_filtered$userId
userIds_trait <- trait_filtered$userId

# Step 4: Count shared userIds
length(intersect(userIds_confounder, userIds_trait))

# Check smoking
confounderfile_smoking_SBP_DBP <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/confounderfile_smoking_SBP_DBP.csv")

table(confounderfile_smoking_SBP_DBP$x20116_2_0)


