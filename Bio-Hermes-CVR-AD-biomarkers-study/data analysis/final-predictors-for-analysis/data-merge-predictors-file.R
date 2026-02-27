
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 24/07/2024
----------------------------------------------------------------------------------

### Data Merging for Predictors

# Loading necessary libraries
library(readr)
library(dplyr)

# Loading three datasets to merge
final_MBH_pg_mL_ABonly <- read_csv("Angelina_Bio_Hermes_R_code/Merck_GAP_Transformations/final_MBH_pg_mL_ABonly.csv", 
                                   show_col_types = FALSE)
View(final_MBH_pg_mL_ABonly)

final_MBH_ABonly_merge <- final_MBH_pg_mL_ABonly %>% select(USUBJID, AB40, AB42, AB4240, AB4240_logtr)
View(final_MBH_ABonly_merge)

# Save file
write.csv(final_MBH_ABonly_merge, "final_MBH_ABonly_merge.csv", row.names = FALSE) 
View(final_MBH_ABonly_merge)

final_QBH_wide_no_NAs <- read_csv("Angelina_Bio_Hermes_R_code/Quanterix_Transformations/final_QBH_wide_no_NAs.csv", 
                                  show_col_types = FALSE)
View(final_QBH_wide_no_NAs)
                            
# Save file
write.csv(final_QBH_wide_no_NAs, "final_QBH_wide_no_NAs.csv", row.names = FALSE)

final_QBH_merge <- final_QBH_wide_no_NAs %>% select(USUBJID, PTAU181, PTAU181_logtr)
View(final_QBH_merge)

# Save file
write.csv(final_QBH_merge, "final_QBH_merge.csv", row.names = FALSE)
View(final_QBH_merge)

final_Lilly_BH_PTAU217_no_NAs <- read_csv("Angelina_Bio_Hermes_R_code/Lilly_MSD_Transformations/final_Lilly_BH_PTAU217_no_NAs.csv", 
                                          show_col_types = FALSE)
View(final_Lilly_BH_PTAU217_no_NAs)

final_Lilly_merge <- final_Lilly_BH_PTAU217_no_NAs %>% select(USUBJID, PTAU217, PTAU217_logtr)
View(final_Lilly_merge)

# Save file
write.csv(final_Lilly_merge, "final_Lilly_merge.csv", row.names = FALSE)

# Merge the data frames
predictors_merged <- final_MBH_ABonly_merge %>%
  merge(final_QBH_merge, by = "USUBJID", all = TRUE) %>%
  merge(final_Lilly_merge, by = "USUBJID", all = TRUE)

# View the merged data
print(predictors_merged)
View(predictors_merged)

# Save file with predictors
write.csv(predictors_merged, "predictors_merged.csv", row.names = FALSE)

predictors_merged <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/predictors_merged.csv")
View(predictors_merged)

# Check if all values in the column 'USUBJID' are unique
all_unique <- !any(duplicated(predictors_merged$USUBJID))

# Print the result
if (all_unique) {
  print("All values in the column USUBJID are unique.")
} else {
  print("There are duplicate values in the column USUBJID.")
}