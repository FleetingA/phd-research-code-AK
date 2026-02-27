
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 05/08/2024
----------------------------------------------------------------------------------
  
### Data Preparation Steps
  
# Load libraries
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load data
# Dataset 1
final_CM_HBP_med_widesr_short <- read_csv("Angelina_Bio_Hermes_R_code/CM_file_Transformations/final_CM_HBP_med_widesr_short.csv")
View(final_CM_HBP_med_widesr_short)

# Dataset 2
DM_selected_2 <- read_csv("Angelina_Bio_Hermes_R_code/DM_file_Transformations/DM_selected_2.csv")
View(DM_selected_2)

# Dataset 3
final_LB_selected_allc_wide <- read_csv("Angelina_Bio_Hermes_R_code/LB_file_Transformations/final_LB_selected_allc_wide.csv")
View(final_LB_selected_allc_wide)

# Merge data frames on USUBJID
CVDRS_data <- final_CM_HBP_med_widesr_short %>%
  inner_join(DM_selected_2, by = "USUBJID") %>%
  inner_join(final_LB_selected_allc_wide, by = "USUBJID")

# View the merged data frame
View(CVDRS_data)

# Save the merged data frame 
write.csv(CVDRS_data, "CVDRS_data.csv", row.names = FALSE)

# Also load smoking data
final_SU_tobacco_recoded <- read_csv("Angelina_Bio_Hermes_R_code/SU_file_Transformations/final_SU_tobacco_recoded.csv")
View(final_SU_tobacco_recoded)

# Merge CVDRS_data with final_SU_tobacco_recoded on USUBJID
final_CVDRS_data <- CVDRS_data %>%
  inner_join(final_SU_tobacco_recoded, by = "USUBJID")

# View the merged data frame
View(final_CVDRS_data)

# Save final merged data frame 
write.csv(final_CVDRS_data, "final_CVDRS_data.csv", row.names = FALSE)

