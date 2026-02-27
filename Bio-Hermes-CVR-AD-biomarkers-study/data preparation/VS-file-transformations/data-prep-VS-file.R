
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 29/07/2024
----------------------------------------------------------------------------------
  
### Data Preparation Steps
## VS stands for 'vital signs'

# Load libraries
library(readr)
library(dplyr)
library(tidyr)

# Load data
VS <- read_csv("Bio_Hermes_Data/Bio_Hermes_Clinical_Data/VS.csv")
View(VS)

# Select only columns needed for analysis
VS_selected <- VS %>% select(USUBJID, VSTEST, VSORRES)
View(VS_selected)

# Filter the data to include only rows with BP data in the VSTEST column
VS_selected_BP <- VS_selected %>%
  dplyr::filter(VSTEST %in% c("Systolic Blood Pressure", "Diastolic Blood Pressure"))
View(VS_selected_BP)

# Convert data from long to wide format 
VS_selected_BP_wide <- VS_selected_BP %>%
  pivot_wider(names_from = VSTEST, values_from = c(VSORRES)) %>%
  drop_na() # Remove rows with any NAs

View(VS_selected_BP_wide)

# Save new data frame
write.csv(VS_selected_BP_wide, "VS_selected_BP_wide.csv", row.names = FALSE) # final BP file to use

