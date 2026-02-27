
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 28/07/2024
----------------------------------------------------------------------------------
  
### Data Preparation Steps
## SU stands for 'substance use'

# Load libraries
library(readr)
library(psych)
library(dplyr)

# Load data
SU <- read_csv("Bio_Hermes_Data/Bio_Hermes_Clinical_Data/SU.csv", show_col_types = FALSE)
View(SU)

# Select only columns needed for analysis
SU_selected <- SU %>% select(USUBJID, SUTRT, SUOCCUR, SUENDTC, SUENRTPT)
head(SU_selected)
View(SU_selected)

# Filter only the TOBACCO rows
SU_tobacco_data <- SU_selected %>%
  filter(SUTRT == "TOBACCO")
# View the filtered data
View(SU_tobacco_data)

# Data exploration 
table(SU_tobacco_data$SUOCCUR) 
table(SU_tobacco_data$SUENRTPT)

## DATA TRANSFORMATIONS

# Create new variable with specified coding
SU_tobacco_recoded <- SU_tobacco_data %>%
  mutate(
    Smoking = case_when(
      SUOCCUR == "N" ~ 0,
      SUOCCUR == "Y" & !is.na(SUENDTC) & is.na(SUENRTPT) ~ 1,
      SUOCCUR == "Y" & is.na(SUENDTC) & SUENRTPT == "ONGOING" ~ 2,
      TRUE ~ NA_real_ # To handle any other cases
    )
  )

View(SU_tobacco_recoded)
table(SU_tobacco_recoded$Smoking)

# Make 'Smoking' variable binary
SU_tobacco_recoded <- SU_tobacco_recoded %>%
  mutate(Current_smoking_binary = case_when(
    Smoking == "0" ~ "0",
    Smoking == "1" ~ "0",
    Smoking == "2" ~ "1",
    TRUE ~ "NA"  # This handles any unexpected values
  ))

table(SU_tobacco_recoded$Current_smoking_binary)
View(SU_tobacco_recoded)

# Select only columns for CVD calculator
final_SU_tobacco_recoded <- SU_tobacco_recoded %>% select(USUBJID, Smoking,
                                                          Current_smoking_binary)
View(final_SU_tobacco_recoded)

# Save datasets
write.csv(SU_selected, "SU_selected.csv", row.names = FALSE) # only columns needed for analysis
write.csv(SU_tobacco_data, "SU_tobacco_data.csv", row.names = FALSE) # only tobacco data
write.csv(SU_tobacco_recoded, "SU_tobacco_recoded.csv", row.names = FALSE) # tobacco data recoded
write.csv(final_SU_tobacco_recoded, "final_SU_tobacco_recoded.csv", row.names = FALSE) # tobacco data further recoded and trimmed
