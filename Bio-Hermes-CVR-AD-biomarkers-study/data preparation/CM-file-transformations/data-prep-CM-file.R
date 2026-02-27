
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 21/07/2024
----------------------------------------------------------------------------------
  
### Data Preparation Steps
## CM stands for 'current medications'
  
# Load libraries
library(readr)
library(dplyr)

# Load data
CM <- read_csv("Bio_Hermes_Data/Bio_Hermes_Clinical_Data/CM.csv", show_col_types = FALSE) # change path to file
View(CM)

# Select only columns needed for analysis
CM_selected <- CM %>% select(STUDYID, DOMAIN, USUBJID, CMTRT, CMDECOD, CMINDC, CMDOSE, 
                             CMDOSU, CMDOSFRQ, CMENRTPT)
View(CM_selected)

# Check content options for variable CMENRTPT 
table(CM_selected$CMENRTPT)
CM_selected %>% count(CMENRTPT)

# Recode values in the CMENRTPT column
CM_selected <- CM_selected %>%
  mutate(CMENRTPT = recode(CMENRTPT, "BEFORE" = 0, "ONGOING" = 1)) # ongoing medication

# Verify the recoding
table(CM_selected$CMENRTPT)
View(CM_selected)

# Check content options for variable CMTRT 
table(CM_selected$CMTRT)

# Output all the unique contents of the CMINDC variable
unique_CMINDC <- unique(CM_selected$CMINDC)
print(unique_CMINDC, max.print=TRUE)

# Define conditions related to HBP 
conditions <- c("Essentail Hypertension", 
                "essential hypertension", 
                "Benign Essential hypertension", 
                "Essential hypertension", 
                "Elevated blood pressure",
                "Essential Hypertension", 
                "HBP", 
                "Heart Health/ Hypertension", 
                "high blood pressure", 
                "blood pressure", 
                "High blood pressure", 
                "High Blood Pressure", 
                "Hyertension", 
                "HTN", 
                "Hyperension", 
                "Hypertenstion", 
                "hypertension", 
                "Hypertension", 
                "HYPERTENSION",
                "benign hypertension", 
                "Blood pressure",
                "Blood Pressure", 
                "Heart Health/ Hypertension", 
                "Hypertension/ Heart Health", 
                "Blood Pressure/ Afib", 
                "Hypertension Prevention", 
                "Preventative Hypertension",
                "Elevated Blood Pressure", 
                "benign essential hypertension", 
                "Hypertesion",
                "Benign Essential Hypertension", 
                "Benign essential hypertension", 
                "Hyperytension", 
                "PVC and High Blood Pressure",
                "Hyperension",
                "Hypertension Fluid retention", 
                "Hypertension & tremors", 
                "Hypertension, Type 2 diabetes, hyperlipidemia", 
                "Hypertenssion", 
                "Hypertesion",
                  "Hypertension", "HYPERTENSION", "hypertension", "hypertension", "High blood pressure", "High Blood Pressure", 
                  "Hypertention", "Pulmonary hypertension", "Elevated blood pressure", 
                  "Essential hypertension", "Benign hypertension", "Elevated Blood Pressure", 
                   "benign essential hypertension", 
                  "Essential Hypertension", "Hypertension", "essential hypertension", 
                  "Hyperension", "Benign Essential hypertension", "Benign essential hypertension", 
                  "Hypertension Fluid retention", "Hypertension & tremors", "HTN", "Hypertension", 
                  "high blood pressure", "Blood pressure", "Blood pressure/ Afib", "Blood Pressure", 
                  "Hypertension/ Heart Health", "Hypertension Prevention", "Heart Health/ Hypertension", 
                  "Preventative Hypertension", "Hypertension/ Heart Health", 
                  "Chronic essential hypertension", "Hypertension", "Hypertension", "Hypertension prevention", "High blood pressure")


# Create a new variable to indicate ongoing medication for hypertension
CM_selected <- CM_selected %>%
  mutate(
    ongoing_med_for_HBP = case_when(
      CMINDC %in% conditions & CMENRTPT == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Print the modified data frame
View(CM_selected)
dimensions <- dim(CM_selected)
print(dimensions)

# Extract only data related to high blood pressure from CM_selected
CM_HBP_med <- CM_selected %>%
  select(USUBJID, CMINDC, CMDOSE, CMDOSU, CMDOSFRQ, CMENRTPT, ongoing_med_for_HBP)

# View the extracted data frame
View(CM_HBP_med)
CM_HBP_med %>% count(ongoing_med_for_HBP)
CM_HBP_med %>% count(CMENRTPT)
CM_HBP_med %>% count(USUBJID)

# Convert long to wide format preserving ongoing_med_for_HBP=1 and rest=0
CM_HBP_med_wide <- CM_HBP_med %>%
  group_by(USUBJID) %>%
  filter(if(any(ongoing_med_for_HBP == 1)) ongoing_med_for_HBP == 1 else row_number() == 1) %>%
  ungroup()

# View the new data frame
View(CM_HBP_med_wide)
CM_HBP_med_wide %>% count(USUBJID)
CM_HBP_med_wide %>% count(ongoing_med_for_HBP)
CM_HBP_med_wide %>% count(CMENRTPT)

# Keep a single row of ongoing_med_for_HBP=1 
CM_HBP_med_widesr <- CM_HBP_med_wide %>%
  group_by(USUBJID) %>%
  # For each participant, keep one row with 1 if any, otherwise keep one row with 0
  summarize(
    USUBJID = first(USUBJID),
    CMINDC = first(CMINDC),
    CMDOSE = first(CMDOSE),
    CMDOSU = first(CMDOSU),
    CMENRTPT = first(CMENRTPT),
    ongoing_med_for_HBP = max(ongoing_med_for_HBP)
  ) %>%
  ungroup()

# View the new data frame
View(CM_HBP_med_widesr)

# Save only relevant information in this DF
CM_HBP_med_widesr_short <- CM_HBP_med_widesr %>%
  select(USUBJID, CMINDC, CMDOSE, CMDOSU, ongoing_med_for_HBP)
View(CM_HBP_med_widesr_short)

# Save data frames as CSV files
write.csv(CM_HBP_med_widesr_short, "CM_HBP_med_widesr_short.csv", row.names = FALSE)
write.csv(CM_HBP_med_widesr, "CM_HBP_med_widesr.csv", row.names = FALSE)
write.csv(CM_HBP_med_wide, "CM_HBP_med_wide.csv", row.names = FALSE)
write.csv(CM_HBP_med, "CM_HBP_med.csv", row.names = FALSE)
write.csv(CM_selected, "CM_selected.csv", row.names = FALSE)
