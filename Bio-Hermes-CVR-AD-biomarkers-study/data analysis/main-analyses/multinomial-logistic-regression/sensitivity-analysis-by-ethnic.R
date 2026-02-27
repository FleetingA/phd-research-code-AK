
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 
  
# Date: 07/11/2024
----------------------------------------------------------------------------------
  
# Load packages
library(readr)
library(dplyr)

### Sensitivity Analyses - Split by ethnicity

# Load data
final_merged_dataset_170924 <- read_csv(final_merged_dataset_170924, "final_merged_dataset_170924.csv", row.names = FALSE)

table(final_merged_dataset_170924$ETHNIC_recode)

# Split data into two ethnic groups
hispanic_data <- final_merged_dataset_170924 %>% filter(ETHNIC_recode == 1)
non_hispanic_data <- final_merged_dataset_170924 %>% filter(ETHNIC_recode == 0)

## Model for Hispanic or Latino group - replace name of biomarker accordingly 

# First example
model_hispanic <- glm(AD_status ~ ApoE4_Roche_logtr_c * ASCVD_score_sqrtc, data = hispanic_data, family = binomial)
summary(model_hispanic)

pseudo_r2 <- pR2(model_hispanic)
print(pseudo_r2)

# Model for Non-Hispanic or Latino group - replace name of biomarker accordingly 
model_non_hispanic <- glm(AD_status ~ ApoE4_Roche_logtr_c * ASCVD_score_sqrtc, data = non_hispanic_data, 
                          family = binomial)
summary(model_non_hispanic)
pseudo_r2 <- pR2(model_non_hispanic)
print(pseudo_r2)

odds_ratios <- exp(coef(model_non_hispanic))
print(odds_ratios)

