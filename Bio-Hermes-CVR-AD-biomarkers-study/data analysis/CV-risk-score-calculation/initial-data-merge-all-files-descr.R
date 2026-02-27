### ---------- Big Merge of Data - 08/09/2024 ----------

## Load all data frames before merging into one big dataset ##

# Load CVDRS data
library(readr)
final_CVDRS_data <- read_csv("Angelina_Bio_Hermes_R_code/CVD_Risk_Score_Calculation/final_CVDRS_data.csv")
View(final_CVDRS_data)

# Load vital signs data
VS_selected_BP_wide <- read_csv("Angelina_Bio_Hermes_R_code/VS_file_Transformations/VS_selected_BP_wide.csv")
View(VS_selected_BP_wide)

# Load predictors and outcome data
merged_pred_outcome_120824 <- read_csv("Angelina_Bio_Hermes_R_code/PCA_ANOVA_T-tests/merged_pred_outcome_120824.csv")
View(merged_pred_outcome_120824)

# Load the dplyr package to prepare data for merge
library(dplyr)

# Merge the datasets using inner_join (keeps only rows that have matches in all datasets)
final_dataset <- final_CVDRS_data %>%
  inner_join(VS_selected_BP_wide, by = "USUBJID") %>%
  inner_join(merged_pred_outcome_120824, by = "USUBJID")

# View the merged dataset
View(final_dataset)

# Optional: If you want to check for missing data, you can use:
sum(is.na(final_dataset))  # Total number of missing values in the final dataset

# Save the merged final dataset as a CSV file
write_csv(final_dataset, "final_merged_dataset_080924.csv")

# Run descriptives on dataset
hist(final_dataset$AGE) # roughly normally distributed
hist(final_dataset$HDL_Cholesterol) # very skewed
hist(final_dataset$LDL_Cholesterol) # skewed
hist(final_dataset$Triglycerides) # relatively skewed
hist(final_dataset$Total_cholesterol2) # skewed
hist(final_dataset$`Systolic Blood Pressure`) # closer to normal
hist(final_dataset$`Diastolic Blood Pressure`) # very skewed

summary(final_dataset$AGE)
summary(final_dataset$`Systolic Blood Pressure`)

# Calculate standard deviation of SBP
std_dev <- sd(final_dataset$`Systolic Blood Pressure`, na.rm = TRUE)
print(std_dev)

# Do the same for DBP
summary(final_dataset$`Diastolic Blood Pressure`)
std_dev2 <- sd(final_dataset$`Diastolic Blood Pressure`, na.rm = TRUE)
print(std_dev2)

# Run descriptive statistics (mean, median, sd, min, max) for numerical columns
descriptive_stats <- final_dataset %>%
  summarise(across(where(is.numeric), list(
    mean = mean, 
    median = median, 
    sd = sd, 
    min = min, 
    max = max
  ), na.rm = TRUE))

# View the output
View(descriptive_stats)

# Frequencies for categorical variables
# Sex
table(final_dataset$SEX)
table(final_dataset$SEX_recode)

# Ongoing medication for HBP
table(final_dataset$ongoing_med_for_HBP)

# Race
table(final_dataset$RACE)
table(final_dataset$RACE_recode)

# Ethnicity
table(final_dataset$ETHNIC)
table(final_dataset$ETHNIC_recode)

# Smoking 
table(final_dataset$Current_smoking_binary)

# Run additional regression analyses with covariates

logistic_model1 <- glm(AD_status ~ TauP181_Roche_logtr + TauP217_Lilly_logtr + AMYLB42_MerckGAP 
                       + AGE + SEX_recode, 
                      data = final_dataset, 
                      family = binomial)

# View the summary of the model
summary(logistic_model1)

# Obtain ORs
exp(coef(logistic_model1))

### New file merge - 13/12/2024 ###

# Load final MH and CM files transformed in December 2024
library(readr)
final_CM_HBP_med_widesr_short <- read_csv("Angelina_Bio_Hermes_R_code/CM_file_Transformations/final_CM_HBP_med_widesr_short.csv")
View(final_CM_HBP_med_widesr_short) # 964 entries

MH_final_131224 <- read_csv("Angelina_Bio_Hermes_R_code/MH_file_Transformations/MH_final_131224.csv")
View(MH_final_131224)

# Filter out only participants where Diabetes status = 1
library(dplyr)
diabetes_positive <- MH_final_131224 %>%
  filter(Diabetes_status_new == 1) %>%
  distinct(USUBJID)
# View the result
print(diabetes_positive, n=Inf)

# Put in a separate vector (only diabetes positive)
usubjid_vector <- c(
  "BIO-HERMES-00101-046",
  "BIO-HERMES-00101-095",
  "BIO-HERMES-00101-099",
  "BIO-HERMES-00101-103",
  "BIO-HERMES-00102-003",
  "BIO-HERMES-00102-004",
  "BIO-HERMES-00102-009",
  "BIO-HERMES-00102-020",
  "BIO-HERMES-00102-024",
  "BIO-HERMES-00102-028",
  "BIO-HERMES-00102-036",
  "BIO-HERMES-00102-068",
  "BIO-HERMES-00102-074",
  "BIO-HERMES-00102-076",
  "BIO-HERMES-00102-083",
  "BIO-HERMES-00103-020",
  "BIO-HERMES-00103-025",
  "BIO-HERMES-00103-060",
  "BIO-HERMES-00104-005",
  "BIO-HERMES-00105-012",
  "BIO-HERMES-00105-016",
  "BIO-HERMES-00105-021",
  "BIO-HERMES-00105-028",
  "BIO-HERMES-00105-038",
  "BIO-HERMES-00105-043",
  "BIO-HERMES-00105-048",
  "BIO-HERMES-00105-049",
  "BIO-HERMES-00105-066",
  "BIO-HERMES-00105-068",
  "BIO-HERMES-00105-085",
  "BIO-HERMES-00105-086",
  "BIO-HERMES-00105-087",
  "BIO-HERMES-00105-090",
  "BIO-HERMES-00105-110",
  "BIO-HERMES-00106-004",
  "BIO-HERMES-00106-018",
  "BIO-HERMES-00106-032",
  "BIO-HERMES-00106-072",
  "BIO-HERMES-00106-079",
  "BIO-HERMES-00107-019",
  "BIO-HERMES-00107-031",
  "BIO-HERMES-00107-037",
  "BIO-HERMES-00107-039",
  "BIO-HERMES-00107-043",
  "BIO-HERMES-00107-051",
  "BIO-HERMES-00109-003",
  "BIO-HERMES-00109-071",
  "BIO-HERMES-00109-073",
  "BIO-HERMES-00110-003",
  "BIO-HERMES-00110-015",
  "BIO-HERMES-00110-018",
  "BIO-HERMES-00110-064",
  "BIO-HERMES-00110-068",
  "BIO-HERMES-00111-014",
  "BIO-HERMES-00111-033",
  "BIO-HERMES-00111-042",
  "BIO-HERMES-00111-044",
  "BIO-HERMES-00111-046",
  "BIO-HERMES-00112-021",
  "BIO-HERMES-00112-028",
  "BIO-HERMES-00112-039",
  "BIO-HERMES-00112-042",
  "BIO-HERMES-00112-043",
  "BIO-HERMES-00112-050",
  "BIO-HERMES-00112-053",
  "BIO-HERMES-00113-002",
  "BIO-HERMES-00113-011",
  "BIO-HERMES-00114-001",
  "BIO-HERMES-00114-002",
  "BIO-HERMES-00114-011",
  "BIO-HERMES-00114-015",
  "BIO-HERMES-00114-020",
  "BIO-HERMES-00115-002",
  "BIO-HERMES-00115-005"
)

# Add Diabetes status variable to CM file, setting it to 1 for matched USUBJIDs and 0 otherwise
final_CM_HBP_med_widesr_short$Diabetes_status <- ifelse(final_CM_HBP_med_widesr_short$USUBJID %in% usubjid_vector, 1, 0)
View(final_CM_HBP_med_widesr_short) # 964 entries in file

# Save file
write.csv(final_CM_HBP_med_widesr_short, "final_CM_Diabetes_131224.csv", row.names = FALSE)

# Merge all final files
VS_selected_BP_wide <- read_csv("Angelina_Bio_Hermes_R_code/VS_file_Transformations/VS_selected_BP_wide.csv")
View(VS_selected_BP_wide) # 1,004 entries in file

final_SU_tobacco_recoded <- read_csv("Angelina_Bio_Hermes_R_code/SU_file_Transformations/final_SU_tobacco_recoded.csv")
View(final_SU_tobacco_recoded) # 1,005 entries in file 

CM_VS_SU_merged <- final_CM_HBP_med_widesr_short %>%
  inner_join(VS_selected_BP_wide, by = "USUBJID") %>%
  inner_join(final_SU_tobacco_recoded, by = "USUBJID")
View(CM_VS_SU_merged) # 964 entries

# Save file
write.csv(CM_VS_SU_merged, "CM_VS_SU_merged_131224.csv", row.names = FALSE)

# Merge CM_VS_SU file with DM file
CM_VS_SU_merged_131224 <- read_csv("Angelina_Bio_Hermes_R_code/Rerunning_Analyses_ASCVD_13122024/CM_VS_SU_merged_131224.csv")

# Load DM file
DM_selected_2 <- read_csv("Angelina_Bio_Hermes_R_code/DM_file_Transformations/DM_selected_2.csv")
View(DM_selected_2) # 1,001 entries in this file 

# Merge files
Complete_demographic_file_131224 <- CM_VS_SU_merged_131224 %>%
  inner_join(DM_selected_2, by = "USUBJID")
View(Complete_demographic_file_131224) # 961 entries in this file

# Save file
write.csv(Complete_demographic_file_131224, "Complete_demographic_file_131224.csv", row.names = FALSE)

# Merge Roche, Lilly, outcomes file and complete demographic file

# Final outcomes file: 
outcome_binary_0sand1s <- read_csv("Angelina_Bio_Hermes_R_code/Outcome_for_Analysis/outcome_binary_0sand1s.csv")
View(outcome_binary_0sand1s) # 1,001 entries in this file

# Final Lilly file (predictors): 
Lilly_to_merge290724 <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/Lilly_to_merge290724.csv")
View(Lilly_to_merge290724) # 809 entries in this file 

# Final Roche file (predictors):
RocheBH_to_merge290724 <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/RocheBH_to_merge290724.csv")
View(RocheBH_to_merge290724) # 993 entries in this file 

# Merge all files but lab tests
Complete_Dataset_Roche_Lilly_131224 <- Complete_demographic_file_131224 %>%
  inner_join(Lilly_to_merge290724, by = "USUBJID") %>%
  inner_join(RocheBH_to_merge290724, by = "USUBJID") %>%
  inner_join(outcome_binary_0sand1s, by = "USUBJID") 
View(Complete_Dataset_Roche_Lilly_131224) # 775 entries

# Save file
write.csv(Complete_Dataset_Roche_Lilly_131224, "Complete_Dataset_Roche_Lilly_131224.csv", row.names = FALSE)

Complete_Dataset_Roche_Lilly_131224 <- read_csv("Angelina_Bio_Hermes_R_code/Rerunning_Analyses_ASCVD_13122024/Complete_Dataset_Roche_Lilly_131224.csv")
View(Complete_Dataset_Roche_Lilly_131224)

# Rename SBP and DBP variables 
library(data.table)
setnames(Complete_Dataset_Roche_Lilly_131224, old = "Systolic Blood Pressure", new = "Systolic_Blood_Pressure")
setnames(Complete_Dataset_Roche_Lilly_131224, old = "Diastolic Blood Pressure", new = "Diastolic_Blood_Pressure")

# Merge all files + lab tests
final_LB_selected_allc_wide <- read_csv("Angelina_Bio_Hermes_R_code/LB_file_Transformations/final_LB_selected_allc_wide.csv")
View(final_LB_selected_allc_wide) # 961 entries in this file

Complete_Dataset_Roche_Lilly_13122024 <- Complete_Dataset_Roche_Lilly_131224 %>%
  inner_join(final_LB_selected_allc_wide, by = "USUBJID")
View(Complete_Dataset_Roche_Lilly_13122024) # 745 entries in this file
  
# Save file
write.csv(Complete_Dataset_Roche_Lilly_13122024, "Complete_Dataset_Roche_Lilly_13122024.csv", row.names = FALSE)