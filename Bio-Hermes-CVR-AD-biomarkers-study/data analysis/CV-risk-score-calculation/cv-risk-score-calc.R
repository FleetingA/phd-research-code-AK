
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 22/10/2024
----------------------------------------------------------------------------------
  
### CV Risk Score Calculation Using RiskScorescvd

# Load packages
install.packages("RiskScorescvd")
library(RiskScorescvd)
library(readr)

# Load data
final_merged_dataset_170924 <- read_csv("Angelina_Bio_Hermes_R_code/Univariate_Regressions_Added_070924/final_merged_dataset_170924.csv")
View(final_merged_dataset_170924)

## Convert Cholesterol units first

# Convert HDL and Total cholesterol from mg/dL to mmol/L
final_merged_dataset_170924$HDL_Cholesterol_mmolL <- final_merged_dataset_170924$HDL_Cholesterol * 0.02586
final_merged_dataset_170924$Total_cholesterol_mmolL <- final_merged_dataset_170924$Total_cholesterol * 0.02586

# View the first few rows to confirm the conversion
head(final_merged_dataset_170924[, c("HDL_Cholesterol", "HDL_Cholesterol_mmolL", "Total_cholesterol", "Total_cholesterol_mmolL")])

View(final_merged_dataset_170924)

# New Ethnicity variable 

table(final_merged_dataset_170924$RACE)
# Create a new Ethnicity variable based on RACE
final_merged_dataset_170924$Ethnicity_RiskScorescvd <- dplyr::case_when(
final_merged_dataset_170924$RACE == "WHITE" ~ "white",
final_merged_dataset_170924$RACE == "BLACK OR AFRICAN AMERICAN" ~ "black",
final_merged_dataset_170924$RACE == "ASIAN" ~ "asian",
final_merged_dataset_170924$RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ "other",
final_merged_dataset_170924$RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "other",
final_merged_dataset_170924$RACE == "UNKNOWN" ~ "other",
TRUE ~ "other"  # For any unexpected values
)

# View the first few rows to confirm the new Ethnicity variable
head(final_merged_dataset_170924[, c("RACE", "Ethnicity_RiskScorescvd")])

table(final_merged_dataset_170924$Ethnicity_RiskScorescvd)

# Create a new Gender variable based on SEX
final_merged_dataset_170924$Gender <- dplyr::case_when(
  final_merged_dataset_170924$SEX == "M" ~ "male",
  final_merged_dataset_170924$SEX == "F" ~ "female"
)

table(final_merged_dataset_170924$Gender)

# Check the column names
colnames(final_merged_dataset_170924)

# View the first few rows of the dataset to check if columns exist as expected
head(final_merged_dataset_170924)

# Check data types
str(final_merged_dataset_170924$Ethnicity_RiskScorescvd)

# Check unique values in the Ethnicity column
unique(final_merged_dataset_170924$Ethnicity_RiskScorescvd)

# Rename variables to match what the function expects
cohort_BH <- final_merged_dataset_170924  # Make a copy to work on

cohort_BH <- cohort_BH %>%
  rename(
    Gender = Gender,                       # Gender (male/female)
    Ethnicity = Ethnicity_RiskScorescvd,    # Ethnicity (white, black, etc.)
    Age = AGE,                             # Age (numeric)
    total.chol = Total_cholesterol_mmolL,   # Total cholesterol in mmol/L
    total.hdl = HDL_Cholesterol_mmolL,      # HDL cholesterol in mmol/L
    systolic.bp = Systolic_Blood_Pressure,  # Systolic BP
    hypertension = ongoing_med_for_HBP,     # Hypertension treatment (1/0)
    smoker = Current_smoking_binary,        # Smoker (1/0)
    diabetes = Diabetes_status              # Diabetes (1/0)
  )

# Call the ASCVD_scores function
result <- ASCVD_scores(data = cohort_BH, classify = TRUE)

# Print the summary of results
summary(result$ASCVD_score)
summary(result$ASCVD_strat)  # ASCVD classification

# Add ASCVD score and classification to original dataset
final_merged_dataset_170924$ASCVD_score <- result$ASCVD_score
final_merged_dataset_170924$ASCVD_classification <- result$ASCVD_strat

# View the first few rows to confirm
head(final_merged_dataset_170924[, c("ASCVD_score", "ASCVD_classification")])

summary(final_merged_dataset_170924$ASCVD_score)
summary(final_merged_dataset_170924$ASCVD_classification)

# Load ggplot2 library for visualisations
library(ggplot2)

# Create a bar plot using ggplot2 to visualise ASCVD classification
ggplot(final_merged_dataset_170924, aes(x = ASCVD_classification)) +
  geom_bar(fill = "green4", color = "grey3") +
  labs(title = "Distribution of ASCVD Classification",
       x = "ASCVD Classification",
       y = "Count") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14), # Increase x-axis title size
    axis.title.y = element_text(size = 14), # Increase y-axis title size
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
    )

## Recode ASCVD score into a categorical one (for exploratory purposes)

# Load dplyr for case_when
library(dplyr)

# Create a new numeric variable based on ASCVD_classification
final_merged_dataset_170924 <- final_merged_dataset_170924 %>%
  mutate(ASCVD_classification_numeric = case_when(
    ASCVD_classification == "Very low risk" ~ 1,
    ASCVD_classification == "Low risk" ~ 2,
    ASCVD_classification == "Moderate risk" ~ 3,
    ASCVD_classification == "High risk" ~ 4,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))

# Create a new categorical variable based on AD status
final_merged_dataset_170924 <- final_merged_dataset_170924 %>%
  mutate(AD_status_cat = case_when(
    QVAL == "Cohort 1 (Healthy)" ~ 0,
    QVAL == "Cohort 2 (MCI)" ~ 1,
    QVAL == "Cohort 3 (Probable AD)" ~ 2,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))

# Save data files
write.csv(final_merged_dataset_170924, "final_merged_dataset_170924.csv", row.names = FALSE)
write.csv(cohort_BH, "cohort_BH_ASCVD.csv", row.names = FALSE)

