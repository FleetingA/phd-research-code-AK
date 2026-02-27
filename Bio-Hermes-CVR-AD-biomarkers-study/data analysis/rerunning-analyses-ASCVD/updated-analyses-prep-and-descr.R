
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 
  
# Date: 13/12/2024
----------------------------------------------------------------------------------
  
### Re-running all analyses

# Load libraries
library(readr)
library(psych)
library(ggplot2)
library(dplyr)
library(RiskScorescvd) # for ASCVD risk score

# Load complete dataset 
Complete_Dataset_Roche_Lilly_13122024 <- read_csv("Angelina_Bio_Hermes_R_code/Rerunning_Analyses_ASCVD_13122024/Complete_Dataset_Roche_Lilly_13122024.csv")
View(Complete_Dataset_Roche_Lilly_13122024) # 775 entries

# Run some descriptives
describe(Complete_Dataset_Roche_Lilly_13122024[, c("AGE", "HDL_Cholesterol", "Total_cholesterol", "Systolic_Blood_Pressure", 
                                         "Diastolic_Blood_Pressure")])

## Identify outliers

# Find participants with values of 999 in LDL cholesterol
outliers <- Complete_Dataset_Roche_Lilly_13122024[
  Complete_Dataset_Roche_Lilly_13122024$LDL_Cholesterol >= 999, 
  c("USUBJID", "LDL_Cholesterol")
]

# Display the participants with these values
print(outliers)

# Summarise LDL chol variable
summary(Complete_Dataset_Roche_Lilly_13122024$LDL_Cholesterol)
hist(Complete_Dataset_Roche_Lilly_13122024$LDL_Cholesterol) # very skewed

# Impute LDL Cholesterol values of 999 with the median value of 87
Complete_Dataset_Roche_Lilly_13122024$LDL_Cholesterol[
  Complete_Dataset_Roche_Lilly_13122024$USUBJID %in% c("BIO-HERMES-00101-045",           
     "BIO-HERMES-00101-052", "BIO-HERMES-00101-092", "BIO-HERMES-00104-005", "BIO-HERMES-00105-017",            
     "BIO-HERMES-00109-055", "BIO-HERMES-00109-109", "BIO-HERMES-00109-116", "BIO-HERMES-00109-130",             
     "BIO-HERMES-00109-133", "BIO-HERMES-00109-139",             
     "BIO-HERMES-00109-148", "BIO-HERMES-00109-149",             
     "BIO-HERMES-00109-150", "BIO-HERMES-00109-153",            
     "BIO-HERMES-00109-154", "BIO-HERMES-00109-155",             
     "BIO-HERMES-00109-157"
  ) # 18 entries
] <- 87

# Summarise LDL chol variable again
summary(Complete_Dataset_Roche_Lilly_13122024$LDL_Cholesterol)
hist(Complete_Dataset_Roche_Lilly_13122024$LDL_Cholesterol) # distribution now normal

# Impute LDL chol value for below participants who had a value of 0 --> changed to 87
Complete_Dataset_Roche_Lilly_13122024$LDL_Cholesterol[Complete_Dataset_Roche_Lilly_13122024$USUBJID == "BIO-HERMES-00103-037"] <- 87

# Find participants with values of 999 in HDL cholesterol
outliers2 <- Complete_Dataset_Roche_Lilly_13122024[
  Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol >= 999, 
  c("USUBJID", "HDL_Cholesterol")
]

# Display the participants with these values
print(outliers2)

# Summarise HDL chol variable
summary(Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol)
hist(Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol) # very skewed

# Impute HDL Cholesterol values of 999 with the median value of 58
Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol[
  Complete_Dataset_Roche_Lilly_13122024$USUBJID %in% c("BIO-HERMES-00101-092",
                                                       "BIO-HERMES-00109-055",
                                                       "BIO-HERMES-00109-109",
                                                       "BIO-HERMES-00109-130",
                                                       "BIO-HERMES-00109-133",
                                                       "BIO-HERMES-00109-139",
                                                       "BIO-HERMES-00109-148",
                                                       "BIO-HERMES-00109-149",
                                                       "BIO-HERMES-00109-150",
                                                       "BIO-HERMES-00109-153",
                                                       "BIO-HERMES-00109-154",
                                                       "BIO-HERMES-00109-155"
                                                       ) # 12 entries
] <- 58

# Impute with median value for below participant who had a score of 3
Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol[Complete_Dataset_Roche_Lilly_13122024$USUBJID == "BIO-HERMES-00110-071"] <- 58

# Summarise HDL chol variable again
summary(Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol)
hist(Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol) # distribution now normal

# Summarise total chol variable
describe(Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol)
hist(Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol) # very skewed

# Find participants with Total Cholesterol values of 1000 or above
high_total_chol <- Complete_Dataset_Roche_Lilly_13122024[
  Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol >= 1000, 
  c("USUBJID", "Total_cholesterol")
]

# Display the participants with these values
print(high_total_chol)

# Impute Total Cholesterol values of 1000 or above with the median of 172.2
Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol[
  Complete_Dataset_Roche_Lilly_13122024$USUBJID %in% c(
    "BIO-HERMES-00101-045",
    "BIO-HERMES-00101-052",
    "BIO-HERMES-00101-092",
    "BIO-HERMES-00104-005",
    "BIO-HERMES-00105-017",
    "BIO-HERMES-00109-055",
    "BIO-HERMES-00109-109",
    "BIO-HERMES-00109-116",
    "BIO-HERMES-00109-130",
    "BIO-HERMES-00109-133",
    "BIO-HERMES-00109-139",
    "BIO-HERMES-00109-148",
    "BIO-HERMES-00109-149",
    "BIO-HERMES-00109-150",
    "BIO-HERMES-00109-153",
    "BIO-HERMES-00109-154",
    "BIO-HERMES-00109-155",
    "BIO-HERMES-00109-157"
    )
] <- 172.2 # 18 entries

# Summarise total chol variable again
describe(Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol)
hist(Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol) # distribution now normal

# Check for duplicates just in case
duplicate_ids <- Complete_Dataset_Roche_Lilly_13122024$USUBJID[duplicated(Complete_Dataset_Roche_Lilly_13122024$USUBJID)]

# If there are no duplicates
if (length(duplicate_ids) == 0) {
  print("All USUBJID entries are unique.")
} else {
  print("There are duplicate USUBJID entries.")
  print(duplicate_ids)
}

## All subjects are unique (n=745).

# Check DBP values

# BIO-HERMES-00102-002 - DBP is 8420 --> implausible
# BIO-HERMES-00109-162 - DBP is 801 --> implausible 

# Summarise DBP variable
summary(Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure)
hist(Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure) # incredibly skewed

# Impute DBP values for the below participants with very high values with the median of 78
Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure[
  Complete_Dataset_Roche_Lilly_13122024$USUBJID %in% c("BIO-HERMES-00102-002", "BIO-HERMES-00109-162")
] <- 78 # 2 entries

# Summarise DBP variable again
summary(Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure)
hist(Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure) # looks pretty normal now

# Check SBP values

# Summarise SBP variable
summary(Complete_Dataset_Roche_Lilly_13122024$Systolic_Blood_Pressure)
hist(Complete_Dataset_Roche_Lilly_13122024$Systolic_Blood_Pressure) # mostly normal

# There are a few participants whose DBP values should most likely be their SBP values:

# USUBJID == "BIO-HERMES-00102-048" should be 120
# USUBJID == "BIO-HERMES-00102-052" should be 121
# USUBJID == "BIO-HERMES-00102-049" should be 148
# USUBJID == "BIO-HERMES-00102-053" should be 133
# USUBJID == "BIO-HERMES-00102-058" should be 123

# Change the values
Complete_Dataset_Roche_Lilly_13122024$Systolic_Blood_Pressure[Complete_Dataset_Roche_Lilly_13122024$USUBJID == "BIO-HERMES-00102-048"] <- 120
Complete_Dataset_Roche_Lilly_13122024$Systolic_Blood_Pressure[Complete_Dataset_Roche_Lilly_13122024$USUBJID == "BIO-HERMES-00102-052"] <- 121
Complete_Dataset_Roche_Lilly_13122024$Systolic_Blood_Pressure[Complete_Dataset_Roche_Lilly_13122024$USUBJID == "BIO-HERMES-00102-049"] <- 148
Complete_Dataset_Roche_Lilly_13122024$Systolic_Blood_Pressure[Complete_Dataset_Roche_Lilly_13122024$USUBJID == "BIO-HERMES-00102-053"] <- 133
Complete_Dataset_Roche_Lilly_13122024$Systolic_Blood_Pressure[Complete_Dataset_Roche_Lilly_13122024$USUBJID == "BIO-HERMES-00102-058"] <- 123

# Check data frame again
View(Complete_Dataset_Roche_Lilly_13122024)

# BIO-HERMES-00102-048, BIO-HERMES-00102-052, BIO-HERMES-00102-058, BIO-HERMES-00102-053 and BIO-HERMES-00102-049
# also have identical SBP and DBP values right now --> their DBP values are the same as the systolic ones.
# Impute the DBP values with the median DBP value of 78.

summary(Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure)

Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure[Complete_Dataset_Roche_Lilly_13122024$USUBJID %in% 
                                                                 c("BIO-HERMES-00102-048", "BIO-HERMES-00102-052", "BIO-HERMES-00102-058",
                                                       "BIO-HERMES-00102-053", "BIO-HERMES-00102-049")
                                                       ] <- 78 

# Summarise DBP variable again
summary(Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure)
hist(Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure) # looks pretty normal now

---------------------------------
## DESCRIPTIVES - DEMOGRAPHICS:

table(Complete_Dataset_Roche_Lilly_13122024$SEX_recode) # 423 females, 322 males

hist(Complete_Dataset_Roche_Lilly_13122024$AGE) # normally distributed
summary(Complete_Dataset_Roche_Lilly_13122024$AGE) # mean=72.29
describe(Complete_Dataset_Roche_Lilly_13122024$AGE)
sd(Complete_Dataset_Roche_Lilly_13122024$AGE) # SD=6.60

table(Complete_Dataset_Roche_Lilly_13122024$Diabetes_status) # Yes=58, No=687

table(Complete_Dataset_Roche_Lilly_13122024$RACE_recode) 
# 0=645; 1=76; 2=14; 3=2; 4=1; 5=7

table(Complete_Dataset_Roche_Lilly_13122024$ETHNIC_recode)
# 0=655; 1=80; 2=10

table(Complete_Dataset_Roche_Lilly_13122024$Current_smoking_binary)
# 0=705; 1=40

table(Complete_Dataset_Roche_Lilly_13122024$AD_status)
# 0=301; 1=444

hist(Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure) # normally distributed
describe(Complete_Dataset_Roche_Lilly_13122024$Diastolic_Blood_Pressure) # mean=77.3; SD=9.01

hist(Complete_Dataset_Roche_Lilly_13122024$Systolic_Blood_Pressure) # normally distributed
describe(Complete_Dataset_Roche_Lilly_13122024$Systolic_Blood_Pressure) # mean=133.24; SD=16.06

hist(Complete_Dataset_Roche_Lilly_13122024$LDL_Cholesterol) # mostly normal
describe(Complete_Dataset_Roche_Lilly_13122024$LDL_Cholesterol) # mean=90.36; SD=34.32

hist(Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol) # mostly normal 
describe(Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol) # mean=59.32; SD=15.92

hist(Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol) # mostly normal
describe(Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol) # mean=176.42; SD=42.61

hist(Complete_Dataset_Roche_Lilly_13122024$ongoing_med_for_HBP) # just 0s and 1s
table(Complete_Dataset_Roche_Lilly_13122024$ongoing_med_for_HBP) # 0=335; 1=410

-------------------------------
## DESCRIPTIVES - BIOMARKERS:

# P-tau 217
hist(Complete_Dataset_Roche_Lilly_13122024$PTAU217) # very skewed
hist(Complete_Dataset_Roche_Lilly_13122024$PTAU217_logtr) # mostly skewed
describe(Complete_Dataset_Roche_Lilly_13122024$PTAU217_logtr) # median=-1.62; range=2.36 
# Center and scale P-tau 217 (continuous variable)
Complete_Dataset_Roche_Lilly_13122024$PTAU217_logtr_c <- scale(Complete_Dataset_Roche_Lilly_13122024$PTAU217_logtr, 
                                                           center = TRUE, scale = TRUE)
hist(Complete_Dataset_Roche_Lilly_13122024$PTAU217_logtr_c) # still not ideal but less skewed

# P-tau 181
hist(Complete_Dataset_Roche_Lilly_13122024$pTau181) # pretty skewed
hist(Complete_Dataset_Roche_Lilly_13122024$pTau181_logtr) # normally distributed
describe(Complete_Dataset_Roche_Lilly_13122024$pTau181_logtr) # mean=0.03; SD=0.44 
# Center and scale P-tau 181 (continuous variable)
Complete_Dataset_Roche_Lilly_13122024$pTau181_logtr_c <- scale(Complete_Dataset_Roche_Lilly_13122024$pTau181_logtr, 
                                                               center = TRUE, scale = TRUE)
hist(Complete_Dataset_Roche_Lilly_13122024$pTau181_logtr_c) # normally distributed

# ApoE4
hist(Complete_Dataset_Roche_Lilly_13122024$ApoE4) # incredibly skewed
hist(Complete_Dataset_Roche_Lilly_13122024$ApoE4_logtr) # patchy
describe(Complete_Dataset_Roche_Lilly_13122024$ApoE4_logtr) # mean=0.64; SD=1.83
# Log transform might not be the best for ApoE4

# Experiment with a square root transformation
Complete_Dataset_Roche_Lilly_13122024$ApoE4_sqrt <- sqrt(Complete_Dataset_Roche_Lilly_13122024$ApoE4)
hist(Complete_Dataset_Roche_Lilly_13122024$ApoE4_sqrt) # not as bad but still quite patchy
describe(Complete_Dataset_Roche_Lilly_13122024$ApoE4_sqrt) # mean=1.43; SD=1.74

# Center and scale sqrt ApoE4 (continuous variable)
Complete_Dataset_Roche_Lilly_13122024$ApoE4_sqrt_c <- scale(Complete_Dataset_Roche_Lilly_13122024$ApoE4_sqrt, 
                                                               center = TRUE, scale = TRUE)
hist(Complete_Dataset_Roche_Lilly_13122024$ApoE4_sqrt_c) # not great but less bad 

## Have to standardise the units of measurement of AB40 and AB42 before proceeding. 

# Convert AMYLB40 from ng/mL to pg/mL
Complete_Dataset_Roche_Lilly_13122024 <- Complete_Dataset_Roche_Lilly_13122024 %>%
  mutate(AMYLB40_pg_mL = AMYLB40 * 1000)

# Create a new variable with the ratio of AB42 to AB40 as this is currently missing
Complete_Dataset_Roche_Lilly_13122024$AB4240_ratio <- Complete_Dataset_Roche_Lilly_13122024$AMYLB42/Complete_Dataset_Roche_Lilly_13122024$AMYLB40_pg_mL
hist(Complete_Dataset_Roche_Lilly_13122024$AB4240_ratio) # mostly normally distributed
# Center and scale Ab42/40 (continuous variable)
Complete_Dataset_Roche_Lilly_13122024$AB4240_ratio_c <- scale(Complete_Dataset_Roche_Lilly_13122024$AB4240_ratio, 
                                                            center = TRUE, scale = TRUE)
hist(Complete_Dataset_Roche_Lilly_13122024$AB4240_ratio_c) # mostly normally distributed, not that much change
describe(Complete_Dataset_Roche_Lilly_13122024$AB4240_ratio_c)

# Save dataset so far
write.csv(Complete_Dataset_Roche_Lilly_13122024, "Complete_Dataset_Roche_Lilly_13122024.csv", row.names = FALSE)


## Convert Cholesterol units first

# Convert HDL and Total cholesterol from mg/dL to mmol/L
Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol_mmolL <- Complete_Dataset_Roche_Lilly_13122024$HDL_Cholesterol * 0.02586
Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol_mmolL <- Complete_Dataset_Roche_Lilly_13122024$Total_cholesterol * 0.02586

# View the first few rows to confirm the conversion
head(Complete_Dataset_Roche_Lilly_13122024[, c("HDL_Cholesterol", "HDL_Cholesterol_mmolL", "Total_cholesterol", "Total_cholesterol_mmolL")])
View(Complete_Dataset_Roche_Lilly_13122024)

# New Ethnicity variable 

table(Complete_Dataset_Roche_Lilly_13122024$RACE)
# Create a new Ethnicity variable based on RACE
Complete_Dataset_Roche_Lilly_13122024$Ethnicity_RiskScorescvd <- dplyr::case_when(
  Complete_Dataset_Roche_Lilly_13122024$RACE == "WHITE" ~ "white",
  Complete_Dataset_Roche_Lilly_13122024$RACE == "BLACK OR AFRICAN AMERICAN" ~ "black",
  Complete_Dataset_Roche_Lilly_13122024$RACE == "ASIAN" ~ "asian",
  Complete_Dataset_Roche_Lilly_13122024$RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ "other",
  Complete_Dataset_Roche_Lilly_13122024$RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "other",
  Complete_Dataset_Roche_Lilly_13122024$RACE == "UNKNOWN" ~ "other",
  TRUE ~ "other"  # For any unexpected values
)

# View the first few rows to confirm the new Ethnicity variable
head(Complete_Dataset_Roche_Lilly_13122024[, c("RACE", "Ethnicity_RiskScorescvd")])

table(Complete_Dataset_Roche_Lilly_13122024$Ethnicity_RiskScorescvd)

# Create a new Gender variable based on SEX
Complete_Dataset_Roche_Lilly_13122024$Gender <- dplyr::case_when(
  Complete_Dataset_Roche_Lilly_13122024$SEX == "M" ~ "male",
  Complete_Dataset_Roche_Lilly_13122024$SEX == "F" ~ "female"
)

table(Complete_Dataset_Roche_Lilly_13122024$Gender)

# Check the column names
colnames(Complete_Dataset_Roche_Lilly_13122024)

# Check unique values in the Ethnicity column
unique(Complete_Dataset_Roche_Lilly_13122024$Ethnicity_RiskScorescvd)

# Rename variables to match what the function expects
cohort_BH <- Complete_Dataset_Roche_Lilly_13122024  # Make a copy to work on

cohort_BH <- cohort_BH %>%
  rename(
    Gender = Gender,                        # Gender (male/female)
    Ethnicity = Ethnicity_RiskScorescvd,    # Ethnicity (white, black, etc.)
    Age = AGE,                              # Age (numeric)
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

# Add the ASCVD score and classification to the dataset
Complete_Dataset_Roche_Lilly_13122024$ASCVD_cont_score <- result$ASCVD_score
Complete_Dataset_Roche_Lilly_13122024$ASCVD_classification <- result$ASCVD_strat

# View the first few rows to confirm
head(Complete_Dataset_Roche_Lilly_13122024[, c("ASCVD_cont_score", "ASCVD_classification")])

summary(Complete_Dataset_Roche_Lilly_13122024$ASCVD_cont_score)
summary(Complete_Dataset_Roche_Lilly_13122024$ASCVD_classification)

# Create a bar plot using ggplot2 to visualize ASCVD classification
ggplot(Complete_Dataset_Roche_Lilly_13122024, aes(x = ASCVD_classification)) +
  geom_bar(fill = "cyan3", color = "grey3") +
  labs(title = "Distribution of ASCVD Classification",
       x = "ASCVD Classification",
       y = "Count") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14), # Increase x-axis title size
    axis.title.y = element_text(size = 14), # Increase y-axis title size
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  )

# Recode ASCVD score into a categorical variable
# Use dplyr for case_when

# Create a new numeric variable based on ASCVD_classification
Complete_Dataset_Roche_Lilly_13122024 <- Complete_Dataset_Roche_Lilly_13122024 %>%
  mutate(ASCVD_classification_numeric = case_when(
    ASCVD_classification == "Very low risk" ~ 1,
    ASCVD_classification == "Low risk" ~ 2,
    ASCVD_classification == "Moderate risk" ~ 3,
    ASCVD_classification == "High risk" ~ 4,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))

# Inspect distribution of ASCVD variable
hist(Complete_Dataset_Roche_Lilly_13122024$ASCVD_cont_score) # slightly skewed
Complete_Dataset_Roche_Lilly_13122024$ASCVD_cont_score_sqrt = sqrt(Complete_Dataset_Roche_Lilly_13122024$ASCVD_cont_score)
hist(Complete_Dataset_Roche_Lilly_13122024$ASCVD_cont_score_sqrt) #  normally distributed
# Center and scale ASCVD (continuous variable)
Complete_Dataset_Roche_Lilly_13122024$ASCVD_cont_score_sqrtc <- scale(Complete_Dataset_Roche_Lilly_13122024$ASCVD_cont_score_sqrt, 
                                                              center = TRUE, scale = TRUE)
hist(Complete_Dataset_Roche_Lilly_13122024$ASCVD_cont_score_sqrtc) # normally distributed

# Save dataset
write.csv(Complete_Dataset_Roche_Lilly_13122024, "Complete_Dataset_Roche_Lilly_13122024.csv", row.names = FALSE)
write.csv(cohort_BH, "Cohort_BH.csv", row.names = FALSE)


----------------------------------------------------
### Updated Moderated Multiple Regression Analyses
----------------------------------------------------

# First, load additional libraries
library(car)       # For VIF
library(pscl)      # For McFadden's R2
library(DescTools) # For additional R2 metrics
library(sjstats)   # Another source for pseudo R2
library(pROC)      # For ROC curves
library(caret)     # For confusion matrix & performance metrics
install.packages("lmtest")
library(lmtest)    # For likelihood ratio tests

# Convert categorical variables to factors 
Complete_Dataset_Roche_Lilly_13122024$AD_status <- factor(Complete_Dataset_Roche_Lilly_13122024$AD_status, levels = c(0, 1, 2))
Complete_Dataset_Roche_Lilly_13122024$ASCVD_classification_numeric <- factor(Complete_Dataset_Roche_Lilly_13122024$ASCVD_classification_numeric, levels = c(1, 2, 3, 4))
View(Complete_Dataset_Roche_Lilly_13122024)

# For simplicity
dataset <- Complete_Dataset_Roche_Lilly_13122024

-------------------------------------------------------------------------------------------------------------
### Re-running analyses in response to reviewer comments from the Journal of the American Heart Association
### Date: 07/08/2025

# Load data again to re-run analyses
dataset_CVD_analysis <- read_csv("Angelina_Bio_Hermes_R_code/Rerunning_Analyses_ASCVD_13122024/dataset_CVD_analysis.csv")
View(dataset_CVD_analysis)
nrow(dataset_CVD_analysis) # 745, as it should be
table(dataset_CVD_analysis$AD_status) # 301 and 444, as it should be

# Check variable distributions again just in case
hist(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, main = "ASCVD Score", xlab = "Value") # mostly normal
hist(dataset_CVD_analysis$AB4240_ratio_c, main = "AB4240", xlab = "Value") # mostly normal
hist(dataset_CVD_analysis$pTau181_logtr_c, main = "pTau181", xlab = "Value") # mostly normal
hist(dataset_CVD_analysis$PTAU217_logtr_c, main = "PTAU217", xlab = "Value") # mostly normal
hist(dataset_CVD_analysis$ApoE4_sqrt_c, main = "ApoE4", xlab = "Value") # pretty skewed

# Run Pearson correlations between ASCVD and each biomarker to check collinearity
cor.test(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, dataset_CVD_analysis$AB4240_ratio_c)
cor.test(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, dataset_CVD_analysis$pTau181_logtr_c)
cor.test(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, dataset_CVD_analysis$PTAU217_logtr_c)
# Spearman correlation for ApoE4
cor.test(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, dataset_CVD_analysis$ApoE4_sqrt_c, method = "spearman")

# Not too bad, all are reasonable.

# Generate correlation matrix 
vars_for_matrix <- dataset_CVD_analysis[, c(
  "ASCVD_cont_score_sqrtc",
  "AB4240_ratio_c",
  "pTau181_logtr_c",
  "PTAU217_logtr_c",
  "ApoE4_sqrt_c"
)]

cor_matrix <- cor(vars_for_matrix, use = "complete.obs")
round(cor_matrix, 3)

install.packages("Hmisc")  # if not already installed
library(Hmisc)

cor_results <- rcorr(as.matrix(vars_for_matrix))  # returns r and p
cor_results$r     # correlation coefficients
cor_results$P     # p-values

# Visualise correlations 
install.packages("corrplot")  # if not already installed
library(corrplot)

# Create cleaner labels
nice_labels <- c(
  "ASCVD Risk Score",
  "AB42/AB40",
  "p-tau181",
  "p-tau217",
  "ApoE4"
)

# Rename rows and columns
colnames(cor_results$r) <- nice_labels
rownames(cor_results$r) <- nice_labels
colnames(cor_results$P) <- nice_labels
rownames(cor_results$P) <- nice_labels

# Now re-run corrplot with these labels
corrplot(cor_results$r,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,
         p.mat = cor_results$P,
         sig.level = 0.05,
         insig = "blank")

--------------------
### TO MAIN MODELS 

dataset <- dataset_CVD_analysis

## AB42/40 Main Effects Model 
mmr_main_glm <- glm(AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                    data = dataset, family = binomial)

# Weighted logistic regression model
mmr_main_glm_weighted <- glm(AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                             data = dataset, 
                             family = binomial, 
                             weights = CVDweight)

# Display results
summary(mmr_main_glm_weighted)

# View the summary of the model
summary(mmr_main_glm)

# Tease out individual contribution 
mmr_component_glm <- glm(AD_status ~ AB4240_ratio_c + AGE + Gender + Ethnicity_RiskScorescvd 
                         + Total_cholesterol_mmolL + HDL_Cholesterol_mmolL
                         + Systolic_Blood_Pressure + ongoing_med_for_HBP
                         + Current_smoking_binary + Diabetes_status, 
                         data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)

summary(mmr_component_glm)
library(car)
vif(mmr_component_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main_glm))

# Print the odds ratios
print(odds_ratios)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main_glm_weighted))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
library(pscl)
library(DescTools)
library(sjstats)
library(car)
pseudo_r2 <- pR2(mmr_main_glm_weighted)
print(pseudo_r2)
vif_values <- vif(mmr_main_glm_weighted)
vif_values <- vif(mmr_main_glm)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_main_glm_weighted, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Load necessary library
library(pROC)
# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_main_glm_weighted))
# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for AB42/AB40 Main Model: Weighted by CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# Fit the null model (intercept-only model)
null_model <- glm(AD_status ~ 1, 
                  data = dataset, 
                  family = binomial, 
                  weights = dataset$CVDweight)

# Compute McFadden's pseudo R-squared
pseudo_r2_mcfadden <- 1 - (logLik(mmr_inter_glm_weighted) / logLik(null_model))

# Print the result
print(pseudo_r2_mcfadden)


## AB42/40 Interaction Model
mmr_inter_glm_weighted <- glm(AD_status ~ AB4240_ratio_c * ASCVD_cont_score_sqrtc, 
                    data = dataset, family = binomial, weights=CVDweight)

mmr_inter_glm <- glm(AD_status ~ AB4240_ratio_c * ASCVD_cont_score_sqrtc, 
                             data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter_glm)
summary(mmr_inter_glm_weighted)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter_glm_weighted)
print(pseudo_r2)
vif_values <- vif(mmr_inter_glm)
print(vif_values)

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter_glm_weighted))
# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for AB42/AB40 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Plot the two ROC curves together - main effects and interaction term models

# Weighted main model
mmr_main_glm_weighted <- glm(
  AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_main <- roc(dataset$AD_status, fitted(mmr_main_glm_weighted))

# Weighted interaction model
mmr_inter_glm_weighted <- glm(
  AD_status ~ AB4240_ratio_c * ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_inter <- roc(dataset$AD_status, fitted(mmr_inter_glm_weighted))

# Plot both ROC curves
plot(roc_main, col = "blue", lwd = 2,
     main = "ROC Curves for Aβ42/Aβ40: Weighted Models")
lines(roc_inter, col = "red", lwd = 2)  # Add second curve

# Add diagonal line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright",
       legend = c("Main Model", "Interaction Model"),
       col = c("blue", "red"),
       lwd = 2)

--------------------------------------------
# Do the same for all remaining biomarkers

## P-tau181:

# Weighted main model
mmr_main2_glm_weighted <- glm(
  AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_main <- roc(dataset$AD_status, fitted(mmr_main2_glm_weighted))

# Weighted interaction model
mmr_inter2_glm_weighted <- glm(
  AD_status ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_inter <- roc(dataset$AD_status, fitted(mmr_inter2_glm_weighted))

# Plot both ROC curves
plot(roc_main, col = "blue", lwd = 2,
     main = "ROC Curves for P-tau181: Weighted Models")
lines(roc_inter, col = "red", lwd = 2)  # Add second curve

# Add diagonal line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright",
       legend = c("Main Model", "Interaction Model"),
       col = c("blue", "red"),
       lwd = 2)


## P-tau217:

# Weighted main model
mmr_main3_glm_weighted <- glm(
  AD_status ~ PTAU217_logtr_c + ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_main <- roc(dataset$AD_status, fitted(mmr_main3_glm_weighted))

# Weighted interaction model
mmr_inter3_glm_weighted <- glm(
  AD_status ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_inter <- roc(dataset$AD_status, fitted(mmr_inter3_glm_weighted))

# Plot both ROC curves
plot(roc_main, col = "blue", lwd = 2,
     main = "ROC Curves for P-tau217: Weighted Models")
lines(roc_inter, col = "red", lwd = 2)  # Add second curve

# Add diagonal line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright",
       legend = c("Main Model", "Interaction Model"),
       col = c("blue", "red"),
       lwd = 2)

# Check the linearity of the relationship of p-tau181 and p-tau217 and CV risk

# Create tertiles (or use quartiles if you prefer finer groups)
dataset <- dataset %>%
  mutate(
    ASCVD_group = cut(
      ASCVD_cont_score_sqrtc,
      breaks = quantile(ASCVD_cont_score_sqrtc, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Low ASCVD", "Medium ASCVD", "High ASCVD")
    )
  )

# ---- pTau181 ----
ggplot(dataset, aes(x = pTau181_logtr_c, y = AD_status, color = ASCVD_group)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Plasma pTau181 (log-transformed, centered)",
    y = "Predicted probability of cognitive impairment",
    color = "ASCVD group"
  ) +
  theme_minimal()

# ---- pTau217 ----
ggplot(dataset, aes(x = PTAU217_logtr_c, y = AD_status, color = ASCVD_group)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Plasma pTau217 (log-transformed, centered)",
    y = "Predicted probability of cognitive impairment",
    color = "ASCVD group"
  ) +
  theme_minimal()

# Check linearity for the other two biomarkers as well
ggplot(dataset, aes(x = ApoE4_sqrt_c, y = AD_status, color = ASCVD_group)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Circulating levels of Apoe4",
    y = "Predicted probability of cognitive impairment",
    color = "ASCVD group"
  ) +
  theme_minimal()

ggplot(dataset, aes(x = AB4240_ratio_c, y = AD_status, color = ASCVD_group)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Plasma AB42/AB40",
    y = "Predicted probability of cognitive impairment",
    color = "ASCVD group"
  ) +
  theme_minimal()


## ApoE4:

# Weighted main model
mmr_main4_glm_weighted <- glm(
  AD_status ~ ApoE4_sqrt_c + ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_main <- roc(dataset$AD_status, fitted(mmr_main4_glm_weighted))

# Weighted interaction model
mmr_inter4_glm_weighted <- glm(
  AD_status ~ ApoE4_sqrt_c * ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_inter <- roc(dataset$AD_status, fitted(mmr_inter4_glm_weighted))

# Plot both ROC curves
plot(roc_main, col = "blue", lwd = 2,
     main = "ROC Curves for ApoE4: Weighted Models")
lines(roc_inter, col = "red", lwd = 2)  # Add second curve

# Add diagonal line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright",
       legend = c("Main Model", "Interaction Model"),
       col = c("blue", "red"),
       lwd = 2)


-----------------------------------------------------------------------------
# Likelihood Ratio Test comparing Main effects model vs Interaction model
  
anova(mmr_main_glm, mmr_inter_glm, test = "LRT")

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_inter_glm_weighted,
                      type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter_glm_weighted))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for AB42/AB40 Interaction Model: Weighted by CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# P-tau 181 Main Effects Model 
mmr_main2_glm <- glm(AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                    data = dataset, family = binomial)

# View the summary of the model
summary(mmr_main2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main2_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
library(pscl)
library(DescTools)
library(sjstats)
library(car)
pseudo_r2 <- pR2(mmr_main2_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main2_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_main2_glm))
plot(roc_curve)
auc(roc_curve)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_main2_glm, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_main2_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau181 Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# P-tau 181 Interaction Model
mmr_inter2_glm <- glm(AD_status ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter2_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter2_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter2_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_inter2_glm))
plot(roc_curve)
auc(roc_curve)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main2_glm, mmr_inter2_glm, test = "LRT")

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_inter2_glm, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter2_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau181 Interaction Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# P-tau 217 Main Effects Model 
mmr_main3_glm <- glm(AD_status ~ PTAU217_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_main3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main3_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main3_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_main3_glm))
plot(roc_curve)
auc(roc_curve)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main3_glm, mmr_inter3_glm, test = "LRT")

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_main3_glm, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_main3_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau217 Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# P-tau 217 Interaction Model
mmr_inter3_glm <- glm(AD_status ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter3_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter3_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_inter3_glm))
plot(roc_curve)
auc(roc_curve)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_inter3_glm, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter3_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau217 Interaction Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# ApoE4 Main Effects Model 
mmr_main4_glm <- glm(AD_status ~ ApoE4_sqrt_c + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_main4_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main4_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main4_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_main4_glm))
plot(roc_curve)
auc(roc_curve)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_main4_glm, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_main4_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for ApoE4 Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# ApoE4 Interaction Model
mmr_inter4_glm <- glm(AD_status ~ ApoE4_sqrt_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter4_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter4_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter4_glm)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_inter4_glm, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter4_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for ApoE4 Interaction Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main4_glm, mmr_inter4_glm, test = "LRT")

# -------------------------------------------------------------------------------
# Sensitivity Analyses - Run only for models with significant interaction terms 

# First, split data according to sex
male_data <- Complete_Dataset_Roche_Lilly_13122024 %>% filter(SEX_recode == 1)
female_data <- Complete_Dataset_Roche_Lilly_13122024 %>% filter(SEX_recode == 0)


# P-tau 181 Main Effects Model 
men_main2_glm <- glm(AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = male_data, family = binomial)
women_main2_glm <- glm(AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = female_data, family = binomial)

# View the summary of the model
summary(men_main2_glm)
summary(women_main2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(men_main2_glm))
odds_ratios <- exp(coef(women_main2_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(men_main2_glm)
pseudo_r2 <- pR2(women_main2_glm)
print(pseudo_r2)
vif_values <- vif(men_main2_glm)
vif_values <- vif(women_main2_glm)
print(vif_values)

## P-tau 181 Interaction Model
men_inter2_glm <- glm(AD_status ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = male_data, family = binomial)
women_inter2_glm <- glm(AD_status ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = female_data, family = binomial)

# View the summary of the model
summary(men_inter2_glm)
summary(women_inter2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(men_inter2_glm))
odds_ratios <- exp(coef(women_inter2_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(men_inter2_glm)
print(pseudo_r2)
vif_values <- vif(men_inter2_glm)
print(vif_values)
pseudo_r2 <- pR2(women_inter2_glm)
print(pseudo_r2)
vif_values <- vif(women_inter2_glm)
print(vif_values)

## P-tau 217 Main Effects Model 
men_main3_glm <- glm(AD_status ~ PTAU217_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = male_data, family = binomial)
women_main3_glm <- glm(AD_status ~ PTAU217_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = female_data, family = binomial)

# View the summary of the model
summary(men_main3_glm)
summary(women_main3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(men_main3_glm))
odds_ratios <- exp(coef(women_main3_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(men_main3_glm)
pseudo_r2 <- pR2(women_main3_glm)
print(pseudo_r2)
vif_values <- vif(men_main3_glm)
vif_values <- vif(women_main3_glm)
print(vif_values)

## P-tau 217 Interaction Model
men_inter3_glm <- glm(AD_status ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = male_data, family = binomial)
women_inter3_glm <- glm(AD_status ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = female_data, family = binomial)

# View the summary of the model
summary(men_inter3_glm)
summary(women_inter3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(men_inter3_glm))
odds_ratios <- exp(coef(women_inter3_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(men_inter3_glm)
pseudo_r2 <- pR2(women_inter3_glm)
print(pseudo_r2)
vif_values <- vif(men_inter3_glm)
vif_values <- vif(women_inter3_glm)
print(vif_values)

# Save dataset
write.csv(Complete_Dataset_Roche_Lilly_13122024, "Complete_Dataset_Roche_Lilly_13122024.csv", 
          row.names = FALSE)

## P-tau 217 only
mmr_ptau217 <- glm(AD_status ~ PTAU217_logtr_c, 
                   data = dataset, family = binomial)

# View the summary of the model
summary(mmr_ptau217)

---------------------------------------
### 30/01/2024: Additional Work 

# Separate models for individual ASCVD risk factors for each biomarker

# AB42/40
glm_age <- glm(AD_status ~ AB4240_ratio_c + AGE, data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_age)
pseudo_r2 <- pR2(glm_age)
print(pseudo_r2)
exp(coef(glm_age))

glm_gender <- glm(AD_status ~ AB4240_ratio_c + Gender, data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_gender)
pseudo_r2 <- pR2(glm_gender)
print(pseudo_r2)
exp(coef(glm_gender))

glm_ethnic <- glm(AD_status ~ AB4240_ratio_c + Ethnicity_RiskScorescvd, 
                  data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_ethnic)
pseudo_r2 <- pR2(glm_ethnic)
print(pseudo_r2)
exp(coef(glm_ethnic))

glm_HDL <- glm(AD_status ~ AB4240_ratio_c + HDL_Cholesterol_mmolL, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_HDL)
pseudo_r2 <- pR2(glm_HDL)
print(pseudo_r2)
exp(coef(glm_HDL))

glm_LDL <- glm(AD_status ~ AB4240_ratio_c + LDL_Cholesterol, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_LDL)
pseudo_r2 <- pR2(glm_LDL)
print(pseudo_r2)
exp(coef(glm_LDL))

glm_totalchol <- glm(AD_status ~ AB4240_ratio_c + Total_cholesterol_mmolL, 
                     data = Complete_Dataset_Roche_Lilly_13122024, 
                     family = binomial)
summary(glm_totalchol)
pseudo_r2 <- pR2(glm_totalchol)
print(pseudo_r2)
exp(coef(glm_totalchol))

glm_smoking <- glm(AD_status ~ AB4240_ratio_c + Current_smoking_binary, data = Complete_Dataset_Roche_Lilly_13122024, 
                   family = binomial)
summary(glm_smoking)
pseudo_r2 <- pR2(glm_smoking)
print(pseudo_r2)
exp(coef(glm_smoking))

glm_sbp <- glm(AD_status ~ AB4240_ratio_c + Systolic_Blood_Pressure, 
               data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_sbp)
pseudo_r2 <- pR2(glm_sbp)
print(pseudo_r2)
exp(coef(glm_sbp))

glm_medHTN <- glm(AD_status ~ AB4240_ratio_c + ongoing_med_for_HBP, 
                  data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_medHTN)
pseudo_r2 <- pR2(glm_medHTN)
print(pseudo_r2)
exp(coef(glm_medHTN))

glm_diabetes <- glm(AD_status ~ AB4240_ratio_c + Diabetes_status, data = Complete_Dataset_Roche_Lilly_13122024, 
                    family = binomial)
summary(glm_diabetes)
pseudo_r2 <- pR2(glm_diabetes)
print(pseudo_r2)
exp(coef(glm_diabetes))

# P-tau181
glm_age <- glm(AD_status ~ pTau181_logtr_c + AGE, data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_age)
pseudo_r2 <- pR2(glm_age)
print(pseudo_r2)
exp(coef(glm_age))

glm_gender <- glm(AD_status ~ pTau181_logtr_c + Gender, data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_gender)
pseudo_r2 <- pR2(glm_gender)
print(pseudo_r2)
exp(coef(glm_gender))

glm_ethnic <- glm(AD_status ~ pTau181_logtr_c + Ethnicity_RiskScorescvd, 
                  data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_ethnic)
pseudo_r2 <- pR2(glm_ethnic)
print(pseudo_r2)
exp(coef(glm_ethnic))

glm_HDL <- glm(AD_status ~ pTau181_logtr_c + HDL_Cholesterol_mmolL, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_HDL)
pseudo_r2 <- pR2(glm_HDL)
print(pseudo_r2)
exp(coef(glm_HDL))

glm_LDL <- glm(AD_status ~ pTau181_logtr_c + LDL_Cholesterol, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_LDL)
pseudo_r2 <- pR2(glm_LDL)
print(pseudo_r2)
exp(coef(glm_LDL))

glm_totalchol <- glm(AD_status ~ pTau181_logtr_c + Total_cholesterol_mmolL, 
                     data = Complete_Dataset_Roche_Lilly_13122024, 
                     family = binomial)
summary(glm_totalchol)
pseudo_r2 <- pR2(glm_totalchol)
print(pseudo_r2)
exp(coef(glm_totalchol))

glm_smoking <- glm(AD_status ~ pTau181_logtr_c + Current_smoking_binary, data = Complete_Dataset_Roche_Lilly_13122024, 
                   family = binomial)
summary(glm_smoking)
pseudo_r2 <- pR2(glm_smoking)
print(pseudo_r2)
exp(coef(glm_smoking))

glm_sbp <- glm(AD_status ~ pTau181_logtr_c + Systolic_Blood_Pressure, 
               data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_sbp)
pseudo_r2 <- pR2(glm_sbp)
print(pseudo_r2)
exp(coef(glm_sbp))

glm_medHTN <- glm(AD_status ~ pTau181_logtr_c + ongoing_med_for_HBP, 
                  data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_medHTN)
pseudo_r2 <- pR2(glm_medHTN)
print(pseudo_r2)
exp(coef(glm_medHTN))

glm_diabetes <- glm(AD_status ~ pTau181_logtr_c + Diabetes_status, data = Complete_Dataset_Roche_Lilly_13122024, 
                    family = binomial)
summary(glm_diabetes)
pseudo_r2 <- pR2(glm_diabetes)
print(pseudo_r2)
exp(coef(glm_diabetes))

# P-tau217
glm_age <- glm(AD_status ~ PTAU217_logtr_c + AGE, data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_age)
pseudo_r2 <- pR2(glm_age)
print(pseudo_r2)
exp(coef(glm_age))

glm_gender <- glm(AD_status ~ PTAU217_logtr_c + Gender, data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_gender)
pseudo_r2 <- pR2(glm_gender)
print(pseudo_r2)
exp(coef(glm_gender))

glm_ethnic <- glm(AD_status ~ PTAU217_logtr_c + Ethnicity_RiskScorescvd, 
                  data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_ethnic)
pseudo_r2 <- pR2(glm_ethnic)
print(pseudo_r2)
exp(coef(glm_ethnic))

glm_HDL <- glm(AD_status ~ PTAU217_logtr_c + HDL_Cholesterol_mmolL, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_HDL)
pseudo_r2 <- pR2(glm_HDL)
print(pseudo_r2)
exp(coef(glm_HDL))

glm_LDL <- glm(AD_status ~ PTAU217_logtr_c + LDL_Cholesterol, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_LDL)
pseudo_r2 <- pR2(glm_LDL)
print(pseudo_r2)
exp(coef(glm_LDL))

glm_totalchol <- glm(AD_status ~ PTAU217_logtr_c + Total_cholesterol_mmolL, 
                     data = Complete_Dataset_Roche_Lilly_13122024, 
                     family = binomial)
summary(glm_totalchol)
pseudo_r2 <- pR2(glm_totalchol)
print(pseudo_r2)
exp(coef(glm_totalchol))

glm_smoking <- glm(AD_status ~ PTAU217_logtr_c + Current_smoking_binary, data = Complete_Dataset_Roche_Lilly_13122024, 
                   family = binomial)
summary(glm_smoking)
pseudo_r2 <- pR2(glm_smoking)
print(pseudo_r2)
exp(coef(glm_smoking))

glm_sbp <- glm(AD_status ~ PTAU217_logtr_c + Systolic_Blood_Pressure, 
               data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_sbp)
pseudo_r2 <- pR2(glm_sbp)
print(pseudo_r2)
exp(coef(glm_sbp))

glm_medHTN <- glm(AD_status ~ PTAU217_logtr_c + ongoing_med_for_HBP, 
                  data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_medHTN)
pseudo_r2 <- pR2(glm_medHTN)
print(pseudo_r2)
exp(coef(glm_medHTN))

glm_diabetes <- glm(AD_status ~ PTAU217_logtr_c + Diabetes_status, data = Complete_Dataset_Roche_Lilly_13122024, 
                    family = binomial)
summary(glm_diabetes)
pseudo_r2 <- pR2(glm_diabetes)
print(pseudo_r2)
exp(coef(glm_diabetes))

# ApoE4
glm_age <- glm(AD_status ~ ApoE4_sqrt_c + AGE, data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_age)
pseudo_r2 <- pR2(glm_age)
print(pseudo_r2)
exp(coef(glm_age))

glm_gender <- glm(AD_status ~ ApoE4_sqrt_c + Gender, data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_gender)
pseudo_r2 <- pR2(glm_gender)
print(pseudo_r2)
exp(coef(glm_gender))

glm_ethnic <- glm(AD_status ~ ApoE4_sqrt_c + Ethnicity_RiskScorescvd, 
                  data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_ethnic)
pseudo_r2 <- pR2(glm_ethnic)
print(pseudo_r2)
exp(coef(glm_ethnic))

glm_HDL <- glm(AD_status ~ ApoE4_sqrt_c + HDL_Cholesterol_mmolL, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_HDL)
pseudo_r2 <- pR2(glm_HDL)
print(pseudo_r2)
exp(coef(glm_HDL))

glm_LDL <- glm(AD_status ~ ApoE4_sqrt_c + LDL_Cholesterol, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_LDL)
pseudo_r2 <- pR2(glm_LDL)
print(pseudo_r2)
exp(coef(glm_LDL))

glm_totalchol <- glm(AD_status ~ ApoE4_sqrt_c + Total_cholesterol_mmolL, 
                     data = Complete_Dataset_Roche_Lilly_13122024, 
                     family = binomial)
summary(glm_totalchol)
pseudo_r2 <- pR2(glm_totalchol)
print(pseudo_r2)
exp(coef(glm_totalchol))

glm_smoking <- glm(AD_status ~ ApoE4_sqrt_c + Current_smoking_binary, data = Complete_Dataset_Roche_Lilly_13122024, 
                   family = binomial)
summary(glm_smoking)
pseudo_r2 <- pR2(glm_smoking)
print(pseudo_r2)
exp(coef(glm_smoking))

glm_sbp <- glm(AD_status ~ ApoE4_sqrt_c + Systolic_Blood_Pressure, 
               data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_sbp)
pseudo_r2 <- pR2(glm_sbp)
print(pseudo_r2)
exp(coef(glm_sbp))

glm_medHTN <- glm(AD_status ~ ApoE4_sqrt_c + ongoing_med_for_HBP, 
                  data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_medHTN)
pseudo_r2 <- pR2(glm_medHTN)
print(pseudo_r2)
exp(coef(glm_medHTN))

glm_diabetes <- glm(AD_status ~ ApoE4_sqrt_c + Diabetes_status, data = Complete_Dataset_Roche_Lilly_13122024, 
                    family = binomial)
summary(glm_diabetes)
pseudo_r2 <- pR2(glm_diabetes)
print(pseudo_r2)
exp(coef(glm_diabetes))

---------------------------------------------
### 07/03/2025: Experimenting with analysis

# Modeling only CVD risk as a predictor

# CVD only 
mmr_cvd_glm <- glm(AD_status ~ ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_cvd_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_cvd_glm))

# Print the odds ratios
print(odds_ratios)

pseudo_r2 <- pR2(mmr_cvd_glm)
print(pseudo_r2)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_ptau217, type = 'response')

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_ptau217))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau217 Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

install.packages("randomForest")
library(randomForest)

set.seed(42)  # For reproducibility

rf_model <- randomForest(
  AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc,  # Add more predictors if needed
  data = dataset,
  ntree = 500,     # Number of trees
  mtry = 2,        # Number of predictors considered at each split
  importance = TRUE
)

print(rf_model)

# Get predicted probabilities for CI=1
rf_probs <- predict(rf_model, type = "prob")[,2]

# Compute ROC Curve & AUC
rf_roc <- roc(dataset$AD_status, rf_probs)
plot(rf_roc, col = "darkgreen", main = "Random Forest ROC Curve - AB42/AB40 Main Model")
auc_rf <- auc(rf_roc)
print(paste("Random Forest AUC:", round(auc_rf, 3)))

lr_probs <- predict(mmr_main_glm, type = "response")
lr_roc <- roc(dataset$AD_status, lr_probs)

# Plot both ROC curves
plot(lr_roc, col = "blue", main = "ROC Curve Comparison", lwd = 2)
lines(rf_roc, col = "darkgreen", lwd = 1)

legend("bottomright", legend = c("AB42/AB40 Logistic", "AB42/AB40 Random Forest"),
       col = c("blue", "darkgreen"), lwd = 1)

# Print AUC values
print(paste("Logistic Regression AUC:", round(auc(lr_roc), 3)))
print(paste("Random Forest AUC:", round(auc(rf_roc), 3)))

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_cvd_glm, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_cvd_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for CVD Risk Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

install.packages("randomForest")
library(randomForest)

set.seed(42)  # For reproducibility

rf_model <- randomForest(
  AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc,  # Add more predictors if needed
  data = dataset,
  ntree = 500,     # Number of trees
  mtry = 2,        # Number of predictors considered at each split
  importance = TRUE
)

print(rf_model)

# Get predicted probabilities for CI=1
rf_probs <- predict(rf_model, type = "prob")[,2]

# Compute ROC Curve & AUC
rf_roc <- roc(dataset$AD_status, rf_probs)
plot(rf_roc, col = "darkgreen", main = "Random Forest ROC Curve - AB42/AB40 Main Model")
auc_rf <- auc(rf_roc)
print(paste("Random Forest AUC:", round(auc_rf, 3)))

lr_probs <- predict(mmr_main_glm, type = "response")
lr_roc <- roc(dataset$AD_status, lr_probs)

# Plot both ROC curves
plot(lr_roc, col = "blue", main = "ROC Curve Comparison", lwd = 2)
lines(rf_roc, col = "darkgreen", lwd = 1)

legend("bottomright", legend = c("AB42/AB40 Logistic", "AB42/AB40 Random Forest"),
       col = c("blue", "darkgreen"), lwd = 1)

# Print AUC values
print(paste("Logistic Regression AUC:", round(auc(lr_roc), 3)))
print(paste("Random Forest AUC:", round(auc(rf_roc), 3)))

# Define a vector of USUBJIDs with prevalent CVD
cvd_usubjids <- c("BIO-HERMES-00101-033", "BIO-HERMES-00101-040", "BIO-HERMES-00101-045", 
                  "BIO-HERMES-00101-045", "BIO-HERMES-00101-046", "BIO-HERMES-00101-046", 
                  "BIO-HERMES-00101-046", "BIO-HERMES-00101-053", "BIO-HERMES-00101-061",
                  "BIO-HERMES-00101-061", "BIO-HERMES-00101-078", "BIO-HERMES-00101-080", 
                  "BIO-HERMES-00101-080", "BIO-HERMES-00101-086", "BIO-HERMES-00101-090", 
                  "BIO-HERMES-00101-102", "BIO-HERMES-00101-102", "BIO-HERMES-00101-109", 
                  "BIO-HERMES-00102-003", "BIO-HERMES-00102-009", "BIO-HERMES-00102-012", 
                  "BIO-HERMES-00102-032", "BIO-HERMES-00102-061", "BIO-HERMES-00102-063",
                  "BIO-HERMES-00102-071", "BIO-HERMES-00102-071", "BIO-HERMES-00102-074",
                  "BIO-HERMES-00102-075", "BIO-HERMES-00102-075", "BIO-HERMES-00102-075",
                  "BIO-HERMES-00102-081", "BIO-HERMES-00102-082", "BIO-HERMES-00102-083",
                  "BIO-HERMES-00102-083", "BIO-HERMES-00102-083", "BIO-HERMES-00102-092",
                  "BIO-HERMES-00103-002", "BIO-HERMES-00103-002", "BIO-HERMES-00103-002",
                  "BIO-HERMES-00103-005", "BIO-HERMES-00103-028", "BIO-HERMES-00103-047",
                  "BIO-HERMES-00104-004", "BIO-HERMES-00104-018", "BIO-HERMES-00105-016",
                  "BIO-HERMES-00105-020", "BIO-HERMES-00105-040", "BIO-HERMES-00105-048",
                  "BIO-HERMES-00105-056", "BIO-HERMES-00105-056", "BIO-HERMES-00105-087",
                  "BIO-HERMES-00105-090", "BIO-HERMES-00105-097", "BIO-HERMES-00105-097",
                  "BIO-HERMES-00105-097", "BIO-HERMES-00106-002", "BIO-HERMES-00106-010",
                  "BIO-HERMES-00106-011", "BIO-HERMES-00106-021", "BIO-HERMES-00106-026",
                  "BIO-HERMES-00106-026", "BIO-HERMES-00106-029", "BIO-HERMES-00106-037",
                  "BIO-HERMES-00106-046", "BIO-HERMES-00106-046", "BIO-HERMES-00106-046",
                  "BIO-HERMES-00106-048", "BIO-HERMES-00106-050", "BIO-HERMES-00106-050",
                  "BIO-HERMES-00106-059", "BIO-HERMES-00106-065", "BIO-HERMES-00106-065",
                  "BIO-HERMES-00106-069", "BIO-HERMES-00106-072", "BIO-HERMES-00106-076",
                  "BIO-HERMES-00106-076", "BIO-HERMES-00106-076", "BIO-HERMES-00106-078",
                  "BIO-HERMES-00106-079", "BIO-HERMES-00106-083", "BIO-HERMES-00106-084",
                  "BIO-HERMES-00106-087", "BIO-HERMES-00106-088", "BIO-HERMES-00106-094",
                  "BIO-HERMES-00107-029", "BIO-HERMES-00107-037", "BIO-HERMES-00107-040",
                  "BIO-HERMES-00107-043", "BIO-HERMES-00107-043", "BIO-HERMES-00108-001",
                  "BIO-HERMES-00108-007", "BIO-HERMES-00108-007", "BIO-HERMES-00108-007",
                  "BIO-HERMES-00108-018", "BIO-HERMES-00109-006", "BIO-HERMES-00109-011",
                  "BIO-HERMES-00109-030", "BIO-HERMES-00109-032", "BIO-HERMES-00109-032",
                  "BIO-HERMES-00109-036", "BIO-HERMES-00109-038", "BIO-HERMES-00109-049",
                  "BIO-HERMES-00109-051", "BIO-HERMES-00109-069", "BIO-HERMES-00109-070",
                  "BIO-HERMES-00109-073", "BIO-HERMES-00109-077", "BIO-HERMES-00109-079",
                  "BIO-HERMES-00109-079", "BIO-HERMES-00109-108", "BIO-HERMES-00109-144",
                  "BIO-HERMES-00109-162", "BIO-HERMES-00109-165", "BIO-HERMES-00110-021",
                  "BIO-HERMES-00110-047", "BIO-HERMES-00110-054", "BIO-HERMES-00111-010",
                  "BIO-HERMES-00111-026", "BIO-HERMES-00111-027", "BIO-HERMES-00111-033",
                  "BIO-HERMES-00111-040", "BIO-HERMES-00112-025", "BIO-HERMES-00112-038",
                  "BIO-HERMES-00112-045", "BIO-HERMES-00113-001", "BIO-HERMES-00113-004",
                  "BIO-HERMES-00113-006", "BIO-HERMES-00113-007", "BIO-HERMES-00114-002")

# Create a CVDweight variable
dataset$CVDweight <- ifelse(dataset$USUBJID %in% cvd_usubjids, 1, 0)

# Display a summary of the new variable
table(dataset$CVDweight) # 0=664; 1=81
View(dataset)

------------------------------------------------------
### 07/03/2025: Stratification according to CV Risk

dataset_CVD <- subset(dataset_CVD_analysis, CVDweight == 1)
dataset_nonCVD <- subset(dataset_CVD_analysis, CVDweight == 0)

## AB42/AB40
# CVD
mmr_glm_CVD <- glm(AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                   data = dataset_CVD, 
                   family = binomial)
summary(mmr_glm_CVD)

# Non CVD
mmr_glm_nonCVD <- glm(AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                      data = dataset_nonCVD, 
                      family = binomial)
summary(mmr_glm_nonCVD)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_glm_CVD))
odds_ratios <- exp(coef(mmr_glm_nonCVD))
                   
# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_glm_CVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_CVD)
print(vif_values)

pseudo_r2 <- pR2(mmr_glm_nonCVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_nonCVD)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Predict probabilities from the model
pred_probs <- predict(mmr_glm_CVD,
                      type = "response")
pred_probs <- predict(mmr_glm_nonCVD,
                      type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset_CVD$AD_status))

# Extract Sensitivity & Specificity
library(caret)
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Load necessary library
library(pROC)

# Generate ROC curve for the model
roc_curve <- roc(dataset_CVD$AD_status, fitted(mmr_glm_CVD))
roc_curve <- roc(dataset_nonCVD$AD_status, fitted(mmr_glm_nonCVD))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for AB42/AB40 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

## P-tau181
# CVD
mmr_glm_CVD <- glm(AD_status ~ pTau181_logtr_c + 
                    ASCVD_cont_score_sqrtc, 
                   data = dataset_CVD, 
                   family = binomial)
summary(mmr_glm_CVD)

# Non CVD
mmr_glm_nonCVD <- glm(AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                      data = dataset_nonCVD, 
                      family = binomial)
summary(mmr_glm_nonCVD)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_glm_CVD))
odds_ratios <- exp(coef(mmr_glm_nonCVD))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_glm_CVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_CVD)
print(vif_values)

pseudo_r2 <- pR2(mmr_glm_nonCVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_nonCVD)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Predict probabilities from the model
pred_probs <- predict(mmr_glm_CVD,
                      type = "response")
pred_probs <- predict(mmr_glm_nonCVD,
                      type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset_nonCVD$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Load necessary library
library(pROC)

# Generate ROC curve for the model
roc_curve <- roc(dataset_CVD$AD_status, fitted(mmr_glm_CVD))
roc_curve <- roc(dataset_nonCVD$AD_status, fitted(mmr_glm_nonCVD))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau181 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

## P-tau217
# CVD
mmr_glm_CVD <- glm(AD_status ~ PTAU217_logtr_c  + 
                     ASCVD_cont_score_sqrtc, 
                   data = dataset_CVD, 
                   family = binomial)
summary(mmr_glm_CVD)

# Non CVD
mmr_glm_nonCVD <- glm(AD_status ~ PTAU217_logtr_c  + ASCVD_cont_score_sqrtc, 
                      data = dataset_nonCVD, 
                      family = binomial)
summary(mmr_glm_nonCVD)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_glm_CVD))
odds_ratios <- exp(coef(mmr_glm_nonCVD))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_glm_CVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_CVD)
print(vif_values)

pseudo_r2 <- pR2(mmr_glm_nonCVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_nonCVD)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Predict probabilities from the model
pred_probs <- predict(mmr_glm_CVD,
                      type = "response")
pred_probs <- predict(mmr_glm_nonCVD,
                      type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset_nonCVD$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset_CVD$AD_status, fitted(mmr_glm_CVD))
roc_curve <- roc(dataset_nonCVD$AD_status, fitted(mmr_glm_nonCVD))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau217 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

## ApoE4
# CVD
mmr_glm_CVD <- glm(AD_status ~ ApoE4_sqrt_c + 
                     ASCVD_cont_score_sqrtc, 
                   data = dataset_CVD, 
                   family = binomial)
summary(mmr_glm_CVD)

# Non CVD
mmr_glm_nonCVD <- glm(AD_status ~ ApoE4_sqrt_c + ASCVD_cont_score_sqrtc, 
                      data = dataset_nonCVD, 
                      family = binomial)
summary(mmr_glm_nonCVD)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_glm_CVD))
odds_ratios <- exp(coef(mmr_glm_nonCVD))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_glm_CVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_CVD)
print(vif_values)

pseudo_r2 <- pR2(mmr_glm_nonCVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_nonCVD)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Predict probabilities from the model
pred_probs <- predict(mmr_glm_CVD,
                      type = "response")
pred_probs <- predict(mmr_glm_nonCVD,
                      type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset_nonCVD$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset_CVD$AD_status, fitted(mmr_glm_CVD))
roc_curve <- roc(dataset_nonCVD$AD_status, fitted(mmr_glm_nonCVD))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for ApoE4 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# Save dataset with new additions
write.csv(dataset, "dataset_CVD_analysis.csv", row.names = FALSE)
dataset_CVD_analysis <- read_csv("Angelina_Bio_Hermes_R_code/Rerunning_Analyses_ASCVD_13122024/dataset_CVD_analysis.csv")
View(dataset_CVD_analysis)

--------------------------------------------------------------------------------
### 07/08/2025: Extra analyses comparing the different cognitive status groups
  
table(dataset$QVAL)

# Dropping MCI cases to only compare Healthy versus Probable AD

# Step 1: Subset the dataset to only include Healthy and Probable AD
dataset <- subset(dataset, QVAL %in% c("Cohort 1 (Healthy)", "Cohort 3 (Probable AD)"))

# Step 2: Create the new binary outcome variable
dataset$AD_status_HAD <- ifelse(dataset$QVAL == "Cohort 3 (Probable AD)", 1, 0)
table(dataset$AD_status_HAD)
# Should return:
#   0   1 
# 301 209

# Re-run all regression analyses

# AB42/40 Main Effects Model 
mmr_main_glm <- glm(AD_status_HAD ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                    data = dataset, family = binomial)

# Display results
summary(mmr_main_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
library(pscl)
library(DescTools)
library(sjstats)
library(car)
pseudo_r2 <- pR2(mmr_main_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main_glm)
print(vif_values)

# AB42/40 Interaction Model
mmr_inter_glm <- glm(AD_status_HAD ~ AB4240_ratio_c * ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter_glm)
print(vif_values)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main_glm, mmr_inter_glm, test = "LRT")

# P-tau181:

# P-tau181 Main Effects Model 
mmr_main2_glm <- glm(AD_status_HAD ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                    data = dataset, family = binomial)

# Display results
summary(mmr_main2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main2_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
library(pscl)
library(DescTools)
library(sjstats)
library(car)
pseudo_r2 <- pR2(mmr_main2_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main2_glm)
print(vif_values)

# P-tau181 Interaction Model
mmr_inter2_glm <- glm(AD_status_HAD ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter2_glm)
# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter2_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter2_glm_weighted)
print(pseudo_r2)
vif_values <- vif(mmr_inter2_glm)
print(vif_values)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main2_glm, mmr_inter2_glm, test = "LRT")

# P-tau217:

# P-tau217 Main Effects Model 
mmr_main3_glm <- glm(AD_status_HAD ~ PTAU217_logtr_c  + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# Display results
summary(mmr_main3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main3_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
library(pscl)
library(DescTools)
library(sjstats)
library(car)
pseudo_r2 <- pR2(mmr_main3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main3_glm)
print(vif_values)

# P-tau217 Interaction Model
mmr_inter3_glm <- glm(AD_status_HAD ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter3_glm)
# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter3_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter3_glm)
print(vif_values)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main3_glm, mmr_inter3_glm, test = "LRT")

# ApoE4:

# ApoE4 Main Effects Model 
mmr_main4_glm <- glm(AD_status_HAD ~ ApoE4_sqrt_c + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# Display results
summary(mmr_main4_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main4_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main4_glm)
print(vif_values)

# ApoE4 Interaction Model
mmr_inter4_glm <- glm(AD_status_HAD ~ ApoE4_sqrt_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter4_glm)
# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter4_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter4_glm)
print(vif_values)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main4_glm, mmr_inter4_glm, test = "LRT")


----------------------------------------------------------------------------------
### 08/08/2025: Additional subgroup analyses - MCI vs CN and Probable AD vs CN 

# Load libraries
library(dplyr)
library(pROC)
library(psych)

# --- Step 1: Recode QVAL to dx3 ---
dataset <- dataset %>%
  mutate(
    dx3 = case_when(
      QVAL == "Cohort 1 (Healthy)"     ~ "CN",
      QVAL == "Cohort 2 (MCI)"         ~ "MCI",
      QVAL == "Cohort 3 (Probable AD)" ~ "mildAD",
      TRUE ~ NA_character_
    )
  )

biomarkers <- c("AB4240_ratio_c", "pTau181_logtr_c", 
                "PTAU217_logtr_c", "ApoE4_sqrt_c")
cvd <- "ASCVD_cont_score_sqrtc"

# --- Step 2: Function to run models, print full summaries, and return ROC ---
run_and_plot <- function(df, bm, contrast) {
  df_sub <- df %>%
    filter(dx3 %in% contrast) %>%
    mutate(outcome = ifelse(dx3 == contrast[2], 1, 0))
  
  # Main model
  f_main <- as.formula(paste0("outcome ~ ", bm, " + ", cvd))
  m_main <- glm(f_main, data = df_sub, family = binomial)
  cat("\n------------------------------------------------\n")
  cat("Main effects model:", bm, "-", paste(contrast, collapse = " vs "), "\n")
  print(summary(m_main))
  
  # Interaction model
  f_int <- as.formula(paste0("outcome ~ ", bm, " * ", cvd))
  m_int <- glm(f_int, data = df_sub, family = binomial)
  cat("\nInteraction model:", bm, "-", paste(contrast, collapse = " vs "), "\n")
  print(summary(m_int))
  
  # ROC curves
  roc_main <- roc(df_sub$outcome, fitted(m_main))
  roc_int  <- roc(df_sub$outcome, fitted(m_int))
  
  plot(roc_main, col = "blue", lwd = 2,
       main = paste("ROC:", bm, "-", paste(contrast, collapse = " vs ")))
  lines(roc_int, col = "red", lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  legend("bottomright",
         legend = c(
           paste("Main (AUC =", round(auc(roc_main), 3), ")"),
           paste("Interaction (AUC =", round(auc(roc_int), 3), ")")
         ),
         col = c("blue", "red"), lwd = 2)
}

# --- Step 3: Run for all biomarkers and both contrasts ---
for (bm in biomarkers) {
  run_and_plot(dataset, bm, c("CN", "MCI"))
  run_and_plot(dataset, bm, c("CN", "mildAD"))
}

# Run descriptives for predictors for power analysis
describe(dataset$AB4240_ratio_c)
describe(dataset$pTau181_logtr_c)
describe(dataset$PTAU217_logtr_c)
describe(dataset$ApoE4_sqrt_c)
describe(dataset$ASCVD_cont_score_sqrtc)

## --- Prospective power for biomarker × ASCVD interaction (lean, no files) ---

suppressPackageStartupMessages({
  library(MASS)      # mvrnorm
  library(dplyr)
  library(ggplot2)
})

set.seed(123)

# ---- Inputs ----
N_total   <- 745L
prev_case <- 444/745         # ≈ 0.596
alpha     <- 0.05
nsim      <- 500             # ↑ for smoother estimates, ↓ for speed

# Standardized predictors (mean=0, sd=1)
# Correlation (biomarker ↔ ASCVD)
rho_map <- c(
  "AB4240_ratio_c"  = -0.16,
  "pTau181_logtr_c" =  0.30,
  "PTAU217_logtr_c" =  0.21,
  "ApoE4_sqrt_c"    = -0.03
)

# Main effects (log-ORs) from your fitted models
beta_map <- list(
  "AB4240_ratio_c"  = list(beta_bio = log(0.78), beta_cvd = log(1.97)),
  "pTau181_logtr_c" = list(beta_bio = log(1.83), beta_cvd = log(1.43)),
  "PTAU217_logtr_c" = list(beta_bio = log(2.33), beta_cvd = log(1.47)),
  "ApoE4_sqrt_c"    = list(beta_bio = log(1.33), beta_cvd = log(1.71))
)

# Interaction OR grid (keep small for speed; expand later if you like)
interaction_ors   <- c(0.75, 0.80, 0.85, 0.90, 1.00, 1.10, 1.20)
interaction_betas <- log(interaction_ors)

# ---- Helpers ----
rmv2 <- function(n, rho){
  Sigma <- matrix(c(1, rho, rho, 1), ncol = 2)
  MASS::mvrnorm(n = n, mu = c(0,0), Sigma = Sigma)
}

solve_intercept <- function(beta_bio, beta_cvd, beta_int, rho, target_prev, n_approx = 8e4){
  X <- rmv2(n_approx, rho)
  bio <- X[,1]; cvd <- X[,2]
  f <- function(b0){
    p <- plogis(b0 + beta_bio*bio + beta_cvd*cvd + beta_int*(bio*cvd))
    mean(p) - target_prev
  }
  uniroot(f, interval = c(-8, 8))$root
}

power_for_one <- function(N, beta_bio, beta_cvd, beta_int, rho, prev_case, nsim = 1000, alpha = 0.05){
  b0 <- solve_intercept(beta_bio, beta_cvd, beta_int, rho, prev_case, n_approx = 6e4)
  sig <- 0L
  for (i in seq_len(nsim)){
    X <- rmv2(N, rho)
    bio <- X[,1]; cvd <- X[,2]
    y <- rbinom(N, 1, plogis(b0 + beta_bio*bio + beta_cvd*cvd + beta_int*(bio*cvd)))
    fit <- glm(y ~ bio*cvd, family = binomial())
    pval <- summary(fit)$coefficients["bio:cvd","Pr(>|z|)"]
    if (!is.na(pval) && pval < alpha) sig <- sig + 1L
  }
  sig / nsim
}

power_grid_for_biomarker <- function(biomarker){
  rho       <- unname(rho_map[biomarker])
  beta_bio  <- beta_map[[biomarker]]$beta_bio
  beta_cvd  <- beta_map[[biomarker]]$beta_cvd
  tibble(
    Biomarker = biomarker,
    Interaction_OR = interaction_ors,
    Power = vapply(interaction_betas, function(bi){
      power_for_one(N_total, beta_bio, beta_cvd, bi, rho, prev_case, nsim = nsim, alpha = alpha)
    }, numeric(1))
  )
}

# ---- Run ----
biomarkers <- names(rho_map)
power_results <- dplyr::bind_rows(lapply(biomarkers, power_grid_for_biomarker))

mde_summary <- power_results %>%
  group_by(Biomarker) %>%
  summarize(
    MDE80 = Interaction_OR[which.min(abs(Power - 0.80))],
    Power_at_MDE80 = Power[which.min(abs(Power - 0.80))],
    MDE90 = Interaction_OR[which.min(abs(Power - 0.90))],
    Power_at_MDE90 = Power[which.min(abs(Power - 0.90))]
  ) %>%
  ungroup()

# ---- Print clean summaries ----
cat("\n=== Power grid (rounded) ===\n")
print(power_results %>% mutate(Power = round(Power, 3)))

cat("\n=== Approx. Minimum Detectable Interaction ORs ===\n")
print(mde_summary %>% mutate(across(starts_with("Power"), ~round(.x, 3))))

# ---- Plot to the graphics window ----
for (bm in biomarkers){
  df <- dplyr::filter(power_results, Biomarker == bm)
  p <- ggplot(df, aes(x = Interaction_OR, y = Power)) +
    geom_line() + geom_point() +
    geom_hline(yintercept = 0.80, linetype = "dashed") +
    geom_hline(yintercept = 0.90, linetype = "dashed") +
    labs(title = paste("Prospective Power Curve:", bm),
         x = "Interaction OR (biomarker × ASCVD)",
         y = "Power") +
    theme_minimal(base_size = 12)
  print(p)
}

cat("\nDone. Increase `nsim` or widen `interaction_ors` for more precision.\n")

library(dplyr)
library(broom)
library(ggplot2)
library(forcats)

-------------------------------------------------
### 09/08/2025: Visualisations for Supplements

## CV Risk Stratification: 

# Load additional libraries
library(tidyverse)

# Data from table
dataset <- tribble(
  ~Group, ~Predictor, ~OR, ~CI_low, ~CI_high,
  "Incident CVD", "AB4240_ratio_c", 1.14, 0.77, 1.69,
  "Incident CVD", "ASCVD_cont_score_sqrt", 2.71, 1.44, 5.04,
  "No CVD", "AB4240_ratio_c", 0.79, 0.66, 0.94,
  "No CVD", "ASCVD_cont_score_sqrt", 1.57, 1.31, 1.87,
  
  "Incident CVD", "pTau181_logtr_c", 1.06, 0.65, 1.73,
  "Incident CVD", "ASCVD Score", 2.57, 1.37, 4.79,
  "No CVD", "pTau181_logtr_c", 1.97, 1.62, 2.40,
  "No CVD", "ASCVD_cont_score_sqrt", 1.37, 1.15, 1.64,
  
  "Incident CVD", "PTAU217_logtr_c", 1.79, 0.92, 3.48,
  "Incident CVD", "ASCVD Score", 2.45, 1.34, 4.52,
  "No CVD", "PTAU217_logtr_c", 2.40, 1.92, 2.96,
  "No CVD", "ASCVD_cont_score_sqrt", 1.41, 1.19, 1.69,
  
  "Incident CVD", "ApoE4_sqrt_c", 0.99, 0.62, 1.59,
  "Incident CVD", "ASCVD Score", 2.62, 1.42, 4.80,
  "No CVD", "ApoE4_sqrt_c", 1.37, 1.14, 1.63,
  "No CVD", "ASCVD_cont_score_sqrt", 1.66, 1.40, 1.99
)

# Order predictors within facets
dataset <- dataset %>%
  mutate(Predictor = factor(Predictor,
                            levels = rev(c("AB4240_ratio_c", "pTau181_logtr_c", "PTAU217_logtr_c", "ApoE4_sqrt_c", "ASCVD_cont_score_sqrt"))))

# Forest plot
ggplot(dataset, aes(x = OR, y = Predictor)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey50") +
  scale_x_log10() +
  facet_wrap(~ Group) +
  labs(x = "Odds Ratio (log scale)", y = NULL,
       title = "Odds Ratios for Biomarkers and ASCVD Score",
       subtitle = "Models stratified by CVD group") +
  theme_minimal()

# Version 2:

# Data from table
dataset <- tribble(
  ~Group, ~Predictor, ~OR, ~CI_low, ~CI_high,
  "Incident CVD", "Aβ42/Aβ40", 1.14, 0.77, 1.69,
  "Incident CVD", "ASCVD Score", 2.71, 1.44, 5.04,
  "No CVD", "Aβ42/Aβ40", 0.79, 0.66, 0.94,
  "No CVD", "ASCVD Score", 1.57, 1.31, 1.87,
  
  "Incident CVD", "P-tau181", 1.06, 0.65, 1.73,
  "Incident CVD", "ASCVD Score", 2.57, 1.37, 4.79,
  "No CVD", "P-tau181", 1.97, 1.62, 2.40,
  "No CVD", "ASCVD Score", 1.37, 1.15, 1.64,
  
  "Incident CVD", "P-tau217", 1.79, 0.92, 3.48,
  "Incident CVD", "ASCVD Score", 2.45, 1.34, 4.52,
  "No CVD", "P-tau217", 2.40, 1.92, 2.96,
  "No CVD", "ASCVD Score", 1.41, 1.19, 1.69,
  
  "Incident CVD", "ApoE4", 0.99, 0.62, 1.59,
  "Incident CVD", "ASCVD Score", 2.62, 1.42, 4.80,
  "No CVD", "ApoE4", 1.37, 1.14, 1.63,
  "No CVD", "ASCVD Score", 1.66, 1.40, 1.99
)

# Order predictors within facets
dataset <- dataset %>%
  mutate(Predictor = factor(Predictor,
                            levels = rev(c("Aβ42/Aβ40", "P-tau181", "P-tau217", "ApoE4", "ASCVD Score"))))

# Forest plot
ggplot(dataset, aes(x = OR, y = Predictor)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0) +
  geom_vline(xintercept = 1, linetype = 2, color = "red4") +
  scale_x_log10() +
  facet_wrap(~ Group) +
  labs(x = "Odds Ratio (log scale)", y = NULL,
       title = "Odds Ratios for Biomarkers and ASCVD Score",
       subtitle = "Models stratified by CVD group") +
  theme_minimal()

# Filter only biomarkers (exclude ASCVD Score)
bio_df <- dataset %>% filter(Predictor != "ASCVD Score")

# Group comparison plot
ggplot(bio_df, aes(x = Predictor, y = OR, color = Group)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey50") +
  scale_y_log10() +
  labs(x = "Biomarker", y = "Odds Ratio (log scale)",
       title = "Biomarker Associations with CI",
       subtitle = "Comparison between Incident CVD and No CVD groups",
       color = "Group") +
  theme_minimal()

# Load additional library
library(scales)

ggplot(bio_df, aes(x = Predictor, y = OR, color = Group)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey50") +
  scale_y_log10(
    breaks = c(0.5, 1, 2, 3),
    labels = c("0.5", "1", "2", "3")
  ) +
  labs(x = "Plasma Biomarker", y = "Odds Ratio",
       title = "Biomarker Associations with Cognitive Status",
       subtitle = "Comparison between Incident CV Risk and No CV Risk Groups",
       color = "Group") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # Center title
  )

-----------------------
# Sex Stratification:

# ---------- Enter results ----------
df <- tribble(
  ~Biomarker, ~Sex,     ~Model,         ~Term,            ~OR,  ~CI_low, ~CI_high,
  # p-tau181 — Males
  "p-tau181", "Male",   "Main",         "Biomarker",      1.67, 1.27,    2.19,
  "p-tau181", "Male",   "Main",         "ASCVD",          1.15, 0.84,    1.57,
  "p-tau181", "Male",   "Interaction",  "Biomarker",      1.89, 1.41,    2.54,
  "p-tau181", "Male",   "Interaction",  "ASCVD",          1.19, 0.87,    1.62,
  "p-tau181", "Male",   "Interaction",  "Interaction",    0.68, 0.49,    0.94,
  
  # p-tau181 — Females
  "p-tau181", "Female", "Main",         "Biomarker",      1.97, 1.56,    2.50,
  "p-tau181", "Female", "Main",         "ASCVD",          1.51, 1.21,    1.87,
  "p-tau181", "Female", "Interaction",  "Biomarker",      1.91, 1.48,    2.47,
  "p-tau181", "Female", "Interaction",  "ASCVD",          1.49, 1.20,    1.85,
  "p-tau181", "Female", "Interaction",  "Interaction",    0.93, 0.72,    1.20,
  
  # p-tau217 — Males
  "p-tau217", "Male",   "Main",         "Biomarker",      2.03, 1.52,    2.73,
  "p-tau217", "Male",   "Main",         "ASCVD",          1.26, 0.92,    1.72,
  "p-tau217", "Male",   "Interaction",  "Biomarker",      2.53, 1.71,    3.75,
  "p-tau217", "Male",   "Interaction",  "ASCVD",          1.22, 0.89,    1.67,
  "p-tau217", "Male",   "Interaction",  "Interaction",    0.62, 0.42,    0.92,
  
  # p-tau217 — Females
  "p-tau217", "Female", "Main",         "Biomarker",      2.58, 1.97,    3.40,
  "p-tau217", "Female", "Main",         "ASCVD",          1.49, 1.20,    1.85,
  "p-tau217", "Female", "Interaction",  "Biomarker",      2.53, 1.89,    3.40,
  "p-tau217", "Female", "Interaction",  "ASCVD",          1.46, 1.16,    1.85,
  "p-tau217", "Female", "Interaction",  "Interaction",    0.93, 0.69,    1.25
)

# helper: a minimal log-OR axis with intuitive ticks
scale_or <- scale_x_log10(breaks = c(0.5, 1, 2, 3, 4),
                          labels = c("0.5","1","2","3","4"))

base_theme <- theme_minimal() +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# ---------- Plot A: Biomarker term ----------
pA <- df %>%
  filter(Term == "Biomarker") %>%
  ggplot(aes(x = OR, y = Sex, color = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.5), height = 0) +
  geom_vline(xintercept = 1, linetype = 2) +
  facet_wrap(~ Biomarker, nrow = 1) +
  scale_or +
  labs(x = "Odds Ratio", y = NULL,
       title = "Biomarker effect (p-tau × CI), by Sex and Model") +
  base_theme

# ---------- Plot B: ASCVD term ----------
pB <- df %>%
  filter(Term == "ASCVD") %>%
  ggplot(aes(x = OR, y = Sex, color = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.5), height = 0) +
  geom_vline(xintercept = 1, linetype = 2) +
  facet_wrap(~ Biomarker, nrow = 1) +
  scale_or +
  labs(x = "Odds Ratio", y = NULL,
       title = "ASCVD effect in the same models, by Sex and Model") +
  base_theme

# ---------- Plot C: Interaction term ----------
pC <- df %>%
  filter(Term == "Interaction") %>%
  ggplot(aes(x = OR, y = Sex)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0) +
  geom_vline(xintercept = 1, linetype = 2) +
  facet_wrap(~ Biomarker, nrow = 1) +
  scale_or +
  labs(x = "Odds Ratio", y = NULL,
       title = "Interaction (p-tau × ASCVD) term, by Sex") +
  base_theme

pA; pB; pC

# Stratification by separate ASCVD components:

# ---- Data (for eAppendices) ----

# ---- Data (p-tau181 + separate ASCVD components) ---- # Replace with values for
# corresponding biomarker

# ---- Data (ApoE4 + separate ASCVD components) ----
abeta <- tribble(
  ~Model, ~Term,                 ~OR,    ~CI_low, ~CI_high, ~Type,
  "Age",  "ApoE4",               1.319,   1.126,   1.546,  "ApoE4",
  "Age",  "Age (per year)",      1.077,   1.053,   1.102,  "ASCVD component",
  
  "Gender","ApoE4",              1.286,   1.102,   1.502,  "ApoE4",
  "Gender","Male (vs ref)",      1.702,   1.258,   2.304,  "ASCVD component",
  
  "Ethnicity","ApoE4",           1.286,   1.104,   1.499,  "ApoE4",
  "Ethnicity","Black (vs ref)",  0.660,   0.259,   1.680,  "ASCVD component",
  "Ethnicity","Other (vs ref)",  0.972,   0.146,   6.452,  "ASCVD component",
  "Ethnicity","White (vs ref)",  0.581,   0.181,   1.865,  "ASCVD component",
  
  "HDL cholesterol","ApoE4",     1.285,   1.103,   1.499,  "ApoE4",
  "HDL cholesterol","HDL (mmol/L)",0.780, 0.543,   1.120,  "ASCVD component",
  
  "Total cholesterol","ApoE4",   1.310,   1.128,   1.521,  "ApoE4",
  "Total cholesterol","Total (mmol/L)",0.883,0.764,1.021,  "ASCVD component",
  
  "LDL cholesterol","ApoE4",     1.304,   1.123,   1.515,  "ApoE4",
  "LDL cholesterol","LDL (mg/dL)",0.997,  0.993,   1.002,  "ASCVD component",
  
  "Systolic BP","ApoE4",         1.290,   1.111,   1.497,  "ApoE4",
  "Systolic BP","SBP (mmHg)",    1.010,   1.001,   1.019,  "ASCVD component",
  
  "Hypertension medication","ApoE4",1.293,1.111,   1.505,  "ApoE4",
  "Hypertension medication","On HBP meds",1.412,1.050,1.898,"ASCVD component",
  
  "Current smoking","ApoE4",     1.287,   1.106,   1.499,  "ApoE4",
  "Current smoking","Smoker",    1.459,   0.756,   2.818,  "ASCVD component",
  
  "Diabetes","ApoE4",            1.285,   1.105,   1.498,  "ApoE4",
  "Diabetes","Diabetes",         0.964,   0.544,   1.708,  "ASCVD component"
) %>%
  mutate(
    Row  = factor(paste(Model, "-", Term), levels = rev(unique(paste(Model, "-", Term)))),
    Type = factor(Type, levels = c("ApoE4","ASCVD component"))
  )

# ---- Plot (ApoE4) ----
unique(abeta$Type)  # should be: "ApoE4" "ASCVD component"

p <- ggplot(abeta, aes(x = OR, y = Row, color = Type)) +
  geom_point(size = 1.6) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0, size = 0.5) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey50") +
  scale_x_log10(breaks = c(0.5, 1, 1.5, 2, 3),
                labels = c("0.5","1","1.5","2","3")) +
  scale_color_manual(
    values = c("ApoE4" = "#1E88E5", "ASCVD component" = "#D55E00"),
    breaks = c("ApoE4", "ASCVD component"),
    labels = c("ApoE4 term", "ASCVD component term"),
    name   = "Term"
  ) +
  labs(x = "Odds Ratio (log scale)", y = NULL,
       title = "ApoE4 with separate ASCVD components") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.y = element_text(size = 6, lineheight = 1.0),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

---------------------------------------------------------------------------------
### 28/10/2025: Exploring non-linear associations - Response to JAHA reviewers 

## ----------------------------
## Setup
## ----------------------------

y  <- "AD_status"
z  <- "ASCVD_cont_score_sqrtc"

# Biomarkers
biomarkers <- c("AB4240_ratio_c", "pTau181_logtr_c", "PTAU217_logtr_c", "ApoE4_sqrt_c")

## ----------------------------
## Helper: fit models & LRTs
## ----------------------------
fit_mods_and_tests <- function(x, data) {
  # 0) MAIN EFFECTS (no interaction)
  f_main <- as.formula(paste0(y, " ~ ", x, " + ", z))
  m_main <- glm(f_main, data = data, family = binomial)
  
  # 1) LINEAR INTERACTION
  f_int <- as.formula(paste0(y, " ~ ", x, " * ", z))
  m_int <- glm(f_int, data = data, family = binomial)
  
  # 2) QUADRATIC MAIN EFFECTS (keep interaction linear)
  f_quad_main <- as.formula(paste0(
    y, " ~ ", x, " * ", z,
    " + I(", x, "^2) + I(", z, "^2)"
  ))
  m_qmain <- glm(f_quad_main, data = data, family = binomial)
  
  # 3) NONLINEAR MODERATION (allow squared terms to interact)
  f_quad_int <- as.formula(paste0(
    y, " ~ ", x, " * ", z,
    " + I(", x, "^2) + I(", z, "^2)",
    " + I(", x, "^2):", z,
    " + ", x, ":I(", z, "^2)"
  ))
  m_qint <- glm(f_quad_int, data = data, family = binomial)
  
  # Likelihood-ratio tests
  lrt_int        <- anova(m_main,  m_int,   test = "LRT")  # does interaction help?
  lrt_quad_main  <- anova(m_int,   m_qmain, test = "LRT")  # do x^2 & z^2 help?
  lrt_quad_int   <- anova(m_qmain, m_qint,  test = "LRT")  # does nonlinear moderation help?
  
  # Extract p-values neatly
  get_p <- function(aov_obj) {
    as.numeric(tail(aov_obj$`Pr(>Chi)`, 1))
  }
  
  out <- data.frame(
    biomarker = x,
    p_interaction_LRT          = get_p(lrt_int),
    p_quad_main_LRT            = get_p(lrt_quad_main),
    p_quad_interaction_LRT     = get_p(lrt_quad_int),
    AIC_linear_interaction     = AIC(m_int),
    AIC_quad_main              = AIC(m_qmain),
    AIC_quad_interaction       = AIC(m_qint),
    row.names = NULL
  )
  
  # Return models and summary row
  list(models = list(
    main    = m_main,
    int     = m_int,
    q_main  = m_qmain,
    q_int   = m_qint
  ),
  lrt_row = out)
}

## ----------------------------
## Run for all biomarkers
## ----------------------------
all_results <- lapply(biomarkers, fit_mods_and_tests, data = dataset)

# Collect p-values/AIC in a single table
lrt_table <- do.call(rbind, lapply(all_results, `[[`, "lrt_row"))
print(lrt_table)

## ----------------------------
## (Optional) Quick odds ratios for the linear interaction model
## ----------------------------
or_tables <- lapply(all_results, function(res) {
  m <- res$models$int
  data.frame(
    term = names(coef(m)),
    OR   = exp(coef(m)),
    lower = exp(coef(m) - 1.96 * sqrt(diag(vcov(m)))),
    upper = exp(coef(m) + 1.96 * sqrt(diag(vcov(m))))
  )
})
names(or_tables) <- biomarkers
# Example: view ORs for AB42/40 interaction model
or_tables$AB4240_ratio_c



