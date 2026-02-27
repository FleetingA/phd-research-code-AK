
### ---------- CVD RS Calculation - 20/10/2024 ----------

# Load final dataset with all variables
library(readr)
final_merged_dataset_080924 <- read_csv("Angelina_Bio_Hermes_R_code/Univariate_Regressions_Added_070924/final_merged_dataset_080924.csv")
View(final_merged_dataset_080924) # 671 entries

summary(final_merged_dataset_080924$ongoing_med_for_HBP)
table(final_merged_dataset_080924$ongoing_med_for_HBP)

# Rename SBP and DBP variables 
library(data.table)
setnames(final_merged_dataset_080924, old = "Systolic Blood Pressure", new = "Systolic_Blood_Pressure")
setnames(final_merged_dataset_080924, old = "Diastolic Blood Pressure", new = "Diastolic_Blood_Pressure")

# Load data anew when repeating the analyses with the cleaned data from outliers 
# 20/10/2024

library(readr)
final_merged_dataset_170924 <- read_csv("Angelina_Bio_Hermes_R_code/Univariate_Regressions_Added_070924/final_merged_dataset_170924.csv")
View(final_merged_dataset_170924)

# Drop a few columns by name
library(dplyr)
final_merged_dataset_170924 <- final_merged_dataset_170924 %>%
  select(-framingham_risk_score)

######## --- CVD Risk Score Calculation - 17/10/24 --- ########

library(readr)
final_merged_dataset_170924 <- read_csv("Angelina_Bio_Hermes_R_code/Univariate_Regressions_Added_070924/final_merged_dataset_170924.csv")
View(final_merged_dataset_170924)

# Remove the first two columns from the data frame as they are redundant (...1 and ...2)
final_merged_dataset_170924 <- final_merged_dataset_170924[, -c(1, 2)]
View(final_merged_dataset_170924)

# Vector of USUBJIDs from MH processed file that correspond to diabetes status = 1
usubjid_vector <- c(
  "BIO-HERMES-00101-046", "BIO-HERMES-00101-095", "BIO-HERMES-00101-099", "BIO-HERMES-00101-103",
  "BIO-HERMES-00102-003", "BIO-HERMES-00102-004", "BIO-HERMES-00102-009", "BIO-HERMES-00102-020",
  "BIO-HERMES-00102-024", "BIO-HERMES-00102-028", "BIO-HERMES-00102-036", "BIO-HERMES-00102-068",
  "BIO-HERMES-00102-074", "BIO-HERMES-00102-076", "BIO-HERMES-00102-083", "BIO-HERMES-00103-020",
  "BIO-HERMES-00103-025", "BIO-HERMES-00103-060", "BIO-HERMES-00104-005", "BIO-HERMES-00105-012",
  "BIO-HERMES-00105-016", "BIO-HERMES-00105-021", "BIO-HERMES-00105-028", "BIO-HERMES-00105-038",
  "BIO-HERMES-00105-043", "BIO-HERMES-00105-048", "BIO-HERMES-00105-049", "BIO-HERMES-00105-066",
  "BIO-HERMES-00105-068", "BIO-HERMES-00105-085", "BIO-HERMES-00105-086", "BIO-HERMES-00105-087",
  "BIO-HERMES-00105-090", "BIO-HERMES-00105-110", "BIO-HERMES-00106-004", "BIO-HERMES-00106-018",
  "BIO-HERMES-00106-024", "BIO-HERMES-00106-026", "BIO-HERMES-00106-031", "BIO-HERMES-00106-032",
  "BIO-HERMES-00106-044", "BIO-HERMES-00106-046", "BIO-HERMES-00106-054", "BIO-HERMES-00106-059",
  "BIO-HERMES-00106-067", "BIO-HERMES-00106-072", "BIO-HERMES-00106-078", "BIO-HERMES-00106-079",
  "BIO-HERMES-00106-081", "BIO-HERMES-00106-088", "BIO-HERMES-00106-089", "BIO-HERMES-00107-012",
  "BIO-HERMES-00107-016", "BIO-HERMES-00107-019", "BIO-HERMES-00107-021", "BIO-HERMES-00107-022",
  "BIO-HERMES-00107-031", "BIO-HERMES-00107-034", "BIO-HERMES-00107-037", "BIO-HERMES-00107-039",
  "BIO-HERMES-00107-043", "BIO-HERMES-00107-051", "BIO-HERMES-00108-007", "BIO-HERMES-00108-009",
  "BIO-HERMES-00108-010", "BIO-HERMES-00108-016", "BIO-HERMES-00108-017", "BIO-HERMES-00109-003",
  "BIO-HERMES-00109-006", "BIO-HERMES-00109-071", "BIO-HERMES-00109-073", "BIO-HERMES-00110-003",
  "BIO-HERMES-00110-015", "BIO-HERMES-00110-018", "BIO-HERMES-00110-034", "BIO-HERMES-00110-064",
  "BIO-HERMES-00110-068", "BIO-HERMES-00111-014", "BIO-HERMES-00111-033", "BIO-HERMES-00111-042",
  "BIO-HERMES-00111-044", "BIO-HERMES-00111-046", "BIO-HERMES-00111-050", "BIO-HERMES-00112-021",
  "BIO-HERMES-00112-028", "BIO-HERMES-00112-034", "BIO-HERMES-00112-039", "BIO-HERMES-00112-042",
  "BIO-HERMES-00112-043", "BIO-HERMES-00112-050", "BIO-HERMES-00112-051", "BIO-HERMES-00112-053",
  "BIO-HERMES-00113-002", "BIO-HERMES-00113-007", "BIO-HERMES-00113-008", "BIO-HERMES-00113-009",
  "BIO-HERMES-00113-010", "BIO-HERMES-00113-011", "BIO-HERMES-00114-001", "BIO-HERMES-00114-002",
  "BIO-HERMES-00114-011", "BIO-HERMES-00114-015", "BIO-HERMES-00114-020", "BIO-HERMES-00115-002",
  "BIO-HERMES-00115-005"
)

# Check if the IDs in usubjid_vector are in the USUBJID column of the 
# final_merged_dataset_170924 data frame

matching_ids <- final_merged_dataset_170924$USUBJID %in% usubjid_vector

# Optional: View which USUBJIDs match
matched_usubjids <- final_merged_dataset_170924[matching_ids, ]

# Print the result
print(matched_usubjids)
View(matched_usubjids) # 63 matched 

# List of matched 63 USUBJIDs
matched_usubjids <- c(
  "BIO-HERMES-00102-036", "BIO-HERMES-00102-068", "BIO-HERMES-00102-074", "BIO-HERMES-00102-076", 
  "BIO-HERMES-00102-083", "BIO-HERMES-00103-025", "BIO-HERMES-00103-060", "BIO-HERMES-00104-005",
  "BIO-HERMES-00105-012", "BIO-HERMES-00105-016", "BIO-HERMES-00105-043", "BIO-HERMES-00105-048",
  "BIO-HERMES-00105-068", "BIO-HERMES-00105-085", "BIO-HERMES-00105-086", "BIO-HERMES-00105-087",
  "BIO-HERMES-00105-110", "BIO-HERMES-00106-004", "BIO-HERMES-00106-018", "BIO-HERMES-00106-024",
  "BIO-HERMES-00106-026", "BIO-HERMES-00106-031", "BIO-HERMES-00106-032", "BIO-HERMES-00106-044",
  "BIO-HERMES-00106-046", "BIO-HERMES-00106-054", "BIO-HERMES-00106-067", "BIO-HERMES-00106-072",
  "BIO-HERMES-00106-079", "BIO-HERMES-00106-081", "BIO-HERMES-00106-088", "BIO-HERMES-00107-012",
  "BIO-HERMES-00107-016", "BIO-HERMES-00107-019", "BIO-HERMES-00107-021", "BIO-HERMES-00107-031",
  "BIO-HERMES-00107-034", "BIO-HERMES-00107-037", "BIO-HERMES-00107-043", "BIO-HERMES-00108-007",
  "BIO-HERMES-00108-010", "BIO-HERMES-00108-017", "BIO-HERMES-00109-003", "BIO-HERMES-00109-006",
  "BIO-HERMES-00109-073", "BIO-HERMES-00110-003", "BIO-HERMES-00110-015", "BIO-HERMES-00110-064",
  "BIO-HERMES-00110-068", "BIO-HERMES-00112-021", "BIO-HERMES-00112-028", "BIO-HERMES-00112-039",
  "BIO-HERMES-00112-042", "BIO-HERMES-00112-043", "BIO-HERMES-00113-002", "BIO-HERMES-00113-007",
  "BIO-HERMES-00114-001", "BIO-HERMES-00114-002", "BIO-HERMES-00114-011", "BIO-HERMES-00114-015",
  "BIO-HERMES-00114-020", "BIO-HERMES-00115-002", "BIO-HERMES-00115-005"
)

# Create Diabetes_status variable, setting it to 1 for matched USUBJIDs and 0 otherwise
final_merged_dataset_170924$Diabetes_status <- ifelse(final_merged_dataset_170924$USUBJID %in% matched_usubjids, 1, 0)

# Optional: Preview the updated dataset
View(final_merged_dataset_170924)
table(final_merged_dataset_170924$Diabetes_status)

# Create automated CVD risk score with appropriate R package

# Installing from Bioconductor if it is hosted there
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("frs")
library(frs) # did not work for frs

# Try CVrisk instead
install.packages("CVRisk")
library(CVrisk)
packageVersion("CVRisk")

# Drop the FRS columns that were calculated - of no use atm
install.packages("dplyr")
library(dplyr)

View(final_merged_dataset_170924)
table(final_merged_dataset_170924)

# Run some descriptives
library(psych)
describe(final_merged_dataset_170924[, c("AGE", "HDL_Cholesterol", "Total_cholesterol", "Systolic_Blood_Pressure", 
                                         "Diastolic_Blood_Pressure")])

# Frequency counts for multiple variables
table(final_merged_dataset_170924$SEX_recode)
table(final_merged_dataset_170924$Diabetes_status)
table(final_merged_dataset_170924$RACE_recode)
table(final_merged_dataset_170924$ETHNIC_recode)
describe(final_merged_dataset_170924$Diastolic_Blood_Pressure)
describe(final_merged_dataset_170924$Systolic_Blood_Pressure)
hist(final_merged_dataset_170924$Systolic_Blood_Pressure)
hist(final_merged_dataset_170924$Diastolic_Blood_Pressure)
describe(final_merged_dataset_170924$Total_cholesterol)

## FURTHER PRE-PROCESSING
# Imputation of some values that do not make a lot of sense

# Check and verify the row for the participant with subject ID "BIO-HERMES-00102-002"
final_merged_dataset_170924[final_merged_dataset_170924$USUBJID == "BIO-HERMES-00102-002", ]

# Impute the Diastolic Blood Pressure for this participant with the median value of 78
final_merged_dataset_170924$Diastolic_Blood_Pressure[final_merged_dataset_170924$USUBJID == "BIO-HERMES-00102-002"] <- 78

# Verify that the value has been imputed correctly
final_merged_dataset_170924[final_merged_dataset_170924$USUBJID == "BIO-HERMES-00102-002", ]

# Impute SBP values for the below participants with the median of 133
final_merged_dataset_170924$Systolic_Blood_Pressure[
  final_merged_dataset_170924$USUBJID %in% c("BIO-HERMES-00102-049", "BIO-HERMES-00102-053", 
                                             "BIO-HERMES-00102-058", "BIO-HERMES-00102-048")
] <- 133

# Verify the changes by checking the updated participants' SBP values
final_merged_dataset_170924[final_merged_dataset_170924$USUBJID %in% c("BIO-HERMES-00102-049", "BIO-HERMES-00102-053", 
                                                                       "BIO-HERMES-00102-058", "BIO-HERMES-00102-048"), ]

# Impute DBP values for the below participants with the median of 78 as well
final_merged_dataset_170924$Diastolic_Blood_Pressure[
  final_merged_dataset_170924$USUBJID %in% c("BIO-HERMES-00102-049", "BIO-HERMES-00102-053", 
                                             "BIO-HERMES-00102-058", "BIO-HERMES-00102-048")
] <- 78

# Check also for HDL and LDL cholesterol
describe(final_merged_dataset_170924$HDL_Cholesterol)
hist(final_merged_dataset_170924$HDL_Cholesterol) # skewed
describe(final_merged_dataset_170924$LDL_Cholesterol)
hist(final_merged_dataset_170924$LDL_Cholesterol) # skewed

# Identify outliers

# Find participants with values of 999 in LDL cholesterol
outliers <- final_merged_dataset_170924[
  final_merged_dataset_170924$LDL_Cholesterol >= 999, 
  c("USUBJID", "LDL_Cholesterol")
]

# Display the participants with these values
print(outliers)

# Impute LDL Cholesterol values of 999 with the median value of 87
final_merged_dataset_170924$LDL_Cholesterol[
  final_merged_dataset_170924$USUBJID %in% c(
    "BIO-HERMES-00101-045", "BIO-HERMES-00101-052", "BIO-HERMES-00101-092",
    "BIO-HERMES-00104-005", "BIO-HERMES-00105-017", "BIO-HERMES-00109-055",
    "BIO-HERMES-00109-109", "BIO-HERMES-00109-116", "BIO-HERMES-00109-130",
    "BIO-HERMES-00109-148", "BIO-HERMES-00109-150", "BIO-HERMES-00109-153",
    "BIO-HERMES-00109-155", "BIO-HERMES-00109-157"
  )
] <- 87

# Verify the changes by checking the updated participants' LDL values
final_merged_dataset_170924[final_merged_dataset_170924$USUBJID %in% c(
  "BIO-HERMES-00101-045", "BIO-HERMES-00101-052", "BIO-HERMES-00101-092",
  "BIO-HERMES-00104-005", "BIO-HERMES-00105-017", "BIO-HERMES-00109-055",
  "BIO-HERMES-00109-109", "BIO-HERMES-00109-116", "BIO-HERMES-00109-130",
  "BIO-HERMES-00109-148", "BIO-HERMES-00109-150", "BIO-HERMES-00109-153",
  "BIO-HERMES-00109-155", "BIO-HERMES-00109-157"
), c("USUBJID", "LDL_Cholesterol")]

# Find participants with values of 999 in HDL cholesterol
outliers1 <- final_merged_dataset_170924[
  final_merged_dataset_170924$HDL_Cholesterol >= 999, 
  c("USUBJID", "HDL_Cholesterol")
]

# Display the participants with these values
print(outliers1)

# Impute HDL Cholesterol values of 999 with the median value of 59
final_merged_dataset_170924$HDL_Cholesterol[
  final_merged_dataset_170924$USUBJID %in% c(
    "BIO-HERMES-00101-092",             
    "BIO-HERMES-00109-055",           
    "BIO-HERMES-00109-109",             
    "BIO-HERMES-00109-130",             
    "BIO-HERMES-00109-148",             
    "BIO-HERMES-00109-150",             
    "BIO-HERMES-00109-153",            
    "BIO-HERMES-00109-155"
  )
] <- 59

# Verify the changes by checking the updated participants' LDL values
final_merged_dataset_170924[final_merged_dataset_170924$USUBJID %in% c(
  "BIO-HERMES-00101-092",             
  "BIO-HERMES-00109-055",           
  "BIO-HERMES-00109-109",             
  "BIO-HERMES-00109-130",             
  "BIO-HERMES-00109-148",             
  "BIO-HERMES-00109-150",             
  "BIO-HERMES-00109-153",            
  "BIO-HERMES-00109-155"
), c("USUBJID", "HDL_Cholesterol")]


# Further transformations

# Impute LDL Cholesterol for BIO-HERMES-00103-037 with the median of 87
final_merged_dataset_170924$LDL_Cholesterol[
  final_merged_dataset_170924$USUBJID == "BIO-HERMES-00103-037"
] <- 87

# Verify the change
final_merged_dataset_170924[final_merged_dataset_170924$USUBJID == "BIO-HERMES-00103-037", c("USUBJID", "LDL_Cholesterol")]

# Impute HDL Cholesterol for BIO-HERMES-00110-071 with the median of 59
final_merged_dataset_170924$HDL_Cholesterol[
  final_merged_dataset_170924$USUBJID == "BIO-HERMES-00110-071"
] <- 59

# Verify the change
final_merged_dataset_170924[final_merged_dataset_170924$USUBJID == "BIO-HERMES-00110-071", c("USUBJID", "HDL_Cholesterol")]

# Find participants with Total Cholesterol values of 1000 or above
high_total_chol <- final_merged_dataset_170924[
  final_merged_dataset_170924$Total_cholesterol >= 1000, 
  c("USUBJID", "Total_cholesterol")
]

# Display the participants with these values
print(high_total_chol)

describe(final_merged_dataset_170924$Total_cholesterol)
hist(final_merged_dataset_170924$Total_cholesterol) # skewed

# Impute Total Cholesterol values of 1000 or above with the median of 172.2
final_merged_dataset_170924$Total_cholesterol[
  final_merged_dataset_170924$USUBJID %in% c(
    "BIO-HERMES-00101-045", "BIO-HERMES-00101-052", "BIO-HERMES-00101-092", 
    "BIO-HERMES-00104-005", "BIO-HERMES-00105-017", "BIO-HERMES-00109-055", 
    "BIO-HERMES-00109-109", "BIO-HERMES-00109-116", "BIO-HERMES-00109-130", 
    "BIO-HERMES-00109-148", "BIO-HERMES-00109-150", "BIO-HERMES-00109-153", 
    "BIO-HERMES-00109-155", "BIO-HERMES-00109-157"
  )
] <- 172.2

# Verify the changes by checking the updated participants' Total Cholesterol values
final_merged_dataset_170924[final_merged_dataset_170924$USUBJID %in% c(
  "BIO-HERMES-00101-045", "BIO-HERMES-00101-052", "BIO-HERMES-00101-092", 
  "BIO-HERMES-00104-005", "BIO-HERMES-00105-017", "BIO-HERMES-00109-055", 
  "BIO-HERMES-00109-109", "BIO-HERMES-00109-116", "BIO-HERMES-00109-130", 
  "BIO-HERMES-00109-148", "BIO-HERMES-00109-150", "BIO-HERMES-00109-153", 
  "BIO-HERMES-00109-155", "BIO-HERMES-00109-157"
), c("USUBJID", "Total_cholesterol")]

# Check for duplicates just in case
duplicate_ids <- final_merged_dataset_170924$USUBJID[duplicated(final_merged_dataset_170924$USUBJID)]

# If there are no duplicates
if (length(duplicate_ids) == 0) {
  print("All USUBJID entries are unique.")
} else {
  print("There are duplicate USUBJID entries.")
  print(duplicate_ids)
}

# All ID entries are unique. 

View(final_merged_dataset_170924)

# Installing from Bioconductor if it is hosted there
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("frs")

install.packages("devtools")
devtools::install_github("PHP2560-Statistical-Programming-R/r-framingham")
library(frs)

# install.packages("devtools")
devtools::install_github("PHP2560-Statistical-Programming-R/r-framingham")

View(final_merged_dataset_170924)

## CVD Risk Score Calculation - Manual/bespoke
final_merged_dataset_170924$CVD_risk <- with(final_merged_dataset_170924, {
  risk_score <- 0
  risk_score <- risk_score + (SEX_recode == 1)
  risk_score <- risk_score + (AGE >= 75)
  risk_score <- risk_score + (Current_smoking_binary == 1)
  risk_score <- risk_score + (Total_cholesterol >= 240)
  risk_score <- risk_score + (HDL_Cholesterol <= 59)
  risk_score <- risk_score + (Systolic_Blood_Pressure >= 130 | ongoing_med_for_HBP == 1)
  risk_score
})

# Check the distribution of CVD_risk scores
table(final_merged_dataset_170924$CVD_risk)
View(final_merged_dataset_170924)
hist(final_merged_dataset_170924$CVD_risk)

# Plotting the CVD groups

# Define counts
custom_counts <- c(50, 123, 188, 192, 100, 8)

# Define breaks
breaks <- seq(-0.5, 5.5, by=1)

# Create the histogram and store the result
hist_data <- hist(
  final_merged_dataset_170924$CVD_risk,
  breaks = breaks,
  col = "lightblue3",
  border = "black",
  main = "Histogram of CVD Risk Scores",
  xlab = "CVD Risk Score",
  ylab = "Frequency",
  ylim = c(0, max(custom_counts) + 30)  # Increased ylim for better label spacing
)

# Add "n=" labels above each bar
text(
  x = hist_data$mids,                     
  y = custom_counts + 10,                 
  labels = paste0("n=", custom_counts),   
  cex = 0.8,                             
  pos = 3                                 
)

# Calculate CVD RS with LDL-c as well 

final_merged_dataset_170924$CVD_risk_2 <- with(final_merged_dataset_170924, {
  risk_score <- 0
  risk_score <- risk_score + (SEX_recode == 1)
  risk_score <- risk_score + (AGE >= 75)
  risk_score <- risk_score + (Current_smoking_binary == 1)
  risk_score <- risk_score + (Total_cholesterol >= 240)
  risk_score <- risk_score + (LDL_Cholesterol >= 100)
  risk_score <- risk_score + (Systolic_Blood_Pressure >= 130 | ongoing_med_for_HBP == 1)
  risk_score
})

# Check the distribution of CVD_risk scores
table(final_merged_dataset_170924$CVD_risk_2)
hist(final_merged_dataset_170924$CVD_risk_2)

# Plot this score as well
# Calculate counts using table()
counts <- table(final_merged_dataset_170924$CVD_risk_2)

# Define the labels
labels <- paste0("n=", counts)

# Create the barplot
bar_positions <- barplot(
  counts,
  col = "lightpink",
  border = "black",
  main = "Histogram of CVD Risk Scores",
  xlab = "CVD Risk Score 2",
  ylab = "Frequency",
  ylim = c(0, max(counts) + 30)          
)

# Add "n=" labels above each bar
text(
  x = bar_positions,
  y = counts + 5,                        
  labels = labels,
  cex = 0.8,
  pos = 3                                 
)

# More data checks and transformations

# Create an AB42/40 ratio variable

# Units for AB40: ng/mL
# Units for AB42: pg/mL

# View summary statistics
summary(final_merged_dataset_170924$AMYLB40_Roche)
summary(final_merged_dataset_170924$AMYLB42_Roche)

# Plot histograms to check distributions
hist(final_merged_dataset_170924$AMYLB40_Roche, main = "Histogram of AMYLB40_Roche (ng/mL)", xlab = "AMYLB40_Roche (ng/mL)")
hist(final_merged_dataset_170924$AMYLB42_Roche, main = "Histogram of AMYLB42_Roche (pg/mL)", xlab = "AMYLB42_Roche (pg/mL)")

# Convert AMYLB40_Roche from ng/mL to pg/mL
final_merged_dataset_170924$AMYLB40_Roche_pgmL <- final_merged_dataset_170924$AMYLB40_Roche * 1000

# Create ratio AB42/40 variable
final_merged_dataset_170924$AB_4240_Roche <- final_merged_dataset_170924$AMYLB42_Roche/final_merged_dataset_170924$AMYLB40_Roche_pgmL

# Verify the new variable
summary(final_merged_dataset_170924$AB_4240_Roche)
hist(final_merged_dataset_170924$AB_4240_Roche)
hist(final_merged_dataset_170924$AB_4240_Roche, main = "Histogram of AB_4240 Ratio", xlab = "AB_4240 (pg/mL/pg/mL)")

##### Run MODERATED MULTIPLE REGRESSION analyses - Main - 20/10/2024 #####

View(final_merged_dataset_170924)

# Check the data types before re-running analyses
str(final_merged_dataset_170924)

# Center AB42/40_Roche (continuous variable)
final_merged_dataset_170924$AB_4240_Roche_c <- scale(final_merged_dataset_170924$AB_4240_Roche, center = TRUE, scale = TRUE)

# Explore the ethnicity variable 
summary(final_merged_dataset_170924$ETHNIC_recode)

# Exclude cases where ethnicity = not reported
final_merged_dataset_170924 <- subset(final_merged_dataset_170924, ETHNIC_recode != 2)

# Verify the exclusion
table(final_merged_dataset_170924$ETHNIC_recode)
View(final_merged_dataset_170924)

library(dplyr)
final_merged_dataset_170924 <- final_merged_dataset_170924 %>%
  mutate(ETHNIC_recode = case_when(
    ETHNIC == "NOT HISPANIC OR LATINO" ~ "0",
    ETHNIC == "HISPANIC OR LATINO" ~ "1",
    ETHNIC == "NOT REPORTED" ~ "2",
    TRUE ~ "NA"  # This handles any unexpected values
  ))

# Exclude cases where ethnicity = not reported
final_merged_dataset_170924 <- subset(final_merged_dataset_170924, ETHNIC_recode != 2)

### AB42/40 ###

# Main Effects Model
mmr_main <- glm(AD_status ~ AB_4240_Roche_c + CVD_risk + ETHNIC_recode, 
                data = final_merged_dataset_170924, 
                family = binomial)

summary(mmr_main)

# Calculate R squared - load necessary libraries
install.packages(c("pscl", "DescTools", "sjstats"))
library(pscl)
library(DescTools)
library(sjstats)

# Checking for multicollinearity
install.packages("car")
library(car)
vif_values <- vif(mmr_main)
print(vif_values)
# Calculate correlation matrix
cor_matrix <- cor(final_merged_dataset_170924[, c("AB_4240_Roche_c", "CVD_risk", "ETHNIC_recode")], use = "complete.obs")
# Print correlation matrix
print(cor_matrix)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_main)
print(pseudo_r2)

# Install and load ggplot2 if not already installed
install.packages("ggplot2")  # Skip if already installed
library(ggplot2)

# Plot AB42/40 Ratio vs. Predicted Probability of AD
library(ggplot2)
ggplot(final_merged_dataset_170924, aes(x = AB_4240_Roche_c, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of AB42/40 Ratio on Probability of AD",
       x = "Centered AB42/40 Ratio",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot CVD Risk vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = CVD_risk_c, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of CVD Risk on Probability of AD",
       x = "Centered CVD Risk",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot Ethnicity vs. Predicted Probability of AD

# Convert ETHNIC_recode to a factor with descriptive labels
final_merged_dataset_170924$ETHNIC_recode <- factor(final_merged_dataset_170924$ETHNIC_recode,
                                                    levels = c(0, 1),
                                                    labels = c("Not Hispanic or Latino", 
                                                               "Hispanic or Latino"))

## OPTIONAL 
# Enhanced Plot with Centered Title and Descriptive Legend
ggplot(final_merged_dataset_170924, aes(x = ETHNIC_recode, y = AD_status, fill = ETHNIC_recode)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Effect of Ethnicity on Probability of AD",
       x = "Ethnicity",
       y = "Mean Probability of AD",
       fill = "Ethnicity") +  # Assigning a descriptive legend title
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centering and styling the title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Interaction Model: AB_4240_Roche x CVD_risk
mmr_interaction <- glm(AD_status ~ AB_4240_Roche_c * CVD_risk + ETHNIC_recode, 
                       data = final_merged_dataset_170924, 
                       family = binomial)

summary(mmr_interaction)

# Calculate Pseudo-R-squared measures
install.packages("pscl")
library(pscl)
pseudo_r2 <- pR2(mmr_interaction)
print(pseudo_r2)
library(car)
vif_values <- vif(mmr_interaction)
print(vif_values)

# Assuming you have calculated VIFs and stored in vif_values
library(ggplot2)
# Convert VIF values to a data frame
vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "cyan4") +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown4", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

## OPTIONAL
# Install required packages if not already installed
install.packages(c("interactions", "ggplot2"))

# Load the packages
library(interactions)

# Check if the 'interact_plot' function is available
exists("interact_plot", where = "package:interactions")

# Plot the non-significant interaction with annotations
interact_plot(mmr_interaction, 
              pred = AB_4240_Roche_c, 
              modx = CVD_risk, 
              plot.points = TRUE, 
              jitter = 0.1,
              main.title = "Moderation of CVD Risk on AB42/40 Ratio and AD Status",
              x.label = "Centered AB42/40 Ratio",
              y.label = "Predicted Probability of AD",
              legend.main = "CVD Risk",
              interval = TRUE,  
              int.width = 0.1,   
              colors = "RdYlBu") 

# Interaction Model: AB_4240_Roche x CVD_risk and AB_4240_Roche x ethnicity
mmr_full <- glm(AD_status ~ AB_4240_Roche_c * CVD_risk + AB_4240_Roche_c * ETHNIC_recode, 
                data = final_merged_dataset_170924, 
                family = binomial)

summary(mmr_full)

# Assuming you have calculated VIFs and stored in vif_values
library(ggplot2)
# Convert VIF values to a data frame
vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

pseudo_r2 <- pR2(mmr_full)
print(pseudo_r2)
library(car)
vif_values <- vif(mmr_full)
print(vif_values)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "cyan4") +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown4", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

####### Experiment with Ridge Regression - 21/10/2024: ####### 

library(readr)
final_merged_dataset_170924 <- read_csv("Angelina_Bio_Hermes_R_code/Univariate_Regressions_Added_070924/final_merged_dataset_170924.csv",
                                        show_col_types = FALSE)
View(final_merged_dataset_170924)

# First, install and load required package
install.packages("glmnet")
library(glmnet)

# Convert predictors to a matrix (excluding the outcome variable)
X <- model.matrix(AD_status ~ AB_4240_Roche_c * CVD_risk + AB_4240_Roche_c * ETHNIC_recode, 
                  data = final_merged_dataset_170924)[,-1]

# Convert AD_status to a numeric vector (0/1 for binary outcome)
y <- as.numeric(final_merged_dataset_170924$AD_status)
View(final_merged_dataset_170924)

# Fit Ridge logistic regression model (alpha = 0 for Ridge, family = "binomial" for logistic regression)
ridge_model <- glmnet(X, y, alpha = 0, family = "binomial")

# View the coefficients at different lambda values (penalty strength)
print(ridge_model)

# Perform cross-validation to find the optimal lambda for Ridge
cv_ridge <- cv.glmnet(X, y, alpha = 0, family = "binomial")

# Get the optimal lambda value
best_lambda_ridge <- cv_ridge$lambda.min
print(best_lambda_ridge) # the value I obtain is 0.01418183

# Fit final Ridge model with optimal lambda
final_ridge_model <- glmnet(X, y, alpha = 0, family = "binomial", lambda = best_lambda_ridge)

# View the coefficients of the final model
ridge_coef <- coef(final_ridge_model)
print(ridge_coef)

# Make predictions with the final Ridge model
ridge_predictions <- predict(final_ridge_model, newx = X, type = "response")

# Make predictions with the final Ridge model
ridge_predictions <- predict(final_ridge_model, newx = X, type = "response")
print(ridge_predictions)

# Calculate ORs
exp(coef(final_ridge_model))

### P-tau 181 ###

# Main Effects Model

hist(final_merged_dataset_170924$TauP181_Roche_logtr)

mmr_main2 <- glm(AD_status ~ TauP181_Roche_logtr + CVD_risk + ETHNIC_recode, 
                data = final_merged_dataset_170924, 
                family = binomial)

summary(mmr_main2)

# Calculate R squared
library(pscl)
library(DescTools)
library(sjstats)
# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_main2)
print(pseudo_r2)

vif_values <- vif(mmr_main2)
print(vif_values)

# Assuming you have calculated VIFs and stored in vif_values
library(ggplot2)
# Convert VIF values to a data frame
vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "cyan4") +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown4", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Plot P-tau 181 vs. Predicted Probability of AD
library(ggplot2)
ggplot(final_merged_dataset_170924, aes(x = TauP181_Roche_logtr, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of P-Tau 181 on Probability of AD",
       x = "P-tau 181",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot CVD Risk vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = CVD_risk, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of CVD Risk on Probability of AD",
       x = "CVD Risk",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot Ethnicity vs. Predicted Probability of AD

## OPTIONAL
# Enhanced Plot with Centered Title and Descriptive Legend
ggplot(final_merged_dataset_170924, aes(x = ETHNIC_recode, y = AD_status, fill = ETHNIC_recode)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Effect of Ethnicity on Probability of AD",
       x = "Ethnicity",
       y = "Mean Probability of AD",
       fill = "Ethnicity") +  # Assigning a descriptive legend title
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centering and styling the title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Interaction Model: P-tau 181 x CVD_risk
mmr_interaction2 <- glm(AD_status ~ TauP181_Roche_logtr * CVD_risk + ETHNIC_recode, 
                       data = final_merged_dataset_170924, 
                       family = binomial)

summary(mmr_interaction2)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_interaction2)
print(pseudo_r2)

vif_values <- vif(mmr_interaction2)
print(vif_values)

# Assuming you have calculated VIFs and stored in vif_values
library(ggplot2)
# Convert VIF values to a data frame
vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_hline(yintercept = 5, color = "orange", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown4", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Install required packages if not already installed
install.packages(c("interactions", "ggplot2"))
# Load the packages
library(interactions)

# Check if the 'interact_plot' function is available
exists("interact_plot", where = "package:interactions")

# Plot the non-significant interaction with annotations
interact_plot(mmr_interaction2, 
              pred = TauP181_Roche_logtr, 
              modx = CVD_risk, 
              plot.points = TRUE, 
              jitter = 0.1,
              main.title = "Moderation of CVD Risk on P-Tau 181 and AD Status",
              x.label = "P-tau 181",
              y.label = "Predicted Probability of AD",
              legend.main = "CVD Risk",
              interval = TRUE,  
              int.width = 0.1,   
              colors = "RdYlBu") 

# Interaction Model: P-tau 181 x CVD_risk and P-tau 181 x ethnicity
mmr_full2 <- glm(AD_status ~ TauP181_Roche_logtr * CVD_risk + TauP181_Roche_logtr * ETHNIC_recode, 
                data = final_merged_dataset_170924, 
                family = binomial)

summary(mmr_full2)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_full2)
print(pseudo_r2)

vif_values <- vif(mmr_full2)
print(vif_values)

vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "green4") +
  geom_hline(yintercept = 5, color = "orange2", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown3", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

### P-tau 217 ###

# Extra pre-processing

hist(final_merged_dataset_170924$TauP217_Lilly_logtr)

# Center P-tau 217 (continuous variable)
final_merged_dataset_170924$TauP217_Lilly_logtr_c <- scale(final_merged_dataset_170924$TauP217_Lilly_logtr, 
                                                           center = TRUE, scale = TRUE)
hist(final_merged_dataset_170924$TauP217_Lilly_logtr_c)

# Main Effects Model
mmr_main3 <- glm(AD_status ~ TauP217_Lilly_logtr_c + CVD_risk + ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_main3)

# Calculate R squared
library(pscl)
library(DescTools)
library(sjstats)
# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_main3)
print(pseudo_r2)

vif_values <- vif(mmr_main3)
print(vif_values)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "green4") +
  geom_hline(yintercept = 5, color = "orange2", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown3", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Plot P-tau 217 vs. Predicted Probability of AD
library(ggplot2)
ggplot(final_merged_dataset_170924, aes(x = TauP217_Lilly_logtr_c, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of P-Tau 217 on Probability of AD",
       x = "P-tau 217",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot CVD Risk vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = CVD_risk, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of CVD Risk on Probability of AD",
       x = "CVD Risk",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot Ethnicity vs. Predicted Probability of AD

## OPTIONAL
# Enhanced Plot with Centered Title and Descriptive Legend
ggplot(final_merged_dataset_170924, aes(x = ETHNIC_recode, y = AD_status, fill = ETHNIC_recode)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Effect of Ethnicity on Probability of AD",
       x = "Ethnicity",
       y = "Mean Probability of AD",
       fill = "Ethnicity") +  # Assigning a descriptive legend title
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centering and styling the title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Interaction Model: P-tau 217 x CVD_risk
mmr_interaction3 <- glm(AD_status ~ TauP217_Lilly_logtr_c * CVD_risk + ETHNIC_recode, 
                        data = final_merged_dataset_170924, 
                        family = binomial)

summary(mmr_interaction3)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_interaction3)
print(pseudo_r2)

vif_values <- vif(mmr_interaction3)
print(vif_values)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "purple4") +
  geom_hline(yintercept = 5, color = "pink2", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey3", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Install required packages if not already installed
install.packages(c("interactions", "ggplot2"))
# Load the packages
library(interactions)

# Check if the 'interact_plot' function is available
exists("interact_plot", where = "package:interactions")

# Plot the non-significant interaction with annotations
interact_plot(mmr_interaction3, 
              pred = TauP217_Lilly_logtr_c, 
              modx = CVD_risk, 
              plot.points = TRUE, 
              jitter = 0.1,
              main.title = "Moderation of CVD Risk on P-Tau 217 and AD Status",
              x.label = "P-tau 217",
              y.label = "Predicted Probability of AD",
              legend.main = "CVD Risk",
              interval = TRUE,  
              int.width = 0.1,   
              colors = "RdYlBu") 

# Interaction Model: P-tau 217 x CVD_risk and P-tau 217 x ethnicity
mmr_full3 <- glm(AD_status ~ TauP217_Lilly_logtr_c * CVD_risk + TauP217_Lilly_logtr_c * 
                   ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_full3)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_full3)
print(pseudo_r2)

vif_values <- vif(mmr_full3)
print(vif_values)

ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "darkmagenta") +
  geom_hline(yintercept = 5, color = "pink", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

### ApoE4 ###

hist(final_merged_dataset_170924$ApoE4_Roche_logtr)
hist(final_merged_dataset_170924$ApoE4_Roche)

# Center ApoE4 (continuous variable)
final_merged_dataset_170924$ApoE4_Roche_logtr_c <- scale(final_merged_dataset_170924$ApoE4_Roche_logtr, 
                                                           center = TRUE, scale = TRUE)
hist(final_merged_dataset_170924$ApoE4_Roche_logtr_c)

# Main Effects Model
mmr_main4 <- glm(AD_status ~ ApoE4_Roche_logtr_c + CVD_risk + ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_main4)

pseudo_r2 <- pR2(mmr_main4)
print(pseudo_r2)

# Checking for multicollinearity
install.packages("car")
library(car)
vif_values <- vif(mmr_main4)
print(vif_values)

ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "darkmagenta") +
  geom_hline(yintercept = 5, color = "pink", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Calculate correlation matrix
cor_matrix <- cor(final_merged_dataset_170924[, c("ApoE4_Roche_logtr_c", "CVD_risk", "ETHNIC_recode")], use = "complete.obs")

# Print correlation matrix
print(cor_matrix)

# Plot P-tau 217 vs. Predicted Probability of AD
library(ggplot2)
ggplot(final_merged_dataset_170924, aes(x = ApoE4_Roche_logtr_c, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of ApoE4 on Probability of AD",
       x = "ApoE4",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot CVD Risk vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = CVD_risk, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of CVD Risk on Probability of AD",
       x = "CVD Risk",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot Ethnicity vs. Predicted Probability of AD

## OPTIONAL
# Enhanced Plot with Centered Title and Descriptive Legend
ggplot(final_merged_dataset_170924, aes(x = ETHNIC_recode, y = AD_status, fill = ETHNIC_recode)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Effect of Ethnicity on Probability of AD",
       x = "Ethnicity",
       y = "Mean Probability of AD",
       fill = "Ethnicity") +  # Assigning a descriptive legend title
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centering and styling the title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Interaction Model: ApoE4 x CVD_risk
mmr_interaction4 <- glm(AD_status ~ ApoE4_Roche_logtr_c * CVD_risk + ETHNIC_recode, 
                        data = final_merged_dataset_170924, 
                        family = binomial)

summary(mmr_interaction4)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_interaction4)
print(pseudo_r2)

vif_values <- vif(mmr_interaction4)
print(vif_values)

ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_hline(yintercept = 5, color = "cornsilk2", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey2", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Install required packages if not already installed
install.packages(c("interactions", "ggplot2"))
# Load the packages
library(interactions)

# Check if the 'interact_plot' function is available
exists("interact_plot", where = "package:interactions")

# Plot the non-significant interaction with annotations
interact_plot(mmr_interaction4, 
              pred = ApoE4_Roche_logtr_c, 
              modx = CVD_risk, 
              plot.points = TRUE, 
              jitter = 0.1,
              main.title = "Moderation of CVD Risk on P-Tau 217 and AD Status",
              x.label = "P-tau 217",
              y.label = "Predicted Probability of AD",
              legend.main = "CVD Risk",
              interval = TRUE,  
              int.width = 0.1,   
              colors = "RdYlBu") 

# Interaction Model: ApoE4 x CVD_risk and ApoE4 x ethnicity
mmr_full4 <- glm(AD_status ~ ApoE4_Roche_logtr_c * CVD_risk + ApoE4_Roche_logtr_c * 
                   ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_full4)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_full4)
print(pseudo_r2)

# Calculate VIF metric
vif_values <- vif(mmr_full4)
print(vif_values)

ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "coral3") +
  geom_hline(yintercept = 5, color = "cornsilk3", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey2", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Save dataset
write.csv(final_merged_dataset_170924, "final_merged_dataset_170924.csv", row.names = FALSE)

# Subgroup analyses

library(readr)
final_merged_dataset_170924 <- read_csv("Angelina_Bio_Hermes_R_code/Univariate_Regressions_Added_070924/final_merged_dataset_170924.csv",
                                        show_col_types = FALSE)
View(final_merged_dataset_170924)

# Split data into two ethnic groups
library(dplyr)
hispanic_data <- final_merged_dataset_170924 %>% filter(ETHNIC_recode == 1)
non_hispanic_data <- final_merged_dataset_170924 %>% filter(ETHNIC_recode == 0)

# Model for Hispanic or Latino group
model_hispanic <- glm(AD_status ~ AB_4240_Roche_c * CVD_risk, data = hispanic_data, family = binomial)
summary(model_hispanic)
pseudo_r2 <- pR2(model_hispanic)
print(pseudo_r2)

# Model for Not Hispanic or Latino group
model_non_hispanic <- glm(AD_status ~ AB_4240_Roche_c * CVD_risk, data = non_hispanic_data, family = binomial)
summary(model_non_hispanic)
pseudo_r2 <- pR2(model_non_hispanic)
print(pseudo_r2)

# Confidence intervals for Hispanic or Latino group
confint(model_hispanic)
# Confidence intervals for Not Hispanic or Latino group
confint(model_non_hispanic)

# Model for Hispanic or Latino group
model_hispanic <- glm(AD_status ~ TauP181_Roche_logtr * CVD_risk, data = hispanic_data,
                      family = binomial)
summary(model_hispanic)
pseudo_r2 <- pR2(model_hispanic)
print(pseudo_r2)

# Model for Not Hispanic or Latino group
model_non_hispanic <- glm(AD_status ~ TauP181_Roche_logtr * CVD_risk, data = non_hispanic_data, 
                          family = binomial)
summary(model_non_hispanic)
pseudo_r2 <- pR2(model_non_hispanic)
print(pseudo_r2)

# Model for Hispanic or Latino group
model_hispanic <- glm(AD_status ~ TauP217_Lilly_logtr_c * CVD_risk, data = hispanic_data,
                      family = binomial)
summary(model_hispanic)
pseudo_r2 <- pR2(model_hispanic)
print(pseudo_r2)

# Model for Not Hispanic or Latino group
model_non_hispanic <- glm(AD_status ~ TauP217_Lilly_logtr_c * CVD_risk, data = non_hispanic_data, 
                          family = binomial)
summary(model_non_hispanic)
pseudo_r2 <- pR2(model_non_hispanic)
print(pseudo_r2)

# Model for Hispanic or Latino group
model_hispanic <- glm(AD_status ~ ApoE4_Roche_logtr_c * CVD_risk, data = hispanic_data,
                      family = binomial)
summary(model_hispanic)
pseudo_r2 <- pR2(model_hispanic)
print(pseudo_r2)

# Model for Not Hispanic or Latino group
model_non_hispanic <- glm(AD_status ~ ApoE4_Roche_logtr_c * CVD_risk, data = non_hispanic_data, 
                          family = binomial)
summary(model_non_hispanic)
pseudo_r2 <- pR2(model_non_hispanic)
print(pseudo_r2)

