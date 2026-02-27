
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 25/07/2024
----------------------------------------------------------------------------------
  
### Data Preparation Steps
  
# Load libraries
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load data
QBH <- read_csv("~/files/Bio_Hermes_Data/Bio_Hermes_Partners_Data/Quanterix/Qunaterix_Blood_Biomarker_LB_Data.csv", trim_ws = FALSE)
View(QBH)

## DATA TRANSFORMATIONS

# Remove entries where LBSTAT=NOT DONE and LBREASND=Assay Failed or Not Tested
# Define vector of USUBJID values to remove
subjids_to_remove <- c("BIO-HERMES-00102-086", "BIO-HERMES-00104-020", "BIO-HERMES-00111-009")

# Filter out the rows where USUBJID is in the vector
QBH_filtered <- QBH %>% filter(!USUBJID %in% subjids_to_remove)

# Check the resulting data frame
print(QBH_filtered)
View(QBH_filtered)

# Clean up data 
QBH_selected <- QBH_filtered %>% select(STUDYID, USUBJID, LBTESTCD, LBORRES, LBORRESU, LBORNRLO, LBORNRHI)
View(QBH_selected)

# Save files so far
write.csv(QBH_selected, "QBH_selected.csv", row.names = FALSE) # only columns needed for analysis
write.csv(QBH_filtered, "QBH_filtered.csv", row.names = FALSE) # LBSTAT=NOT DONE and LBREASND=Assay Failed or Not Tested removed

# View data again
View(QBH_selected)
typeof(QBH_selected$LBTESTCD)
typeof(QBH_selected$LBORRES)

# Convert the data from long to wide format
QBH_wide <- pivot_wider(QBH_selected,
                         id_cols = c(STUDYID, USUBJID),
                         names_from = LBTESTCD,
                         values_from = c(LBORRES, LBORRESU, LBORNRLO, LBORNRHI))
View(QBH_wide)

# Get the names of all columns in the QBH_wide data frame
column_names <- colnames(QBH_wide)
# Print column names
print(column_names)

QBH_wide <- QBH_wide %>% select(USUBJID, LBORRES_TAU181P, LBORRES_TPROT)
View(QBH_wide)

# Save file in wide format
write.csv(QBH_wide, "QBH_wide.csv", row.names = FALSE) # Tau relevant only, wide format

typeof(QBH_wide$LBORRES_TAU181P)
typeof(QBH_wide$LBORRES_TPROT)

# Convert specific columns from character to numeric
QBH_wide <- QBH_wide %>%
  mutate(
    LBORRES_TAU181P = as.numeric(LBORRES_TAU181P),
    LBORRES_TPROT = as.numeric(LBORRES_TPROT)
  )

typeof(QBH_wide$LBORRES_TAU181P)
typeof(QBH_wide$LBORRES_TPROT)

# Check for missing data
summary(QBH_wide$LBORRES_TAU181P) # 106 NAs
summary(QBH_wide$LBORRES_TPROT) # 2 NAs

# Identify rows with NAs in LBORRES_TAU181P
na_indices_TAU181P <- which(is.na(QBH_wide$LBORRES_TAU181P))
print("Rows with NAs in LBORRES_TAU181P:")
print(na_indices_TAU181P)

# Identify rows with NAs in LBORRES_TPROT
na_indices_TPROT <- which(is.na(QBH_wide$LBORRES_TPROT))
print("Rows with NAs in LBORRES_TPROT:")
print(na_indices_TPROT)

# Convert LBORRES_TAU181P and LBORRES_TPROT to numeric, handling NAs
QBH_wide$LBORRES_TAU181P <- as.numeric(QBH_wide$LBORRES_TAU181P)
QBH_wide$LBORRES_TPROT <- as.numeric(QBH_wide$LBORRES_TPROT)

# Remove rows with NA values in the column LBORRES_TAU181P using na.omit()
QBH_wide_no_NAs <- na.omit(QBH_wide)
QBH_wide_no_NAs <- QBH_wide[!is.na(QBH_wide$LBORRES_TAU181P), ]
View(QBH_wide_no_NAs)

# Renaming the LBORRES_TAU181P and LBORRES_TPROT columns
names(QBH_wide_no_NAs)[names(QBH_wide_no_NAs) == "LBORRES_TAU181P"] <- "PTAU181"
names(QBH_wide_no_NAs)[names(QBH_wide_no_NAs) == "LBORRES_TPROT"] <- "Tau_Protein"
View(QBH_wide_no_NAs)

# Get the names of all columns in the QBH_wide_no_NAs data frame
columns <- colnames(QBH_wide_no_NAs)
# Print column names
print(columns)

# Plot distribution of PTAU181
ggplot(QBH_wide_no_NAs, aes(x = PTAU181)) +
  geom_histogram(fill = "cyan4", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of PTAU181 (Quanterix)",
    x = "Phosphorylated tau 181",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Save file in wide format
write.csv(QBH_wide_no_NAs, "QBH_wide_no_NAs.csv", row.names = FALSE) # Tau relevant only, no NAs (ie. missing), wide format


### 29/07/2024: Log transform data before running a regression analysis

final_QBH_wide_no_NAs <- read_csv("Angelina_Bio_Hermes_R_code/Quanterix_Transformations/final_QBH_wide_no_NAs.csv")
View(final_QBH_wide_no_NAs)

# Log transform PTAU181
final_QBH_wide_no_NAs$PTAU181_logtr <- log(final_QBH_wide_no_NAs$PTAU181)

View(final_QBH_wide_no_NAs)

# Plot distribution of PTAU181 log transformed
library(ggplot2)
ggplot(final_QBH_wide_no_NAs, aes(x = PTAU181_logtr)) +
  geom_histogram(fill = "cyan2", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of PTAU181 (Quanterix)",
    x = "Phosphorylated tau 181",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Save new data frame after log transformation of PTAU181
write.csv(final_QBH_wide_no_NAs, "final_QBH_wide_no_NAs.csv", row.names = FALSE)
