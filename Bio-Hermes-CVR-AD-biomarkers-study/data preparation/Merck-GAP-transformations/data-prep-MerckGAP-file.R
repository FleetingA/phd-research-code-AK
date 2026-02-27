
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 23/07/2024
----------------------------------------------------------------------------------
  
### Data Preparation Steps
  
# Load libraries
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load data
MBH <- read_csv("Bio_Hermes_Data/Bio_Hermes_Partners_Data/Merck/MerckBiohermesBiomarker_Quanterix Neuro 4 Plex E_13102023.csv")
View(MBH)

# Explore files - LBSTAT (Lab tests completion status)
table(MBH$LBSTAT == "NOT DONE") # 56 with lab test completion status = not done
table(MBH$LBSTAT == "NA")

# Plot frequencies for LBSTAT
ggplot(MBH, aes(x = LBSTAT)) +
  geom_bar(fill = "plum2") +
  theme_minimal() +
  labs(title = "Laboratory Tests Completion Status",
       x = "Completion Status",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Explore files - LBREASND (Lab tests completion status)
print(MBH$LBREASND == "INTERFERENTS PRESENT") 
table(MBH$LBREASND == "NA")
summary(MBH$LBREASND)

# Plot frequencies for LBREASND
ggplot(MBH, aes(x = LBREASND)) +
  geom_bar(fill = "plum3") +
  theme_minimal() +
  labs(title = "Laboratory Tests Completion Status",
       x = "Completion Status",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


## DATA TRANSFORMATIONS

# Remove entries where LBSTAT=NOT DONE and LBREASND=INTERFERENTS PRESENT

# Define vector of SUBJID values to remove
subjids_to_remove <- c("00114-027", "00110-055", "00109-039", "00109-095", 
                       "00105-057", "00102-028", "00102-023", "00101-002", 
                       "00103-044", "00103-046", "00110-005", "00109-154", 
                       "00109-149")

# Filter out the rows where SUBJID is in the vector
MBH_filtered <- MBH %>% filter(!SUBJID %in% subjids_to_remove)

# Check the resulting data frame
print(MBH_filtered)
View(MBH_filtered)

# Get the names of all columns in the MBH_filtered data frame
column_names <- colnames(MBH_filtered)
# Print column names
print(column_names)

# Select only columns needed for analysis
View(MBH_filtered)
MBH_selected <- MBH_filtered %>% select(STUDYID, SUBJID, LBTESTCD, LBORRES, LBORRESU)
View(MBH_selected)

# Rename column SUBJID to match ID variable across files (=USUBJID)
MBH_selected <- MBH_selected %>%
  rename(USUBJID = SUBJID)
View(MBH_selected)

# Add prefix "BIO-HERMES-" to all entries in the USUBJID column
MBH_selected <- MBH_selected %>%
  mutate(USUBJID = paste0("BIO-HERMES-", USUBJID))

# Print the first few rows to confirm the change
head(MBH_selected$USUBJID)
View(MBH_selected)
                                    
# Save new files before proceeding
write.csv(MBH_selected, "MBH_selected.csv", row.names = FALSE) # only columns needed for analysis
write.csv(MBH_filtered, "MBH_filtered.csv", row.names = FALSE) # LBSTAT=NOT DONE and LBREASND=INTERFERENTS PRESENT removed

table(MBH_selected$LBTESTCD)
View(MBH_selected)

# Plot frequencies for LBTESTCD
ggplot(MBH_selected, aes(x = LBTESTCD)) +
  geom_bar(fill = "sienna2") +
  theme_minimal() +
  labs(title = "Types of Lab Tests Conducted",
       x = "Lab Test",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

typeof(MBH_selected$LBTESTCD)

# Pivot the data - from long to wide format
MBH_wide <- MBH_selected %>%
  pivot_wider(names_from = LBTESTCD, values_from = LBORRES)

typeof(MBH_wide$AB42)
typeof(MBH_wide$AB40)
typeof(MBH_selected$LBTESTCD)

# Check the wide format data
print(MBH_wide)
View(MBH_wide)

# Create a new data frame for all tests where units are pg_mL 
MBH_pg_mL <- MBH_wide %>% filter(LBORRESU == "pg/mL")
View(MBH_pg_mL)

# Remove two subjects that need removed
subj_rem <- c("BIO-HERMES-00109-133", "BIO-HERMES-00109-139")
# Filter out the rows where SUBJID is in the vector
MBH_pg_mL <- MBH_pg_mL %>% filter(!USUBJID %in% subj_rem)

MBH_pg_mL[[4]] <- as.numeric(unlist(MBH_pg_mL[[4]]))
MBH_pg_mL[[5]] <- as.numeric(unlist(MBH_pg_mL[[5]]))
MBH_pg_mL[[6]] <- as.numeric(unlist(MBH_pg_mL[[6]]))
MBH_pg_mL[[7]] <- as.numeric(unlist(MBH_pg_mL[[7]]))
MBH_pg_mL[[8]] <- as.numeric(unlist(MBH_pg_mL[[8]]))
View(MBH_pg_mL)
 
typeof(MBH_pg_mL[[4]]) # double
typeof(MBH_pg_mL[[5]]) # double
typeof(MBH_pg_mL[[6]]) # double
typeof(MBH_pg_mL[[7]]) # double
typeof(MBH_pg_mL[[8]]) # list

# Renaming the 4th, 5th, 6th, and 7th columns
names(MBH_pg_mL)[4] <- "AB40"
names(MBH_pg_mL)[5] <- "AB42"
names(MBH_pg_mL)[6] <- "GFAP"
names(MBH_pg_mL)[7] <- "NFL"

# Check if the column names are correctly renamed
names(MBH_pg_mL)
View(MBH_pg_mL)

# Double-check data types of the variables
typeof(MBH_pg_mL$AB40) # double
typeof(MBH_pg_mL$AB42) # double
typeof(MBH_pg_mL$GFAP) # double
typeof(MBH_pg_mL$NFL) # double

# Save new data frame
write.csv(MBH_pg_mL, "MBH_pg_mL.csv", row.names = FALSE) # new data frame for all tests where units are pg_mL

# Ensure the columns AB40 and AB42 are numeric
MBH_pg_mL$AB40 <- as.numeric(MBH_pg_mL$AB40)
MBH_pg_mL$AB42 <- as.numeric(MBH_pg_mL$AB42)

# Create new variable AB4240 as the ratio of AB42 to AB40
MBH_pg_mL$AB4240 <- MBH_pg_mL$AB42 / MBH_pg_mL$AB40
View(MBH_pg_mL)

MBH_pg_mL$AB40_numeric <- as.numeric(as.character(unlist(MBH_pg_mL[, 4])))
MBH_pg_mL$AB42_numeric <- as.numeric(as.character(unlist(MBH_pg_mL[, 5])))

# Save new data frame
write.csv(MBH_pg_mL, "MBH_pg_mL.csv", row.names = FALSE) # new data frame for all tests where units are pg_mL

# Final transformation - only select AB relevant lab results
MBH_pg_mL_ABonly <- MBH_pg_mL %>% select(STUDYID, USUBJID, LBORRESU, 
                                         AB40, AB42, AB4240)
View(MBH_pg_mL_ABonly) # 978 entries

# Save this data frame
write.csv(MBH_pg_mL_ABonly, "final_MBH_pg_mL_ABonly.csv", row.names = FALSE) # AB relevant only, wide format
View(MBH_pg_mL_ABonly)

final_MBH_pg_mL_ABonly <- read_csv("Angelina_Bio_Hermes_R_code/Merck_GAP_Transformations/final_MBH_pg_mL_ABonly.csv", show_col_types = FALSE)
View(final_MBH_pg_mL_ABonly)

final_MBH_pg_mL_ABonly <- final_MBH_pg_mL_ABonly %>% select(USUBJID, 
                                         AB40, AB42, AB4240)
View(final_MBH_pg_mL_ABonly)

# Plot distribution of AB40
ggplot(final_MBH_pg_mL_ABonly, aes(x = AB40)) +
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of AB40 (Merck GAP)",
    x = "AB40",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Plot distribution of AB42
ggplot(final_MBH_pg_mL_ABonly, aes(x = AB42)) +
  geom_histogram(fill = "green4", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of AB42 (Merck GAP)",
    x = "AB42",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Plot distribution of AB4240
ggplot(final_MBH_pg_mL_ABonly, aes(x = AB4240)) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of AB42/40 (Merck GAP)",
    x = "AB4240",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Save this new data frame
write.csv(final_MBH_pg_mL_ABonly, "final_MBH_pg_mL_ABonly.csv", row.names = FALSE) 


### 29/07/2024: Log transform the AB4240 ratio variable before running a regression analysis

final_MBH_pg_mL_ABonly <- read_csv("Angelina_Bio_Hermes_R_code/Merck_GAP_Transformations/final_MBH_ABonly_merge.csv")
View(final_MBH_pg_mL_ABonly)

# Log transform AB4240
final_MBH_pg_mL_ABonly$AB4240_logtr <- log(final_MBH_pg_mL_ABonly$AB4240)
hist(final_MBH_pg_mL_ABonly$AB4240_logtr)

View(final_MBH_pg_mL_ABonly)

# Plot distribution of AB4240 log transformed
library(ggplot2)
ggplot(final_MBH_pg_mL_ABonly, aes(x = AB4240_logtr)) +
  geom_histogram(fill = "red3", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of AB42/40 (Merck GAP)",
    x = "AB4240",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Save new data frame after log transformation of PTAU217
write.csv(final_MBH_pg_mL_ABonly, "final_MBH_pg_mL_ABonly.csv", row.names = FALSE)

