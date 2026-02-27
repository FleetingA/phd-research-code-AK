
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 22/07/2024
----------------------------------------------------------------------------------
  
### Data Preparation Steps
## LB stands for 'laboratory tests'
  
# Load libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr) # to convert from long to wide format 

# Load data
LB <- read_csv("Bio_Hermes_Data/Bio_Hermes_Clinical_Data/LB.csv", show_col_types = FALSE)
View(LB)

# Explore the data
summary(LB)
table(LB$LBTEST)
table(LB$USUBJID)

# Count the number of unique values in 'USUBJID'
num_unique_values <- n_distinct(LB$USUBJID)
# Print the number of unique values
print(num_unique_values)

# Plot frequencies for LBTEST
ggplot(LB, aes(x = LBTEST)) +
  geom_bar(fill = "olivedrab3") +
  theme_minimal() +
  labs(title = "Overview of Laboratory Tests",
       x = "Laboratory Test",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Plot frequencies for LBNRIND
ggplot(LB, aes(x = LBNRIND)) +
  geom_bar(fill = "pink2") +
  theme_minimal() +
  labs(title = "Distribution of Classification Status",
       x = "Classification Status",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Calculate frequencies for LB using dplyr
lb_frequencies_df <- LB %>%
  group_by(LBTEST) %>%
  summarize(Frequency = n())

# Select only columns needed for analysis
LB_selected <- LB %>% select(STUDYID, DOMAIN, USUBJID, LBTEST, LBSCAT, LBORRES,
                             LBORRESU, LBNRIND)
head(LB_selected)
View(LB_selected)

# Select only columns relevant to cholesterol for CVD risk score calculator
LB_cholesterol <- LB_selected %>% select(STUDYID, DOMAIN, USUBJID, LBTEST, LBORRES,
                                   LBORRESU, LBNRIND)
View(LB_cholesterol)
table(LB_cholesterol$LBTEST)

# Filter data frame LB_HDL_c to contain only rows where LBTEST=HDLc
HDLc_data_only <- LB_cholesterol %>% filter(LBTEST == "HDL Cholesterol")

# Display the new data frame
print(HDLc_data_only)
View(HDLc_data_only)

table(HDLc_data_only$LBNRIND)

# Plot frequencies for HDL cholesterol - only classification status 
ggplot(HDLc_data_only, aes(x = LBNRIND)) +
  geom_bar(fill = "pink2") +
  theme_minimal() +
  labs(title = "HDL Cholesterol Classification Status",
       x = "Classification Status",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Recode ABNORMAL and NORMAL HDL cholesterol for CVD risk score calculator
HDLc_data_only <- HDLc_data_only %>%
  mutate(HDL_status = case_when(
    LBNRIND == "ABNORMAL" ~ "1",
    LBNRIND == "NORMAL" ~ "0",
    TRUE ~ "NA"  # This handles any unexpected values
  ))

table(HDLc_data_only$HDL_status)
View(HDLc_data_only)

# Save new data frame as CSV file
write.csv(LB_selected, "LB_selected.csv", row.names = FALSE)
write.csv(LB_cholesterol, "LB_cholesterol.csv", row.names = FALSE) # all cholesterol lab results 
write.csv(HDLc_data_only, "HDLc_data_only.csv", row.names = FALSE) # only HDL cholesterol + status recoded

----------------------------------------------------
### 28/07/2024: More work on the cholesterol data

LB_selected <- read_csv("Angelina_Bio_Hermes_R_code/LB_file_Transformations/LB_selected.csv")
View(LB_selected)

# Filter the data to include only rows with cholesterol data in the LBTEST column
typeof(LB_selected$LBTEST)
LB_selected_allc <- LB_selected %>%
  dplyr::filter(LBTEST %in% c("HDL Cholesterol", "LDL Cholesterol", "Triglycerides"))
View(LB_selected_allc)

LB_selected_allc <- LB_selected_allc %>% select(USUBJID, LBTEST, LBORRES, LBNRIND)
View(LB_selected_allc)

# Convert from long to wide format 
LB_selected_allc_wide <- LB_selected_allc %>%
  pivot_wider(names_from = LBTEST, values_from = c(LBORRES, LBNRIND)) %>%
  drop_na() # Remove rows with any NAs

# View the wide format data
View(LB_selected_allc_wide)

# Recode 'NORMAL' to 0 and 'ABNORMAL' to 1 in the last three columns
LB_selected_allc_wide <- LB_selected_allc_wide %>%
  mutate(across(starts_with("LBNRIND"), ~ ifelse(. == "NORMAL", 0, ifelse(. == "ABNORMAL", 1, .))))

# Rename columns using set names
LB_selected_allc_wide <- setNames(LB_selected_allc_wide, c(
  "USUBJID",
  "HDL_Cholesterol",
  "LDL_Cholesterol",
  "Triglycerides",
  "HDL_Status",
  "LDL_Status",
  "Triglycerides_Status"
))

# Create total cholesterol as a new column in the data frame
LB_selected_allc_wide$Total_cholesterol <- LB_selected_allc_wide$HDL_Cholesterol + LB_selected_allc_wide$LDL_Cholesterol + (0.2 * LB_selected_allc_wide$Triglycerides)

# View the modified data frame to verify
print(LB_selected_allc_wide)
View(LB_selected_allc_wide)

# Save new datasets
write.csv(LB_selected_allc_wide, "final_LB_selected_allc_wide.csv", row.names = FALSE) # final cholesterol file to use

