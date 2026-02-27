
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
RocheBH <- read_csv("Bio_Hermes_Data/Bio_Hermes_Partners_Data/Roche_Diagnostics/BIOHERMES_RD_PROD_20230605154531.csv",
                    show_col_types = FALSE)
View(RocheBH)

# Add prefix "BIO-HERMES-" to all entries in the SUBJID column
RocheBH_selected <- RocheBH %>%
  mutate(SUBJID = paste0("BIO-HERMES-", SUBJID))
View(RocheBH_selected)

# Renaming the SUBJID column
names(RocheBH_selected)[names(RocheBH_selected) == "SUBJID"] <- "USUBJID2"

# Select only columns needed for analysis
RocheBH_analysis_vars <- RocheBH_selected %>% select(USUBJID2, LBTESTCD, LBORRES, LBORRESU)
names(RocheBH_analysis_vars)[names(RocheBH_analysis_vars) == "USUBJID2"] <- "USUBJID"
View(RocheBH_analysis_vars)

# Plot frequencies for LBTESTCD
ggplot(RocheBH_analysis_vars, aes(x = LBTESTCD)) +
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

typeof(RocheBH_analysis_vars$LBTESTCD)

# Save new data frame
write.csv(RocheBH_selected, "RocheBH_selected.csv", row.names = FALSE) # fixed prefix, long format
write.csv(RocheBH_analysis_vars, "RocheBH_analysis_vars.csv", row.names = FALSE) # data to use for analysis

# Convert data from long to wide format
RocheBH_analysis_wide <- RocheBH_analysis_vars %>%
  pivot_wider(names_from = LBTESTCD, values_from = c(LBORRES, LBORRESU))

# View the resulting wide data frame
View(RocheBH_analysis_wide)

# Save this data frame
write.csv(RocheBH_analysis_wide, "RocheBH_analysis_wide.csv", row.names = FALSE) # wide format, detailed data frame

# Using the names() function
column_names <- names(RocheBH_analysis_wide)
# Print the column names
print(column_names)

# Select only columns for regression
RocheBH_analysis_wide_clean <- RocheBH_analysis_wide %>% select(USUBJID, LBORRES_AMYLB40, LBORRES_AMYLB42,
                                                                LBORRES_ApoE4, LBORRES_pTau181)
View(RocheBH_analysis_wide_clean)

# All vars are of type character; convert to numeric
RocheBH_analysis_wide_clean <- RocheBH_analysis_wide_clean %>%
  mutate_at(vars(LBORRES_AMYLB40, LBORRES_AMYLB42, LBORRES_ApoE4, LBORRES_pTau181), as.numeric)

# Check the structure of the data frame to confirm the changes
str(RocheBH_analysis_wide_clean)

# Save this new data frame
write.csv(RocheBH_analysis_wide_clean, "RocheBH_analysis_wide.csv", row.names = FALSE) # wide format, cleaned data frame with fewer vars

# Replace NA values in the LBORRES_ApoE4 column with 0.00
RocheBH_analysis_wide_clean <- RocheBH_analysis_wide_clean %>%
  mutate(LBORRES_ApoE4 = ifelse(is.na(LBORRES_ApoE4), 0.00, LBORRES_ApoE4))

# Rename columns to remove "LBORRES_"
names(RocheBH_analysis_wide_clean) <- gsub("^LBORRES_", "", names(RocheBH_analysis_wide_clean))
View(RocheBH_analysis_wide_clean)

## Plot distribution of AMYLB40

ggplot(RocheBH_analysis_wide_clean, aes(x = AMYLB40)) +
  geom_histogram(fill = "orange", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of AB40 (Roche Diagnostics)",
    x = "AB40",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

## Plot distribution of AMYLB42

ggplot(RocheBH_analysis_wide_clean, aes(x = AMYLB42)) +
  geom_histogram(fill = "yellow3", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of AB42 (Roche Diagnostics)",
    x = "AB42",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

## Plot distribution of ApoE4

ggplot(RocheBH_analysis_wide_clean, aes(x = ApoE4)) +
  geom_histogram(fill = "brown4", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of ApoE4 (Roche Diagnostics)",
    x = "ApoE4",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# ApoE4 very skewed, applying log transform
RocheBH_analysis_wide_clean$ApoE4_logtr <- log(RocheBH_analysis_wide_clean$ApoE4)

# Replace -Inf values in ApoE4_logtr with 0
RocheBH_analysis_wide_clean <- RocheBH_analysis_wide_clean %>% mutate(ApoE4_logtr = ifelse(ApoE4_logtr == -Inf, 0.00, ApoE4_logtr))
View(RocheBH_analysis_wide_clean)

## Plot distribution of ApoE4 log transformed

ggplot(RocheBH_analysis_wide_clean, aes(x = ApoE4_logtr)) +
  geom_histogram(fill = "darkgreen", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of ApoE4 log-transformed (Roche Diagnostics)",
    x = "ApoE4",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

## Plot distribution of pTau181

ggplot(RocheBH_analysis_wide_clean, aes(x = pTau181)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of PTAU181 (Roche Diagnostics)",
    x = "Phosphorylated tau 181",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Ptau181 also skewed, applying log transform
RocheBH_analysis_wide_clean$pTau181_logtr <- log(RocheBH_analysis_wide_clean$pTau181)

## Plot distribution of ApoE4 log-transformed

ggplot(RocheBH_analysis_wide_clean, aes(x = ApoE4_logtr)) +
  geom_histogram(fill = "darkgreen", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of ApoE4 log-transformed (Roche Diagnostics)",
    x = "ApoE4",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

## Plot distribution of pTau181 log-transformed

ggplot(RocheBH_analysis_wide_clean, aes(x = pTau181_logtr)) +
  geom_histogram(fill = "lightblue3", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of PTAU181 log-transformed (Roche Diagnostics)",
    x = "Phosphorylated tau 181",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

View(RocheBH_analysis_wide_clean)

# Save new data frame after log transformation 
write.csv(RocheBH_analysis_wide_clean, "RocheBH_analysis_wide_clean.csv", row.names = FALSE)