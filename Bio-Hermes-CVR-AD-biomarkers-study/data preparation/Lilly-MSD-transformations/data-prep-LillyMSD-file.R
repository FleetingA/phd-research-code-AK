
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 24/07/2024
----------------------------------------------------------------------------------
  
### Data Preparation Steps
  
# Load libraries
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load data
Lilly_BH <- read_csv("Bio_Hermes_Data/Bio_Hermes_Partners_Data/Lilly_MSD/pTau217_Data/Lilly MSD_Blood_Biomarker_LB_Data.csv")
View(Lilly_BH)

# Filter data 
Lilly_BH_selected <- Lilly_BH %>% select(USUBJID, LBTESTCD, LBORRES)
View(Lilly_BH_selected)

# Renaming the LBTESTCD column
names(Lilly_BH_selected)[names(Lilly_BH_selected) == "LBTESTCD"] <- "PTAU217"

# Check for missing data
summary(Lilly_BH_selected$"PTAU217") # no missing

Lilly_BH_selected_onlyPTAU217 <- Lilly_BH_selected %>% select(USUBJID, LBORRES)
View(Lilly_BH_selected_onlyPTAU217)

# Renaming the LBORRES column
names(Lilly_BH_selected_onlyPTAU217)[names(Lilly_BH_selected_onlyPTAU217) == "LBORRES"] <- "PTAU217"
View(Lilly_BH_selected_onlyPTAU217)

typeof(Lilly_BH_selected_onlyPTAU217$PTAU217) # character

# Convert Tau217P from character to numeric, handling NAs
Lilly_BH_selected_onlyPTAU217$PTAU217 <- as.numeric(Lilly_BH_selected_onlyPTAU217$PTAU217)

summary(Lilly_BH_selected_onlyPTAU217$PTAU217)
View(Lilly_BH_selected_onlyPTAU217)

# Remove rows with NA values in the column Tau217P using na.omit()
Lilly_BH_PTAU217_no_NAs <- na.omit(Lilly_BH_selected_onlyPTAU217)
Lilly_BH_PTAU217_no_NAs <- Lilly_BH_selected_onlyPTAU217[!is.na(Lilly_BH_selected_onlyPTAU217$PTAU217), ]
View(Lilly_BH_PTAU217_no_NAs)

# Plot distribution of PTAU217
library(ggplot2)
ggplot(Lilly_BH_PTAU217_no_NAs, aes(x = PTAU217)) +
  geom_histogram(fill = "purple", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of PTAU217 (Lilly MSD)",
    x = "Phosphorylated tau 217",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Save files
write.csv(Lilly_BH_PTAU217_no_NAs, "final_Lilly_BH_PTAU217_no_NAs.csv", row.names = FALSE) # wide format, no missing values
write.csv(Lilly_BH_selected_onlyPTAU217, "Lilly_BH_selected_onlyPTAU217.csv", row.names = FALSE) # wide format, still missing values present
write.csv(Lilly_BH_selected, "Lilly_BH_selected.csv", row.names = FALSE) # initial cleaned-up data file, not final 


### 29/07/2024: Log transform the data before running a regression analysis

final_Lilly_BH_PTAU217_no_NAs <- read_csv("Angelina_Bio_Hermes_R_code/Lilly_MSD_Transformations/final_Lilly_BH_PTAU217_no_NAs.csv")
View(final_Lilly_BH_PTAU217_no_NAs)

# Log transform PTAU217
final_Lilly_BH_PTAU217_no_NAs$PTAU217_logtr <- log(final_Lilly_BH_PTAU217_no_NAs$PTAU217)

View(final_Lilly_BH_PTAU217_no_NAs)

# Plot distribution of PTAU217 log transformed
library(ggplot2)
ggplot(final_Lilly_BH_PTAU217_no_NAs, aes(x = PTAU217_logtr)) +
  geom_histogram(fill = "pink", color = "black", bins = 30) +
  theme_minimal() +
  labs(
    title = "Distribution of PTAU217 (Lilly MSD)",
    x = "Phosphorylated tau 217",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Save new data frame after log transformation of PTAU217
write.csv(final_Lilly_BH_PTAU217_no_NAs, "final_Lilly_BH_PTAU217_no_NAs.csv", row.names = FALSE)
