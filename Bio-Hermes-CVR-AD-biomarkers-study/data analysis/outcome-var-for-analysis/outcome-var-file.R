
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project

# Author: Angelina Kancheva 
# Date: 24/07/2024
----------------------------------------------------------------------------------

### Data Preparation Steps

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load data
outcome <- read_csv("Bio_Hermes_Data/Bio_Hermes_Clinical_Data/SUPPDM.csv")
View(outcome)

# Filter data 
outcome_selected <- outcome %>% select(USUBJID, QVAL)
View(outcome_selected)

# Categorise the QVAL variable
outcome_selected <- outcome_selected %>%
  mutate(AD_status = case_when(
    QVAL == "Cohort 1 (Healthy)" ~ 1,
    QVAL == "Cohort 2 (MCI)" ~ 2,
    QVAL == "Cohort 3 (Probable AD)" ~ 3
  ))

typeof(outcome_selected$AD_status)

# Plot the data
ggplot(outcome_selected, aes(x = QVAL)) +
  geom_bar(fill = "pink3") +
  theme_minimal() +
  labs(
    title = "Distribution of AD Status",
    x = "Cohort Category",
    y = "Count"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    plot.title = element_text(size = 17, hjust = 0.5)
  )

# Save file
write.csv(outcome_selected, "outcome_selected.csv", row.names = FALSE)


-------------------------------------
### 26/07/2024: Further Processing

outcome_selected <- read_csv("Angelina_Bio_Hermes_R_code/Outcome_for_Analysis/outcome_selected.csv")
View(outcome_selected)

# Categorize the QVAL variable into 2 categories instead of 3
outcome_selected_binary <- outcome_selected %>%
  mutate(AD_status = case_when(
    QVAL == "Cohort 1 (Healthy)" ~ 0,
    QVAL == "Cohort 2 (MCI)" ~ 1,
    QVAL == "Cohort 3 (Probable AD)" ~ 1
  ))

View(outcome_selected_binary)

# Plot binary AD status categories
outcome_refined <- outcome_selected_binary %>% filter(AD_status %in% c(0, 1))

# Recode AD_status to have meaningful labels
outcome_refined$AD_status <- factor(outcome_refined$AD_status, levels = c(0, 1), labels = c("Healthy", "MCI and Probable AD"))

# Create the plot
ggplot(outcome_refined, aes(x = AD_status)) +
  geom_bar(fill = "pink") +
  theme_minimal() +
  labs(
    title = "Distribution of AD Status - Binary",
    x = "Cohort Category",
    y = "Count"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    plot.title = element_text(size = 17, hjust = 0.5)
  )

# Save new outcomes file
write.csv(outcome_selected_binary, "outcome_selected_binary.csv", row.names = FALSE)
View(outcome_selected_binary)

