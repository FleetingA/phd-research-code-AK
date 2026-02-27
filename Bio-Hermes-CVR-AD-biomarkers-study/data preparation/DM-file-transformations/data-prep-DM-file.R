
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 22/07/2024
----------------------------------------------------------------------------------

### Data Preparation Steps
## DM stands for 'demographic'

# Load libraries
library(readr)
library(psych)
library(ggplot2) # for plotting
library(dplyr)

# Load data
DM <- read_csv("Bio_Hermes_Data/Bio_Hermes_Clinical_Data/DM.csv", show_col_types = FALSE)
View(DM)

# Data exploration 
table(DM$AGEU) # one entry only: unit=years (n=1005)
describe(DM$AGE)
summary(DM$AGE)

# AGE:
# Histogram with density plot for age
hist(DM$AGE, probability = TRUE, main = "Histogram and Density Plot of Age", xlab = "Age")
lines(density(DM$AGE), col = "red", lwd = 2)

# Q-Q plot for age 
qqnorm(DM$AGE, main = "Q-Q Plot for Age")
qqline(DM$AGE, col = "red")

# Shapiro test for normality of distribution of age 
shapiro.test(DM$AGE) # age is normally distributed

table(DM$ACTARM) # one entry only: observation group (n=1001)
table(DM$COUNTRY) # one entry only: USA (n=1005)
table(DM$SEX) # F=564; M=441

# RACE:
# Explore race in more detail
table(DM$RACE)
race_frequencies <- table(DM$RACE)
print(race_frequencies)
summary(DM$RACE)
describe(DM$RACE)

# Plot frequencies for race
install.packages("ggplot2")

ggplot(DM, aes(x = RACE)) +
  geom_bar(fill = "cyan2") +
  theme_minimal() +
  labs(title = "Distribution of Race",
       x = "Race",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Calculate frequencies for race using dplyr
library(dplyr)
race_frequencies_df <- DM %>%
  group_by(RACE) %>%
  summarize(Frequency = n())

# Print the frequency data frame
print(race_frequencies_df)

# ETHNICITY:
table(DM$ETHNIC)

# Plot frequencies for ethnicity
ggplot(DM, aes(x = ETHNIC)) +
  geom_bar(fill = "orange") +
  theme_minimal() +
  labs(title = "Distribution of Ethnicity",
       x = "Ethnicity",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Select only columns needed for analysis
DM_selected <- DM %>% select(STUDYID, DOMAIN, USUBJID, AGE, SEX, RACE, ETHNIC)
head(DM_selected)
View(DM_selected)

# Recoding string vars into numeric
# RACE:
DM_selected <- DM_selected %>%
  mutate(RACE_recode = case_when(
    RACE == "WHITE" ~ "1",
    RACE == "BLACK OR AFRICAN AMERICAN" ~ "2",
    RACE == "ASIAN" ~ "3",
    RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ "4",
    RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "5",
    RACE == "UNKNOWN" ~ "6",
    TRUE ~ "NA"  # This handles any unexpected values
  ))

head(DM_selected)
table(DM_selected$RACE_recode)

# ETHNICITY:
DM_selected <- DM_selected %>%
  mutate(ETHNIC_recode = case_when(
    ETHNIC == "NOT HISPANIC OR LATINO" ~ "1",
    ETHNIC == "HISPANIC OR LATINO" ~ "2",
    ETHNIC == "NOT REPORTED" ~ "3",
    TRUE ~ "NA"  # This handles any unexpected values
  ))

head(DM_selected)
table(DM_selected$ETHNIC_recode)

# SEX:
DM_selected <- DM_selected %>%
  mutate(SEX_recode = case_when(
    SEX == "F" ~ "0",
    SEX == "M" ~ "1",
    TRUE ~ "NA"  # This handles any unexpected values
  ))

head(DM_selected)
table(DM_selected$SEX_recode)

# Save new data frame as CSV file
write.csv(DM_selected, "DM_selected.csv", row.names = FALSE)

--------------------------------------------
### 05/08/2024: Further Data Preparation 

# Load data
DM_selected <- read_csv("Angelina_Bio_Hermes_R_code/DM_file_Transformations/DM_selected.csv")
View(DM_selected)

# Further recode variables relevant for analysis
DM_selected_2 <- DM_selected_2 %>%
  mutate(RACE_recode = case_when(
    RACE == "WHITE" ~ "0",
    RACE == "BLACK OR AFRICAN AMERICAN" ~ "1",
    RACE == "ASIAN" ~ "2",
    RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ "3",
    RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "4",
    RACE == "UNKNOWN" ~ "5",
    TRUE ~ "NA"  # This handles any unexpected values
  ))

DM_selected_2 <- DM_selected_2 %>%
  mutate(ETHNIC_recode = case_when(
    ETHNIC == "NOT HISPANIC OR LATINO" ~ "0",
    ETHNIC == "HISPANIC OR LATINO" ~ "1",
    ETHNIC == "NOT REPORTED" ~ "2",
    TRUE ~ "NA"  # This handles any unexpected values
  ))

View(DM_selected_2)

# Further trim data
library(dplyr)
DM_selected_2 <- DM_selected_2 %>% select(USUBJID, AGE, SEX, SEX_recode, RACE, RACE_recode,
                                        ETHNIC, ETHNIC_recode)
View(DM_selected_2)

# IDs to check (excluded in IE file)
ids_to_check <- c("BIO-HERMES-00102-092", "BIO-HERMES-00107-022", "BIO-HERMES-00107-053", "BIO-HERMES-00111-050")
print(ids_to_check)

# Check if IDs are present and remove them
DM_selected_2 <- DM_selected_2[!DM_selected_2$USUBJID %in% ids_to_check, ]

# View the updated data frame
View(DM_selected_2)

# Save new file
write.csv(DM_selected_2, "DM_selected_2.csv", row.names = FALSE)