
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date: 24/04/2025
--------------------------------------------------------------------------
  
### Comparing WMH Progressors and Regressors

# Load packages and data
library(readr)
library(dplyr)
library(psych)
phenotypes_260325 <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/phenotypes_260325.csv")
View(phenotypes_260325)

# Read in final dataset
df <- read.delim("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended_final.tsv",
                 header = TRUE,
                 sep = "\t")
View(df)

## Summarise data on progressors and regressors initially
# Prog
describe(df$x25781_2_0)
IQR(df$x25781_2_0)
quantile(df$x25781_2_0, probs = c(0.25, 0.75))

# Reg
describe(df$x25781_3_0)
IQR(df$x25781_3_0)
quantile(df$x25781_3_0, probs = c(0.25, 0.75))

# Progressors
tofi_progressors_only_no_outliers <- read_csv("C:/Users/angel/Desktop/PHESANT-master/PHESANT-master/MyWas/data16042025/tofi_progressors_only_no_outliers.csv")
View(tofi_progressors_only_no_outliers)

# Regressors
tofi_regressors_only_no_outliers <- read_csv("C:/Users/angel/Desktop/PHESANT-master/PHESANT-master/MyWas/data16042025/tofi_regressors_only_no_outliers.csv")
View(tofi_regressors_only_no_outliers)

## Run comparisons

# To counteract potential parsing issues
phenotypes_260325 <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/phenotypes_260325.csv",
                              col_types = cols(userId = col_character()))

head(phenotypes_260325$userId, 5)
head(tofi_progressors_only_no_outliers$userId, 5)
head(tofi_regressors_only_no_outliers$userId, 5)

head(phenotypes_260325$userId)
sum(is.na(phenotypes_260325$userId))

# Continue creating groups
phenotypes_260325$userId <- tolower(trimws(phenotypes_260325$userId))
tofi_progressors_only_no_outliers$userId <- tolower(trimws(tofi_progressors_only_no_outliers$userId))
tofi_regressors_only_no_outliers$userId <- tolower(trimws(tofi_regressors_only_no_outliers$userId))

phenotypes_260325 <- phenotypes_260325 %>%
  mutate(Group = case_when(
    userId %in% tofi_progressors_only_no_outliers$userId ~ "WMH Progressor",
    userId %in% tofi_regressors_only_no_outliers$userId ~ "WMH Regressor",
    TRUE ~ NA_character_
  ))

comparison_data <- phenotypes_260325 %>%
  filter(!is.na(Group)) %>%
  mutate(Group = factor(Group, levels = c("WMH Progressor", "WMH Regressor")))

# Number of participants in each group
table(comparison_data$Group) # WMH prog=2750; WMH reg=1579 


t.test(x21022_0_0 ~ Group, data = comparison_data)

# Age
comparison_data %>%
  group_by(Group) %>%
  summarise(
    median_age = median(x21022_0_0, na.rm = TRUE),
    IQR_age = IQR(x21022_0_0, na.rm = TRUE),
    n = n()
  )

table(comparison_data$Group, comparison_data$x31_0_0)
table(comparison_data$Group, comparison_data$x4080_2_0) # systolic
table(comparison_data$Group, comparison_data$x4079_2_0) # diastolic

# SBP
comparison_data %>%
  group_by(Group) %>%
  summarise(
    mean_sbp = mean(x4080_2_0, na.rm = TRUE),
    sd_sbp = sd(x4080_2_0, na.rm = TRUE),
    n = n()
  )

# DBP
comparison_data %>%
  group_by(Group) %>%
  summarise(
    mean_dbp = mean(x4079_2_0, na.rm = TRUE),
    sd_dbp = sd(x4079_2_0, na.rm = TRUE),
    n = n()
  ) 

# BMI
comparison_data %>%
  group_by(Group) %>%
  summarise(
    mean_BMI = mean(x21001_2_0, na.rm = TRUE),
    sd_BMI = sd(x21001_2_0, na.rm = TRUE),
    n = n()
  ) 


# WMH volume TP 1
comparison_data %>%
  group_by(Group) %>%
  summarise(
    median_wmh = median(x25781_2_0, na.rm = TRUE),
    IQR_wmh = IQR(x25781_2_0, na.rm = TRUE),
    n = n()
  )

# WMH volume TP 2
comparison_data %>%
  group_by(Group) %>%
  summarise(
    median_wmh = median(x25781_3_0, na.rm = TRUE),
    IQR_wmh = IQR(x25781_3_0, na.rm = TRUE),
    n = n()
  )

# Formal comparison
wilcox.test(x21022_0_0 ~ Group, data = comparison_data) # age
wilcox.test(x4080_2_0 ~ Group, data = comparison_data) # sbp
wilcox.test(x4079_2_0 ~ Group, data = comparison_data) # dbp
wilcox.test(x21001_2_0 ~ Group, data = comparison_data) # bmi
wilcox.test(x25781_2_0 ~ Group, data = comparison_data) # wmhs tp1
wilcox.test(x25781_3_0 ~ Group, data = comparison_data) # wmhs tp2

# Visualise WMHs at TP 1
ggplot(comparison_data, aes(x = Group, y = x25781_2_0)) +
  geom_violin(fill = "lightgreen") +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = "WMH Volume Distribution - First Imaging Visit",
       y = "WMH volume (mm3)", x = "") +
  theme_minimal()

# Visualise WMHs at TP 2
ggplot(comparison_data, aes(x = Group, y = x25781_3_0)) +
  geom_violin(fill = "lightpink") +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = "WMH Volume Distribution - First Repeat Imaging Visit",
       y = "WMH volume (mm3)", x = "") +
  theme_minimal()
