
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date: 01/04/2025
--------------------------------------------------------------------------
  
# Load libraries
library(dplyr)
library(psych)
library(readr)
library(broom)
library(car) # for Anova()
library(emmeans)

### Dealing with WMH outliers 

# Progressors
tofi_progressors_only <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/tofi_progressors_only.csv")
View(tofi_progressors_only)
hist(tofi_progressors_only$x25781_difference) # very extreme

# Regressors
tofi_regressors_only <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/tofi_regressors_only.csv")
View(tofi_regressors_only)
hist(tofi_regressors_only$x25781_difference) # very extreme too

# Visualise outliers - different ways
boxplot(tofi_progressors_only$x25781_difference, main = "Boxplot of WMH Volume Difference")
library(ggplot2)
ggplot(tofi_progressors_only, aes(x = "", y = tofi_progressors_only$x25781_difference)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 8)

ggplot(tofi_progressors_only, aes(x = "", y = tofi_progressors_only$x25781_difference)) +
  geom_boxplot(outlier.colour = "red") +
  coord_cartesian(ylim = c(0, 20000)) +  # adjust the upper limit
  labs(title = "Zoomed Boxplot", y = "Difference")

# Visualise outliers for progressors
ggplot(tofi_progressors_only, aes(x = "", y = tofi_progressors_only$x25781_difference)) +
  geom_boxplot(outlier.colour = "red") +
  labs(
    title = "Boxplot of WMH Volume Differences between TP1 and TP2 - Progressors",
    x = "Participants",
    y = "WMH Volume Difference (mm³)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),   # title size
    axis.title.x = element_text(size = 14),                             # x-axis title size
    axis.title.y = element_text(size = 14),                             # y-axis title size
    axis.text.x = element_text(size = 12),                              # x tick labels
    axis.text.y = element_text(size = 12)                               # y tick labels
  )

# Visualise outliers for regressors
ggplot(tofi_regressors_only, aes(x = "", y = tofi_regressors_only$x25781_difference)) +
  geom_boxplot(outlier.colour = "blue2") +
  labs(
    title = "Boxplot of WMH Volume Differences between TP1 and TP2 - Regressors",
    x = "Participants",
    y = "WMH Volume Difference (mm³)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),   # title size
    axis.title.x = element_text(size = 14),                             # x-axis title size
    axis.title.y = element_text(size = 14),                             # y-axis title size
    axis.text.x = element_text(size = 12),                              # x tick labels
    axis.text.y = element_text(size = 12)                               # y tick labels
  )

# How many cases to remove - progressors
Q1 <- quantile(tofi_progressors_only$x25781_difference, 0.25, na.rm = TRUE)
Q3 <- quantile(tofi_progressors_only$x25781_difference, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

upper_bound <- Q3 + 3 * IQR_value

# Count how many are extreme outliers
sum(tofi_progressors_only$x25781_difference > upper_bound, na.rm = TRUE)

# How many cases to remove - regressors
Q1 <- quantile(tofi_regressors_only$x25781_difference, 0.25, na.rm = TRUE)
Q3 <- quantile(tofi_regressors_only$x25781_difference, 0.75, na.rm = TRUE)
IQR_value2 <- Q3 - Q1

upper_bound2 <- Q3 + 3 * IQR_value2

# Count how many are extreme outliers
sum(tofi_regressors_only$x25781_difference > upper_bound2, na.rm = TRUE)

# For progressors
Q1_prog <- quantile(tofi_progressors_only$x25781_difference, 0.25, na.rm = TRUE)
Q3_prog <- quantile(tofi_progressors_only$x25781_difference, 0.75, na.rm = TRUE)
IQR_prog <- Q3_prog - Q1_prog

lower_prog <- Q1_prog - 3 * IQR_prog
upper_prog <- Q3_prog + 3 * IQR_prog

n_outliers_prog <- sum(tofi_progressors_only$x25781_difference < lower_prog |
                         tofi_progressors_only$x25781_difference > upper_prog, na.rm = TRUE)

# For regressors
Q1_regr <- quantile(tofi_regressors_only$x25781_difference, 0.25, na.rm = TRUE)
Q3_regr <- quantile(tofi_regressors_only$x25781_difference, 0.75, na.rm = TRUE)
IQR_regr <- Q3_regr - Q1_regr

lower_regr <- Q1_regr - 3 * IQR_regr
upper_regr <- Q3_regr + 3 * IQR_regr

n_outliers_regr <- sum(tofi_regressors_only$x25781_difference < lower_regr |
                         tofi_regressors_only$x25781_difference > upper_regr, na.rm = TRUE)

# Results
n_outliers_prog
n_outliers_regr

# Progressors without extreme outliers
clean_progressors <- tofi_progressors_only %>%
  filter(x25781_difference >= lower_prog & x25781_difference <= upper_prog)

# Regressors without extreme outliers
clean_regressors <- tofi_regressors_only %>%
  filter(x25781_difference >= lower_regr & x25781_difference <= upper_regr)

# Add group labels
clean_progressors$Group <- "Progressors"
clean_regressors$Group <- "Regressors"

# Combine into one data frame
combined_clean <- rbind(clean_progressors, clean_regressors)

# Library for plotting results
library(ggplot2)

ggplot(combined_clean, aes(x = x25781_difference, fill = Group)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribution of WMH Volume Differences (Outliers Removed)",
    x = "WMH Volume Difference (mm³)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(combined_clean, aes(x = x25781_difference, fill = Group)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  labs(
    title = "Histogram of WMH Volume Differences (Outliers Removed)",
    x = "WMH Volume Difference (mm³)",
    y = "Count"
  ) +
  theme_minimal()

# Compare progressors and regressors
library(dplyr)
library(ggplot2)

# Add group & status to original datasets
progressors_original <- tofi_progressors_only %>%
  mutate(Group = "Progressors", Status = "Original")

regressors_original <- tofi_regressors_only %>%
  mutate(Group = "Regressors", Status = "Original")

# Cleaned datasets (after outlier removal)
clean_progressors <- tofi_progressors_only %>%
  filter(x25781_difference >= lower_prog & x25781_difference <= upper_prog) %>%
  mutate(Group = "Progressors", Status = "Cleaned")

clean_regressors <- tofi_regressors_only %>%
  filter(x25781_difference >= lower_regr & x25781_difference <= upper_regr) %>%
  mutate(Group = "Regressors", Status = "Cleaned")

# Combine all
combined_all <- bind_rows(progressors_original, regressors_original,
                          clean_progressors, clean_regressors)

ggplot(combined_all, aes(x = x25781_difference, fill = Status)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ Group, scales = "free", ncol = 1) +  # stack vertically
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +  # pad sides
  labs(
    title = "Distribution of WMH Volume Differences Before and After Outlier Removal",
    x = "WMH Volume Difference (mm³)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)  # bump up base text size

# Remove outliers from progressors based on our calculated bounds
clean_progressors <- tofi_progressors_only %>%
  filter(x25781_difference >= lower_prog & x25781_difference <= upper_prog)

# Plot the cleaned boxplot for progressors
ggplot(clean_progressors, aes(x = "", y = x25781_difference)) +
  geom_boxplot(outlier.colour = "red") +
  labs(
    title = "Boxplot of WMH Volume Differences between TP1 and TP2 - Progressors (Cleaned)",
    x = "Participants",
    y = "WMH Volume Difference (mm³)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Center and style the title
    axis.title.x = element_text(size = 14),  # x-axis title size
    axis.title.y = element_text(size = 14),  # y-axis title size
    axis.text.x = element_text(size = 12),   # x tick label size
    axis.text.y = element_text(size = 12)    # y tick label size
  )

# Remove outliers from regressors based on our calculated bounds
clean_regressors <- tofi_regressors_only %>%
  filter(x25781_difference >= lower_regr & x25781_difference <= upper_regr)

# Plot the cleaned boxplot for regressors
ggplot(clean_regressors, aes(x = "", y = x25781_difference)) +
  geom_boxplot(outlier.colour = "blue2") +
  labs(
    title = "Boxplot of WMH Volume Differences between TP1 and TP2 - Regressors (Cleaned)",
    x = "Participants",
    y = "WMH Volume Difference (mm³)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Center and style the title
    axis.title.x = element_text(size = 14),  # x-axis title size
    axis.title.y = element_text(size = 14),  # y-axis title size
    axis.text.x = element_text(size = 12),   # x tick label size
    axis.text.y = element_text(size = 12)    # y tick label size
  )

# Number of original observations
n_total_prog <- nrow(tofi_progressors_only)
print(n_total_prog) # 2886
n_total_regr <- nrow(tofi_regressors_only)
print(n_total_regr) # 1686

# Number of cleaned observations (after removing outliers)
n_clean_prog <- nrow(clean_progressors)
print(n_clean_prog) # 2750
n_clean_regr <- nrow(clean_regressors)
print(n_clean_regr) # 1579

# Print results
cat("Progressors:\n",
    "  Total: ", n_total_prog, "\n",
    "  Cleaned (no extreme outliers): ", n_clean_prog, "\n",
    "  Outliers removed: ", n_total_prog - n_clean_prog, "\n\n")

cat("Regressors:\n",
    "  Total: ", n_total_regr, "\n",
    "  Cleaned (no extreme outliers): ", n_clean_regr, "\n",
    "  Outliers removed: ", n_total_regr - n_clean_regr, "\n")

# Get outlier IDs and values

outliers_progressors <- tofi_progressors_only %>%
  filter(x25781_difference < lower_prog | x25781_difference > upper_prog) %>%
  select(userId, x25781_difference)

# View or print
print(outliers_progressors, n=Inf)

outliers_regressors <- tofi_regressors_only %>%
  filter(x25781_difference < lower_regr | x25781_difference > upper_regr) %>%
  select(userId, x25781_difference)

# View or print
print(outliers_regressors, n=Inf)

# Remove outliers from trait of interest file

traitofinterest <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/traitofinterest.csv")
View(traitofinterest)

outlier_user_ids <- c(
  5999271, 4229410, 5462804, 3474911, 1662078, 4917199, 4769419, 1376505, 1894494, 1035008,
  3788679, 3649731, 2060292, 1836104, 5179351, 2471201, 5141454, 3482047, 5703267, 4813642,
  2159943, 2488125, 3777486, 3677628, 1201565, 3643936, 4639810, 5924848, 1818771, 2459653,
  3735096, 1508218, 1218751, 3011367, 3560398, 1379682, 5746249, 4744235, 5027712, 4080315,
  2819514, 2639105, 3596724, 5746458, 1732187, 3871481, 5135361, 3053446, 4035659, 4393144,
  2958726, 2394063, 2411109, 3181050, 5467689, 2079724, 4597961, 1093625, 3894745, 3561728,
  5985427, 3260520, 1937726, 5958374, 5167183, 4795101, 2340964, 3503280, 5091109, 4768297,
  5787126, 1022924, 4901866, 1761223, 4197438, 4618521, 4581620, 2032587, 1084270, 2639943,
  6025655, 1508376, 4252208, 2320160, 3653682, 3857196, 3253347, 1331132, 2083131, 4540617,
  1939745, 5734470, 1153164, 2439809, 4283082, 1264532, 4211883, 5085992, 1822165, 4351541,
  4270641, 4385195, 5879414, 3197729, 5878849, 3094500, 4505989, 5701950, 5396679, 5004740,
  5500322, 5173912, 2564948, 5431750, 2118823, 1481801, 4089876, 1543908, 1797439, 1457608,
  2360660, 2394235, 3556187, 1176776, 4947812, 5205973, 1935206, 4964414, 2996409, 3777915,
  5218624, 3427385, 4942293, 5576257, 3579882, 2745184, 4478857, 3643560, 4678741, 4718089,
  3413033, 1285464, 5416027, 3754177, 4529522, 5105341, 1110949, 5501006, 5612978, 3549876,
  4716790, 2471165, 5996256, 5219884, 5220970, 1178505, 4171567, 5333102, 2055570, 1062159,
  2765026, 3674963, 5484681, 5279573, 5025654, 5110666, 1349842, 1801459, 1136339, 4403910,
  2441326, 3276949, 4664691, 4552363, 5369816, 4554053, 3232683, 5941856, 4086342, 5987057,
  4921776, 4635169, 1090842, 1269026, 5979476, 2794426, 5359266, 5610834, 1089115, 4735552,
  5610732, 2152062, 4733269, 3707534, 2320839, 5152025, 5585125, 2871249, 3333216, 5292266,
  2782033, 4905858, 4684814, 4918638, 1611792, 2061618, 1471246, 4603126, 5891672, 2936737,
  4736472, 2923222, 1338466, 4833833, 2428750, 5107265, 5506370, 2080488, 3614506, 1278586,
  4815197, 5243077, 4550854, 2379730, 3916537, 5978828, 3381360, 1556688, 2580286, 5745070,
  4519848, 2434240, 3299524, 3673908, 3573785, 5592300, 5792449, 1118467, 5195110, 1030337,
  1295405, 1399629, 4891641
)

length(outlier_user_ids) # 243

library(dplyr)
# Remove rows where userIds match any in outlier_user_ids
traitofinterest_cleaned <- traitofinterest %>%
  filter(!userId %in% outlier_user_ids)
sum(!is.na(traitofinterest_cleaned$x25781_3_0)) # 4520

any(traitofinterest_cleaned$userId %in% outlier_user_ids) # FALSE

traitofinterest_cleaned %>%
  filter(userId %in% outlier_user_ids)

traitofinterest <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/traitofinterest.csv")
View(traitofinterest)
hist(traitofinterest$x25781_3_0) # very extreme as well 

# Check variable distributions before recoding

table(phenotypes_260325$x20116_2_0) # smoking
table(phenotypes_260325$x20117_2_0) # drinking
table(phenotypes_260325$x20126_0_0) # bipolar & depression
table(phenotypes_260325$x20165_0_0)
table(phenotypes_260325$x5699_2_0)
table(phenotypes_260325$x5779_2_0)
table(phenotypes_260325$x20167_0_0)
table(phenotypes_260325$x20169_0_0)
table(phenotypes_260325$x20171_0_0)
table(phenotypes_260325$x20177_0_0)
table(phenotypes_260325$x20179_0_0)
table(phenotypes_260325$x20123_0_0)
table(phenotypes_260325$x1558_2_0)

# Save new dataset - n=4520 cases with values for x25781_3_0 after removing outliers
write.csv(
  traitofinterest_cleaned,
  "C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data25032025/traitofinterest_cleaned.csv",
  row.names = FALSE
)

