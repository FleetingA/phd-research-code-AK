
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date: 14/05/2025
--------------------------------------------------------------------------
  
### Running Descriptives on the WMH Volume Time Point 1 and Time Point 2 Data

# Load libraries
library(readr)
library(psych)
library(tidyr)
library(dplyr)

# Load data
data <- read_csv("C:/Users/angel/Desktop/PRECISION MEDICINE/PHESANT-master/PHESANT-master/MyWas/data16042025/traitofinterest_allTPs_and_TPdiff_NO_outliers.csv")
View(data)

# Describe the data
summary(data)
describe(data)

## Split the data

# Positive values
data_positive <- subset(data, x25781_difference >= 0)

# Negative values
data_negative <- subset(data, x25781_difference < 0)

# Descriptives for positive group
describe(data_positive)

# Descriptives for negative group
describe(data_negative)

# Run repeated measures ANOVA

# Positive group to long format
data_pos_long <- data_positive %>%
  select(userId, x25781_2_0, x25781_3_0) %>%
  pivot_longer(cols = starts_with("x25781"), names_to = "Time", values_to = "Volume") %>%
  mutate(Time = recode(Time,
                       x25781_2_0 = "TP1",
                       x25781_3_0 = "TP2"),
         Time = factor(Time, levels = c("TP1", "TP2")))

# Same for negative group
data_neg_long <- data_negative %>%
  select(userId, x25781_2_0, x25781_3_0) %>%
  pivot_longer(cols = starts_with("x25781"), names_to = "Time", values_to = "Volume") %>%
  mutate(Time = recode(Time,
                       x25781_2_0 = "TP1",
                       x25781_3_0 = "TP2"),
         Time = factor(Time, levels = c("TP1", "TP2")))

install.packages("afex")  # Only once
library(afex)

# Positive group
aov_pos <- aov_ez(id = "userId", dv = "Volume", within = "Time", data = data_pos_long)

# Negative group
aov_neg <- aov_ez(id = "userId", dv = "Volume", within = "Time", data = data_neg_long)

# View results
summary(aov_pos)
summary(aov_neg)

# Plot data
library(ggplot2) # ggplot needed

# Combine both groups with a Group variable
data_combined <- data %>%
  mutate(Group = ifelse(x25781_difference > 0, "Positive",
                        ifelse(x25781_difference < 0, "Negative", NA))) %>%
  filter(!is.na(Group)) %>%
  select(userId, Group, x25781_2_0, x25781_3_0) %>%
  pivot_longer(cols = starts_with("x25781"), names_to = "Time", values_to = "Volume") %>%
  mutate(Time = recode(Time,
                       x25781_2_0 = "TP1",
                       x25781_3_0 = "TP2"),
         Time = factor(Time, levels = c("TP1", "TP2")))

# Plot with individual lines + group means
ggplot(data_combined, aes(x = Time, y = Volume, group = userId, color = Group)) +
  geom_line(alpha = 0.3) +
  stat_summary(aes(group = Group), fun = mean, geom = "line", size = 1.2) +
  labs(title = "Brain Volume Change Over Time by Group",
       y = "Brain Volume", x = "Time Point") +
  theme_minimal()

# Other visualisation options

# Line plot
ggplot(data_pos_long, aes(x = Time, y = Volume, group = userId)) +
  geom_line(alpha = 0.1, color = "steelblue") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), size = 1.5, color = "darkblue") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "darkblue") +
  labs(title = "WMH Volume Change Over Time (Positive Group)",
       y = "WMH Volume") +
  theme_minimal()

# Box plot
ggplot(data_pos_long, aes(x = Time, y = Volume)) +
  geom_line(aes(group = userId), alpha = 0.1, color = "lightgray") +
  geom_boxplot(width = 0.4, outlier.shape = NA, fill = "#4F81BD", alpha = 0.7, color = "#2F4F4F") +
  stat_summary(fun = mean, geom = "point", color = "black", size = 2.5) +
  labs(title = "Distribution of WMH Volume by Time (Positive Group)",
       y = "WMH Volume") +
  theme_minimal()

# Violin plot
ggplot(data_pos_long, aes(x = Time, y = Volume)) +
  geom_line(aes(group = userId), alpha = 0.1, color = "gray80") +
  geom_violin(fill = "#00A99D", color = "#00695C", alpha = 0.6, width = 0.8, trim = FALSE) +
  stat_summary(fun = mean, geom = "point", color = "#FFB000", size = 3) +
  labs(title = "WMH Volume Distribution Over Time (Positive Group)",
       y = "WMH Volume") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggplot(data_neg_long, aes(x = Time, y = Volume)) +
  geom_line(aes(group = userId), alpha = 0.1, color = "gray80") +
  geom_violin(fill = "#E07B91", color = "#8B3A62", alpha = 0.6, width = 0.8, trim = FALSE) +
  stat_summary(fun = mean, geom = "point", color = "#003366", size = 3) +
  labs(title = "WMH Volume Distribution Over Time (Negative Group)",
       y = "WMH Volume") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Plot the two groups together
data_pos_long$Group <- "Progressors"
data_neg_long$Group <- "Regressors"

data_combined <- rbind(data_pos_long, data_neg_long)

ggplot(data_combined, aes(x = Time, y = Volume, fill = Group, color = Group)) +
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.6, width = 0.8, trim = FALSE) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.9), 
               color = "black", size = 2.5) +
  scale_fill_manual(values = c("Progressors" = "#00A99D", "Regressors" = "#E07B91")) +
  scale_color_manual(values = c("Progressors" = "#00695C", "Regressors" = "#8B3A62")) +
  labs(title = "WMH Volume Distribution by Group and Time Point",
       x = "Time Point",
       y = "WMH Volume",
       fill = "Group",
       color = "Group") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold")
  )
