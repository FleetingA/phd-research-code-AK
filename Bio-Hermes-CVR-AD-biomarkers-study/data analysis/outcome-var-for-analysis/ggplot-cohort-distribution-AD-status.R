----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project

# Author: Angelina Kancheva 
# Date: 19/07/2024
----------------------------------------------------------------------------------
  
### Cohort Distribution - AD Status

# Load libraries 
library(readr)
library(ggplot2)

# Load data
dataset <- read_csv("~/files/Bio_Hermes_Data/Bio_Hermes_Clinical_Data/SUPPDM.csv", trim_ws = FALSE)

# Make a plot
base_plot <- ggplot(dataset, aes(x = QVAL))
base_plot + geom_bar(fill = "skyblue") + theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45,hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(
    title = "Cohort Distribution in terms of AD Status",
    x = "Cohort Status",
    y = "Number of Participants"
  )

        
