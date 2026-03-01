
--------------------------------------------------------------------------
# PhD Project 2023: UKB PheWas cSVD Clinical Phenotypes Project
  
# Author: Angelina Kancheva 
# Date/period: January 2024
--------------------------------------------------------------------------
  
### PheWas Publication - Code for Figures

# Load packages
library(readxl)
library(ggplot2)
library(viridis)

# Read the Excel Data
df <- read_excel("C:/Users/angel/Desktop/2024_PHESANT-master/MyWas/results_ICV/Linear_ICV_corrected.xlsx")

All_linear_raw <- read_excel("C:/Users/angel/Desktop/PHESANT-master/PHESANT-master/MyWas/results_RAW_SCORES_250425/All_linear_raw.xlsx")
View(All_linear_raw)

# Create plot
ggplot(All_linear_raw, aes(x = Beta, y = Clinical_Phenotype, size = Beta, color = Beta)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 17)) +
  scale_color_viridis_c(option = "magno") +
  expand_limits(x = c(0.05, 0.12)) +
  labs(
    title = "Clinical Phenotypes Associated with WMH Volume Change - Linear Associations (unadjusted; raw difference)",
    x = "Beta Values",
    y = "UKB Clinical Phenotype"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 12)),  # top margin for x-axis title
    axis.title.y = element_text(size = 15, margin = margin(r = 12)),  # right margin for y-axis title
    plot.title = element_text(size = 18, face = "bold")
  ) +
  guides(
    size = guide_legend(title = "Beta"),
    color = guide_legend(title = "Beta")
  )
  

