
--------------------------------------------------------------------------
# PhD Project 2023: UKB PheWas cSVD Clinical Phenotypes Project
  
# Author: Angelina Kancheva 
# Date/period: January 2024
--------------------------------------------------------------------------
  
### PheWas Publication - Code for Figures

# Load packages
library(readxl)
library(ggplot2)
library(forcats)
library(viridis)

# Read the Excel Data
df <- read_excel("C:/Users/angel/Desktop/2024_PHESANT-master/MyWas/results/Logistic_Associations_260124.xlsx")

# Create the bubble plot
ggplot(df, aes(x = OR, y = Clinical_Phenotype, color = OR)) +
  geom_point(alpha = 0.7) +
  #scale_size_continuous(range = c(3, 15)) +
  scale_color_viridis_c(option = "magma") +
  labs(title = "Bubble Plot of Clinical Phenotypes",
       x = "Odds Ratio (OR)",
       y = "Clinical Phenotype") +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(size = guide_legend(title = "OR"),
  color = guide_legend(title = "OR"))
