
--------------------------------------------------------------------------
# PhD Project 2023: UKB PheWas cSVD Clinical Phenotypes Project
  
# Author: Angelina Kancheva 
# Date/period: November 2023
--------------------------------------------------------------------------
  
### PheWas Publication - Code for Figures

# Load packages
library(readxl)
library(ggplot2)
library(ggdist)# Read the Excel Data
library(dplyr)

# Load data
PheWas_Results_Excel <- read_excel("C:/Users/angel/Desktop/PheWas Paper Figures/ORmeanvaluesAllPhenotypes.xlsx")

# Create Manhattan plot
manhattan_plot <- ggplot(PheWas_Results_Excel, aes(x = Phenotype, y = mean_OR)) +
  geom_point(aes(size = max_OR - min_OR), color = "blue") +
  geom_errorbar(aes(ymin = min_OR, ymax = max_OR), width = 0.2, color = "blue") +
  coord_flip() +  # Flip the axes to have phenotypes on the y-axis
  labs(x = "Phenotype", y = "Mean Odds Ratio (OR)") +
  scale_size_continuous(range = c(1, 5)) +  # Adjust the size range as needed
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

# Print Manhattan plot
print(manhattan_plot)