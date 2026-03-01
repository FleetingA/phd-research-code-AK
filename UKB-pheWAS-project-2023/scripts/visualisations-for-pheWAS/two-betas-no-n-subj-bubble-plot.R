
--------------------------------------------------------------------------
# PhD Project 2023: UKB PheWas cSVD Clinical Phenotypes Project
  
# Author: Angelina Kancheva 
# Date/period: October 2023
--------------------------------------------------------------------------
  
### PheWas Publication - Code for Figures

# Install required packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("ggdist")
install.packages("dplyr")

# Load packages
library(readxl)
library(ggplot2)
library(ggdist) # Read the Excel Data
library(dplyr)

# Load data
data <- read_excel("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/PheWas Paper Figures/NEW ALL PHENOTYPES LINEAR 051223.xlsx")

# Create the bubble plot
bubble_plot <- ggplot(data, aes(x = Beta, y = UKB_Phenotype_Description, size = pvalue_FDR, color = pvalue_FDR)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(5, 13), breaks = seq(0, 0.05, by = 0.01), trans = "reverse") +
  scale_color_viridis_c(name = "P-Value", option = "viridis", direction = -1) +  # Reverse color scale
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 14),
    strip.text.y = element_text(size = 14)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "lightcoral")  # Add dashed line at y = 0
  
# Print the bubble plot
print(bubble_plot)