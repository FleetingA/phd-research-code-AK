
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

# Load packages
library(readxl)
library(ggplot2)
library(ggdist)# Read the Excel Data

# Load data
PheWas_Results_Excel <- read_excel("C:/Users/angel/Desktop/PheWas Paper Figures/NEW Val Phenotypes 311023.xlsx")

# Create the bubble plot with transparency and a catchy colour palette
bubble_plot <- ggplot(PheWas_Results_Excel, aes(x = round(Beta, 3), y = Clinical_Phenotype, size = `p-value`, color = `p-value`)) +
  geom_point(position = position_dodge(width = 1), alpha = 0.5, stroke = 0.1, na.rm = TRUE) +
  scale_size_continuous(
    range = c(11, 3),
    name = "p-value",
    guide = "legend"  # Add size legend
  ) +
  scale_color_viridis_c(name = "p-value") +  # Use P-value for color
  theme_minimal() +
  labs(x = "Beta", y = "Clinical Phenotype") +  # Update axis labels
  ggtitle("Clinical Phenotypes Associated with cSVD — Proof of Concept") +
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 23), face = "bold"),
    legend.spacing.y = unit(4, "in"),
    legend.key.size = unit(0.7, "in"),
    axis.title.x = element_text(margin = margin(t = 20), size = 13), 
    axis.title.y = element_text(margin = margin(r = 5), size = 13), 
    axis.text.y = element_text(size = 9, margin = margin(t = 10, b = 10)),  # Increase distance between displayed phenotypes
    strip.text.y = element_text(margin = margin(t = 5, b = 5), size = 20)  # Increase text size and margin
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "orange2") +  # Add a vertical line at 0
  scale_x_continuous(limits = c(min(round(PheWas_Results_Excel$Beta, 3)), max(round(PheWas_Results_Excel$Beta, 3))), 
                     breaks = seq(min(round(PheWas_Results_Excel$Beta, 3)), max(round(PheWas_Results_Excel$Beta, 3)), by = 0.05)) +
                     coord_cartesian(ylim = c(0, nrow(PheWas_Results_Excel) + 1))
                     
# Print the bubble plot
print(bubble_plot)
                     