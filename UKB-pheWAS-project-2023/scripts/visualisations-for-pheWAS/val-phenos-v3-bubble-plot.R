
--------------------------------------------------------------------------
# PhD Project 2023: UKB PheWas cSVD Clinical Phenotypes Project
  
# Author: Angelina Kancheva 
# Date/period: September 2023
--------------------------------------------------------------------------
  
### PheWas Publication - Code for Figures

# Install required packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("forcats")
install.packages("viridis")

# Load packages
library(readxl)
library(ggplot2)
library(forcats)
library(viridis)

# Read the Excel Data
PheWas_Results_Excel <- read_excel("PheWas Results Excel Val.xlsx")
View(data)

# Create a vector of Clinical_Phenotype levels ordered by P_value
phenotype_levels <- PheWas_Results_Excel$Clinical_Phenotype[order(PheWas_Results_Excel$P_value)]

# Set the order of levels for Clinical_Phenotype
PheWas_Results_Excel$Clinical_Phenotype <- factor(PheWas_Results_Excel$Clinical_Phenotype, levels = phenotype_levels)

# Create the bubble plot with transparency and a custom colour gradient
bubble_plot <- ggplot(PheWas_Results_Excel, aes(x = N_of_subjects, y = reorder(Clinical_Phenotype, -P_value), size = P_value, color = P_value)) +
  geom_point(position = position_dodge(width = 0.5), alpha = 0.8, stroke = 0.1) +
  geom_point(na.rm = FALSE) +  # Keep rows with missing values
  scale_size_continuous(range = c(12, 4), name = "P-value") +
  scale_color_gradient(low = "salmon2", high = "springgreen2") +  # Create a gradient from green to grey
  theme_minimal() +
  labs(x = "Number of Subjects", y = "Clinical Phenotype", color = "P-value") +
  ggtitle("Clinical Phenotypes Associated with cSVD") +
  #guides(
  #color = guide_colorbar(
  #title = "P-value",
  #barwidth = 20,
  # barheight = 4,
  #keywidth = unit(2, "in"),
  #label.position = "right",
  #label.vjust = 0.5,
  #label.hjust = 1
  #)
  #) +
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 23), face = "bold"),
    legend.spacing.y = unit(4, "in"),
    legend.key.size = unit(0.7, "in"),
    axis.title.x = element_text(margin = margin(t = 20), size = 13),
    axis.title.y = element_text(margin = margin(r = 5), size = 13),
    axis.text.y = element_text(size = 10),
    strip.text.y = element_text(margin = margin(t = 10, b = 10)),
  ) +
  scale_x_continuous(limits = c(900, 50000), breaks = seq(900, 50000, by = 5000)) +
  coord_cartesian(ylim = rev(c(0, nrow(PheWas_Results_Excel) + 1)))  # Reverse y-axis limits

# Print the bubble plot
print(bubble_plot)
