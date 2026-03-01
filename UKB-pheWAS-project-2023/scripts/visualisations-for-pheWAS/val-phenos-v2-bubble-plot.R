
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
PheWas_Results_Excel_Val <- read_excel("C:/Users/angel/Desktop/PheWas Paper Figures/PheWas Results Excel Val.xlsx")

# Set the maximum dimension to a larger value (e.g., 100000)
options(ragg.max_dim = 1000)

# Assuming PheWas_Results_Excel is your data frame
#PheWas_Results_Excel$P_value <- as.numeric(gsub("E-", "e-", PheWas_Results_Excel$P_value))

# Create the bubble plot with transparency and a custom colour gradient
bubble_plot <- ggplot(PheWas_Results_Excel_Val, aes(x = N_of_subjects, y = reorder(Clinical_Phenotype, -P_value), size = P_value, color = P_value)) +
  geom_point(alpha = 0.9, stroke = 0) +  # Remove unnecessary position_dodge() and increase spacing
  geom_point(na.rm = FALSE) +  # Keep rows with missing values
  scale_size_continuous(range = c(8.5, 1.5), name = "P-value") +
  scale_color_viridis_c(option = "magma") + # Change the palette to "magma"
  theme_minimal() +  
  # guides(
  # color = guide_colorbar(
  # title = "P-value",
  # barwidth = 20,
  # barheight = 4,
  #  keywidth = unit(2, "in"),
  # label.position = "right",
  #  label.vjust = 0.5,
  # label.hjust = 1
  #  )
  #  ) +
  theme(
    legend.spacing.y = unit(4, "in"),
    legend.key.size = unit(0.7, "in"),
    strip.text.y = element_text(margin = margin(t = 500, b = 500), size = 16),
    plot.margin = margin(5, 30, 5, 100)  # Adjust the left margin (e.g., increase it to 30)
  ) +
  scale_x_continuous(limits = c(500, 50000), breaks = seq(500, 50000, by = 5000)) +
  coord_cartesian(ylim = c(0, nrow(PheWas_Results_Excel_Val) + 2))

# Print the bubble plot
print(bubble_plot)

# Adjust the dimensions and save the plot as an image with increased height
ggsave("your_plot.png", plot = bubble_plot, width = 80, height = 120, limitsize = FALSE)
