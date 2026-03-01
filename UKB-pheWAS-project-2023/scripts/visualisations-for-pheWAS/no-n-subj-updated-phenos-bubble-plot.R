
--------------------------------------------------------------------------
# PhD Project 2023: UKB PheWas cSVD Clinical Phenotypes Project
  
# Author: Angelina Kancheva 
# Date/period: September 2023
--------------------------------------------------------------------------
  
### PheWas Publication - Code for Figures

# Install required packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("ggdist")

# Load packages
library(readxl)
library(ggplot2)
library(ggdist)

# Read the Excel Data
df <- read_excel("PheWas Results Excel Novel.xlsx")

# Plot data
ggplot(df, aes(x = P_value, y = reorder(Clinical_Phenotype, -P_value), size = -P_value, color = P_value)) +
  geom_point(alpha = 0.6, stroke = 0) +  # Increase alpha (transparency) and set stroke
  scale_size_continuous(range = c(1, 5)) +  # Adjust the size range for bubbles
  scale_color_gradient(
    low = "deeppink3", high = "cyan3", 
    name = "P-value",
    breaks = c(0.001, 0.01, 0.02, 0.03, 0.04, 0.05),
    labels = c("0.001", "0.01", "0.02", "0.03", "0.04", "0.05")
  ) +
  labs(x = "P-value", y = "Clinical Phenotype") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    strip.text.y = element_text(margin = margin(t = 10, b = 10), size = 20)
  ) +
  ggtitle("Clinical Phenotypes Associated with cSVD: Novel Findings")
  coord_cartesian(ylim = c(-10, nrow(df) + 30))  # Adjust the upper limit of the y-axis
  
