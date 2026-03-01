
--------------------------------------------------------------------------
# PhD Project 2023: UKB PheWas cSVD Clinical Phenotypes Project
  
# Author: Angelina Kancheva 
# Date/period: September 2023
--------------------------------------------------------------------------
  
### PheWas Publication - Code for Figures

# Install required packages
install.packages("readxl")
install.packages("ggplot2")

# Load packages
library(readxl)
library(ggplot2)

# Read the Excel Data
df <- read_excel("PheWas Results Excel Val.xlsx")

# Plot data
ggplot(df, aes(x = P_value, y = reorder(Clinical_Phenotype, -P_value), color = P_value)) +
  geom_point(size = 5, alpha = 0.7) +  # Increase size and set transparency (alpha)
  scale_color_gradient(
    low = "orange", high = "red", 
    name = "P-value",
    breaks = c(0.001, 0.01, 0.02, 0.03, 0.04, 0.05),  # Customize the breaks
    labels = c("0.001", "0.01", "0.02", "0.03", "0.04", "0.05")  # Customize the labels
  ) +
  labs(x = "P-value", y = "Clinical Phenotype") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5), # Increase size, make it bold, and center it
    strip.text.y = element_text(margin = margin(t = 10, b = 10), size = 16)  # Increase size of y-axis labels
  ) +
  ggtitle("Clinical Phenotypes Associated with cSVD: Proof of Concept")  # Add a title
