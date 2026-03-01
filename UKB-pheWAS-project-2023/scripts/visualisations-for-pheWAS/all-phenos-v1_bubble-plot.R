
### PheWas Paper - Figures - 09/2023 ###

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

# Read the Excel Data - make sure you specify correct file to path
PheWas_Results_Excel <- read_excel("PheWas Paper Figures/PheWas Results Excel.xlsx")

# Reverse the order of Clinical_Phenotype and P_value
data$Clinical_Phenotype <- fct_reorder(data$Clinical_Phenotype, -data$P_value)

# Create the bubble plot with transparency and a catchy color palette
bubble_plot <- ggplot(PheWas_Results_Excel, aes(x = N_of_subjects, y = Clinical_Phenotype, size = P_value, color = P_value)) +
  geom_point(position = position_dodge(width = 0.5), alpha = 0.5, stroke = 0.3) +
  scale_size_continuous(range = c(12,4), name = "P-value") +
  scale_color_viridis_c(option="plasma") + 
  theme_minimal() +
  labs(x = "Number of Subjects", y = "Clinical Phenotype", color = "P-value") +
  ggtitle("Clinical Phenotypes Associated with cSVD") +
  guides(
    color = guide_colorbar(
      title = "P-value",
      barwidth = 20,
      barheight = 4,
      keywidth = unit(2, "in"),
      label.position = "right",
      label.vjust = 0.5,
      label.hjust = 1
    )
  ) +
  theme(
    plot.title = element_text(size = 14, margin = margin(b = 23), face = "bold"),
    legend.spacing.y = unit(4, "in"),
    legend.key.size = unit(0.7, "in"),
    axis.title.x = element_text(margin = margin(t = 20), size = 13), 
    axis.title.y = element_text(margin = margin(r = 5), size = 13), 
    axis.text.y = element_text(size = 10),
    strip.text.y = element_text(margin = margin(t = 10, b = 10)),
  ) +
  scale_x_continuous(limits = c(5000, 45000), breaks = seq(5000, 45000, by = 5000)) +
  coord_cartesian(ylim= c(0, nrow(data) + 1))

# Print the bubble plot
print(bubble_plot)

