
### PheWas Paper - Figures - 01/24 ###

# Install required packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("ggdist")
install.packages("dplyr")

# Load packages
library(ggplot2)
library(viridis)

ls() 

## MANUAL WAY

# ORs vector
ors <- c(2.85,
         2.51,
         1.44,
         0.47
)

# Clinical phenotypes vector

phenotypes <- c("Diabetes related eye disease",
                "Difficulty stopping worrying during worst period of anxiety",
                "Number of things worried about during worst period of anxiety",
                "Single episode of probable major depression"
)

# Check the correspondence
df <- data.frame(OR = ors, Clinical_Phenotype = phenotypes)

# Create the bubble plot
bubble_plot <- ggplot(df, aes(x = OR, y = Clinical_Phenotype, size = OR, color = OR)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(2, 14)) +
  scale_color_viridis_c(name = "OR", option = "inferno") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 13),
    strip.text.y = element_text(size = 13),
    panel.spacing.y = unit(0.9, "lines")
  ) +  
  coord_cartesian(ylim = c(-1, length(phenotypes) + 1))  # Adjust ylim based on the number of phenotypes

# Print the bubble plot
print(bubble_plot)

## MORE EFFICIENT WAY - 23/04/2025

library(readxl)
All_logistic_raw_adjusted <- read_excel("C:/Users/angel/Desktop/PHESANT-master/PHESANT-master/MyWas/Visualizations/All_logistic_raw_adjusted.xlsx")
View(All_logistic_raw_adjusted)

ggplot(All_logistic_raw_adjusted, aes(x = OR, y = Clinical_Phenotype, size = OR, color = OR)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 17)) +
  scale_color_viridis_c(option = "plasma") +
  expand_limits(x = c(0.05, 0.12)) +
  labs(
    title = "Clinical Phenotypes Associated with WMH Volume Change - Logistic Associations (fully adjusted; raw difference)",
    x = "Odds Ratios",
    y = "UKB Clinical Phenotype"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 12)),
    axis.title.y = element_text(size = 15, margin = margin(r = 12)),
    plot.title = element_text(size = 18, face = "bold", hjust = 5, margin = margin(b = 10)),  # Title aligned to the left
    plot.margin = margin(t = 30, r = 20, b = 20, l = 50)  # More space at the top
  ) +
  guides(
    size = guide_legend(title = "OR"),
    color = guide_legend(title = "OR")
  )


