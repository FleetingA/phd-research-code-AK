
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 13/08/2024
----------------------------------------------------------------------------------
  
# Loading necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

### PCA Analysis

# Select relevant variables for PCA
merged_pred_outcome_120824 <- read_csv("Angelina_Bio_Hermes_R_code/PCA_ANOVA_T-tests/merged_pred_outcome_120824.csv")
View(merged_pred_outcome_120824)
pca_data <- merged_pred_outcome_120824[, c("TauP217_Lilly", "TauP181_Quant", "AMYLB40_Roche", 
                                           "AMYLB42_Roche", "ApoE4_Roche", "TauP181_Roche",
                                           "AMYLB40_MerckGAP", "AMYLB42_MerckGAP", "AB4240_MerckGAP")]

# Run PCA without scaling (as the variables might already be standardised)
pca_result <- prcomp(pca_data, scale = TRUE)

# Summarise PCA results
summary(pca_result)

# View the loadings (contributions of the variables to the principal components)
pca_result$rotation

# Visualise PCA results (scree plot)
screeplot(pca_result, type = "lines")

# Calculate explained variance for each component
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
explained_variance_df <- data.frame(
  Component = paste0("PC", 1:length(explained_variance)),
  Variance_Explained = explained_variance,
  Cumulative_Variance = cumsum(explained_variance)
)

# Create the scree plot using ggplot2
ggplot(explained_variance_df, aes(x = Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "cadetblue3") +
  geom_text(aes(label = sprintf("%.2f%%", Variance_Explained * 100)),
            vjust = -1, size = 4) +  # Adjusted vjust to move the text higher
  geom_line(aes(y = Cumulative_Variance), color = "coral2", group = 1) +
  geom_point(aes(y = Cumulative_Variance), color = "coral2") +
  geom_text(aes(y = Cumulative_Variance, label = sprintf("%.2f%%", Cumulative_Variance * 100)),
            vjust = 2, hjust = 0.2, size = 4, color = "coral2") +  # Adjusted vjust and hjust for better spacing
  labs(
    title = "Scree Plot with Explained Variance",
    x = "Principal Components",
    y = "Variance Explained"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Get the loadings of PCA
loadings <- pca_result$rotation

# Convert the loadings into a data frame for easier inspection
loadings_df <- as.data.frame(loadings)
View(loadings_df)

# Save loadings into a table
write.csv(loadings_df, "loadings_PCA_table.csv", row.names = FALSE)

# Loop over each principal component and plot the loadings
for (i in 1:ncol(loadings_df)) {
  component_name <- colnames(loadings_df)[i]
  
  ggplot(loadings_df, aes_string(x = "rownames(loadings_df)", y = component_name)) +
    geom_bar(stat = "identity", color = "goldenrod1", fill = "goldenrod1") +
    labs(
      title = paste("Loadings for", component_name),
      x = "Variables",
      y = "Loading"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip() -> p
  
  print(p)
}

# More plots - can play around with the components 
pca_data <- data.frame(USUBJID = merged_pred_outcome_120824$USUBJID,
                       PC2 = pca_result$x[, 2],
                       PC3 = pca_result$x[, 3],
                       AD_status = merged_pred_outcome_120824$AD_status)

# Scatter plot of PC2 and PC3
ggplot(pca_data, aes(x = PC2, y = PC3, color = factor(AD_status))) +
  geom_point(size = 3) +
  labs(
    title = "PCA Scatter Plot: PC2 vs PC3",
    x = "Principal Component 2",
    y = "Principal Component 3",
    color = "AD Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Biplot to visualize the original variables and PCs
biplot(pca_result, scale = 0, cex = 0.7)

# Perform K-means clustering on the first two principal components
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(pca_data[, c("PC2", "PC3")], centers = 3)

# Add cluster information to the data
pca_data$Cluster <- as.factor(kmeans_result$cluster)

# Scatter plot with clusters
ggplot(pca_data, aes(x = PC2, y = PC3, color = Cluster)) +
  geom_point(size = 3) +
  labs(
    title = "K-Means Clustering on PCA Results",
    x = "Principal Component 2",
    y = "Principal Component 3",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Load more necessary libraries
library(plotly)

# Assuming `pca_result` is the PCA result
pca_scores <- as.data.frame(pca_result$x) # Get the principal component scores

# Create a 3D plot for the first three principal components
p <- plot_ly(pca_scores, x = ~PC1, y = ~PC2, z = ~PC3, 
             color = ~"AD_status", # Use your grouping variable, e.g., AD_status, to color points
             colors = c("blue", "red")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         title = "3D Plot of the First Three Principal Components")

p

# Create a 3D scatter plot using the first three principal components
pca_3d_data <- data.frame(pca_result$x[, 1:3], AD_status = merged_pred_outcome_120824$AD_status)

# Plot using plotly for interactive 3D visualisation
plot_ly(
  pca_3d_data,
  x = ~PC1, y = ~PC2, z = ~PC3,
  color = ~factor(AD_status),
  colors = c("blue", "red"),
  type = "scatter3d",
  mode = "markers"
) %>%
  layout(
    title = "3D PCA Plot",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )



