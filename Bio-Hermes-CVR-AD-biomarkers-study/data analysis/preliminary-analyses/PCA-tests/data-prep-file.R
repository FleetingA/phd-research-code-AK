
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 12/08/2024
----------------------------------------------------------------------------------

### First, Extra Processing for Predictors

# Loading necessary libraries
library(readr)
library(dplyr)

# Loading datasets to merge
X120824_final_LillyPredictors_merge <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/120824_final_LillyPredictors_merge.csv")
View(X120824_final_LillyPredictors_merge)

X120824_final_QBHPredictors_merge <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/120824_final_QBHPredictors_merge.csv")
View(X120824_final_QBHPredictors_merge)

X120824_RocheBHPredictors_to_merge <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/120824_RocheBHPredictors_to_merge.csv")
View(X120824_RocheBHPredictors_to_merge)

# Rename all column vars according to provider
colnames(X120824_final_LillyPredictors_merge) <- c("USUBJID", "TauP217_Lilly", "TauP217_Lilly_logtr")
View(X120824_final_LillyPredictors_merge)

colnames(X120824_final_QBHPredictors_merge) <- c("USUBJID", "TauP181_Quant", "TauP181_Quant_logtr")
View(X120824_final_LillyPredictors_merge)

print(colnames(X120824_RocheBHPredictors_to_merge))
colnames(X120824_RocheBHPredictors_to_merge) <- c("USUBJID", "AMYLB40_Roche", "AMYLB42_Roche", "ApoE4_Roche", "TauP181_Roche",
                                                  "ApoE4_Roche_logtr", "TauP181_Roche_logtr")
View(X120824_RocheBHPredictors_to_merge)

final_MBH_pg_mL_ABonly <- read_csv("Angelina_Bio_Hermes_R_code/Merck_GAP_Transformations/final_MBH_pg_mL_ABonly.csv")
View(final_MBH_pg_mL_ABonly)    

print(colnames(final_MBH_pg_mL_ABonly))
colnames(final_MBH_pg_mL_ABonly) <- c("USUBJID", "AMYLB40_MerckGAP", "AMYLB42_MerckGAP", "AB4240_MerckGAP", "AB4240_MerckGAP_logtr")
View(final_MBH_pg_mL_ABonly)
                                                
# Save new data frames
write.csv(X120824_final_LillyPredictors_merge, "120824_final_LillyPredict.csv", row.names = FALSE) 
write.csv(X120824_final_QBHPredictors_merge, "120824_final_QBHPredict.csv", row.names = FALSE) 
write.csv(X120824_RocheBHPredictors_to_merge, "120824_final_RochePredict.csv", row.names = FALSE) 
write.csv(final_MBH_pg_mL_ABonly, "120824_final_MerckGAPPredict.csv", row.names = FALSE) 

# Merging the dataframes sequentially by 'USUBJID'
merged_pred <- merge(X120824_final_LillyPredictors_merge, X120824_final_QBHPredictors_merge, by = "USUBJID", all = FALSE)
merged_pred_2 <- merge(merged_pred, X120824_RocheBHPredictors_to_merge, by = "USUBJID", all = FALSE)
merged_pred_120824 <- merge(merged_pred_2, final_MBH_pg_mL_ABonly, by = "USUBJID", all = FALSE)
View(merged_pred_120824)

# Saving the final merged data frame
write.csv(merged_pred_120824, "merged_pred_120824.csv", row.names = FALSE)
View(merged_pred_120824)

# Check if all values in the column 'USUBJID' are unique
all_unique <- !any(duplicated(merged_pred_120824$USUBJID))

# Print the result
if (all_unique) {
  print("All values in the column USUBJID are unique.")
} else {
  print("There are duplicate values in the column USUBJID.")
}

# Read in outcome file 
outcome_selected_binary300724 <- read_csv("Angelina_Bio_Hermes_R_code/Outcome_for_Analysis/outcome_selected_binary300724.csv")
View(outcome_selected_binary300724)

merged_pred_120824 <- read_csv("Angelina_Bio_Hermes_R_code/PCA_ANOVA_T-tests/merged_pred_120824.csv")
View(merged_pred_120824)

# Merge new predictors with outcome 
merged_pred_outcome_120824 <- merge(merged_pred_120824, outcome_selected_binary300724, by = "USUBJID", all = FALSE)

# Save this file
write.csv(merged_pred_outcome_120824, "merged_pred_outcome_120824.csv", row.names = FALSE)
View(merged_pred_outcome_120824)

# Explore differences between those with AD and without

# Load final file again 
merged_pred_outcome_120824 <- read_csv("Angelina_Bio_Hermes_R_code/PCA_ANOVA_T-tests/merged_pred_outcome_120824.csv")
View(merged_pred_outcome_120824)

# First, check data for normality (Shapiro-Wilk test)
shapiro.test(merged_pred_outcome_120824$TauP217_Lilly) # not normally distributed
shapiro.test(merged_pred_outcome_120824$TauP181_Quant) # not normally distributed
shapiro.test(merged_pred_outcome_120824$AMYLB40_Roche) # not normally distributed
shapiro.test(merged_pred_outcome_120824$AMYLB42_Roche) # not normally distributed
shapiro.test(merged_pred_outcome_120824$ApoE4_Roche) # not normally distributed
shapiro.test(merged_pred_outcome_120824$AMYLB40_MerckGAP) # not normally distributed
shapiro.test(merged_pred_outcome_120824$AMYLB42_MerckGAP) # not normally distributed
shapiro.test(merged_pred_outcome_120824$AB4240_MerckGAP) # not normally distributed

# Run non-parametric t-tests to compare AD group (collapsed) versus No AD group
wilcox_test_TauP217_Lilly <- wilcox.test(TauP217_Lilly ~ AD_status, data = merged_pred_outcome_120824)
wilcox_test_TauP181_Quant <- wilcox.test(TauP181_Quant ~ AD_status, data = merged_pred_outcome_120824)
wilcox_test_AMYLB40_Roche <- wilcox.test(AMYLB40_Roche ~ AD_status, data = merged_pred_outcome_120824)
wilcox_test_AMYLB42_Roche <- wilcox.test(AMYLB42_Roche ~ AD_status, data = merged_pred_outcome_120824)
wilcox_test_ApoE4_Roche <- wilcox.test(ApoE4_Roche ~ AD_status, data = merged_pred_outcome_120824)
wilcox_test_AMYLB40_MerckGAP <- wilcox.test(AMYLB40_MerckGAP ~ AD_status, data = merged_pred_outcome_120824)
wilcox_test_AMYLB42_MerckGAP <- wilcox.test(AMYLB42_MerckGAP ~ AD_status, data = merged_pred_outcome_120824)
wilcox_test_AB4240_MerckGAP <- wilcox.test(AB4240_MerckGAP ~ AD_status, data = merged_pred_outcome_120824)

# Print results 
print(wilcox_test_TauP217_Lilly) # significant difference in TauP217_Lilly levels between the 'AD' and 'No AD' groups
print(wilcox_test_TauP181_Quant) # significant difference in TauP181_Quant levels between the 'AD' and 'No AD' groups
print(wilcox_test_AMYLB40_Roche) # NO significant difference in AB40_Roche levels between the 'AD' and 'No AD' groups
print(wilcox_test_AMYLB42_Roche) # significant difference in AB42_Roche levels between the 'AD' and 'No AD' groups
print(wilcox_test_ApoE4_Roche) # significant difference in ApoE4_Roche levels between the 'AD' and 'No AD' groups
print(wilcox_test_AMYLB40_MerckGAP) # NO significant difference in AB40_MerckGAP levels between the 'AD' and 'No AD' groups
print(wilcox_test_AMYLB42_MerckGAP) # significant difference in AB42_MerckGAP levels between the 'AD' and 'No AD' groups
print(wilcox_test_AB4240_MerckGAP) # significant difference in AB4240_MerckGAP levels between the 'AD' and 'No AD' groups

# Visualize results
# Box plots: Load necessary library
library(ggplot2)

# Create a list of variables to plot
variables <- c("TauP217_Lilly", "TauP181_Quant", "AMYLB40_Roche", "AMYLB42_Roche", "ApoE4_Roche",
               "AMYLB40_MerckGAP", "AMYLB42_MerckGAP", "AB4240_MerckGAP")

# Plot for TauP217_Lilly
p1 <- ggplot(merged_pred_outcome_120824, aes(x = factor(AD_status), y = TauP217_Lilly)) +
  geom_boxplot(aes(fill = factor(AD_status))) +
  labs(
    title = "Distribution of TauP217 (Lilly) by AD Status",
    x = "AD Status",
    y = "TauP217 (Lilly)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "pink"), labels = c("No AD", "AD")) +
  scale_x_discrete(labels = c("0" = "No AD", "1" = "AD")) + # Adding labels for AD_status
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

print(p1)

# Plot for TauP181_Quant
p2 <- ggplot(merged_pred_outcome_120824, aes(x = factor(AD_status), y = TauP181_Quant)) +
  geom_boxplot(aes(fill = factor(AD_status))) +
  labs(
    title = "Distribution of TauP181 (Quanterix) by AD Status",
    x = "AD Status",
    y = "TauP181 (Quanterix)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "orange"), labels = c("No AD", "AD")) +
  scale_x_discrete(labels = c("0" = "No AD", "1" = "AD")) + # Adding labels for AD_status
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

print(p2)

# Plot for AMYLB40_Roche
p3 <- ggplot(merged_pred_outcome_120824, aes(x = factor(AD_status), y = AMYLB40_Roche)) +
  geom_boxplot(aes(fill = factor(AD_status))) +
  labs(
    title = "Distribution of AB40 (Roche) by AD Status",
    x = "AD Status",
    y = "AB40 (Roche)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue4", "yellow2"), labels = c("No AD", "AD")) +
  scale_x_discrete(labels = c("0" = "No AD", "1" = "AD")) + # Adding labels for AD_status
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

print(p3)

# Plot for AMYLB42_Roche
p4 <- ggplot(merged_pred_outcome_120824, aes(x = factor(AD_status), y = AMYLB42_Roche)) +
  geom_boxplot(aes(fill = factor(AD_status))) +
  labs(
    title = "Distribution of AB42 (Roche) by AD Status",
    x = "AD Status",
    y = "AB42 (Roche)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("purple2", "beige"), labels = c("No AD", "AD")) +
  scale_x_discrete(labels = c("0" = "No AD", "1" = "AD")) + # Adding labels for AD_status
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

print(p4)

# Plot for ApoE4_Roche
p5 <- ggplot(merged_pred_outcome_120824, aes(x = factor(AD_status), y = ApoE4_Roche)) +
  geom_boxplot(aes(fill = factor(AD_status))) +
  labs(
    title = "Distribution of ApoE4 (Roche) by AD Status",
    x = "AD Status",
    y = "ApoE4 (Roche)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("cyan4", "pink3"), labels = c("No AD", "AD")) +
  scale_x_discrete(labels = c("0" = "No AD", "1" = "AD")) + # Adding labels for AD_status
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

print(p5)

# Plot for AMYLB40_MerckGAP
p6 <- ggplot(merged_pred_outcome_120824, aes(x = factor(AD_status), y = AMYLB40_MerckGAP)) +
  geom_boxplot(aes(fill = factor(AD_status))) +
  labs(
    title = "Distribution of AB40 (Merck GAP) by AD Status",
    x = "AD Status",
    y = "AB40 (Merck GAP)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("aquamarine2", "bisque"), labels = c("No AD", "AD")) +
  scale_x_discrete(labels = c("0" = "No AD", "1" = "AD")) + # Adding labels for AD_status
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

print(p6)

# Plot for AMYLB42_MerckGAP
p7 <- ggplot(merged_pred_outcome_120824, aes(x = factor(AD_status), y = AMYLB42_MerckGAP)) +
  geom_boxplot(aes(fill = factor(AD_status))) +
  labs(
    title = "Distribution of AB42 (Merck GAP) by AD Status",
    x = "AD Status",
    y = "AB42 (Merck GAP)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("cadetblue", "coral1"), labels = c("No AD", "AD")) +
  scale_x_discrete(labels = c("0" = "No AD", "1" = "AD")) + # Adding labels for AD_status
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

print(p7)

p8 <- ggplot(merged_pred_outcome_120824, aes(x = factor(AD_status), y = AB4240_MerckGAP)) +
  geom_boxplot(aes(fill = factor(AD_status))) +
  labs(
    title = "Distribution of AB42/40 (Merck GAP) by AD Status",
    x = "AD Status",
    y = "AB42/40 (Merck GAP)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("cornflowerblue", "pink2"), labels = c("No AD", "AD")) +
  scale_x_discrete(labels = c("0" = "No AD", "1" = "AD")) + # Adding labels for AD_status
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

print(p8)

# Violin plots: Load necessary library
library(ggplot2)

# Create a list of variables to plot
variables <- c("TauP217_Lilly", "TauP181_Quant", "AMYLB40_Roche", "AMYLB42_Roche", "ApoE4_Roche",
               "AMYLB40_MerckGAP", "AMYLB42_MerckGAP", "AB4240_MerckGAP")

new_names <- c("TauP217 (Lilly)", "TauP181 (Quanterix)", "AB40 (Roche)", "AB42 (Roche)", "ApoE4 (Roche)",
                      "AB40 (Merck GAP)", "AB42 (Merck GAP)", "AB42/40 (Merck GAP)")
  
# Create a list of p-values for each variable
p_values <- c("0.001", "0.001", "0.991", "0.001", "0.0006", "0.825", "0.007", "0.005")

# Loop through each variable to create violin plots with p-values
for (i in seq_along(variables)) {
  var <- variables[i]
  new_name <- new_names[i]
  p_val <- p_values[i]
  
  p <- ggplot(merged_pred_outcome_120824, aes(x = factor(AD_status), y = .data[[var]])) +
    geom_violin(aes(fill = factor(AD_status))) +
    labs(
      title = paste("Distribution of", new_name, "by AD Status"),
      x = "AD Status",
      y = var
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("lightblue", "pink"), labels = c("No AD", "AD")) +
    scale_x_discrete(labels = c("0" = "No AD", "1" = "AD")) + # Adding labels for AD_status
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    ) +
    annotate("text", x = 1.5, y = max(merged_pred_outcome_120824[[var]], na.rm = TRUE) * 1.05, 
             label = paste("p <", p_val), size = 5, hjust = 0.5)
  
  print(p)
}

### PCA Analysis - 13/08/2024 ###

# Select the relevant variables for PCA
merged_pred_outcome_120824 <- read_csv("Angelina_Bio_Hermes_R_code/PCA_ANOVA_T-tests/merged_pred_outcome_120824.csv")
View(merged_pred_outcome_120824)
pca_data <- merged_pred_outcome_120824[, c("TauP217_Lilly", "TauP181_Quant", "AMYLB40_Roche", 
                                           "AMYLB42_Roche", "ApoE4_Roche", "TauP181_Roche",
                                           "AMYLB40_MerckGAP", "AMYLB42_MerckGAP", "AB4240_MerckGAP")]

# Run PCA without scaling (as the variables might already be standardized)
pca_result <- prcomp(pca_data, scale = TRUE)

# Summarize the PCA result
summary(pca_result)

# View the loadings (contributions of the variables to the principal components)
pca_result$rotation

# To visualize the PCA results (scree plot)
library(ggplot2)
screeplot(pca_result, type = "lines")

# Calculate the explained variance for each component
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
explained_variance_df <- data.frame(
  Component = paste0("PC", 1:length(explained_variance)),
  Variance_Explained = explained_variance,
  Cumulative_Variance = cumsum(explained_variance)
)

# Create the scree plot using ggplot2
library(ggplot2)
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

# Get the loadings of the PCA
loadings <- pca_result$rotation

# Convert the loadings into a data frame for easier inspection
loadings_df <- as.data.frame(loadings)
View(loadings_df)

# Save loadings into a table
write.csv(loadings_df, "loadings_PCA_table.csv", row.names = FALSE)

library(ggplot2)

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

# Load necessary libraries
library(ggplot2)
library(plotly)

# Assuming `pca_result` is your PCA result
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

# Plot using plotly for interactive 3D visualization
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



