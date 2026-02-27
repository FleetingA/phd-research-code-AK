
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 29/07/2024
----------------------------------------------------------------------------------
  
### First Data Analysis - Merck GAP, Quanterix and Lilly Data 

# Load libraries 
library(readr)
library(dplyr)
library(ggplot2)

# Load data
outcome_selected_binary300724 <- read_csv("Angelina_Bio_Hermes_R_code/Outcome_for_Analysis/outcome_selected_binary300724.csv")
View(outcome_selected_binary300724)

# As of 07/09/2024, file "outcome_selected_binary300724" renamed to "outcome_binary_0sand1s.csv". 
# Replace to correct name if re-running code in this file!
# Contents of the two files are the same!

predictors_merged_MerckGAP_Quant_Lilly <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/predictors_merged_MerckGAP_Quant_Lilly.csv")
View(predictors_merged_MerckGAP_Quant_Lilly)

# Merge the two data frames by USUBJID
merged_first_analysis290724 <- merge(outcome_selected_binary300724, predictors_merged_MerckGAP_Quant_Lilly, by = "USUBJID", all = FALSE) # experimented with TRUE and FALSE
# Check the merged data
View(merged_first_analysis290724)

# Save new data frame
write.csv(merged_first_analysis290724, "merged_first_analysis290724.csv", row.names = FALSE)
table(merged_first_analysis290724$USUBJID == "NA") # no missing values

# Count the total number of NA values in the data frame
total_na <- sum(is.na(merged_first_analysis290724))
# Print the result
print(total_na)

# Remove NAs before running the analysis
merged_first_analysis_noNAs290724 <- na.omit(merged_first_analysis290724)
View(merged_first_analysis_noNAs290724)

# Save file
write.csv(merged_first_analysis_noNAs290724, "merged_first_analysis_noNAs290724.csv", row.names = FALSE) # merged data for first glm
View(merged_first_analysis_noNAs290724)

## ANALYSIS:

# Prepare data for analysis

# Standardise all variables
merged_first_analysis_noNAs290724 <- merged_first_analysis_noNAs290724 %>%
  mutate(
    AB40_std = scale(AB40),
    AB42_std = scale(AB42),
    AB4240_logtr_std = scale(AB4240_logtr),
    PTAU181_logtr_std = scale(PTAU181_logtr),
    PTAU217_logtr_std = scale(PTAU217_logtr)
  )

# Check that the outcome variable is a factor 
typeof(merged_first_analysis_noNAs290724$AD_status)
unique(merged_first_analysis_noNAs290724$AD_status) # only values 0 and 1

# Run the logistic regression model using the standardised variables
logistic_r_model <- glm(AD_status ~ AB40_std + AB42_std + AB4240_logtr_std + PTAU181_logtr_std + PTAU217_logtr_std, 
                        data = merged_first_analysis_noNAs290724, 
                        family = "binomial")

# Summary of the model
summary(logistic_r_model)

# Obtain coefficients 
library(MASS) # additional library needed
coef(logistic_r_model)

# Calculating Pseudo R-squared
library(pscl)
pseudo_r_squared <- pR2(logistic_r_model)
print(pseudo_r_squared)

# Calculate AIC and BIC
aic_value <- AIC(logistic_r_model)
print(paste("AIC:", aic_value))

bic_value <- BIC(logistic_r_model)
print(paste("BIC:", bic_value))

# Create a data frame for the coefficients
coefficients <- summary(logistic_r_model)$coefficients
coef_df <- data.frame(
  Predictor = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  StdError = coefficients[, "Std. Error"],
  ZValue = coefficients[, "z value"],
  PValue = coefficients[, "Pr(>|z|)"]
)

# Calculate confidence intervals
coef_df$LowerCI <- coef_df$Estimate - 1.96 * coef_df$StdError
coef_df$UpperCI <- coef_df$Estimate + 1.96 * coef_df$StdError

# Plot
ggplot(coef_df, aes(x = Predictor, y = Estimate)) +
  geom_point(color = "darkblue") +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, color = "firebrick") +
  theme_minimal() +
  labs(title = "Model Coefficients with 95% Confidence Intervals",
       x = "Predictor",
       y = "Estimate") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Center the title and change its size
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12)
  )

# Load necessary library to generate predicted probabilities
library(pROC) # additional library needed

# Predict probabilities
predicted_probs <- predict(logistic_r_model, type = "response")

# Generate ROC curve
roc_curve <- roc(merged_first_analysis_noNAs290724$AD_status ~ predicted_probs)
plot(roc_curve, main = "ROC Curve for Model 1 - Merck GAP, Quanterix and Lilly MSD Data")

