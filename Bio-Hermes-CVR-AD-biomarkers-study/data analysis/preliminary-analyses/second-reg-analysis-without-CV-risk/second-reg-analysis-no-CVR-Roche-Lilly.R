
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 29/07/2024
----------------------------------------------------------------------------------
  
### Second Data Analysis - Roche Diagnostics and Lilly Data

# Load libraries and required data
library(readr)
library(dplyr)
library(ggplot2)

# Loading datasets to merge
RocheBHPredictors_to_merge290724 <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/290724_RocheBHPredictors_to_merge.csv")
View(RocheBHPredictors_to_merge290724)

table(RocheBHPredictors_to_merge290724$USUBJID == "NA") # no missing values in this data frame

LillyPredictors_to_merge290724 <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/290724_final_LillyPredictors_merge.csv")
View(LillyPredictors_to_merge290724)
table(LillyPredictors_to_merge290724$USUBJID == "NA") # no missing values in this data frame

# Save new data frames to merge
write.csv(RocheBHPredictors_to_merge290724, "RocheBH_to_merge290724.csv", row.names = FALSE)
write.csv(LillyPredictors_to_merge290724, "Lilly_to_merge290724.csv", row.names = FALSE)

# Merge two files to obtain second file of predictors merged
predictors_merged_2_290724 <- merge(RocheBHPredictors_to_merge290724, LillyPredictors_to_merge290724, by = "USUBJID", all = FALSE)
View(predictors_merged_2_290724)

# Save this file
write.csv(predictors_merged_2_290724, "predictors_merged_2_290724.csv", row.names = FALSE)

# Outcome file to merge new predictors with
outcome_selected_binary300724 <- read_csv("Angelina_Bio_Hermes_R_code/Outcome_for_Analysis/outcome_selected_binary300724.csv")
View(outcome_selected_binary300724)

# Merge data frames by USUBJID
merged_complete_second_analysis290724 <- merge(outcome_selected_binary300724, predictors_merged_2_290724, by = "USUBJID", all = FALSE) 
# Check the merged data
View(merged_complete_second_analysis290724)

# Save data frame for second analysis
write.csv(merged_complete_second_analysis290724, "merged_complete_second_analysis290724.csv", row.names = FALSE)
table(merged_complete_second_analysis290724$USUBJID == "NA") # 808 final entries

# Check if all values in the column 'USUBJID' are unique
all_unique <- !any(duplicated(merged_complete_second_analysis290724$USUBJID))
print(all_unique) # all values unique indeed
# Count the number of unique values
num_unique <- n_distinct(merged_complete_second_analysis290724$USUBJID)
print(num_unique) 

# Summary of variables
summary(merged_complete_second_analysis290724$AMYLB40)
summary(merged_complete_second_analysis290724$AMYLB42)
summary(merged_complete_second_analysis290724$ApoE4)
summary(merged_complete_second_analysis290724$ApoE4_logtr)
summary(merged_complete_second_analysis290724$pTau181)
summary(merged_complete_second_analysis290724$pTau181_logtr)
summary(merged_complete_second_analysis290724$PTAU217)
summary(merged_complete_second_analysis290724$PTAU217_logtr)

# Have to standardise the units of measurement of AB40 and AB42 before proceeding. 
# Convert AB40 to pg/mL from ng/mL.

# Convert AMYLB40 from ng/mL to pg/mL
merged_complete_second_analysis290724 <- merged_complete_second_analysis290724 %>%
  mutate(AMYLB40_pg_mL = AMYLB40 * 1000)

# Create a new variable with the ratio of AB42 to AB40 as this is currently missing
merged_complete_second_analysis290724$AB4240_ratio <- merged_complete_second_analysis290724$AMYLB42/merged_complete_second_analysis290724$AMYLB40_pg_mL
hist(merged_complete_second_analysis290724$AB4240_ratio)

# Standardise the predictors before running glm
merged_complete_second_analysis290724 <- merged_complete_second_analysis290724 %>%
  mutate(
    AMYLB40_std = scale(AMYLB40_pg_mL),
    AMYLB42_std = scale(AMYLB42),
    ApoE4_std = scale(ApoE4_logtr),
    TAU181P_std = scale(pTau181_logtr),
    TAU217P_std = scale(PTAU217_logtr),
    AB4240_ratio_std = scale(AB4240_ratio)
  )

# View the standardised data
View(merged_complete_second_analysis290724)

# Store column names in a variable
column_names <- colnames(merged_complete_second_analysis290724)
# Print column names
print(column_names)

# Save new data frame 
write.csv(merged_complete_second_analysis290724, "merged_complete_second_analysis290724.csv", row.names = FALSE)

## ANALYSIS:

# Prepare data for analysis

# Check that the outcome variable is a factor 
typeof(merged_complete_second_analysis290724$AD_status)
unique(merged_complete_second_analysis290724$AD_status) # only values 0 and 1

# Run the logistic regression model using the standardised variables
logistic_r_model <- glm(AD_status ~ AMYLB40_std + AMYLB42_std + AB4240_ratio_std + TAU181P_std + TAU217P_std, 
                        data = merged_complete_second_analysis290724, 
                        family = "binomial")

# Summary of the model
summary(logistic_r_model)

# Obtain coefficients 
library(MASS)
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
library(pROC)

# Predict probabilities
predicted_probs <- predict(logistic_r_model, type = "response")

# Generate ROC curve
roc_curve <- roc(merged_complete_second_analysis290724$AD_status ~ predicted_probs)
plot(roc_curve, main = "ROC Curve for Model 2 - Roche Diagnostics and Lilly MSD Data")

