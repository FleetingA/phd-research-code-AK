
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 07/09/2024
----------------------------------------------------------------------------------
  
### More Data Analyses - Univariate Regression Models 

# Load required libraries
library(readr)

# Load final outcome file
outcome_binary_0sand1s <- read_csv("Angelina_Bio_Hermes_R_code/Outcome_for_Analysis/outcome_binary_0sand1s.csv")
View(outcome_binary_0sand1s)

### Merck GAP, Quanterix and Lilly Data

# Load final predictor file for 1st analysis - Merck GAP, Quanterix and Lilly
merged_first_analysis_noNAs290724 <- read_csv("Angelina_Bio_Hermes_R_code/First_Analysis_250724/merged_first_analysis_noNAs290724.csv")
View(merged_first_analysis_noNAs290724)

# Inspect predictors once again
hist(merged_first_analysis_noNAs290724$AB40) # looks normally distributed
hist(merged_first_analysis_noNAs290724$AB42) # looks normally distributed

hist(merged_first_analysis_noNAs290724$AB4240) # does not look normally distributed
hist(merged_first_analysis_noNAs290724$AB4240_logtr) # closer to normally distributed

hist(merged_first_analysis_noNAs290724$PTAU181) # does not look normally distributed
hist(merged_first_analysis_noNAs290724$PTAU181_logtr) # closer to normally distributed
hist(merged_first_analysis_noNAs290724$PTAU217) # does not look normally distributed
hist(merged_first_analysis_noNAs290724$PTAU217_logtr) # closer to normally distributed

# Fit a multivariate logistic regression model (vars log transformed as required but not standardized)
logistic_r_model <- glm(AD_status ~ AB40 + AB42 + AB4240_logtr + PTAU181_logtr + PTAU217_logtr, 
                        data = merged_first_analysis_noNAs290724, 
                        family = "binomial")

# Summary of the model
summary(logistic_r_model)

# Standardise all variables before running again
library(dplyr)
merged_first_analysis_noNAs290724 <- merged_first_analysis_noNAs290724 %>%
  mutate(
    AB40_std = scale(AB40),
    AB42_std = scale(AB42),
    AB4240_logtr_std = scale(AB4240_logtr),
    PTAU181_logtr_std = scale(PTAU181_logtr),
    PTAU217_logtr_std = scale(PTAU217_logtr)
  )

# Fit another logistic regression model (after scaling)
logistic_r_model2 <- glm(AD_status ~ AB40_std + AB42_std + AB4240_logtr_std + PTAU181_logtr_std + PTAU217_logtr_std, 
                         data = merged_first_analysis_noNAs290724, 
                         family = "binomial")

# Summary of the model
summary(logistic_r_model2)

## UNIVARIATE regression models 

# Logistic regression for AB40_std
model_AB40 <- glm(AD_status ~ AB40_std, family = "binomial", data = merged_first_analysis_noNAs290724)
summary(model_AB40)
# Get odds ratios for AB40_std
exp(coef(model_AB40))

# Logistic regression for AB42_std
model_AB42 <- glm(AD_status ~ AB42_std, family = "binomial", data = merged_first_analysis_noNAs290724)
summary(model_AB42)
# Get odds ratios for AB42_std
exp(coef(model_AB42))

# Logistic regression for AB4240_logtr (unstandardized)
model_AB4240 <- glm(AD_status ~ AB4240_logtr, family = "binomial", data = merged_first_analysis_noNAs290724)
summary(model_AB4240)
# Get odds ratios for AB4240_std
exp(coef(model_AB4240))

# Standardised
# Logistic regression for AB4240_logtr_std
model_AB4240v2 <- glm(AD_status ~ AB4240_logtr_std, family = "binomial", data = merged_first_analysis_noNAs290724)
summary(model_AB4240v2)
# Get odds ratios for AB4240_std
exp(coef(model_AB4240v2))

# Logistic regression for PTAU181_logtr_std
model_PTAU181 <- glm(AD_status ~ PTAU181_logtr_std, family = "binomial", data = merged_first_analysis_noNAs290724)
summary(model_PTAU181)
# Get odds ratios for PTAU181_logtr_std
exp(coef(model_PTAU181))

# Logistic regression for PTAU217_logtr_std
model_PTAU217 <- glm(AD_status ~ PTAU217_logtr_std, family = "binomial", data = merged_first_analysis_noNAs290724)
summary(model_PTAU217)
# Get odds ratios for PTAU217_logtr_std
exp(coef(model_PTAU217))

### Roche Diagnostics and Lilly Data

# Load final predictor file for 2nd analysis - Roche Diagnostics and Lilly
merged_complete_second_analysis290724 <- read_csv("Angelina_Bio_Hermes_R_code/Final_Predictors_for_Analysis/merged_complete_second_analysis290724.csv",
                                                  show_col_types = FALSE)
View(merged_complete_second_analysis290724)

# List column names in data frame
colnames(merged_complete_second_analysis290724)

# Fit a multivariate logistic regression model (vars log transformed as required but not standardized)
logistic_r_model_2 <- glm(AD_status ~ AMYLB40 + AMYLB42 + AB4240_ratio + ApoE4_logtr + pTau181_logtr + PTAU217_logtr, 
                              data = merged_complete_second_analysis290724, 
                              family = "binomial")

# Summary of the model
summary(logistic_r_model_2)

# Calculating Pseudo R-squared
library(pscl)
pseudo_r_squared_2 <- pR2(logistic_r_model_2)
print(pseudo_r_squared_2)

# Generate ROC curve
library(pROC)
predicted_probs <- predict(logistic_r_model_2, type = "response")
roc_curve <- roc(merged_complete_second_analysis290724$AD_status ~ predicted_probs)
plot(roc_curve, main = "ROC Curve for Model 2 - Roche Diagnostics and Lilly MSD Data")

# Scale predictors
merged_complete_second_analysis290724 <- merged_complete_second_analysis290724 %>%
  mutate(
    AMYLB40_std = scale(AMYLB40_pg_mL),
    AMYLB42_std = scale(AMYLB42),
    ApoE4_logtr_std = scale(ApoE4_logtr),
    TAU181P_logtr_std = scale(pTau181_logtr),
    TAU217P_logtr_std = scale(PTAU217_logtr),
    AB4240_ratio_std = scale(AB4240_ratio)
  )

# Fit another logistic regression model (after scaling)
logistic_r_model_2_scaled <- glm(AD_status ~ AMYLB40_std + AMYLB42_std + AB4240_ratio_std + 
                                   ApoE4_logtr_std + TAU181P_logtr_std + TAU217P_logtr_std, 
                          data = merged_complete_second_analysis290724, 
                          family = "binomial")

# Summary of the model
summary(logistic_r_model_2_scaled)

# Calculating Pseudo R-squared
library(pscl)
pseudo_r_squared_2_scaled <- pR2(logistic_r_model_2_scaled)
print(pseudo_r_squared_2_scaled)

# Generate ROC curve
library(pROC)
predicted_probs2 <- predict(logistic_r_model_2_scaled, type = "response")
roc_curve <- roc(merged_complete_second_analysis290724$AD_status ~ predicted_probs2)
plot(roc_curve, main = "ROC Curve for Model 2 - Roche Diagnostics and Lilly MSD Data")

# UNIVARIATE regression models 

# Logistic regression for AB40_std
model_AB40_2 <- glm(AD_status ~ AMYLB40_std, family = "binomial", data = merged_complete_second_analysis290724)
summary(model_AB40_2)
# Get odds ratios for AB40_std
exp(coef(model_AB40_2))

# Logistic regression for AB42_std
model_AB42_2 <- glm(AD_status ~ AMYLB42_std, family = "binomial", data = merged_complete_second_analysis290724)
summary(model_AB42_2)
# Get odds ratios for AB42_std
exp(coef(model_AB42_2))

# Logistic regression for AB4240_logtr
model_AB4240_2 <- glm(AD_status ~ AB4240_ratio_std, family = "binomial", data = merged_complete_second_analysis290724)
summary(model_AB4240_2)
# Get odds ratios for AB4240_std
exp(coef(model_AB4240_2))

# Logistic regression for ApoE4_logtr
model_ApoE4_2 <- glm(AD_status ~ ApoE4_logtr_std, family = "binomial", data = merged_complete_second_analysis290724)
summary(model_ApoE4_2)
# Get odds ratios for AB4240_std
exp(coef(model_ApoE4_2))

# Logistic regression for PTAU181_logtr_std
model_PTAU181_2 <- glm(AD_status ~ TAU181P_logtr_std, family = "binomial", data = merged_complete_second_analysis290724)
summary(model_PTAU181_2)
# Get odds ratios for PTAU181_logtr_std
exp(coef(model_PTAU181_2))

# Logistic regression for PTAU217_logtr_std
model_PTAU217_2 <- glm(AD_status ~ TAU217P_logtr_std, family = "binomial", data = merged_complete_second_analysis290724)
summary(model_PTAU217_2)
# Get odds ratios for PTAU217_logtr_std
exp(coef(model_PTAU217_2))