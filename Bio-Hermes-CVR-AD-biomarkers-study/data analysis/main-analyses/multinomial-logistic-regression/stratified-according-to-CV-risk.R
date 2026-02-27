
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 
  
# Date: 07/03/2025
----------------------------------------------------------------------------------
  
### More Analyses: Modelling only CV Risk as a predictor
  
## Make sure all necessary datasets are loaded before proceeding.

# Load libraries
library(readr)
library(psych)
library(ggplot2)
library(dplyr)

# CVD only 
mmr_cvd_glm <- glm(AD_status ~ ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_cvd_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_cvd_glm))

# Print the odds ratios
print(odds_ratios)

pseudo_r2 <- pR2(mmr_cvd_glm)
print(pseudo_r2)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_ptau217, type = 'response')

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_ptau217))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau217 Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

install.packages("randomForest")
library(randomForest)

set.seed(42)  # For reproducibility

rf_model <- randomForest(
  AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc,  # Add more predictors if needed
  data = dataset,
  ntree = 500,     # Number of trees
  mtry = 2,        # Number of predictors considered at each split
  importance = TRUE
)

print(rf_model)

# Get predicted probabilities for CI=1
rf_probs <- predict(rf_model, type = "prob")[,2]

# Compute ROC Curve & AUC
rf_roc <- roc(dataset$AD_status, rf_probs)
plot(rf_roc, col = "darkgreen", main = "Random Forest ROC Curve - AB42/AB40 Main Model")
auc_rf <- auc(rf_roc)
print(paste("Random Forest AUC:", round(auc_rf, 3)))

lr_probs <- predict(mmr_main_glm, type = "response")
lr_roc <- roc(dataset$AD_status, lr_probs)

# Plot both ROC curves
plot(lr_roc, col = "blue", main = "ROC Curve Comparison", lwd = 2)
lines(rf_roc, col = "darkgreen", lwd = 1)

legend("bottomright", legend = c("AB42/AB40 Logistic", "AB42/AB40 Random Forest"),
       col = c("blue", "darkgreen"), lwd = 1)

# Print AUC values
print(paste("Logistic Regression AUC:", round(auc(lr_roc), 3)))
print(paste("Random Forest AUC:", round(auc(rf_roc), 3)))

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_cvd_glm, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_cvd_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for CVD Risk Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

install.packages("randomForest")
library(randomForest)

set.seed(42)  # For reproducibility

rf_model <- randomForest(
  AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc,  # Add more predictors if needed
  data = dataset,
  ntree = 500,     # Number of trees
  mtry = 2,        # Number of predictors considered at each split
  importance = TRUE
)

print(rf_model)

# Get predicted probabilities for CI=1
rf_probs <- predict(rf_model, type = "prob")[,2]

# Compute ROC Curve & AUC
rf_roc <- roc(dataset$AD_status, rf_probs)
plot(rf_roc, col = "darkgreen", main = "Random Forest ROC Curve - AB42/AB40 Main Model")
auc_rf <- auc(rf_roc)
print(paste("Random Forest AUC:", round(auc_rf, 3)))

lr_probs <- predict(mmr_main_glm, type = "response")
lr_roc <- roc(dataset$AD_status, lr_probs)

# Plot both ROC curves
plot(lr_roc, col = "blue", main = "ROC Curve Comparison", lwd = 2)
lines(rf_roc, col = "darkgreen", lwd = 1)

legend("bottomright", legend = c("AB42/AB40 Logistic", "AB42/AB40 Random Forest"),
       col = c("blue", "darkgreen"), lwd = 1)

# Print AUC values
print(paste("Logistic Regression AUC:", round(auc(lr_roc), 3)))
print(paste("Random Forest AUC:", round(auc(rf_roc), 3)))

# Define a vector of USUBJIDs with prevalent CVD
cvd_usubjids <- c("BIO-HERMES-00101-033", "BIO-HERMES-00101-040", "BIO-HERMES-00101-045", 
                  "BIO-HERMES-00101-045", "BIO-HERMES-00101-046", "BIO-HERMES-00101-046", 
                  "BIO-HERMES-00101-046", "BIO-HERMES-00101-053", "BIO-HERMES-00101-061",
                  "BIO-HERMES-00101-061", "BIO-HERMES-00101-078", "BIO-HERMES-00101-080", 
                  "BIO-HERMES-00101-080", "BIO-HERMES-00101-086", "BIO-HERMES-00101-090", 
                  "BIO-HERMES-00101-102", "BIO-HERMES-00101-102", "BIO-HERMES-00101-109", 
                  "BIO-HERMES-00102-003", "BIO-HERMES-00102-009", "BIO-HERMES-00102-012", 
                  "BIO-HERMES-00102-032", "BIO-HERMES-00102-061", "BIO-HERMES-00102-063",
                  "BIO-HERMES-00102-071", "BIO-HERMES-00102-071", "BIO-HERMES-00102-074",
                  "BIO-HERMES-00102-075", "BIO-HERMES-00102-075", "BIO-HERMES-00102-075",
                  "BIO-HERMES-00102-081", "BIO-HERMES-00102-082", "BIO-HERMES-00102-083",
                  "BIO-HERMES-00102-083", "BIO-HERMES-00102-083", "BIO-HERMES-00102-092",
                  "BIO-HERMES-00103-002", "BIO-HERMES-00103-002", "BIO-HERMES-00103-002",
                  "BIO-HERMES-00103-005", "BIO-HERMES-00103-028", "BIO-HERMES-00103-047",
                  "BIO-HERMES-00104-004", "BIO-HERMES-00104-018", "BIO-HERMES-00105-016",
                  "BIO-HERMES-00105-020", "BIO-HERMES-00105-040", "BIO-HERMES-00105-048",
                  "BIO-HERMES-00105-056", "BIO-HERMES-00105-056", "BIO-HERMES-00105-087",
                  "BIO-HERMES-00105-090", "BIO-HERMES-00105-097", "BIO-HERMES-00105-097",
                  "BIO-HERMES-00105-097", "BIO-HERMES-00106-002", "BIO-HERMES-00106-010",
                  "BIO-HERMES-00106-011", "BIO-HERMES-00106-021", "BIO-HERMES-00106-026",
                  "BIO-HERMES-00106-026", "BIO-HERMES-00106-029", "BIO-HERMES-00106-037",
                  "BIO-HERMES-00106-046", "BIO-HERMES-00106-046", "BIO-HERMES-00106-046",
                  "BIO-HERMES-00106-048", "BIO-HERMES-00106-050", "BIO-HERMES-00106-050",
                  "BIO-HERMES-00106-059", "BIO-HERMES-00106-065", "BIO-HERMES-00106-065",
                  "BIO-HERMES-00106-069", "BIO-HERMES-00106-072", "BIO-HERMES-00106-076",
                  "BIO-HERMES-00106-076", "BIO-HERMES-00106-076", "BIO-HERMES-00106-078",
                  "BIO-HERMES-00106-079", "BIO-HERMES-00106-083", "BIO-HERMES-00106-084",
                  "BIO-HERMES-00106-087", "BIO-HERMES-00106-088", "BIO-HERMES-00106-094",
                  "BIO-HERMES-00107-029", "BIO-HERMES-00107-037", "BIO-HERMES-00107-040",
                  "BIO-HERMES-00107-043", "BIO-HERMES-00107-043", "BIO-HERMES-00108-001",
                  "BIO-HERMES-00108-007", "BIO-HERMES-00108-007", "BIO-HERMES-00108-007",
                  "BIO-HERMES-00108-018", "BIO-HERMES-00109-006", "BIO-HERMES-00109-011",
                  "BIO-HERMES-00109-030", "BIO-HERMES-00109-032", "BIO-HERMES-00109-032",
                  "BIO-HERMES-00109-036", "BIO-HERMES-00109-038", "BIO-HERMES-00109-049",
                  "BIO-HERMES-00109-051", "BIO-HERMES-00109-069", "BIO-HERMES-00109-070",
                  "BIO-HERMES-00109-073", "BIO-HERMES-00109-077", "BIO-HERMES-00109-079",
                  "BIO-HERMES-00109-079", "BIO-HERMES-00109-108", "BIO-HERMES-00109-144",
                  "BIO-HERMES-00109-162", "BIO-HERMES-00109-165", "BIO-HERMES-00110-021",
                  "BIO-HERMES-00110-047", "BIO-HERMES-00110-054", "BIO-HERMES-00111-010",
                  "BIO-HERMES-00111-026", "BIO-HERMES-00111-027", "BIO-HERMES-00111-033",
                  "BIO-HERMES-00111-040", "BIO-HERMES-00112-025", "BIO-HERMES-00112-038",
                  "BIO-HERMES-00112-045", "BIO-HERMES-00113-001", "BIO-HERMES-00113-004",
                  "BIO-HERMES-00113-006", "BIO-HERMES-00113-007", "BIO-HERMES-00114-002")

# Create a CVDweight variable
dataset$CVDweight <- ifelse(dataset$USUBJID %in% cvd_usubjids, 1, 0)

# Display a summary of the new variable
table(dataset$CVDweight) # 0=664; 1=81
View(dataset)

------------------------------------------------------
### Stratification according to CV Risk
  
dataset_CVD <- subset(dataset_CVD_analysis, CVDweight == 1)
dataset_nonCVD <- subset(dataset_CVD_analysis, CVDweight == 0)

## AB42/AB40
# CVD
mmr_glm_CVD <- glm(AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                   data = dataset_CVD, 
                   family = binomial)
summary(mmr_glm_CVD)

# Non CVD
mmr_glm_nonCVD <- glm(AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                      data = dataset_nonCVD, 
                      family = binomial)
summary(mmr_glm_nonCVD)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_glm_CVD))
odds_ratios <- exp(coef(mmr_glm_nonCVD))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_glm_CVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_CVD)
print(vif_values)

pseudo_r2 <- pR2(mmr_glm_nonCVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_nonCVD)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Predict probabilities from the model
pred_probs <- predict(mmr_glm_CVD,
                      type = "response")
pred_probs <- predict(mmr_glm_nonCVD,
                      type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset_CVD$AD_status))

# Extract Sensitivity & Specificity
library(caret)
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Load necessary library
library(pROC)

# Generate ROC curve for the model
roc_curve <- roc(dataset_CVD$AD_status, fitted(mmr_glm_CVD))
roc_curve <- roc(dataset_nonCVD$AD_status, fitted(mmr_glm_nonCVD))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for AB42/AB40 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

## P-tau181
# CVD
mmr_glm_CVD <- glm(AD_status ~ pTau181_logtr_c + 
                     ASCVD_cont_score_sqrtc, 
                   data = dataset_CVD, 
                   family = binomial)
summary(mmr_glm_CVD)

# Non CVD
mmr_glm_nonCVD <- glm(AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                      data = dataset_nonCVD, 
                      family = binomial)
summary(mmr_glm_nonCVD)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_glm_CVD))
odds_ratios <- exp(coef(mmr_glm_nonCVD))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_glm_CVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_CVD)
print(vif_values)

pseudo_r2 <- pR2(mmr_glm_nonCVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_nonCVD)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Predict probabilities from the model
pred_probs <- predict(mmr_glm_CVD,
                      type = "response")
pred_probs <- predict(mmr_glm_nonCVD,
                      type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset_nonCVD$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Load necessary library
library(pROC)

# Generate ROC curve for the model
roc_curve <- roc(dataset_CVD$AD_status, fitted(mmr_glm_CVD))
roc_curve <- roc(dataset_nonCVD$AD_status, fitted(mmr_glm_nonCVD))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau181 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

## P-tau217
# CVD
mmr_glm_CVD <- glm(AD_status ~ PTAU217_logtr_c  + 
                     ASCVD_cont_score_sqrtc, 
                   data = dataset_CVD, 
                   family = binomial)
summary(mmr_glm_CVD)

# Non CVD
mmr_glm_nonCVD <- glm(AD_status ~ PTAU217_logtr_c  + ASCVD_cont_score_sqrtc, 
                      data = dataset_nonCVD, 
                      family = binomial)
summary(mmr_glm_nonCVD)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_glm_CVD))
odds_ratios <- exp(coef(mmr_glm_nonCVD))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_glm_CVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_CVD)
print(vif_values)

pseudo_r2 <- pR2(mmr_glm_nonCVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_nonCVD)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Predict probabilities from the model
pred_probs <- predict(mmr_glm_CVD,
                      type = "response")
pred_probs <- predict(mmr_glm_nonCVD,
                      type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset_nonCVD$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset_CVD$AD_status, fitted(mmr_glm_CVD))
roc_curve <- roc(dataset_nonCVD$AD_status, fitted(mmr_glm_nonCVD))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau217 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

## ApoE4
# CVD
mmr_glm_CVD <- glm(AD_status ~ ApoE4_sqrt_c + 
                     ASCVD_cont_score_sqrtc, 
                   data = dataset_CVD, 
                   family = binomial)
summary(mmr_glm_CVD)

# Non CVD
mmr_glm_nonCVD <- glm(AD_status ~ ApoE4_sqrt_c + ASCVD_cont_score_sqrtc, 
                      data = dataset_nonCVD, 
                      family = binomial)
summary(mmr_glm_nonCVD)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_glm_CVD))
odds_ratios <- exp(coef(mmr_glm_nonCVD))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_glm_CVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_CVD)
print(vif_values)

pseudo_r2 <- pR2(mmr_glm_nonCVD)
print(pseudo_r2)
vif_values <- vif(mmr_glm_nonCVD)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Predict probabilities from the model
pred_probs <- predict(mmr_glm_CVD,
                      type = "response")
pred_probs <- predict(mmr_glm_nonCVD,
                      type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset_nonCVD$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Generate ROC curve for the model
roc_curve <- roc(dataset_CVD$AD_status, fitted(mmr_glm_CVD))
roc_curve <- roc(dataset_nonCVD$AD_status, fitted(mmr_glm_nonCVD))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for ApoE4 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# Save dataset with new additions
write.csv(dataset, "dataset_CVD_analysis.csv", row.names = FALSE)
dataset_CVD_analysis <- read_csv("Angelina_Bio_Hermes_R_code/Rerunning_Analyses_ASCVD_13122024/dataset_CVD_analysis.csv")
View(dataset_CVD_analysis)
