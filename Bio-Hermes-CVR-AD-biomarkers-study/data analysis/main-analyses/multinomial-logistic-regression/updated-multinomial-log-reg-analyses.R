
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 
  
# Date: 07/08/2025
----------------------------------------------------------------------------------

----------------------------------------------------
### Updated Moderated Multiple Regression Analyses
----------------------------------------------------
  
# First, load libraries
library(car)       # For VIF
library(pscl)      # For McFadden's R2
library(DescTools) # For additional R2 metrics
library(sjstats)   # Another source for pseudo R2
library(pROC)      # For ROC curves
library(caret)     # For confusion matrix & performance metrics
install.packages("lmtest")
library(lmtest)    # For likelihood ratio tests
library(DescTools)

## Make sure all datasets are loaded before proceeding.

# Convert categorical variables to factors 
Complete_Dataset_Roche_Lilly_13122024$AD_status <- factor(Complete_Dataset_Roche_Lilly_13122024$AD_status, levels = c(0, 1, 2))
Complete_Dataset_Roche_Lilly_13122024$ASCVD_classification_numeric <- factor(Complete_Dataset_Roche_Lilly_13122024$ASCVD_classification_numeric, levels = c(1, 2, 3, 4))
View(Complete_Dataset_Roche_Lilly_13122024)

# For simplicity
dataset <- Complete_Dataset_Roche_Lilly_13122024


### Re-running analyses in response to reviewer comments from the Journal of the American Heart Association

# Load data again to re-run analyses
dataset_CVD_analysis <- read_csv("Angelina_Bio_Hermes_R_code/Rerunning_Analyses_ASCVD_13122024/dataset_CVD_analysis.csv")
View(dataset_CVD_analysis)
nrow(dataset_CVD_analysis) # 745, as it should be
table(dataset_CVD_analysis$AD_status) # 301 and 444, as it should be

# Check variable distributions again just in case
hist(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, main = "ASCVD Score", xlab = "Value") # mostly normal
hist(dataset_CVD_analysis$AB4240_ratio_c, main = "AB4240", xlab = "Value") # mostly normal
hist(dataset_CVD_analysis$pTau181_logtr_c, main = "pTau181", xlab = "Value") # mostly normal
hist(dataset_CVD_analysis$PTAU217_logtr_c, main = "PTAU217", xlab = "Value") # mostly normal
hist(dataset_CVD_analysis$ApoE4_sqrt_c, main = "ApoE4", xlab = "Value") # pretty skewed

# Run Pearson correlations between ASCVD and each biomarker to check collinearity
cor.test(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, dataset_CVD_analysis$AB4240_ratio_c)
cor.test(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, dataset_CVD_analysis$pTau181_logtr_c)
cor.test(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, dataset_CVD_analysis$PTAU217_logtr_c)
# Spearman correlation for ApoE4
cor.test(dataset_CVD_analysis$ASCVD_cont_score_sqrtc, dataset_CVD_analysis$ApoE4_sqrt_c, method = "spearman")

# Not too bad, all are reasonable.

# Generate correlation matrix 
vars_for_matrix <- dataset_CVD_analysis[, c(
  "ASCVD_cont_score_sqrtc",
  "AB4240_ratio_c",
  "pTau181_logtr_c",
  "PTAU217_logtr_c",
  "ApoE4_sqrt_c"
)]

cor_matrix <- cor(vars_for_matrix, use = "complete.obs")
round(cor_matrix, 3)

install.packages("Hmisc")  # if not already installed
library(Hmisc)

cor_results <- rcorr(as.matrix(vars_for_matrix))  # returns r and p
cor_results$r     # correlation coefficients
cor_results$P     # p-values

# Visualise correlations 
install.packages("corrplot")  # if not already installed
library(corrplot)

# Create cleaner labels
nice_labels <- c(
  "ASCVD Risk Score",
  "AB42/AB40",
  "p-tau181",
  "p-tau217",
  "ApoE4"
)

# Rename rows and columns
colnames(cor_results$r) <- nice_labels
rownames(cor_results$r) <- nice_labels
colnames(cor_results$P) <- nice_labels
rownames(cor_results$P) <- nice_labels

# Now re-run corrplot with these labels
corrplot(cor_results$r,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,
         p.mat = cor_results$P,
         sig.level = 0.05,
         insig = "blank")

--------------------
### TO MAIN MODELS 
  
dataset <- dataset_CVD_analysis

## AB42/40 Main Effects Model 
mmr_main_glm <- glm(AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                    data = dataset, family = binomial)

# Weighted logistic regression model
mmr_main_glm_weighted <- glm(AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                             data = dataset, 
                             family = binomial, 
                             weights = CVDweight)

# Display results
summary(mmr_main_glm_weighted)

# View the summary of the model
summary(mmr_main_glm)

# Tease out individual contribution 
mmr_component_glm <- glm(AD_status ~ AB4240_ratio_c + AGE + Gender + Ethnicity_RiskScorescvd 
                         + Total_cholesterol_mmolL + HDL_Cholesterol_mmolL
                         + Systolic_Blood_Pressure + ongoing_med_for_HBP
                         + Current_smoking_binary + Diabetes_status, 
                         data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)

summary(mmr_component_glm)
vif(mmr_component_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main_glm))

# Print the odds ratios
print(odds_ratios)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main_glm_weighted))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main_glm_weighted)
print(pseudo_r2)
vif_values <- vif(mmr_main_glm_weighted)
vif_values <- vif(mmr_main_glm)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_main_glm_weighted, type = "response")

# Convert probabilities to binary predictions (threshold 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Generate confusion matrix

conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(dataset$AD_status))

# Extract Sensitivity & Specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Load necessary library
# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_main_glm_weighted))
# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for AB42/AB40 Main Model: Weighted by CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# Fit the null model (intercept-only model)
null_model <- glm(AD_status ~ 1, 
                  data = dataset, 
                  family = binomial, 
                  weights = dataset$CVDweight)

# Compute McFadden's pseudo R-squared
pseudo_r2_mcfadden <- 1 - (logLik(mmr_inter_glm_weighted) / logLik(null_model))

# Print the result
print(pseudo_r2_mcfadden)


## AB42/40 Interaction Model
mmr_inter_glm_weighted <- glm(AD_status ~ AB4240_ratio_c * ASCVD_cont_score_sqrtc, 
                              data = dataset, family = binomial, weights=CVDweight)

mmr_inter_glm <- glm(AD_status ~ AB4240_ratio_c * ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter_glm)
summary(mmr_inter_glm_weighted)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter_glm_weighted)
print(pseudo_r2)
vif_values <- vif(mmr_inter_glm)
print(vif_values)

# Generate ROC curve for the model
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter_glm_weighted))
# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for AB42/AB40 Main Model: No CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Plot the two ROC curves together - main effects and interaction term models

# Weighted main model
mmr_main_glm_weighted <- glm(
  AD_status ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_main <- roc(dataset$AD_status, fitted(mmr_main_glm_weighted))

# Weighted interaction model
mmr_inter_glm_weighted <- glm(
  AD_status ~ AB4240_ratio_c * ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_inter <- roc(dataset$AD_status, fitted(mmr_inter_glm_weighted))

# Plot both ROC curves
plot(roc_main, col = "blue", lwd = 2,
     main = "ROC Curves for Aβ42/Aβ40: Weighted Models")
lines(roc_inter, col = "red", lwd = 2)  # Add second curve

# Add diagonal line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright",
       legend = c("Main Model", "Interaction Model"),
       col = c("blue", "red"),
       lwd = 2)

--------------------------------------------
# Do the same for all remaining biomarkers
  
## P-tau181:
  
# Weighted main model
  mmr_main2_glm_weighted <- glm(
    AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc,
    data = dataset,
    family = binomial,
    weights = CVDweight
  )

roc_main <- roc(dataset$AD_status, fitted(mmr_main2_glm_weighted))

# Weighted interaction model
mmr_inter2_glm_weighted <- glm(
  AD_status ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_inter <- roc(dataset$AD_status, fitted(mmr_inter2_glm_weighted))

# Plot both ROC curves
plot(roc_main, col = "blue", lwd = 2,
     main = "ROC Curves for P-tau181: Weighted Models")
lines(roc_inter, col = "red", lwd = 2)  # Add second curve

# Add diagonal line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright",
       legend = c("Main Model", "Interaction Model"),
       col = c("blue", "red"),
       lwd = 2)


## P-tau217:

# Weighted main model
mmr_main3_glm_weighted <- glm(
  AD_status ~ PTAU217_logtr_c + ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_main <- roc(dataset$AD_status, fitted(mmr_main3_glm_weighted))

# Weighted interaction model
mmr_inter3_glm_weighted <- glm(
  AD_status ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_inter <- roc(dataset$AD_status, fitted(mmr_inter3_glm_weighted))

# Plot both ROC curves
plot(roc_main, col = "blue", lwd = 2,
     main = "ROC Curves for P-tau217: Weighted Models")
lines(roc_inter, col = "red", lwd = 2)  # Add second curve

# Add diagonal line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright",
       legend = c("Main Model", "Interaction Model"),
       col = c("blue", "red"),
       lwd = 2)

# Check the linearity of the relationship of p-tau181 and p-tau217 and CV risk

# Create tertiles (or use quartiles if you prefer finer groups)
dataset <- dataset %>%
  mutate(
    ASCVD_group = cut(
      ASCVD_cont_score_sqrtc,
      breaks = quantile(ASCVD_cont_score_sqrtc, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Low ASCVD", "Medium ASCVD", "High ASCVD")
    )
  )

# ---- pTau181 ----
ggplot(dataset, aes(x = pTau181_logtr_c, y = AD_status, color = ASCVD_group)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Plasma pTau181 (log-transformed, centered)",
    y = "Predicted probability of cognitive impairment",
    color = "ASCVD group"
  ) +
  theme_minimal()

# ---- pTau217 ----
ggplot(dataset, aes(x = PTAU217_logtr_c, y = AD_status, color = ASCVD_group)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Plasma pTau217 (log-transformed, centered)",
    y = "Predicted probability of cognitive impairment",
    color = "ASCVD group"
  ) +
  theme_minimal()

# Check linearity for the other two biomarkers as well
ggplot(dataset, aes(x = ApoE4_sqrt_c, y = AD_status, color = ASCVD_group)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Circulating levels of Apoe4",
    y = "Predicted probability of cognitive impairment",
    color = "ASCVD group"
  ) +
  theme_minimal()

ggplot(dataset, aes(x = AB4240_ratio_c, y = AD_status, color = ASCVD_group)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Plasma AB42/AB40",
    y = "Predicted probability of cognitive impairment",
    color = "ASCVD group"
  ) +
  theme_minimal()


## ApoE4:

# Weighted main model
mmr_main4_glm_weighted <- glm(
  AD_status ~ ApoE4_sqrt_c + ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_main <- roc(dataset$AD_status, fitted(mmr_main4_glm_weighted))

# Weighted interaction model
mmr_inter4_glm_weighted <- glm(
  AD_status ~ ApoE4_sqrt_c * ASCVD_cont_score_sqrtc,
  data = dataset,
  family = binomial,
  weights = CVDweight
)

roc_inter <- roc(dataset$AD_status, fitted(mmr_inter4_glm_weighted))

# Plot both ROC curves
plot(roc_main, col = "blue", lwd = 2,
     main = "ROC Curves for ApoE4: Weighted Models")
lines(roc_inter, col = "red", lwd = 2)  # Add second curve

# Add diagonal line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright",
       legend = c("Main Model", "Interaction Model"),
       col = c("blue", "red"),
       lwd = 2)


-----------------------------------------------------------------------------
# Likelihood Ratio Test comparing Main effects model vs Interaction model
  
anova(mmr_main_glm, mmr_inter_glm, test = "LRT")

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_inter_glm_weighted,
                      type = "response")

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
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter_glm_weighted))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for AB42/AB40 Interaction Model: Weighted by CVD")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# P-tau 181 Main Effects Model 
mmr_main2_glm <- glm(AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_main2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main2_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
library(pscl)
library(DescTools)
library(sjstats)
library(car)
pseudo_r2 <- pR2(mmr_main2_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main2_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_main2_glm))
plot(roc_curve)
auc(roc_curve)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_main2_glm, type = "response")

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
roc_curve <- roc(dataset$AD_status, fitted(mmr_main2_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau181 Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# P-tau 181 Interaction Model
mmr_inter2_glm <- glm(AD_status ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter2_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter2_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter2_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_inter2_glm))
plot(roc_curve)
auc(roc_curve)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main2_glm, mmr_inter2_glm, test = "LRT")

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_inter2_glm, type = "response")

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
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter2_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau181 Interaction Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# P-tau 217 Main Effects Model 
mmr_main3_glm <- glm(AD_status ~ PTAU217_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_main3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main3_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main3_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_main3_glm))
plot(roc_curve)
auc(roc_curve)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main3_glm, mmr_inter3_glm, test = "LRT")

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_main3_glm, type = "response")

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
roc_curve <- roc(dataset$AD_status, fitted(mmr_main3_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau217 Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# P-tau 217 Interaction Model
mmr_inter3_glm <- glm(AD_status ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter3_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter3_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_inter3_glm))
plot(roc_curve)
auc(roc_curve)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_inter3_glm, type = "response")

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
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter3_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for P-tau217 Interaction Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# ApoE4 Main Effects Model 
mmr_main4_glm <- glm(AD_status ~ ApoE4_sqrt_c + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_main4_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main4_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main4_glm)
print(vif_values)

roc_curve <- roc(Complete_Dataset_Roche_Lilly_13122024$AD_status, fitted(mmr_main4_glm))
plot(roc_curve)
auc(roc_curve)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_main4_glm, type = "response")

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
roc_curve <- roc(dataset$AD_status, fitted(mmr_main4_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for ApoE4 Main Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# ApoE4 Interaction Model
mmr_inter4_glm <- glm(AD_status ~ ApoE4_sqrt_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter4_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter4_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter4_glm)
print(vif_values)

### ** Sensitivity & Specificity Calculation **

# Make sure AD status is a factor
dataset$AD_status[dataset$AD_status == 2] <- 1  # Merge 2 into 1
dataset$AD_status <- factor(dataset$AD_status, levels = c(0, 1))  # Ensure proper factor levels

# Predict probabilities from the model
pred_probs <- predict(mmr_inter4_glm, type = "response")

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
roc_curve <- roc(dataset$AD_status, fitted(mmr_inter4_glm))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for ApoE4 Interaction Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Display AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main4_glm, mmr_inter4_glm, test = "LRT")