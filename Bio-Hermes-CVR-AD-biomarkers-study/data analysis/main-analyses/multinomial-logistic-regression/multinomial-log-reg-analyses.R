
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 

# Date: 23/10/2024
----------------------------------------------------------------------------------
  
### Multinomial Logistic Regression Analyses with CV Risk Score

# Load packages
library(readr)
install.packages("nnet")  # for multinomial logistic regression
library(nnet)

# Load data
final_merged_dataset_170924 <- read_csv(final_merged_dataset_170924, "final_merged_dataset_170924.csv", row.names = FALSE)

# Convert categorical variables to factors 
final_merged_dataset_170924$AD_status <- factor(final_merged_dataset_170924$AD_status, levels = c(0, 1, 2))
final_merged_dataset_170924$ASCVD_classification_numeric <- factor(final_merged_dataset_170924$ASCVD_classification_numeric, levels = c(1, 2, 3, 4))
View(final_merged_dataset_170924)

-------------------
### AB42/40 ###
-------------------

# Main Effects Model

# Run multinomial logistic regression
mmr_main <- multinom(AD_status_cat ~ AB_4240_Roche_c + ASCVD_classification_numeric + ETHNIC_recode, 
                     data = final_merged_dataset_170924)

# View the summary of the model
summary(mmr_main)

# Calculate p-values using the Wald test
z <- summary(mmr_main)$coefficients / summary(mmr_main)$standard.errors
p_values <- (1 - pnorm(abs(z), 0, 1)) * 2

# View p-values
p_values

# Predicted probabilities
predicted_probabilities <- predict(mmr_main, type = "probs")

# Check predicted classifications
predicted_classifications <- predict(mmr_main)

# Confusion matrix (if you have true values to compare against)
table(Predicted = predicted_classifications, Actual = final_merged_dataset_170924$AD_status)

# Calculate performance metrics
install.packages("caret")  
library(caret)

# Create a factor for the actual AD_status and the predicted classifications
actual <- final_merged_dataset_170924$AD_status_cat
predicted <- predicted_classifications

# Create confusion matrix
confusion <- confusionMatrix(factor(predicted), factor(actual))

# Print the confusion matrix results
print(confusion)

# Calculate accuracy, precision, recall, and F1-score for each class
accuracy <- confusion$overall["Accuracy"]
precision <- confusion$byClass["Precision"]
recall <- confusion$byClass["Recall"]
f1 <- confusion$byClass["F1"]

# Print the results
print(paste("Accuracy: ", round(accuracy, 4)))
print(paste("Precision (class-wise): ", round(precision, 4)))
print(paste("Recall (class-wise): ", round(recall, 4)))
print(paste("F1-score (class-wise): ", round(f1, 4)))

## Back to running logistic regression

# Convert AD_status to a binary variable (0 = healthy, 1 = dementia)
final_merged_dataset_170924$AD_status <- ifelse(final_merged_dataset_170924$AD_status == 0, 0, 1)

View(final_merged_dataset_170924)

# Fit the logistic regression model
mmr_main_glm <- glm(AD_status ~ AB_4240_Roche_c + ASCVD_classification_numeric, 
                     data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_main_glm)

## Fit another logistic regression model - ASCVD as a continuous var this time around

# First, some transformations of the ASCVD continuous variable
hist(final_merged_dataset_170924$ASCVD_score) # slightly right-skewed 

## Apply square root transformation on ASCVD

# Log and inverse transformation only shifted or increased the skew, hence, 
# applying a square root transformation now

final_merged_dataset_170924$ASCVD_score_sqrt <- sqrt(final_merged_dataset_170924$ASCVD_score)

# Center and scale the square root transformed score
final_merged_dataset_170924$ASCVD_score_sqrtc <- scale(final_merged_dataset_170924$ASCVD_score_sqrt, center = TRUE, scale = TRUE)

# Inspect the distribution of the variable now
hist(final_merged_dataset_170924$ASCVD_score_sqrtc)

# Now fit the model
mmr_main_glm_cont <- glm(AD_status ~ AB_4240_Roche_c + ASCVD_score_sqrtc, 
                         data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_main_glm_cont)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main_glm))

# Print the odds ratios
print(odds_ratios)

odds_ratios <- exp(coef(mmr_main_glm_cont))
print(odds_ratios)

# McFadden's Pseudo R-squared
library(pscl)
library(DescTools)
library(sjstats)
library(car)
pseudo_r2 <- pR2(mmr_main_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main_glm)
print(vif_values)

pseudo_r2 <- pR2(mmr_main_glm_cont)
print(pseudo_r2)
vif_values <- vif(mmr_main_glm_cont)
print(vif_values)
cor(final_merged_dataset_170924$AB_4240_Roche_c, final_merged_dataset_170924$ASCVD_score_sqrtc)

# Calculate accuracy
accuracy <- sum(predicted_classifications == final_merged_dataset_170924$AD_status) / nrow(final_merged_dataset_170924)
print(paste("Accuracy: ", round(accuracy, 4)))

# Fit the logistic regression model
mmr_inter_glm <- glm(AD_status ~ AB_4240_Roche_c * ASCVD_classification_numeric, 
                    data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_inter_glm)

mmr_inter_glm_cont <- glm(AD_status ~ AB_4240_Roche_c * ASCVD_score_sqrtc, 
                     data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_inter_glm_cont)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter_glm))

# Print the odds ratios
print(odds_ratios)

odds_ratios <- exp(coef(mmr_inter_glm_cont))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter_glm, type="predictor")
print(vif_values)

pseudo_r2 <- pR2(mmr_inter_glm_cont)
print(pseudo_r2)
vif_values <- vif(mmr_inter_glm_cont)
print(vif_values)

# Calculate accuracy
accuracy <- sum(predicted_classifications == final_merged_dataset_170924$AD_status) / nrow(final_merged_dataset_170924)
print(paste("Accuracy: ", round(accuracy, 4)))

# Log-Likelihood
log_likelihood <- logLik(mmr_inter_glm)
print(paste("Log-Likelihood: ", log_likelihood))

cor(final_merged_dataset_170924$AB_4240_Roche_c, as.numeric(final_merged_dataset_170924$ASCVD_classification_numeric))
cor(final_merged_dataset_170924$AB_4240_Roche_c, as.numeric(final_merged_dataset_170924$ASCVD_score_sqrtc))
# Correlation is -0.15.

# Experiment with Ridge regression

library(glmnet)
x <- model.matrix(AD_status ~ AB_4240_Roche_c * ASCVD_classification_numeric, 
                  data = final_merged_dataset_170924)[, -1]  # Remove intercept

y <- final_merged_dataset_170924$AD_status

# Perform cross-validation to find the optimal lambda for Ridge
cv_ridge <- cv.glmnet(x, y, alpha = 0, family = "binomial")

# Get the optimal lambda value
best_lambda_ridge <- cv_ridge$lambda.min
print(best_lambda_ridge) # the value I obtain is 0.01639

# Fit final Ridge model with optimal lambda
final_ridge_model <- glmnet(x, y, alpha = 0, family = "binomial", lambda = best_lambda_ridge)

# View the coefficients of the final model
ridge_coef <- coef(final_ridge_model)
print(ridge_coef)

# Make predictions with the final Ridge model
ridge_predictions <- predict(final_ridge_model, newx = x, type = "response")

# Make predictions with the final Ridge model
ridge_predictions <- predict(final_ridge_model, newx = x, type = "response")
print(ridge_predictions)

# Calculate ORs
exp(coef(final_ridge_model))
vif(final_ridge_model)

# Cross-validation plot for ridge model
plot(ridge_model)

# Get lambda with minimum cross-validation error
best_lambda <- ridge_model$lambda.min
best_lambda

# Get cross-validation error at best lambda
cv_error <- ridge_model$cvm[ridge_model$lambda == best_lambda]
cat("Cross-validated error for the ridge model:", cv_error, "\n")

--------------------
### P-tau 181 ###
--------------------
  
# Calculate the centered and scaled TauP181_Roche_logtr
final_merged_dataset_170924$TauP181_Roche_logtr_c <- scale(final_merged_dataset_170924$TauP181_Roche_logtr, center = TRUE, scale = TRUE)
hist(final_merged_dataset_170924$TauP181_Roche_logtr_c)

mmr_main2_glm <- glm(AD_status ~ TauP181_Roche_logtr_c + ASCVD_classification_numeric, 
                    data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_main2_glm)

mmr_main2_glm_cont <- glm(AD_status ~ TauP181_Roche_logtr_c + ASCVD_score_sqrtc, 
                     data = final_merged_dataset_170924, family = binomial)
summary(mmr_main2_glm_cont)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main2_glm))

# Print the odds ratios
print(odds_ratios)

odds_ratios <- exp(coef(mmr_main2_glm_cont))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main2_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main2_glm)
print(vif_values)

pseudo_r2 <- pR2(mmr_main2_glm_cont)
print(pseudo_r2)
vif_values <- vif(mmr_main2_glm_cont)
print(vif_values)

cor(final_merged_dataset_170924$TauP181_Roche_logtr_c, as.numeric(final_merged_dataset_170924$ASCVD_score_sqrtc))

# Fit the logistic regression model
mmr_inter2_glm <- glm(AD_status ~ TauP181_Roche_logtr_c * ASCVD_classification_numeric, 
                     data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_inter2_glm)

mmr_inter2_glm_cont <- glm(AD_status ~ TauP181_Roche_logtr_c * ASCVD_score_sqrtc, 
                      data = final_merged_dataset_170924, family = binomial)
summary(mmr_inter2_glm_cont)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter2_glm))

# Print the odds ratios
print(odds_ratios)

odds_ratios <- exp(coef(mmr_inter2_glm_cont))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter2_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter2_glm)
print(vif_values)

pseudo_r2 <- pR2(mmr_inter2_glm_cont)
print(pseudo_r2)
vif_values <- vif(mmr_inter2_glm_cont)
print(vif_values)

# Log-Likelihood
log_likelihood <- logLik(mmr_inter2_glm)
print(paste("Log-Likelihood: ", log_likelihood))

cor(final_merged_dataset_170924$TauP181_Roche_logtr_c, as.numeric(final_merged_dataset_170924$ASCVD_classification_numeric))
cor(final_merged_dataset_170924$TauP181_Roche_logtr_c, as.numeric(final_merged_dataset_170924$ASCVD_score_sqrtc))

---------------------
### P-tau 217 ###
---------------------

mmr_main3_glm <- glm(AD_status ~ TauP217_Lilly_logtr_c + ASCVD_classification_numeric, 
                     data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_main3_glm)

mmr_main3_glm_cont <- glm(AD_status ~ TauP217_Lilly_logtr_c + ASCVD_score_sqrtc, 
                     data = final_merged_dataset_170924, family = binomial)

summary(mmr_main3_glm_cont)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main3_glm))

# Print the odds ratios
print(odds_ratios)

odds_ratios <- exp(coef(mmr_main3_glm_cont))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main3_glm)
print(vif_values)

pseudo_r2 <- pR2(mmr_main3_glm_cont)
print(pseudo_r2)
vif_values <- vif(mmr_main3_glm_cont)
print(vif_values)

cor(final_merged_dataset_170924$TauP217_Lilly_logtr_c, as.numeric(final_merged_dataset_170924$ASCVD_score_sqrtc))

# Fit the interaction logistic regression model
mmr_inter3_glm <- glm(AD_status ~ TauP217_Lilly_logtr_c * ASCVD_classification_numeric, 
                      data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_inter3_glm)

mmr_inter3_glm_cont <- glm(AD_status ~ TauP217_Lilly_logtr_c * ASCVD_score_sqrtc, 
                      data = final_merged_dataset_170924, family = binomial)

summary(mmr_inter3_glm_cont)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter3_glm))

# Print the odds ratios
print(odds_ratios)

odds_ratios <- exp(coef(mmr_inter3_glm_cont))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter3_glm)
print(vif_values)

pseudo_r2 <- pR2(mmr_inter3_glm_cont)
print(pseudo_r2)
vif_values <- vif(mmr_inter3_glm_cont, type="predictor")
print(vif_values)

cor(final_merged_dataset_170924$TauP217_Lilly_logtr_c, as.numeric(final_merged_dataset_170924$ASCVD_score_sqrtc))

# Log-Likelihood
log_likelihood <- logLik(mmr_inter3_glm)
print(paste("Log-Likelihood: ", log_likelihood))

cor(final_merged_dataset_170924$TauP217_Lilly_logtr_c, as.numeric(final_merged_dataset_170924$ASCVD_score_sqrtc))

----------------
### ApoE4 ###
----------------

mmr_main4_glm <- glm(AD_status ~ ApoE4_Roche_logtr_c + ASCVD_classification_numeric, 
                     data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_main4_glm)

mmr_main4_glm_cont <- glm(AD_status ~ ApoE4_Roche_logtr_c + ASCVD_score_sqrtc, 
                     data = final_merged_dataset_170924, family = binomial)
summary(mmr_main4_glm_cont)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main4_glm))

# Print the odds ratios
print(odds_ratios)

odds_ratios <- exp(coef(mmr_main4_glm_cont))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main4_glm)
print(vif_values)

pseudo_r2 <- pR2(mmr_main4_glm_cont)
print(pseudo_r2)
vif_values <- vif(mmr_main4_glm_cont)
print(vif_values)

cor(final_merged_dataset_170924$ApoE4_Roche_logtr_c, as.numeric(final_merged_dataset_170924$ASCVD_score_sqrtc))

# Fit the interaction logistic regression model
mmr_inter4_glm <- glm(AD_status ~ ApoE4_Roche_logtr_c * ASCVD_classification_numeric, 
                      data = final_merged_dataset_170924, family = binomial)

# View the summary of the model
summary(mmr_inter4_glm)

mmr_inter4_glm_cont <- glm(AD_status ~ ApoE4_Roche_logtr_c * ASCVD_score_sqrtc, 
                      data = final_merged_dataset_170924, family = binomial)

summary(mmr_inter4_glm_cont)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter4_glm))

# Print the odds ratios
print(odds_ratios)

odds_ratios <- exp(coef(mmr_inter4_glm_cont))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter4_glm)
print(vif_values)

pseudo_r2 <- pR2(mmr_inter4_glm_cont)
print(pseudo_r2)
vif_values <- vif(mmr_inter4_glm_cont)
print(vif_values)

# Log-Likelihood
log_likelihood <- logLik(mmr_inter4_glm)
print(paste("Log-Likelihood: ", log_likelihood))

cor(final_merged_dataset_170924$ApoE4_Roche_logtr_c, as.numeric(final_merged_dataset_170924$ASCVD_classification_numeric))
cor(final_merged_dataset_170924$ApoE4_Roche_logtr_c, as.numeric(final_merged_dataset_170924$ASCVD_score_sqrtc))


