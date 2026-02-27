
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 20/10/2024
----------------------------------------------------------------------------------
  
#### Run MODERATED MULTIPLE REGRESSION analyses

# Load packages
library(readr)
library(dply)
library(pscl)
library(DescTools)
library(sjstats)
library(car)
library(ggplot2)
library(car)

## Always make sure required datasets are loaded.

# Data to work with
View(final_merged_dataset_170924)

# Check the data types before re-running analyses
str(final_merged_dataset_170924)

# Center AB42/40_Roche (continuous variable)
final_merged_dataset_170924$AB_4240_Roche_c <- scale(final_merged_dataset_170924$AB_4240_Roche, center = TRUE, scale = TRUE)

# Explore the ethnicity variable 
summary(final_merged_dataset_170924$ETHNIC_recode)

# Exclude cases where ethnicity = not reported
final_merged_dataset_170924 <- subset(final_merged_dataset_170924, ETHNIC_recode != 2)

# Verify the exclusion
table(final_merged_dataset_170924$ETHNIC_recode)
View(final_merged_dataset_170924)

final_merged_dataset_170924 <- final_merged_dataset_170924 %>%
  mutate(ETHNIC_recode = case_when(
    ETHNIC == "NOT HISPANIC OR LATINO" ~ "0",
    ETHNIC == "HISPANIC OR LATINO" ~ "1",
    ETHNIC == "NOT REPORTED" ~ "2",
    TRUE ~ "NA"  # This handles any unexpected values
  ))

# Exclude cases where ethnicity = not reported
final_merged_dataset_170924 <- subset(final_merged_dataset_170924, ETHNIC_recode != 2)

-------------
### AB42/40

# Main Effects Model
mmr_main <- glm(AD_status ~ AB_4240_Roche_c + CVD_risk + ETHNIC_recode, 
                data = final_merged_dataset_170924, 
                family = binomial)

summary(mmr_main)

# Calculate R squared - load necessary libraries
install.packages(c("pscl", "DescTools", "sjstats"))

# Checking for multicollinearity
install.packages("car")
vif_values <- vif(mmr_main)
print(vif_values)
# Calculate correlation matrix
cor_matrix <- cor(final_merged_dataset_170924[, c("AB_4240_Roche_c", "CVD_risk", "ETHNIC_recode")], use = "complete.obs")
# Print correlation matrix
print(cor_matrix)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_main)
print(pseudo_r2)

# Plot AB42/40 Ratio vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = AB_4240_Roche_c, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of AB42/40 Ratio on Probability of AD",
       x = "Centered AB42/40 Ratio",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot CVD Risk vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = CVD_risk_c, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of CVD Risk on Probability of AD",
       x = "Centered CVD Risk",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot Ethnicity vs. Predicted Probability of AD

# Convert ETHNIC_recode to a factor with descriptive labels
final_merged_dataset_170924$ETHNIC_recode <- factor(final_merged_dataset_170924$ETHNIC_recode,
                                                    levels = c(0, 1),
                                                    labels = c("Not Hispanic or Latino", 
                                                               "Hispanic or Latino"))

## OPTIONAL:

# Enhanced Plot with Centered Title and Descriptive Legend
ggplot(final_merged_dataset_170924, aes(x = ETHNIC_recode, y = AD_status, fill = ETHNIC_recode)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Effect of Ethnicity on Probability of AD",
       x = "Ethnicity",
       y = "Mean Probability of AD",
       fill = "Ethnicity") +  # Assigning a descriptive legend title
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centering and styling the title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Interaction Model: AB_4240_Roche x CVD_risk
mmr_interaction <- glm(AD_status ~ AB_4240_Roche_c * CVD_risk + ETHNIC_recode, 
                       data = final_merged_dataset_170924, 
                       family = binomial)

summary(mmr_interaction)

# Calculate Pseudo-R-squared measures
install.packages("pscl")
library(pscl)
pseudo_r2 <- pR2(mmr_interaction)
print(pseudo_r2)
vif_values <- vif(mmr_interaction)
print(vif_values)

# Assuming you have calculated VIFs and stored in vif_values
# Convert VIF values to a data frame
vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "cyan4") +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown4", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

## OPTIONAL:

# Install required packages if not already installed
install.packages(c("interactions", "ggplot2"))

# Load the packages
library(interactions)

# Check if the 'interact_plot' function is available
exists("interact_plot", where = "package:interactions")

# Plot the non-significant interaction with annotations
interact_plot(mmr_interaction, 
              pred = AB_4240_Roche_c, 
              modx = CVD_risk, 
              plot.points = TRUE, 
              jitter = 0.1,
              main.title = "Moderation of CVD Risk on AB42/40 Ratio and AD Status",
              x.label = "Centered AB42/40 Ratio",
              y.label = "Predicted Probability of AD",
              legend.main = "CVD Risk",
              interval = TRUE,  
              int.width = 0.1,   
              colors = "RdYlBu") 

# Interaction Model: AB_4240_Roche x CVD_risk and AB_4240_Roche x ethnicity
mmr_full <- glm(AD_status ~ AB_4240_Roche_c * CVD_risk + AB_4240_Roche_c * ETHNIC_recode, 
                data = final_merged_dataset_170924, 
                family = binomial)

summary(mmr_full)

# Assuming you have calculated VIFs and stored in vif_values
# Convert VIF values to a data frame
vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

pseudo_r2 <- pR2(mmr_full)
print(pseudo_r2)
vif_values <- vif(mmr_full)
print(vif_values)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "cyan4") +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown4", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

### 21/10/2024: Experimenting with Ridge Regression 

final_merged_dataset_170924 <- read_csv("Angelina_Bio_Hermes_R_code/Univariate_Regressions_Added_070924/final_merged_dataset_170924.csv",
                                        show_col_types = FALSE)
View(final_merged_dataset_170924)

# First, install and load required package
install.packages("glmnet")
library(glmnet)

# Convert predictors to a matrix (excluding the outcome variable)
X <- model.matrix(AD_status ~ AB_4240_Roche_c * CVD_risk + AB_4240_Roche_c * ETHNIC_recode, 
                  data = final_merged_dataset_170924)[,-1]

# Convert AD_status to a numeric vector (0/1 for binary outcome)
y <- as.numeric(final_merged_dataset_170924$AD_status)
View(final_merged_dataset_170924)

# Fit Ridge logistic regression model (alpha = 0 for Ridge, family = "binomial" for logistic regression)
ridge_model <- glmnet(X, y, alpha = 0, family = "binomial")

# View the coefficients at different lambda values (penalty strength)
print(ridge_model)

# Perform cross-validation to find the optimal lambda for Ridge
cv_ridge <- cv.glmnet(X, y, alpha = 0, family = "binomial")

# Get the optimal lambda value
best_lambda_ridge <- cv_ridge$lambda.min
print(best_lambda_ridge) # the value I obtain is 0.01418183

# Fit final Ridge model with optimal lambda
final_ridge_model <- glmnet(X, y, alpha = 0, family = "binomial", lambda = best_lambda_ridge)

# View the coefficients of the final model
ridge_coef <- coef(final_ridge_model)
print(ridge_coef)

# Make predictions with the final Ridge model
ridge_predictions <- predict(final_ridge_model, newx = X, type = "response")

# Make predictions with the final Ridge model
ridge_predictions <- predict(final_ridge_model, newx = X, type = "response")
print(ridge_predictions)

# Calculate ORs
exp(coef(final_ridge_model))

----------------
### P-tau 181

# Main Effects Model

hist(final_merged_dataset_170924$TauP181_Roche_logtr)

mmr_main2 <- glm(AD_status ~ TauP181_Roche_logtr + CVD_risk + ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_main2)

# Calculate R squared
library(pscl)
library(DescTools)
library(sjstats)
# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_main2)
print(pseudo_r2)

vif_values <- vif(mmr_main2)
print(vif_values)

# Assuming you have calculated VIFs and stored in vif_values
library(ggplot2)
# Convert VIF values to a data frame
vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "cyan4") +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown4", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Plot P-tau 181 vs. Predicted Probability of AD
library(ggplot2)
ggplot(final_merged_dataset_170924, aes(x = TauP181_Roche_logtr, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of P-Tau 181 on Probability of AD",
       x = "P-tau 181",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot CVD Risk vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = CVD_risk, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of CVD Risk on Probability of AD",
       x = "CVD Risk",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot Ethnicity vs. Predicted Probability of AD

## OPTIONAL:

# Enhanced Plot with Centered Title and Descriptive Legend
ggplot(final_merged_dataset_170924, aes(x = ETHNIC_recode, y = AD_status, fill = ETHNIC_recode)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Effect of Ethnicity on Probability of AD",
       x = "Ethnicity",
       y = "Mean Probability of AD",
       fill = "Ethnicity") +  # Assigning a descriptive legend title
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centering and styling the title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Interaction Model: P-tau 181 x CVD_risk
mmr_interaction2 <- glm(AD_status ~ TauP181_Roche_logtr * CVD_risk + ETHNIC_recode, 
                        data = final_merged_dataset_170924, 
                        family = binomial)

summary(mmr_interaction2)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_interaction2)
print(pseudo_r2)

vif_values <- vif(mmr_interaction2)
print(vif_values)

# Assuming you have calculated VIFs and stored in vif_values
library(ggplot2)
# Convert VIF values to a data frame
vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_hline(yintercept = 5, color = "orange", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown4", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Install required packages if not already installed
install.packages(c("interactions", "ggplot2"))
# Load the packages
library(interactions)

# Check if the 'interact_plot' function is available
exists("interact_plot", where = "package:interactions")

# Plot the non-significant interaction with annotations
interact_plot(mmr_interaction2, 
              pred = TauP181_Roche_logtr, 
              modx = CVD_risk, 
              plot.points = TRUE, 
              jitter = 0.1,
              main.title = "Moderation of CVD Risk on P-Tau 181 and AD Status",
              x.label = "P-tau 181",
              y.label = "Predicted Probability of AD",
              legend.main = "CVD Risk",
              interval = TRUE,  
              int.width = 0.1,   
              colors = "RdYlBu") 

# Interaction Model: P-tau 181 x CVD_risk and P-tau 181 x ethnicity
mmr_full2 <- glm(AD_status ~ TauP181_Roche_logtr * CVD_risk + TauP181_Roche_logtr * ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_full2)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_full2)
print(pseudo_r2)

vif_values <- vif(mmr_full2)
print(vif_values)

vif_df <- data.frame(
  Predictor = names(vif_values),
  VIF = vif_values
)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "green4") +
  geom_hline(yintercept = 5, color = "orange2", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown3", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

---------------
### P-tau 217

# Extra pre-processing

hist(final_merged_dataset_170924$TauP217_Lilly_logtr)

# Center P-tau 217 (continuous variable)
final_merged_dataset_170924$TauP217_Lilly_logtr_c <- scale(final_merged_dataset_170924$TauP217_Lilly_logtr, 
                                                           center = TRUE, scale = TRUE)
hist(final_merged_dataset_170924$TauP217_Lilly_logtr_c)

# Main Effects Model
mmr_main3 <- glm(AD_status ~ TauP217_Lilly_logtr_c + CVD_risk + ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_main3)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_main3)
print(pseudo_r2)

vif_values <- vif(mmr_main3)
print(vif_values)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "green4") +
  geom_hline(yintercept = 5, color = "orange2", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "brown3", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Plot P-tau 217 vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = TauP217_Lilly_logtr_c, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of P-Tau 217 on Probability of AD",
       x = "P-tau 217",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot CVD Risk vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = CVD_risk, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of CVD Risk on Probability of AD",
       x = "CVD Risk",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot Ethnicity vs. Predicted Probability of AD

## OPTIONAL:

# Enhanced Plot with Centered Title and Descriptive Legend
ggplot(final_merged_dataset_170924, aes(x = ETHNIC_recode, y = AD_status, fill = ETHNIC_recode)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Effect of Ethnicity on Probability of AD",
       x = "Ethnicity",
       y = "Mean Probability of AD",
       fill = "Ethnicity") +  # Assigning a descriptive legend title
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centering and styling the title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Interaction Model: P-tau 217 x CVD_risk
mmr_interaction3 <- glm(AD_status ~ TauP217_Lilly_logtr_c * CVD_risk + ETHNIC_recode, 
                        data = final_merged_dataset_170924, 
                        family = binomial)

summary(mmr_interaction3)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_interaction3)
print(pseudo_r2)

vif_values <- vif(mmr_interaction3)
print(vif_values)

# Create a bar plot
ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "purple4") +
  geom_hline(yintercept = 5, color = "pink2", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey3", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Install required packages if not already installed
install.packages(c("interactions", "ggplot2"))
# Load the packages
library(interactions)

# Check if the 'interact_plot' function is available
exists("interact_plot", where = "package:interactions")

# Plot the non-significant interaction with annotations
interact_plot(mmr_interaction3, 
              pred = TauP217_Lilly_logtr_c, 
              modx = CVD_risk, 
              plot.points = TRUE, 
              jitter = 0.1,
              main.title = "Moderation of CVD Risk on P-Tau 217 and AD Status",
              x.label = "P-tau 217",
              y.label = "Predicted Probability of AD",
              legend.main = "CVD Risk",
              interval = TRUE,  
              int.width = 0.1,   
              colors = "RdYlBu") 

# Interaction Model: P-tau 217 x CVD_risk and P-tau 217 x ethnicity
mmr_full3 <- glm(AD_status ~ TauP217_Lilly_logtr_c * CVD_risk + TauP217_Lilly_logtr_c * 
                   ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_full3)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_full3)
print(pseudo_r2)

vif_values <- vif(mmr_full3)
print(vif_values)

ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "darkmagenta") +
  geom_hline(yintercept = 5, color = "pink", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

------------
### ApoE4

hist(final_merged_dataset_170924$ApoE4_Roche_logtr)
hist(final_merged_dataset_170924$ApoE4_Roche)

# Center ApoE4 (continuous variable)
final_merged_dataset_170924$ApoE4_Roche_logtr_c <- scale(final_merged_dataset_170924$ApoE4_Roche_logtr, 
                                                         center = TRUE, scale = TRUE)
hist(final_merged_dataset_170924$ApoE4_Roche_logtr_c)

# Main Effects Model
mmr_main4 <- glm(AD_status ~ ApoE4_Roche_logtr_c + CVD_risk + ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_main4)

pseudo_r2 <- pR2(mmr_main4)
print(pseudo_r2)

# Checking for multicollinearity
install.packages("car")
vif_values <- vif(mmr_main4)
print(vif_values)

ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "darkmagenta") +
  geom_hline(yintercept = 5, color = "pink", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Calculate correlation matrix
cor_matrix <- cor(final_merged_dataset_170924[, c("ApoE4_Roche_logtr_c", "CVD_risk", "ETHNIC_recode")], use = "complete.obs")

# Print correlation matrix
print(cor_matrix)

# Plot P-tau 217 vs. Predicted Probability of AD
library(ggplot2)
ggplot(final_merged_dataset_170924, aes(x = ApoE4_Roche_logtr_c, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of ApoE4 on Probability of AD",
       x = "ApoE4",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot CVD Risk vs. Predicted Probability of AD
ggplot(final_merged_dataset_170924, aes(x = CVD_risk, y = AD_status)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Effect of CVD Risk on Probability of AD",
       x = "CVD Risk",
       y = "Predicted Probability of AD") +
  theme_minimal()

# Plot Ethnicity vs. Predicted Probability of AD

## OPTIONAL:

# Enhanced Plot with Centered Title and Descriptive Legend
ggplot(final_merged_dataset_170924, aes(x = ETHNIC_recode, y = AD_status, fill = ETHNIC_recode)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Effect of Ethnicity on Probability of AD",
       x = "Ethnicity",
       y = "Mean Probability of AD",
       fill = "Ethnicity") +  # Assigning a descriptive legend title
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centering and styling the title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Interaction Model: ApoE4 x CVD_risk
mmr_interaction4 <- glm(AD_status ~ ApoE4_Roche_logtr_c * CVD_risk + ETHNIC_recode, 
                        data = final_merged_dataset_170924, 
                        family = binomial)

summary(mmr_interaction4)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_interaction4)
print(pseudo_r2)

vif_values <- vif(mmr_interaction4)
print(vif_values)

ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_hline(yintercept = 5, color = "cornsilk2", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey2", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Install required packages if not already installed
install.packages(c("interactions", "ggplot2"))
# Load the packages
library(interactions)

# Check if the 'interact_plot' function is available
exists("interact_plot", where = "package:interactions")

# Plot the non-significant interaction with annotations
interact_plot(mmr_interaction4, 
              pred = ApoE4_Roche_logtr_c, 
              modx = CVD_risk, 
              plot.points = TRUE, 
              jitter = 0.1,
              main.title = "Moderation of CVD Risk on P-Tau 217 and AD Status",
              x.label = "P-tau 217",
              y.label = "Predicted Probability of AD",
              legend.main = "CVD Risk",
              interval = TRUE,  
              int.width = 0.1,   
              colors = "RdYlBu") 

# Interaction Model: ApoE4 x CVD_risk and ApoE4 x ethnicity
mmr_full4 <- glm(AD_status ~ ApoE4_Roche_logtr_c * CVD_risk + ApoE4_Roche_logtr_c * 
                   ETHNIC_recode, 
                 data = final_merged_dataset_170924, 
                 family = binomial)

summary(mmr_full4)

# Calculate Pseudo-R-squared measures
pseudo_r2 <- pR2(mmr_full4)
print(pseudo_r2)

# Calculate VIF metric
vif_values <- vif(mmr_full4)
print(vif_values)

ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "coral3") +
  geom_hline(yintercept = 5, color = "cornsilk3", linetype = "dashed") +
  geom_hline(yintercept = 10, color = "grey2", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal()

# Save dataset
write.csv(final_merged_dataset_170924, "final_merged_dataset_170924.csv", row.names = FALSE)


-----------------------
## Subgroup analyses

final_merged_dataset_170924 <- read_csv("Angelina_Bio_Hermes_R_code/Univariate_Regressions_Added_070924/final_merged_dataset_170924.csv",
                                        show_col_types = FALSE)
View(final_merged_dataset_170924)

# Split data into two ethnic groups
hispanic_data <- final_merged_dataset_170924 %>% filter(ETHNIC_recode == 1)
non_hispanic_data <- final_merged_dataset_170924 %>% filter(ETHNIC_recode == 0)

# Model for Hispanic or Latino group
model_hispanic <- glm(AD_status ~ AB_4240_Roche_c * CVD_risk, data = hispanic_data, family = binomial)
summary(model_hispanic)
pseudo_r2 <- pR2(model_hispanic)
print(pseudo_r2)

# Model for Not Hispanic or Latino group
model_non_hispanic <- glm(AD_status ~ AB_4240_Roche_c * CVD_risk, data = non_hispanic_data, family = binomial)
summary(model_non_hispanic)
pseudo_r2 <- pR2(model_non_hispanic)
print(pseudo_r2)

# Confidence intervals for Hispanic or Latino group
confint(model_hispanic)
# Confidence intervals for Not Hispanic or Latino group
confint(model_non_hispanic)

# Model for Hispanic or Latino group
model_hispanic <- glm(AD_status ~ TauP181_Roche_logtr * CVD_risk, data = hispanic_data,
                      family = binomial)
summary(model_hispanic)
pseudo_r2 <- pR2(model_hispanic)
print(pseudo_r2)

# Model for Not Hispanic or Latino group
model_non_hispanic <- glm(AD_status ~ TauP181_Roche_logtr * CVD_risk, data = non_hispanic_data, 
                          family = binomial)
summary(model_non_hispanic)
pseudo_r2 <- pR2(model_non_hispanic)
print(pseudo_r2)

# Model for Hispanic or Latino group
model_hispanic <- glm(AD_status ~ TauP217_Lilly_logtr_c * CVD_risk, data = hispanic_data,
                      family = binomial)
summary(model_hispanic)
pseudo_r2 <- pR2(model_hispanic)
print(pseudo_r2)

# Model for Not Hispanic or Latino group
model_non_hispanic <- glm(AD_status ~ TauP217_Lilly_logtr_c * CVD_risk, data = non_hispanic_data, 
                          family = binomial)
summary(model_non_hispanic)
pseudo_r2 <- pR2(model_non_hispanic)
print(pseudo_r2)

# Model for Hispanic or Latino group
model_hispanic <- glm(AD_status ~ ApoE4_Roche_logtr_c * CVD_risk, data = hispanic_data,
                      family = binomial)
summary(model_hispanic)
pseudo_r2 <- pR2(model_hispanic)
print(pseudo_r2)

# Model for Not Hispanic or Latino group
model_non_hispanic <- glm(AD_status ~ ApoE4_Roche_logtr_c * CVD_risk, data = non_hispanic_data, 
                          family = binomial)
summary(model_non_hispanic)
pseudo_r2 <- pR2(model_non_hispanic)
print(pseudo_r2)

