
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 
  
# Date: 07/08/2025
----------------------------------------------------------------------------------
  
# Load libraries
library(readr)
library(psych)
library(ggplot2)
library(dplyr)

-------------------------------------------------------------------------

### Additional analyses comparing the different cognitive status groups

## Make sure all necessary datasets are loaded before proceeding.
  
table(dataset$QVAL)

# Dropping MCI cases to only compare 'Healthy' versus 'Probable AD'

# Step 1: Subset the dataset to only include Healthy and Probable AD
dataset <- subset(dataset, QVAL %in% c("Cohort 1 (Healthy)", "Cohort 3 (Probable AD)"))

# Step 2: Create the new binary outcome variable
dataset$AD_status_HAD <- ifelse(dataset$QVAL == "Cohort 3 (Probable AD)", 1, 0)
table(dataset$AD_status_HAD)
# Should return:
#   0   1 
# 301 209

## Re-run all regression analyses

------------
# AB42/40:

# AB42/40 Main Effects Model 
mmr_main_glm <- glm(AD_status_HAD ~ AB4240_ratio_c + ASCVD_cont_score_sqrtc, 
                    data = dataset, family = binomial)

# Display results
summary(mmr_main_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main_glm))
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

# AB42/40 Interaction Model
mmr_inter_glm <- glm(AD_status_HAD ~ AB4240_ratio_c * ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter_glm)
print(vif_values)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main_glm, mmr_inter_glm, test = "LRT")

-------------
# P-tau181:

# P-tau181 Main Effects Model 
mmr_main2_glm <- glm(AD_status_HAD ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# Display results
summary(mmr_main2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main2_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main2_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main2_glm)
print(vif_values)

# P-tau181 Interaction Model
mmr_inter2_glm <- glm(AD_status_HAD ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter2_glm)
# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter2_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter2_glm_weighted)
print(pseudo_r2)
vif_values <- vif(mmr_inter2_glm)
print(vif_values)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main2_glm, mmr_inter2_glm, test = "LRT")

-------------
# P-tau217:

# P-tau217 Main Effects Model 
mmr_main3_glm <- glm(AD_status_HAD ~ PTAU217_logtr_c  + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# Display results
summary(mmr_main3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main3_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main3_glm)
print(vif_values)

# P-tau217 Interaction Model
mmr_inter3_glm <- glm(AD_status_HAD ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter3_glm)
# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter3_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter3_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter3_glm)
print(vif_values)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main3_glm, mmr_inter3_glm, test = "LRT")

-----------
# ApoE4:

# ApoE4 Main Effects Model 
mmr_main4_glm <- glm(AD_status_HAD ~ ApoE4_sqrt_c + ASCVD_cont_score_sqrtc, 
                     data = dataset, family = binomial)

# Display results
summary(mmr_main4_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_main4_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_main4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_main4_glm)
print(vif_values)

# ApoE4 Interaction Model
mmr_inter4_glm <- glm(AD_status_HAD ~ ApoE4_sqrt_c * ASCVD_cont_score_sqrtc, 
                      data = dataset, family = binomial)

# View the summary of the model
summary(mmr_inter4_glm)
# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(mmr_inter4_glm))
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(mmr_inter4_glm)
print(pseudo_r2)
vif_values <- vif(mmr_inter4_glm)
print(vif_values)

# Likelihood Ratio Test comparing main effects model vs interaction model
anova(mmr_main4_glm, mmr_inter4_glm, test = "LRT")

