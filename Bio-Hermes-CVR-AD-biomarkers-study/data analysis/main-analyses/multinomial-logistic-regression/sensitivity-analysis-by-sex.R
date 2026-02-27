
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 
  
# Period: December 2024
----------------------------------------------------------------------------------
  
# Load libraries
library(readr)
library(psych)
library(ggplot2)
library(dplyr)

# --------------------------------------------------------------------------------
### Sensitivity Analyses - Run only for models with significant interaction terms 

## Make sure all datasets are properly loaded before running code. 

# First, split data according to sex
male_data <- Complete_Dataset_Roche_Lilly_13122024 %>% filter(SEX_recode == 1)
female_data <- Complete_Dataset_Roche_Lilly_13122024 %>% filter(SEX_recode == 0)


## P-tau 181 Main Effects Model 
men_main2_glm <- glm(AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = male_data, family = binomial)
women_main2_glm <- glm(AD_status ~ pTau181_logtr_c + ASCVD_cont_score_sqrtc, 
                       data = female_data, family = binomial)

# View the summary of the model
summary(men_main2_glm)
summary(women_main2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(men_main2_glm))
odds_ratios <- exp(coef(women_main2_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(men_main2_glm)
pseudo_r2 <- pR2(women_main2_glm)
print(pseudo_r2)
vif_values <- vif(men_main2_glm)
vif_values <- vif(women_main2_glm)
print(vif_values)

## P-tau 181 Interaction Model
men_inter2_glm <- glm(AD_status ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = male_data, family = binomial)
women_inter2_glm <- glm(AD_status ~ pTau181_logtr_c * ASCVD_cont_score_sqrtc, 
                        data = female_data, family = binomial)

# View the summary of the model
summary(men_inter2_glm)
summary(women_inter2_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(men_inter2_glm))
odds_ratios <- exp(coef(women_inter2_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(men_inter2_glm)
print(pseudo_r2)
vif_values <- vif(men_inter2_glm)
print(vif_values)
pseudo_r2 <- pR2(women_inter2_glm)
print(pseudo_r2)
vif_values <- vif(women_inter2_glm)
print(vif_values)

## P-tau 217 Main Effects Model 
men_main3_glm <- glm(AD_status ~ PTAU217_logtr_c + ASCVD_cont_score_sqrtc, 
                     data = male_data, family = binomial)
women_main3_glm <- glm(AD_status ~ PTAU217_logtr_c + ASCVD_cont_score_sqrtc, 
                       data = female_data, family = binomial)

# View the summary of the model
summary(men_main3_glm)
summary(women_main3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(men_main3_glm))
odds_ratios <- exp(coef(women_main3_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(men_main3_glm)
pseudo_r2 <- pR2(women_main3_glm)
print(pseudo_r2)
vif_values <- vif(men_main3_glm)
vif_values <- vif(women_main3_glm)
print(vif_values)

## P-tau 217 Interaction Model
men_inter3_glm <- glm(AD_status ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc, 
                      data = male_data, family = binomial)
women_inter3_glm <- glm(AD_status ~ PTAU217_logtr_c * ASCVD_cont_score_sqrtc, 
                        data = female_data, family = binomial)

# View the summary of the model
summary(men_inter3_glm)
summary(women_inter3_glm)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coef(men_inter3_glm))
odds_ratios <- exp(coef(women_inter3_glm))

# Print the odds ratios
print(odds_ratios)

# McFadden's Pseudo R-squared
pseudo_r2 <- pR2(men_inter3_glm)
pseudo_r2 <- pR2(women_inter3_glm)
print(pseudo_r2)
vif_values <- vif(men_inter3_glm)
vif_values <- vif(women_inter3_glm)
print(vif_values)

# Save dataset
write.csv(Complete_Dataset_Roche_Lilly_13122024, "Complete_Dataset_Roche_Lilly_13122024.csv", 
          row.names = FALSE)

## P-tau 217 only
mmr_ptau217 <- glm(AD_status ~ PTAU217_logtr_c, 
                   data = dataset, family = binomial)

# View the summary of the model
summary(mmr_ptau217)