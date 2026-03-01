
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 
  
# Date: 30/01/2025
----------------------------------------------------------------------------------

# Load libraries
library(readr)
library(psych)
library(ggplot2)
library(dplyr)

### Separate models for individual ASCVD risk factors for each biomarker

# AB42/40
glm_age <- glm(AD_status ~ AB4240_ratio_c + AGE, data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_age)
pseudo_r2 <- pR2(glm_age)
print(pseudo_r2)
exp(coef(glm_age))

glm_gender <- glm(AD_status ~ AB4240_ratio_c + Gender, data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_gender)
pseudo_r2 <- pR2(glm_gender)
print(pseudo_r2)
exp(coef(glm_gender))

glm_ethnic <- glm(AD_status ~ AB4240_ratio_c + Ethnicity_RiskScorescvd, 
                  data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_ethnic)
pseudo_r2 <- pR2(glm_ethnic)
print(pseudo_r2)
exp(coef(glm_ethnic))

glm_HDL <- glm(AD_status ~ AB4240_ratio_c + HDL_Cholesterol_mmolL, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_HDL)
pseudo_r2 <- pR2(glm_HDL)
print(pseudo_r2)
exp(coef(glm_HDL))

glm_LDL <- glm(AD_status ~ AB4240_ratio_c + LDL_Cholesterol, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_LDL)
pseudo_r2 <- pR2(glm_LDL)
print(pseudo_r2)
exp(coef(glm_LDL))

glm_totalchol <- glm(AD_status ~ AB4240_ratio_c + Total_cholesterol_mmolL, 
                     data = Complete_Dataset_Roche_Lilly_13122024, 
                     family = binomial)
summary(glm_totalchol)
pseudo_r2 <- pR2(glm_totalchol)
print(pseudo_r2)
exp(coef(glm_totalchol))

glm_smoking <- glm(AD_status ~ AB4240_ratio_c + Current_smoking_binary, data = Complete_Dataset_Roche_Lilly_13122024, 
                   family = binomial)
summary(glm_smoking)
pseudo_r2 <- pR2(glm_smoking)
print(pseudo_r2)
exp(coef(glm_smoking))

glm_sbp <- glm(AD_status ~ AB4240_ratio_c + Systolic_Blood_Pressure, 
               data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_sbp)
pseudo_r2 <- pR2(glm_sbp)
print(pseudo_r2)
exp(coef(glm_sbp))

glm_medHTN <- glm(AD_status ~ AB4240_ratio_c + ongoing_med_for_HBP, 
                  data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_medHTN)
pseudo_r2 <- pR2(glm_medHTN)
print(pseudo_r2)
exp(coef(glm_medHTN))

glm_diabetes <- glm(AD_status ~ AB4240_ratio_c + Diabetes_status, data = Complete_Dataset_Roche_Lilly_13122024, 
                    family = binomial)
summary(glm_diabetes)
pseudo_r2 <- pR2(glm_diabetes)
print(pseudo_r2)
exp(coef(glm_diabetes))

# P-tau181
glm_age <- glm(AD_status ~ pTau181_logtr_c + AGE, data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_age)
pseudo_r2 <- pR2(glm_age)
print(pseudo_r2)
exp(coef(glm_age))

glm_gender <- glm(AD_status ~ pTau181_logtr_c + Gender, data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_gender)
pseudo_r2 <- pR2(glm_gender)
print(pseudo_r2)
exp(coef(glm_gender))

glm_ethnic <- glm(AD_status ~ pTau181_logtr_c + Ethnicity_RiskScorescvd, 
                  data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_ethnic)
pseudo_r2 <- pR2(glm_ethnic)
print(pseudo_r2)
exp(coef(glm_ethnic))

glm_HDL <- glm(AD_status ~ pTau181_logtr_c + HDL_Cholesterol_mmolL, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_HDL)
pseudo_r2 <- pR2(glm_HDL)
print(pseudo_r2)
exp(coef(glm_HDL))

glm_LDL <- glm(AD_status ~ pTau181_logtr_c + LDL_Cholesterol, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_LDL)
pseudo_r2 <- pR2(glm_LDL)
print(pseudo_r2)
exp(coef(glm_LDL))

glm_totalchol <- glm(AD_status ~ pTau181_logtr_c + Total_cholesterol_mmolL, 
                     data = Complete_Dataset_Roche_Lilly_13122024, 
                     family = binomial)
summary(glm_totalchol)
pseudo_r2 <- pR2(glm_totalchol)
print(pseudo_r2)
exp(coef(glm_totalchol))

glm_smoking <- glm(AD_status ~ pTau181_logtr_c + Current_smoking_binary, data = Complete_Dataset_Roche_Lilly_13122024, 
                   family = binomial)
summary(glm_smoking)
pseudo_r2 <- pR2(glm_smoking)
print(pseudo_r2)
exp(coef(glm_smoking))

glm_sbp <- glm(AD_status ~ pTau181_logtr_c + Systolic_Blood_Pressure, 
               data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_sbp)
pseudo_r2 <- pR2(glm_sbp)
print(pseudo_r2)
exp(coef(glm_sbp))

glm_medHTN <- glm(AD_status ~ pTau181_logtr_c + ongoing_med_for_HBP, 
                  data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_medHTN)
pseudo_r2 <- pR2(glm_medHTN)
print(pseudo_r2)
exp(coef(glm_medHTN))

glm_diabetes <- glm(AD_status ~ pTau181_logtr_c + Diabetes_status, data = Complete_Dataset_Roche_Lilly_13122024, 
                    family = binomial)
summary(glm_diabetes)
pseudo_r2 <- pR2(glm_diabetes)
print(pseudo_r2)
exp(coef(glm_diabetes))

# P-tau217
glm_age <- glm(AD_status ~ PTAU217_logtr_c + AGE, data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_age)
pseudo_r2 <- pR2(glm_age)
print(pseudo_r2)
exp(coef(glm_age))

glm_gender <- glm(AD_status ~ PTAU217_logtr_c + Gender, data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_gender)
pseudo_r2 <- pR2(glm_gender)
print(pseudo_r2)
exp(coef(glm_gender))

glm_ethnic <- glm(AD_status ~ PTAU217_logtr_c + Ethnicity_RiskScorescvd, 
                  data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_ethnic)
pseudo_r2 <- pR2(glm_ethnic)
print(pseudo_r2)
exp(coef(glm_ethnic))

glm_HDL <- glm(AD_status ~ PTAU217_logtr_c + HDL_Cholesterol_mmolL, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_HDL)
pseudo_r2 <- pR2(glm_HDL)
print(pseudo_r2)
exp(coef(glm_HDL))

glm_LDL <- glm(AD_status ~ PTAU217_logtr_c + LDL_Cholesterol, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_LDL)
pseudo_r2 <- pR2(glm_LDL)
print(pseudo_r2)
exp(coef(glm_LDL))

glm_totalchol <- glm(AD_status ~ PTAU217_logtr_c + Total_cholesterol_mmolL, 
                     data = Complete_Dataset_Roche_Lilly_13122024, 
                     family = binomial)
summary(glm_totalchol)
pseudo_r2 <- pR2(glm_totalchol)
print(pseudo_r2)
exp(coef(glm_totalchol))

glm_smoking <- glm(AD_status ~ PTAU217_logtr_c + Current_smoking_binary, data = Complete_Dataset_Roche_Lilly_13122024, 
                   family = binomial)
summary(glm_smoking)
pseudo_r2 <- pR2(glm_smoking)
print(pseudo_r2)
exp(coef(glm_smoking))

glm_sbp <- glm(AD_status ~ PTAU217_logtr_c + Systolic_Blood_Pressure, 
               data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_sbp)
pseudo_r2 <- pR2(glm_sbp)
print(pseudo_r2)
exp(coef(glm_sbp))

glm_medHTN <- glm(AD_status ~ PTAU217_logtr_c + ongoing_med_for_HBP, 
                  data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_medHTN)
pseudo_r2 <- pR2(glm_medHTN)
print(pseudo_r2)
exp(coef(glm_medHTN))

glm_diabetes <- glm(AD_status ~ PTAU217_logtr_c + Diabetes_status, data = Complete_Dataset_Roche_Lilly_13122024, 
                    family = binomial)
summary(glm_diabetes)
pseudo_r2 <- pR2(glm_diabetes)
print(pseudo_r2)
exp(coef(glm_diabetes))

# ApoE4
glm_age <- glm(AD_status ~ ApoE4_sqrt_c + AGE, data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_age)
pseudo_r2 <- pR2(glm_age)
print(pseudo_r2)
exp(coef(glm_age))

glm_gender <- glm(AD_status ~ ApoE4_sqrt_c + Gender, data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_gender)
pseudo_r2 <- pR2(glm_gender)
print(pseudo_r2)
exp(coef(glm_gender))

glm_ethnic <- glm(AD_status ~ ApoE4_sqrt_c + Ethnicity_RiskScorescvd, 
                  data = Complete_Dataset_Roche_Lilly_13122024, 
                  family = binomial)
summary(glm_ethnic)
pseudo_r2 <- pR2(glm_ethnic)
print(pseudo_r2)
exp(coef(glm_ethnic))

glm_HDL <- glm(AD_status ~ ApoE4_sqrt_c + HDL_Cholesterol_mmolL, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_HDL)
pseudo_r2 <- pR2(glm_HDL)
print(pseudo_r2)
exp(coef(glm_HDL))

glm_LDL <- glm(AD_status ~ ApoE4_sqrt_c + LDL_Cholesterol, 
               data = Complete_Dataset_Roche_Lilly_13122024, 
               family = binomial)
summary(glm_LDL)
pseudo_r2 <- pR2(glm_LDL)
print(pseudo_r2)
exp(coef(glm_LDL))

glm_totalchol <- glm(AD_status ~ ApoE4_sqrt_c + Total_cholesterol_mmolL, 
                     data = Complete_Dataset_Roche_Lilly_13122024, 
                     family = binomial)
summary(glm_totalchol)
pseudo_r2 <- pR2(glm_totalchol)
print(pseudo_r2)
exp(coef(glm_totalchol))

glm_smoking <- glm(AD_status ~ ApoE4_sqrt_c + Current_smoking_binary, data = Complete_Dataset_Roche_Lilly_13122024, 
                   family = binomial)
summary(glm_smoking)
pseudo_r2 <- pR2(glm_smoking)
print(pseudo_r2)
exp(coef(glm_smoking))

glm_sbp <- glm(AD_status ~ ApoE4_sqrt_c + Systolic_Blood_Pressure, 
               data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_sbp)
pseudo_r2 <- pR2(glm_sbp)
print(pseudo_r2)
exp(coef(glm_sbp))

glm_medHTN <- glm(AD_status ~ ApoE4_sqrt_c + ongoing_med_for_HBP, 
                  data = Complete_Dataset_Roche_Lilly_13122024, family = binomial)
summary(glm_medHTN)
pseudo_r2 <- pR2(glm_medHTN)
print(pseudo_r2)
exp(coef(glm_medHTN))

glm_diabetes <- glm(AD_status ~ ApoE4_sqrt_c + Diabetes_status, data = Complete_Dataset_Roche_Lilly_13122024, 
                    family = binomial)
summary(glm_diabetes)
pseudo_r2 <- pR2(glm_diabetes)
print(pseudo_r2)
exp(coef(glm_diabetes))
