
--------------------------------------------------------------------------
  # PhD Project 2025: Lothian Birth Cohort + WMH volume SEM analysis 
  
  # Author: Angelina Kancheva 
  # Date/Period: Summer 2025
--------------------------------------------------------------------------
  
# Install/load libraries
library(readr)
library(readxl)
library(haven)
library(lavaan)
library(psych)

### Exploring SEM Analyses

### Building the Structural Model 

# Reload LBC data (if needed)
lbc_data <- read.csv("C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.csv",
                     row.names = FALSE)

library(lavaan)
LBC_structural_model <- '

  # Measurement models
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2

  wellbeing =~ wemwbs1_w2 + wemwbs2_w2 + wemwbs3_w2 + wemwbs4_w2 + wemwbs5_w2 +
               wemwbs6_w2 + wemwbs7_w2 + wemwbs8_w2 + wemwbs9_w2 + wemwbs10_w2 +
               wemwbs11_w2 + wemwbs12_w2 + wemwbs13_w2 + wemwbs14_w2

  # Residual covariances for wellbeing
  wemwbs9_w2 ~~ wemwbs12_w2
  wemwbs4_w2 ~~ wemwbs9_w2
  wemwbs6_w2 ~~ wemwbs7_w2
  wemwbs7_w2 ~~ wemwbs11_w2

  optimism =~ lotr1_w2 + lotr4_w2 + lotr10_w2
  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev

  sense_coherence =~ socunderstand_rev + sochopeless_w2 + socsatisfaction_w2

  perceived_support =~ socsupporthappy_w2 + socsupportloved_w2 + socsupportrelied_w2 +
                       socsupportcare_w2 + socsupportaccept_w2 + socsupportimpor_w2 +
                       socsupportencour_w2

  # Attitudes to Ageing
  Psychosocial_Loss =~ eaaq3_rv + eaaq6_rv + eaaq9_rv + eaaq12_rv + 
                       eaaq15_rv + eaaq17_rv + eaaq20_rv + eaaq22_rv
  Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2
  Psychological_Growth =~ eaaq1_w2 + eaaq2_w2 + eaaq4_w2 + eaaq5_w2 + 
                          eaaq10_w2 + eaaq18_w2 + eaaq19_w2 + eaaq21_w2

  eaaq18_w2 ~~ eaaq21_w2
  eaaq3_rv ~~ eaaq6_rv
  eaaq17_rv ~~ eaaq20_rv

  # Structural paths (H1–H5)
  satisfaction ~ log_WMHICV_ratio
  wellbeing ~ log_WMHICV_ratio
  optimism ~ log_WMHICV_ratio
  pessimism ~ log_WMHICV_ratio
  sense_coherence ~ log_WMHICV_ratio
  perceived_support ~ log_WMHICV_ratio
  Psychosocial_Loss ~ log_WMHICV_ratio
  Physical_Change ~ log_WMHICV_ratio
  Psychological_Growth ~ log_WMHICV_ratio

  numteeth_w2 ~ log_WMHICV_ratio        # H2: Dentition
  eGFR_binary ~ log_WMHICV_ratio        # H3: Renal function
  va_composite ~ log_WMHICV_ratio       # H4: Visual acuity
  demspn_w2 ~ log_WMHICV_ratio          # H5: Mobility
'

fit_LBC_structural <- sem(model = LBC_structural_model, 
                          data = lbc_data, 
                          estimator = "MLR", # robust maximum likelihood 
                          missing = "fiml")  # handles missingness directly under the assumption of MAR

summary(fit_LBC_structural, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Inspect large variances
varTable(fit_LBC_structural)


# ---Very small variance for the VA composite variable. Re-run using two separate VA variables for 
# left and right eye instead---

# Unadjusted model first

------------------------------------------------------
### This will correspond to MODEL 1 in my PhD thesis.
------------------------------------------------------

LBC_structural_model <- '

  # Measurement models
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2

  wellbeing =~ wemwbs1_w2 + wemwbs2_w2 + wemwbs3_w2 + wemwbs4_w2 + wemwbs5_w2 +
               wemwbs6_w2 + wemwbs7_w2 + wemwbs8_w2 + wemwbs9_w2 + wemwbs10_w2 +
               wemwbs11_w2 + wemwbs12_w2 + wemwbs13_w2 + wemwbs14_w2

  # Residual covariances for wellbeing
  wemwbs9_w2 ~~ wemwbs12_w2
  wemwbs4_w2 ~~ wemwbs9_w2
  wemwbs6_w2 ~~ wemwbs7_w2
  wemwbs7_w2 ~~ wemwbs11_w2

  optimism =~ lotr1_w2 + lotr4_w2 + lotr10_w2
  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev

  sense_coherence =~ socunderstand_rev + sochopeless_w2 + socsatisfaction_w2

  perceived_support =~ socsupporthappy_w2 + socsupportloved_w2 + socsupportrelied_w2 +
                       socsupportcare_w2 + socsupportaccept_w2 + socsupportimpor_w2 +
                       socsupportencour_w2

  # Attitudes to Ageing
  Psychosocial_Loss =~ eaaq3_rv + eaaq6_rv + eaaq9_rv + eaaq12_rv + 
                       eaaq15_rv + eaaq17_rv + eaaq20_rv + eaaq22_rv
  Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2
  Psychological_Growth =~ eaaq1_w2 + eaaq2_w2 + eaaq4_w2 + eaaq5_w2 + 
                          eaaq10_w2 + eaaq18_w2 + eaaq19_w2 + eaaq21_w2

  eaaq18_w2 ~~ eaaq21_w2
  eaaq3_rv ~~ eaaq6_rv
  eaaq17_rv ~~ eaaq20_rv

  # Structural paths (H1–H5)
  satisfaction ~ log_WMHICV_ratio
  wellbeing ~ log_WMHICV_ratio
  optimism ~ log_WMHICV_ratio
  pessimism ~ log_WMHICV_ratio
  sense_coherence ~ log_WMHICV_ratio
  perceived_support ~ log_WMHICV_ratio
  Psychosocial_Loss ~ log_WMHICV_ratio
  Physical_Change ~ log_WMHICV_ratio
  Psychological_Growth ~ log_WMHICV_ratio

  numteeth_w2 ~ log_WMHICV_ratio        # H2: Dentition
  eGFR_binary ~ log_WMHICV_ratio        # H3: Renal function
  visl_z ~ log_WMHICV_ratio             # H4a: Visual acuity left
  visr_z ~ log_WMHICV_ratio             # H4b: Visual acuity right
  demspn_w2 ~ log_WMHICV_ratio          # H5: Mobility
'

fit_LBC_structural <- sem(model = LBC_structural_model, 
                          data = lbc_data, 
                          estimator = "MLR", # robust maximum likelihood 
                          missing = "fiml", # handles missingness directly under the assumption of MAR
                          fixed.x = FALSE) # enables full maximum likelihood

summary(fit_LBC_structural, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Create plots
# First, load additional libraries if needed
install.packages("lavaanPlot")  # if not yet installed
library(lavaanPlot)

lavaanPlot(model = fit_LBC_structural,
           stand = TRUE,
           coefs = TRUE,
           covs = TRUE,
           stars = "regress",
           sig = 0.05)

library(dplyr)
library(ggplot2)

colnames(parameterEstimates(fit_LBC_structural))

estimates <- parameterEstimates(fit_LBC_structural, standardized = TRUE) %>%
  filter(op == "~", rhs == "log_WMHICV_ratio") %>%
  select(lhs, est, se, pvalue, std.all) %>%
  mutate(Significant = pvalue < 0.05,
         Label = c(
           satisfaction = "Satisfaction with Life",
           wellbeing = "Mental Wellbeing",
           optimism = "Optimism",
           pessimism = "Pessimism",
           sense_coherence = "Sense of Coherence",
           perceived_support = "Perceived Social Support",
           Psychosocial_Loss = "Psychosocial Loss (Attitudes to Ageing)",
           Physical_Change = "Physical Change (Attitudes to Ageing)",
           Psychological_Growth = "Psychological Growth (Attitudes to Ageing)",
           numteeth_w2 = "Number of Teeth",
           eGFR_binary = "eGFR (Impaired Kidney Function)",
           visl_z = "Visual Acuity (Left Eye)",
           visr_z = "Visual Acuity (Right Eye)",
           demspn_w2 = "Mobility (Demi-span)"
         )[lhs])


ggplot(estimates, aes(x = reorder(Label, std.all), y = std.all, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = std.all - 1.96 * se, ymax = std.all + 1.96 * se),
                width = 0.2, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("TRUE" = "tomato", "FALSE" = "black")) +
  coord_flip() +
  labs(
    x = "",
    y = "Standardized β (Effect of WMH Burden)",
    title = "Unadjusted Associations Between WMH Burden and Clinical Outcomes",
    color = "p < .05"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

# RESULTS: Only two significant associations: with fewer teeth and demi-span.

# Now, add COVARIATES: first, age and sex only.

# Inspect age again
describe(lbc_data$agedays_w2) # N=809; mean=26475.05; SD=257.5
hist(lbc_data$agedays_w2) # normal

describe(lbc_data$ageMRI_w2) # N=682; mean=26542.84; SD=262.89
hist(lbc_data$ageMRI_w2) # mostly normal

# Inspect sex again
table(lbc_data$sex)
table(lbc_data$sex_recode) # 0=518 (male); 1=516 (female)

# Adjusted SEM (age + sex)
LBC_structural_model_cov <- '

  # Measurement models
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2

  wellbeing =~ wemwbs1_w2 + wemwbs2_w2 + wemwbs3_w2 + wemwbs4_w2 + wemwbs5_w2 +
               wemwbs6_w2 + wemwbs7_w2 + wemwbs8_w2 + wemwbs9_w2 + wemwbs10_w2 +
               wemwbs11_w2 + wemwbs12_w2 + wemwbs13_w2 + wemwbs14_w2

  # Residual covariances for wellbeing
  wemwbs9_w2 ~~ wemwbs12_w2
  wemwbs4_w2 ~~ wemwbs9_w2
  wemwbs6_w2 ~~ wemwbs7_w2
  wemwbs7_w2 ~~ wemwbs11_w2

  optimism =~ lotr1_w2 + lotr4_w2 + lotr10_w2
  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev

  sense_coherence =~ socunderstand_rev + sochopeless_w2 + socsatisfaction_w2

  perceived_support =~ socsupporthappy_w2 + socsupportloved_w2 + socsupportrelied_w2 +
                       socsupportcare_w2 + socsupportaccept_w2 + socsupportimpor_w2 +
                       socsupportencour_w2

  Psychosocial_Loss =~ eaaq3_rv + eaaq6_rv + eaaq9_rv + eaaq12_rv + 
                       eaaq15_rv + eaaq17_rv + eaaq20_rv + eaaq22_rv
  Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2
  Psychological_Growth =~ eaaq1_w2 + eaaq2_w2 + eaaq4_w2 + eaaq5_w2 + 
                          eaaq10_w2 + eaaq18_w2 + eaaq19_w2 + eaaq21_w2

  eaaq18_w2 ~~ eaaq21_w2
  eaaq3_rv ~~ eaaq6_rv
  eaaq17_rv ~~ eaaq20_rv

  # Structural paths (adjusted)
  satisfaction ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  wellbeing ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  optimism ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  pessimism ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  sense_coherence ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  perceived_support ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  Psychosocial_Loss ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  Physical_Change ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  Psychological_Growth ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode

  numteeth_w2 ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  eGFR_binary ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  visl_z ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  visr_z ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
  demspn_w2 ~ log_WMHICV_ratio + ageMRI_w2 + sex_recode
'

fit_LBC_structural_cov <- sem(model = LBC_structural_model_cov,
                              data = lbc_data,
                              estimator = "MLR",
                              missing = "fiml",
                              fixed.x = FALSE)

summary(fit_LBC_structural_cov, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Model does not seem to fit very well. Check for multicollinearity.

cor(lbc_data[, c("log_WMHICV_ratio", "ageMRI_w2", "sex_recode")], use = "pairwise.complete.obs")

# Correlation is not too bad.

# Standardise age MRI assessment
lbc_data$ageMRI_z <- scale(lbc_data$ageMRI_w2)
# Standardize WMH log variable
lbc_data$WMHICVratio_z <- scale(lbc_data$log_WMHICV_ratio)
hist(lbc_data$WMHICVratio_z)

# Re-run model

------------------------------------------------------
### This will correspond to MODEL 2 in my PhD thesis.
------------------------------------------------------

library(lavaan)
LBC_structural_model_cov <- '

  # Measurement models
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2

  wellbeing =~ wemwbs1_w2 + wemwbs2_w2 + wemwbs3_w2 + wemwbs4_w2 + wemwbs5_w2 +
               wemwbs6_w2 + wemwbs7_w2 + wemwbs8_w2 + wemwbs9_w2 + wemwbs10_w2 +
               wemwbs11_w2 + wemwbs12_w2 + wemwbs13_w2 + wemwbs14_w2

  wemwbs9_w2 ~~ wemwbs12_w2
  wemwbs4_w2 ~~ wemwbs9_w2
  wemwbs6_w2 ~~ wemwbs7_w2
  wemwbs7_w2 ~~ wemwbs11_w2

  optimism =~ lotr1_w2 + lotr4_w2 + lotr10_w2
  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev

  sense_coherence =~ socunderstand_rev + sochopeless_w2 + socsatisfaction_w2

  perceived_support =~ socsupporthappy_w2 + socsupportloved_w2 + socsupportrelied_w2 +
                       socsupportcare_w2 + socsupportaccept_w2 + socsupportimpor_w2 +
                       socsupportencour_w2

  Psychosocial_Loss =~ eaaq3_rv + eaaq6_rv + eaaq9_rv + eaaq12_rv + 
                       eaaq15_rv + eaaq17_rv + eaaq20_rv + eaaq22_rv

  Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2

  Psychological_Growth =~ eaaq1_w2 + eaaq2_w2 + eaaq4_w2 + eaaq5_w2 + 
                          eaaq10_w2 + eaaq18_w2 + eaaq19_w2 + eaaq21_w2

  eaaq18_w2 ~~ eaaq21_w2
  eaaq3_rv ~~ eaaq6_rv
  eaaq17_rv ~~ eaaq20_rv

  satisfaction ~ WMHICVratio_z + ageMRI_z + sex_recode
  wellbeing ~ WMHICVratio_z + ageMRI_z + sex_recode
  optimism ~ WMHICVratio_z + ageMRI_z + sex_recode
  pessimism ~ WMHICVratio_z + ageMRI_z + sex_recode
  sense_coherence ~ WMHICVratio_z + ageMRI_z + sex_recode
  perceived_support ~ WMHICVratio_z + ageMRI_z + sex_recode
  Psychosocial_Loss ~ WMHICVratio_z + ageMRI_z + sex_recode
  Physical_Change ~ WMHICVratio_z + ageMRI_z + sex_recode
  Psychological_Growth ~ WMHICVratio_z + ageMRI_z + sex_recode

  numteeth_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
  eGFR_binary ~ WMHICVratio_z + ageMRI_z + sex_recode
  visl_z ~ WMHICVratio_z + ageMRI_z + sex_recode
  visr_z ~ WMHICVratio_z + ageMRI_z + sex_recode
  demspn_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
'

fit_LBC_structural_cov <- sem(model = LBC_structural_model_cov,
                              data = lbc_data,
                              estimator = "MLR",
                              missing = "fiml",
                              fixed.x = FALSE)

summary(fit_LBC_structural_cov, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Simplified model (but still adjusted for age and sex)

------------------------------------------------------
### This will correspond to MODEL 3 in my PhD thesis.
------------------------------------------------------

LBC_trimmed_model_adj <- '

  # Measurement models
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2

  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev

  Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2

  # Structural regressions (adjusted for age and sex)
  satisfaction ~ WMHICVratio_z + ageMRI_z + sex_recode
  pessimism ~ WMHICVratio_z + ageMRI_z + sex_recode
  Physical_Change ~ WMHICVratio_z + ageMRI_z + sex_recode

  visl_z ~ WMHICVratio_z + ageMRI_z + sex_recode
  visr_z ~ WMHICVratio_z + ageMRI_z + sex_recode
  demspn_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
  numteeth_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
  eGFR_binary ~ WMHICVratio_z + ageMRI_z + sex_recode

  # Covariances
  visl_z ~~ visr_z
'

fit_LBC_trimmed <- sem(model = LBC_trimmed_model_adj,
                       data = lbc_data,
                       estimator = "MLR",
                       missing = "fiml",
                       fixed.x = FALSE)

summary(fit_LBC_trimmed, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Try unadjusted and trimmed + add six minute walk test and uncorrected VA data (transformed and scaled)

LBC_trimmed_model2_unadj <- '

  # Measurement models
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2

  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev

  Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2

  # Structural regressions (unadjusted)
  satisfaction ~ WMHICVratio_z 
  pessimism ~ WMHICVratio_z 
  Physical_Change ~ WMHICVratio_z 

  visr_u_sqrtz ~ WMHICVratio_z 
  visl_u_sqrtz ~ WMHICVratio_z 
  demspn_w2 ~ WMHICVratio_z 
  sixmwk_w2_logz ~ WMHICVratio_z 
  numteeth_w2 ~ WMHICVratio_z 
  eGFR_binary ~ WMHICVratio_z 

  # Covariances
  visr_u_sqrtz ~~ visl_u_sqrtz
'

fit_LBC_trimmed_model2_unadj <- sem(model = LBC_trimmed_model2_unadj,
                                    data = lbc_data,
                                    estimator = "MLR",
                                    missing = "fiml",
                                    fixed.x = FALSE)

summary(fit_LBC_trimmed_model2_unadj, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Try adjusted and trimmed + add six minute walk test and uncorrected VA data (transformed and scaled)

------------------------------------------------------
### This will correspond to MODEL 4 in my PhD thesis.
------------------------------------------------------

LBC_trimmed_model2_adj <- '

  # Measurement models
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2

  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev

  Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2

  # Structural regressions (adjusted for age and sex)
  satisfaction ~ WMHICVratio_z + ageMRI_z + sex_recode
  pessimism ~ WMHICVratio_z + ageMRI_z + sex_recode
  Physical_Change ~ WMHICVratio_z + ageMRI_z + sex_recode

  visr_u_sqrtz ~ WMHICVratio_z + ageMRI_z + sex_recode
  visl_u_sqrtz ~ WMHICVratio_z + ageMRI_z + sex_recode
  demspn_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
  sixmwk_w2_logz ~ WMHICVratio_z + ageMRI_z + sex_recode
  numteeth_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
  eGFR_binary ~ WMHICVratio_z + ageMRI_z + sex_recode

  # Covariances
  visr_u_sqrtz ~~ visl_u_sqrtz
'

fit_LBC_trimmed_model2_adj <- sem(model = LBC_trimmed_model2_adj,
                                  data = lbc_data,
                                  estimator = "MLR",
                                  missing = "fiml",
                                  fixed.x = FALSE)

summary(fit_LBC_trimmed_model2_adj, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

---------------------------------------------------------------------------------------------------

### 01/07/2025: More experimentation with modelling

# Test the association with eGFR on its own:
lm_eGFR <- lm(eGFR_binary ~ WMHICVratio_z, data = lbc_data)
summary(lm_eGFR)

exp(coef(lm_eGFR))

library(lavaan)

# Check model fit with modification indices
modindices(fit_LBC_trimmed_model2_adj, sort.=TRUE, minimum.value=10)

# There are several residual covariances, which have to be explicitly modeled. Thus:

LBC_trimmed_model2_adj_rc <- '

  # Measurement models
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2

  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev

  Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2

  # Structural regressions (adjusted for age and sex)
  satisfaction ~ WMHICVratio_z + ageMRI_z + sex_recode
  pessimism ~ WMHICVratio_z + ageMRI_z + sex_recode
  Physical_Change ~ WMHICVratio_z + ageMRI_z + sex_recode

  visr_u_sqrtz ~ WMHICVratio_z + ageMRI_z + sex_recode
  visl_u_sqrtz ~ WMHICVratio_z + ageMRI_z + sex_recode
  demspn_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
  sixmwk_w2_logz ~ WMHICVratio_z + ageMRI_z + sex_recode
  numteeth_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
  eGFR_binary ~ WMHICVratio_z + ageMRI_z + sex_recode

  # Residual covariances between SWLS items (within satisfaction factor)
  swls1_w2 ~~ swls2_w2
  swls1_w2 ~~ swls2_w2
  swls4_w2 ~~ swls5_w2
  swls2_w2 ~~ swls4_w2
  swls1_w2 ~~ swls4_w2
  swls3_w2 ~~ swls4_w2
  visr_u_sqrtz ~~ visl_u_sqrtz
'

fit_LBC_trimmed_model2_adj_rc <- sem(model = LBC_trimmed_model2_adj_rc,
                                     data = lbc_data,
                                     estimator = "MLR",
                                     missing = "fiml",
                                     fixed.x = FALSE)

summary(fit_LBC_trimmed_model2_adj_rc, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Check residual variance-covariance matrix
lavInspect(fit_LBC_trimmed_model2_adj_rc, "theta")

# Check for multicollinearity among indicators
inspect(fit_LBC_trimmed_model2_adj, "cor.ov")

# Or get modification indices to guide trimming
modindices(fit_LBC_trimmed_model2_adj, sort. = TRUE, minimum.value = 10)

# Model became more unstable. Only retain one or two residual covariances instead:

LBC_trimmed_model2_adj_rc2 <- '

  # Measurement models
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2

  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev

  Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2

  # Structural regressions (adjusted for age and sex)
  satisfaction ~ WMHICVratio_z + ageMRI_z + sex_recode
  pessimism ~ WMHICVratio_z + ageMRI_z + sex_recode
  Physical_Change ~ WMHICVratio_z + ageMRI_z + sex_recode

  visr_u_sqrtz ~ WMHICVratio_z + ageMRI_z + sex_recode
  visl_u_sqrtz ~ WMHICVratio_z + ageMRI_z + sex_recode
  demspn_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
  sixmwk_w2_logz ~ WMHICVratio_z + ageMRI_z + sex_recode
  numteeth_w2 ~ WMHICVratio_z + ageMRI_z + sex_recode
  eGFR_binary ~ WMHICVratio_z + ageMRI_z + sex_recode

  # Residual covariances between SWLS items (within satisfaction factor)
  swls1_w2 ~~ swls2_w2
  visr_u_sqrtz ~~ visl_u_sqrtz
'

fit_LBC_trimmed_model2_adj_rc2 <- sem(model = LBC_trimmed_model2_adj_rc2,
                                      data = lbc_data,
                                      estimator = "MLR",
                                      missing = "fiml",
                                      fixed.x = FALSE)

summary(fit_LBC_trimmed_model2_adj_rc2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# TRY RUNNING THIS MODEL TO SEE IF MODEL FIT IMPROVES. THINK ABOUT WHETHER YOU WANT TO CHANGE THE 
# MODEL ESTIMATOR OR ANY OF THE OTHER PARAMETERS!

### All models are somewhat preliminary and further experimentation and tweaking is possible. 
