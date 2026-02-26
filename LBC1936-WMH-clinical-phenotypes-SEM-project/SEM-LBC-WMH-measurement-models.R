
--------------------------------------------------------------------------
# PhD Project 2025: Lothian Birth Cohort + WMH volume SEM analysis 
  
# Author: Angelina Kancheva 
# Date/Period: Summer 2025
--------------------------------------------------------------------------
  
# Install/load libraries
library(readr)
library(readxl)
library(haven)
install.packages("lavaan")
library(lavaan)
library(psych)

### Exploring SEM Analyses

## First, exclude some people that need excluded

# Load LBC data
lbc_data <- read.csv("C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.csv",
                     row.names = FALSE)

# Stroke
table(lbc_data$stroke_w2) # history of stroke (809 No, 55 Yes)
# Remove positive cases from the sample and save with same name
lbc_data <- subset(lbc_data, stroke_w2 != 1 | is.na(stroke_w2)) # Now, N=809 (no history of stroke)

# Save data in R
save(lbc_data, file = "C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.RData")
# Save as csv also
write.csv(lbc_data, file = "C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.csv", row.names = FALSE)

# Dementia
table(lbc_data$dement_w2) # dementia
describe(lbc_data$dement_w2) # N=866; N=2 have dementia
which(lbc_data$dement_w2 == 1)
# Remove positive cases from the sample and save with same name
lbc_data <- subset(lbc_data, dement_w2 != 1 | is.na(dement_w2)) # Now, N=864 (dementia-free)

# Save data in R
save(lbc_data, file = "C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.RData")
# Save as csv also
write.csv(lbc_data, file = "C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.csv", row.names = FALSE)


### 26/06/2025: Measurement Models with Latent Variables 

View(lbc_data)

## Satisfaction with Life
satisfaction_model <- '
  satisfaction =~ swls1_w2 + swls2_w2 + swls3_w2 + swls4_w2 + swls5_w2
'

fit_satisfaction <- cfa(satisfaction_model, data = lbc_data, missing = "ML")
summary(fit_satisfaction, fit.measures = TRUE, standardized = TRUE)

## Mental Wellbeing
mental_wellbeing_model <- '
  wellbeing =~ wemwbs1_w2 + wemwbs2_w2 + wemwbs3_w2 + wemwbs4_w2 + wemwbs5_w2 +
               wemwbs6_w2 + wemwbs7_w2 + wemwbs8_w2 + wemwbs9_w2 + wemwbs10_w2 +
               wemwbs11_w2 + wemwbs12_w2 + wemwbs13_w2 + wemwbs14_w2
'

fit_wellbeing <- cfa(mental_wellbeing_model, data = lbc_data, missing = "ML")
summary(fit_wellbeing, fit.measures = TRUE, standardized = TRUE)


# Model fit is poor, hence checking for residual correlations
modindices(fit_wellbeing, sort = TRUE, minimum.value = 10)


# Freeing the top 2-4 residuals and re-running the model
mental_wellbeing_model_v2 <- '
  wellbeing =~ wemwbs1_w2 + wemwbs2_w2 + wemwbs3_w2 + wemwbs4_w2 + wemwbs5_w2 +
               wemwbs6_w2 + wemwbs7_w2 + wemwbs8_w2 + wemwbs9_w2 + wemwbs10_w2 +
               wemwbs11_w2 + wemwbs12_w2 + wemwbs13_w2 + wemwbs14_w2

  # Residual correlations based on highest MIs
  wemwbs9_w2 ~~ wemwbs12_w2
  wemwbs4_w2 ~~ wemwbs9_w2
  wemwbs6_w2 ~~ wemwbs7_w2
  wemwbs7_w2 ~~ wemwbs11_w2
'

fit_wellbeing_v2 <- cfa(mental_wellbeing_model_v2, 
                        data = lbc_data, 
                        missing = "ML")

summary(fit_wellbeing_v2, fit.measures = TRUE, standardized = TRUE)

# Model fit is much better now.


## Optimism

# First, reverse-code the pessimism items
lbc_data$lotr3_rev <- 6 - lbc_data$lotr3_w2
lbc_data$lotr7_rev <- 6 - lbc_data$lotr7_w2
lbc_data$lotr9_rev <- 6 - lbc_data$lotr9_w2

# ---For this factor, I will remove the filler items and only model the optimism/pessimism ones.---
# ---Recoded the pessimism items so that higher scores on all items indicate higher optimism.--- 

table(lbc_data$lotr3_w2, lbc_data$lotr3_rev, useNA = "ifany")
table(lbc_data$lotr7_w2, lbc_data$lotr7_rev, useNA = "ifany")
table(lbc_data$lotr9_w2, lbc_data$lotr9_rev, useNA = "ifany")

# Now, fit the model without the filler items and with the recoded items
optimism_model <- '
  optimism =~ lotr1_w2 + lotr4_w2 + lotr10_w2 + lotr3_rev + lotr7_rev + lotr9_rev
'

fit_optimism <- cfa(optimism_model, 
                    data = lbc_data, 
                    missing = "ML")

summary(fit_optimism, fit.measures = TRUE, standardized = TRUE)

# Fit is quite poor, hence fitting a two-factor model instead.

optimism_model_2f <- '
  optimism =~ lotr1_w2 + lotr4_w2 + lotr10_w2
  pessimism =~ lotr3_rev + lotr7_rev + lotr9_rev
'

fit_optimism_2f <- cfa(
  optimism_model_2f,
  data = lbc_data,
  missing = "ML"
)

summary(fit_optimism_2f, fit.measures = TRUE, standardized = TRUE)

# Fit is much better now: improved!

## Sense of Coherence

lbc_data$socunderstand_rev <- ifelse(is.na(lbc_data$socunderstand_w2), NA,
                                     ifelse(lbc_data$socunderstand_w2 == 0, 2,
                                            ifelse(lbc_data$socunderstand_w2 == 1, 1,
                                                   ifelse(lbc_data$socunderstand_w2 == 2, 0, NA))))
# keep the others unchanged (i.e., use them directly)


# Define the model
soc_model <- '
  sense_coherence =~ socunderstand_rev + sochopeless_w2 + socsatisfaction_w2
'

soc_fit <- cfa(soc_model, data = lbc_data, missing = "ML")
summary(soc_fit, fit.measures = TRUE, standardized = TRUE)

## Public Attitudes and Beliefs - Leave for now, potentially return to it later.

## Perceived Social Support
perceived_support_model <- '
  perceived_support =~ socsupporthappy_w2 + socsupportloved_w2 + socsupportrelied_w2 +
                       socsupportcare_w2 + socsupportaccept_w2 + socsupportimpor_w2 +
                       socsupportencour_w2
'

fit_support <- cfa(perceived_support_model, data = lbc_data, missing = "ML")
summary(fit_support, fit.measures = TRUE, standardized = TRUE)

## Attitudes to Ageing

# Reverse-code negatively framed F1 items before fitting a model
lbc_data$eaaq3_rv  <- 6 - lbc_data$eaaq3_w2
lbc_data$eaaq6_rv  <- 6 - lbc_data$eaaq6_w2
lbc_data$eaaq9_rv  <- 6 - lbc_data$eaaq9_w2
lbc_data$eaaq12_rv <- 6 - lbc_data$eaaq12_w2
lbc_data$eaaq15_rv <- 6 - lbc_data$eaaq15_w2
lbc_data$eaaq17_rv <- 6 - lbc_data$eaaq17_w2
lbc_data$eaaq20_rv <- 6 - lbc_data$eaaq20_w2
lbc_data$eaaq22_rv <- 6 - lbc_data$eaaq22_w2

# There are 3 Factors, which can be distinguished quite well. Thus:

# Fit a three-factor model
attitudes_ageing_model <- '
Psychosocial_Loss =~ eaaq3_rv + eaaq6_rv + eaaq9_rv + eaaq12_rv + 
  eaaq15_rv + eaaq17_rv + eaaq20_rv + eaaq22_rv

Physical_Change =~ eaaq7_w2 + eaaq8_w2 + eaaq11_w2 + eaaq13_w2 + 
  eaaq14_w2 + eaaq16_w2 + eaaq23_w2 + eaaq24_w2

Psychological_Growth =~ eaaq1_w2 + eaaq2_w2 + eaaq4_w2 + eaaq5_w2 + 
  eaaq10_w2 + eaaq18_w2 + eaaq19_w2 + eaaq21_w2
'

fit_attitudes_ageing <- cfa(attitudes_ageing_model,
                            data = lbc_data,
                            missing = "ML")

summary(fit_attitudes_ageing, fit.measures = TRUE, standardized = TRUE)

# Model fit is not great, thus do:

modindices(fit_attitudes_ageing, sort = TRUE, minimum.value = 10)

# Indeed, we have to free the top residuals and re-run the model

attitudes_ageing_modeladj <- '
Psychosocial_Loss =~ eaaq3_rv + eaaq6_rv + eaaq9_rv + eaaq12_rv + 
  eaaq15_rv + eaaq17_rv + eaaq20_rv + eaaq22_rv

Physical_Change =~ eaaq7_w2 + eaaq8_w2 + eaaq11_w2 + eaaq13_w2 + 
  eaaq14_w2 + eaaq16_w2 + eaaq23_w2 + eaaq24_w2

Psychological_Growth =~ eaaq1_w2 + eaaq2_w2 + eaaq4_w2 + eaaq5_w2 + 
  eaaq10_w2 + eaaq18_w2 + eaaq19_w2 + eaaq21_w2

  # Added residual covariances:
  eaaq7_w2 ~~ eaaq24_w2
  eaaq18_w2 ~~ eaaq21_w2
  eaaq3_rv ~~ eaaq6_rv
  eaaq17_rv ~~ eaaq20_rv
'

fit_attitudes_ageing_adj <- cfa(attitudes_ageing_modeladj,
                                data = lbc_data,
                                missing = "ML")

summary(fit_attitudes_ageing_adj, fit.measures = TRUE, standardized = TRUE)

# Still not great. Drop items 7 and 13 (low factor loadings, high residual covariances) and re-run:

attitudes_ageing_modeladj_2 <- '
Psychosocial_Loss =~ eaaq3_rv + eaaq6_rv + eaaq9_rv + eaaq12_rv + 
  eaaq15_rv + eaaq17_rv + eaaq20_rv + eaaq22_rv

Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq16_w2 + eaaq23_w2 + eaaq24_w2

Psychological_Growth =~ eaaq1_w2 + eaaq2_w2 + eaaq4_w2 + eaaq5_w2 + 
  eaaq10_w2 + eaaq18_w2 + eaaq19_w2 + eaaq21_w2

  # Added residual covariances:
  eaaq7_w2 ~~ eaaq24_w2
  eaaq18_w2 ~~ eaaq21_w2
  eaaq3_rv ~~ eaaq6_rv
  eaaq17_rv ~~ eaaq20_rv
'

fit_attitudes_ageing_adj_2 <- cfa(attitudes_ageing_modeladj_2,
                                  data = lbc_data,
                                  missing = "ML")

summary(fit_attitudes_ageing_adj_2, fit.measures = TRUE, standardized = TRUE)

# Also drop items 16 and 24 (without changing the name of the model this time):

attitudes_ageing_modeladj_2 <- '
Psychosocial_Loss =~ eaaq3_rv + eaaq6_rv + eaaq9_rv + eaaq12_rv + 
  eaaq15_rv + eaaq17_rv + eaaq20_rv + eaaq22_rv

Physical_Change =~ eaaq8_w2 + eaaq11_w2 + eaaq14_w2 + eaaq23_w2 

Psychological_Growth =~ eaaq1_w2 + eaaq2_w2 + eaaq4_w2 + eaaq5_w2 + 
  eaaq10_w2 + eaaq18_w2 + eaaq19_w2 + eaaq21_w2

  # Added residual covariances:
  eaaq18_w2 ~~ eaaq21_w2
  eaaq3_rv ~~ eaaq6_rv
  eaaq17_rv ~~ eaaq20_rv
'

fit_attitudes_ageing_adj_2 <- cfa(attitudes_ageing_modeladj_2,
                                  data = lbc_data,
                                  missing = "ML")

summary(fit_attitudes_ageing_adj_2, fit.measures = TRUE, standardized = TRUE)

## Mobility

# Some recoding required first - 99 to NA
library(dplyr)
lbc_data <- lbc_data %>%
  mutate(
    sidest_w2 = na_if(sidest_w2, 99),
    semtanst_w2 = na_if(semtanst_w2, 99),
    tanst_w2 = na_if(tanst_w2, 99),
    chairst_w2 = na_if(chairst_w2, 99)
  )

# Convert into binary (held 10 seconds vs not)
lbc_data <- lbc_data %>%
  mutate(tanst10_w2 = case_when(
    tanst_w2 == 2 ~ 1,
    tanst_w2 %in% c(0, 1) ~ 0,
    TRUE ~ NA_real_
  ))


# Inspect all variables before fitting model again

table(lbc_data$sidest_w2) # 1=807 --> no variation, not useful in CFA 
table(lbc_data$semtanst_w2) # 0=16; 1=790
table(lbc_data$tanst10_w2) # 0=83; 1=709
table(lbc_data$chairst_w2) # 0=9; 1=787

mobility_model <- '
  Mobility =~ semtanst_w2 + tanst10_w2 + chairst_w2
'
library(lavaan)

fit_mobility <- cfa(
  model = mobility_model,
  data = lbc_data,
  estimator = "WLSMV",
  ordered = c("semtanst_w2", "tanst10_w2", "chairst_w2")
)

summary(fit_mobility, fit.measures = TRUE, standardized = TRUE)

# Poor fit. 

# Explore other mobility variables
describe(lbc_data$demspn_w2) # N=808; mean=78.24; SD=4.72; min=64; max=95
hist(lbc_data$demspn_w2) # almost perfectly normal 

describe(lbc_data$sixmwk_w2) # N=803; median=4.14 seconds; range=13.77 seconds; min=1.63; max=15.4
hist(lbc_data$sixmwk_w2) # skewed
# Log transform and standardize 6 metre walk in seconds variable
lbc_data$sixmwk_w2_logz <- scale(log(lbc_data$sixmwk_w2 + 1))
hist(lbc_data$sixmwk_w2_logz) # work with this variable

# Save var as numeric 
lbc_data$sixmwk_w2_logz <- as.numeric(scale(lbc_data$sixmwk_w2_logz))

# Using this variable might be better indeed! 

## Visual Acuity 
cor(lbc_data$Recoded_visr_c_w2, lbc_data$Recoded_visl_c_w2, use = "pairwise.complete.obs") # 0.32 only

visual_model <- '
  Visual_Acuity =~ Recoded_visr_c_w2 + Recoded_visl_c_w2
'

fit_visual <- cfa(
  visual_model,
  data = lbc_data,
  estimator = "MLR",   # robust to skewness
  missing = "ML"       # handles missing data properly
)

summary(fit_visual, fit.measures = TRUE, standardized = TRUE)

# Model cannot be identified. Use VA as a composite score instead:

lbc_data$va_composite <- rowMeans(
  lbc_data[, c("Recoded_visr_c_w2", "Recoded_visl_c_w2")],
  na.rm = TRUE
)

library(psych)
describe(lbc_data$va_composite)

## Inspect left and right VA data separately (corrected)

# Right
hist(lbc_data$Recoded_visr_c_w2)
describe(lbc_data$Recoded_visr_c_w2) # N=555
summary(lbc_data$Recoded_visr_c_w2)

# Left
hist(lbc_data$Recoded_visl_c_w2)
describe(lbc_data$Recoded_visl_c_w2) # N=557

## Inspect left and right VA data separately (uncorrected)

# Right
describe(lbc_data$Recoded_visr_u_w2) # N=763
summary(lbc_data$Recoded_visr_u_w2)
hist(lbc_data$Recoded_visr_u_w2) # very skewed
table(lbc_data$Recoded_visr_u_w2) # 13 people with a value of -0.1

# Left
describe(lbc_data$Recoded_visl_u_w2) # N=768
hist(lbc_data$Recoded_visl_u_w2) # quite okay, not too skewed
summary(lbc_data$Recoded_visl_u_w2)
table(lbc_data$Recoded_visl_u_w2) # 21 people with a value of -0.1

# Variables need log transforming and standardising, apply

## Step 1: Shift the values so they're all positive
# Uncorrected right
min_val <- min(lbc_data$Recoded_visr_u_w2, na.rm = TRUE)
lbc_data$visr_u_shifted <- lbc_data$Recoded_visr_u_w2 - min_val + 0.01
hist(lbc_data$visr_u_shifted)

# Uncorrected left
min_val <- min(lbc_data$Recoded_visl_u_w2, na.rm = TRUE)
lbc_data$visl_u_shifted <- lbc_data$Recoded_visl_u_w2 - min_val + 0.01
hist(lbc_data$visl_u_shifted)

## Now apply transformation and inspect distributions
lbc_data$visr_u_sqrtz <- scale(sqrt(lbc_data$visr_u_shifted)) # sqrt less aggressive than log 
hist(lbc_data$visr_u_sqrtz) # quite okay
lbc_data$visl_u_sqrtz <- scale(sqrt(lbc_data$visl_u_shifted))
hist(lbc_data$visl_u_sqrtz) # quite okay

## Standardise (z-score) the sqrt-transformed, shifted variables
lbc_data$visr_u_sqrtz <- scale(lbc_data$visr_u_sqrtz)
lbc_data$visl_u_sqrtz <- scale(lbc_data$visl_u_sqrtz)

# Save vars as numeric 
lbc_data$visr_u_sqrtz <- as.numeric(scale(lbc_data$visr_u_sqrtz))
lbc_data$visl_u_sqrtz <- as.numeric(scale(lbc_data$visl_u_sqrtz))

# Inspect their distributions 
hist(lbc_data$visr_u_sqrtz)
hist(lbc_data$visl_u_sqrtz)

# NB: Both mobility and VA will be treated as directly observed variables instead! 

# Check directly measured variables again:

## Oral health
hist(lbc_data$numteeth_w2)
describe(lbc_data$numteeth_w2)

## eGFR
hist(lbc_data$bld_eGFR_w2)
describe(lbc_data$bld_eGFR_w2)
table(lbc_data$bld_eGFR_w2)

# Make eGFR binary
lbc_data$eGFR_binary <- ifelse(lbc_data$bld_eGFR_w2 < 60, 1, 0)
# Check again
hist(lbc_data$eGFR_binary)
table(lbc_data$eGFR_binary) # 0=680; 1=85

# Log-transform and scale
lbc_data$bld_eGFR_log <- log(lbc_data$bld_eGFR_w2)
hist(lbc_data$bld_eGFR_log) # still very skewed

# Thus, just use the (raw) continuous values but scale them for interpretability

# Standardise (z-score)
lbc_data$bld_eGFR_w2_z <- scale(lbc_data$bld_eGFR_w2)
hist(lbc_data$bld_eGFR_w2_z)

# Still pretty bad and heavily skewed. The best option would be to use the binary version of the variable.

# Try running another measurement model for the new VA data
va_model_new <- '
  # Measurement model
  VisualAcuity =~ visl_u_sqrtz + visr_u_sqrtz
  VisualAcuity ~~ 1*VisualAcuity

'

fit_visual2 <- cfa(
  va_model_new,
  data = lbc_data,
  estimator = "MLR",   # robust to skewness
  missing = "ML"       # handles missing data properly
)

summary(fit_visual2, fit.measures = TRUE, standardized = TRUE)

# Model is not identified.
colSums(is.na(lbc_data[c("visr_u_sqrtz", "visl_u_sqrtz")]))
cor(lbc_data$visr_u_sqrtz, lbc_data$visl_u_sqrtz, use = "pairwise.complete.obs")

# Fixing the latent variance to 1 led to just-identification though. Not great.

### Measurement models complete. 