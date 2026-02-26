# ==============================================================

## Response to Reviewers - Neurology Submission 

# UKB WMH Volume Change + Clinical Phenotypes PhD Project 
# 10/01/2026: New Analyses

# Author: Angelina Kancheva

# ==============================================================

# Load required libraries
library(dplyr)
library(psych)
library(readr)
library(broom)
library(car)

# Load dataset 
merged_extended <- read.table(
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Check data
View(merged_extended)

# Load WMH variables
describe(merged_extended$x25781_2_0)
hist(merged_extended$x25781_2_0) # very skewed
describe(merged_extended$x25781_3_0)
hist(merged_extended$x25781_3_0) # very skewed

# Log transform the WMH vars
merged_extended$x25781_2_0_log <- log1p(merged_extended$x25781_2_0)
merged_extended$x25781_3_0_log <- log1p(merged_extended$x25781_3_0)

# Inspect them again
describe(merged_extended$x25781_2_0_log)
hist(merged_extended$x25781_2_0_log) # much improved
describe(merged_extended$x25781_3_0_log)
hist(merged_extended$x25781_3_0_log) # much improved

# Check that raw and log transformed WMH vars are there
names(merged_extended)[grepl("25781", names(merged_extended))]

# Check variables related to time/follow-up
describe(merged_extended$xYears_2) # available for n=4201
hist(merged_extended$xYears_2) # obviously, very skewed

# Save file for now 
write.table(
  merged_extended,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

### New WMH outcome variable calculation (as requested from reviewers)

# Annualised absolute WMH change (mm3/year)
merged_extended$WMH_change_mm3_per_year <-
  (merged_extended$x25781_3_0 - merged_extended$x25781_2_0) /
  merged_extended$xYears_2

# Check this new var
describe(merged_extended$WMH_change_mm3_per_year)
hist(merged_extended$WMH_change_mm3_per_year, breaks = 50,
     main = "Annualised WMH change (mm3/year)") # fairly okay

# Baseline-adjusted annualised WMH change (optional)

#wmh_annualised_adj_model <- lm(
  #WMH_change_mm3_per_year ~ x25781_2_0,
  #data = merged_extended
#)

#merged_extended$WMH_change_mm3_per_year_adj <-
  #resid(wmh_annualised_adj_model)

#merged_extended$WMH_change_mm3_per_year_adj_std <-
  #scale(merged_extended$WMH_change_mm3_per_year_adj)

# Check covariate variables
table(merged_extended$x31_0_0) # 0=2362; 1=1967
describe(merged_extended$x21022_0_0.y.y) # N=4329; mean=52.57; SD=7.35; median=52; range=30 (40-70)
hist(merged_extended$x21022_0_0.y.y) # very skewed
describe(merged_extended$xICV_2_0) # N=4329; mean=1203360; median=1196714; SD=115137.1; range=827888.2
describe(merged_extended$x4079_2_0.x) # N=3129 
describe(merged_extended$x4080_2_0.x) # N=3129

-----------------------------------------
# Setup for the new Regression Analyses 
-----------------------------------------

#### Running only on Significant Associations from last time ####

# Outcome variable
outcome <- merged_extended$WMH_change_mm3_per_year

# Covariates
covars_min <- c("x31_0_0", "x21022_0_0.y.y", "xICV_2_0") # age, sex, ICV
covars_ext <- c("x31_0_0", "x21022_0_0.y.y", "xICV_2_0", "x4079_2_0.x", "x4080_2_0.x") # + DBP, SBP

str(merged_extended[, covars_min])
str(merged_extended[, covars_ext])

# ------------------------------------
##### Binary vars - Significant #####
# ------------------------------------

# Binary predictors 
binary_vars <- c("x6150.4", "x6149.6", "x3414", "x6148.1")

# Labels table for binary predictors
binary_labels <- data.frame(
  phenotype = binary_vars,
  label = c("Cataract", "Dentures", "Hip pain 3+ months", "Diabetes-related eye disease"))

# Check all good
binary_vars
binary_vars[!binary_vars %in% names(merged_extended)]

covars_min
covars_min[!covars_min %in% names(merged_extended)]

covars_ext
covars_ext[!covars_ext %in% names(merged_extended)]

# Check that binary vars are truly binary
lapply(merged_extended[, binary_vars], function(x) table(x, useNA = "ifany"))


# =======================================
# Run unadjusted regressions
# =======================================
results_unadj <- list()

for (var in binary_vars) {
  form <- as.formula(paste("WMH_change_mm3_per_year ~", var))
  
  fit <- try(lm(form, data = merged_extended), silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    tidy_fit <- broom::tidy(fit, conf.int = TRUE)
    
    if (var %in% tidy_fit$term) {
      predictor_res <- tidy_fit %>% filter(term == var)
      
      results_unadj[[var]] <- data.frame(
        phenotype = var,
        beta = predictor_res$estimate,
        se = predictor_res$std.error,
        ci_low = predictor_res$conf.low,
        ci_high = predictor_res$conf.high,
        pval = predictor_res$p.value
      )
    }
  }
}

results_unadj_df <- bind_rows(results_unadj) %>%
  left_join(binary_labels, by = "phenotype") %>%
  select(phenotype, label, everything())

results_unadj_df$pval_fdr <- p.adjust(results_unadj_df$pval, method = "fdr")


# =======================================
# Run minimally adjusted regressions
# =======================================
results_minadj <- list()

for (var in binary_vars) {
  form <- as.formula(
    paste("WMH_change_mm3_per_year ~", var, "+", paste(covars_min, collapse = " + "))
  )
  
  fit <- try(lm(form, data = merged_extended), silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    tidy_fit <- broom::tidy(fit, conf.int = TRUE)
    
    if (var %in% tidy_fit$term) {
      predictor_res <- tidy_fit %>% filter(term == var)
      
      results_minadj[[var]] <- data.frame(
        phenotype = var,
        beta = predictor_res$estimate,
        se = predictor_res$std.error,
        ci_low = predictor_res$conf.low,
        ci_high = predictor_res$conf.high,
        pval = predictor_res$p.value
      )
    }
  }
}

results_minadj_df <- bind_rows(results_minadj) %>%
  left_join(binary_labels, by = "phenotype") %>%
  select(phenotype, label, everything())

results_minadj_df$pval_fdr <- p.adjust(results_minadj_df$pval, method = "fdr")


# =======================================
# Run fully adjusted regressions
# =======================================
results_fulladj <- list()

for (var in binary_vars) {
  form <- as.formula(
    paste("WMH_change_mm3_per_year ~", var, "+", paste(covars_ext, collapse = " + "))
  )
  
  fit <- try(lm(form, data = merged_extended), silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    tidy_fit <- broom::tidy(fit, conf.int = TRUE)
    
    if (var %in% tidy_fit$term) {
      predictor_res <- tidy_fit %>% filter(term == var)
      
      results_fulladj[[var]] <- data.frame(
        phenotype = var,
        beta = predictor_res$estimate,
        se = predictor_res$std.error,
        ci_low = predictor_res$conf.low,
        ci_high = predictor_res$conf.high,
        pval = predictor_res$p.value
      )
    }
  }
}

results_fulladj_df <- bind_rows(results_fulladj) %>%
  left_join(binary_labels, by = "phenotype") %>%
  select(phenotype, label, everything())

results_fulladj_df$pval_fdr <- p.adjust(results_fulladj_df$pval, method = "fdr")


# =========================
# Save results
# =========================
write.csv(
  results_unadj_df,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results12012026/binary_unadj.csv",
  row.names = FALSE
)

write.csv(
  results_minadj_df,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results12012026/binary_min_adj.csv",
  row.names = FALSE
)

write.csv(
  results_fulladj_df,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results12012026/binary_full_adj.csv",
  row.names = FALSE
)

# ----------------------------------------
##### Continuous vars - Significant #####
# ----------------------------------------

outcome_name <- "WMH_change_mm3_per_year"  

# Continuous predictors 
cont_vars <- c("x4080_2_0.x", "x4079_2_0.x", "x20023")

# Labels table for continuous predictors
cont_labels <- data.frame(
  phenotype = cont_vars,
  label = c("Systolic BP (automated)", "Diastolic BP (automated)", "Reaction time (mean)"))


run_cont_models <- function(data, outcome_name, predictors, covars, labels, outfile) {
  results <- list()
  
  for (var in predictors) {
    covars_use <- setdiff(covars, var)  
    
    rhs <- var
    if (length(covars_use) > 0) rhs <- paste(rhs, paste(covars_use, collapse = " + "), sep = " + ")
    
    form <- as.formula(paste(outcome_name, "~", rhs))
    fit <- try(lm(form, data = data), silent = TRUE)
    
    if (!inherits(fit, "try-error")) {
      tidy_fit <- broom::tidy(fit, conf.int = TRUE)
      
      if (var %in% tidy_fit$term) {
        predictor_res <- tidy_fit %>% dplyr::filter(term == var)
        
        results[[var]] <- data.frame(
          phenotype = var,
          beta = predictor_res$estimate,
          se = predictor_res$std.error,
          ci_low = predictor_res$conf.low,
          ci_high = predictor_res$conf.high,
          pval = predictor_res$p.value
        )
      } else {
        message("Skipping variable: ", var, " (no variation or not found)")
      }
    }
  }
  
  results_df <- dplyr::bind_rows(results) %>%
    dplyr::left_join(labels, by = "phenotype") %>%
    dplyr::select(phenotype, label, dplyr::everything())
  
  stopifnot(!any(is.na(results_df$label)))
  
  results_df$pval_fdr <- p.adjust(results_df$pval, method = "fdr")
  results_df <- results_df %>% dplyr::arrange(pval_fdr)
  
  write.csv(results_df, outfile, row.names = FALSE)
  results_df
}

# Save unadjusted
res_cont_unadj <- run_cont_models(
  data = merged_extended,
  outcome_name = outcome_name,
  predictors = cont_vars,
  covars = c(),   # no covariates
  labels = cont_labels,
  outfile = "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results12012026/continuous_unadj.csv")

# Save minimally adjusted
res_cont_min <- run_cont_models(
  data = merged_extended,
  outcome_name = outcome_name,
  predictors = cont_vars,
  covars = covars_min,
  labels = cont_labels,
  outfile = "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results12012026/continuous_min_adj.csv")

# Save fully adjusted
res_cont_ext <- run_cont_models(
  data = merged_extended,
  outcome_name = outcome_name,
  predictors = cont_vars,
  covars = covars_ext,
  labels = cont_labels,
  outfile = "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results12012026/continuous_full_adj.csv")

# -------------------------------------------------
##### Ordered categorical vars - Significant #####
# -------------------------------------------------

outcome_name <- "WMH_change_mm3_per_year"  

table(merged_extended$x2060, useNA = "ifany")
str(merged_extended$x2060)

# Ordered predictors 
ordcat_vars <- c("x2060")

# Labels table for ordered categorical predictors
ordcat_labels <- data.frame(
  phenotype = ordcat_vars,
  label = c("Unenthusiasm (2 weeks)"))

ordcat_vars
length(ordcat_vars)

run_ordcat_models <- function(data, outcome_name, predictors, covars, labels, outfile) {
  results <- list()
  
  for (var in predictors) {
    covars_use <- setdiff(covars, var)  
    
    rhs <- var
    if (length(covars_use) > 0) rhs <- paste(rhs, paste(covars_use, collapse = " + "), sep = " + ")
    
    form <- as.formula(paste(outcome_name, "~", rhs))
    fit <- try(lm(form, data = data), silent = TRUE)
    
    if (!inherits(fit, "try-error")) {
      tidy_fit <- broom::tidy(fit, conf.int = TRUE)
      
      if (var %in% tidy_fit$term) {
        predictor_res <- tidy_fit %>% dplyr::filter(term == var)
        
        results[[var]] <- data.frame(
          phenotype = var,
          beta = predictor_res$estimate,
          se = predictor_res$std.error,
          ci_low = predictor_res$conf.low,
          ci_high = predictor_res$conf.high,
          pval = predictor_res$p.value
        )
      } else {
        message("Skipping variable: ", var, " (no variation or not found)")
      }
    }
  }
  
  results_df <- dplyr::bind_rows(results) %>%
    dplyr::left_join(labels, by = "phenotype") %>%
    dplyr::select(phenotype, label, dplyr::everything())
  
  stopifnot(!any(is.na(results_df$label)))
  
  results_df$pval_fdr <- p.adjust(results_df$pval, method = "fdr")
  results_df <- results_df %>% dplyr::arrange(pval_fdr)
  
  write.csv(results_df, outfile, row.names = FALSE)
  results_df
}

# Save unadjusted
stopifnot(identical(ordcat_vars, "x2060"))

res_ordcat_unadj <- run_ordcat_models(
  data = merged_extended,
  outcome_name = outcome_name,
  predictors = ordcat_vars,
  covars = c(),   # no covariates
  labels = ordcat_labels,
  outfile = "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results12012026/orderedcat_unadj.csv")

# Save minimally adjusted
stopifnot(identical(ordcat_vars, "x2060"))

res_ordcat_min <- run_ordcat_models(
  data = merged_extended,
  outcome_name = outcome_name,
  predictors = ordcat_vars,
  covars = covars_min,
  labels = ordcat_labels,
  outfile = "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results12012026/orderedcat_min_adj.csv")

# Save fully adjusted
stopifnot(identical(ordcat_vars, "x2060"))

res_ordcat_ext <- run_ordcat_models(
  data = merged_extended,
  outcome_name = outcome_name,
  predictors = ordcat_vars,
  covars = covars_ext,
  labels = ordcat_labels,
  outfile = "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results12012026/orderedcat_full_adj.csv")



