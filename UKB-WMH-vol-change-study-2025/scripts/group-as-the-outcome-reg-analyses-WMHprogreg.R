
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date/period: September 2025
--------------------------------------------------------------------------
  
### Phenotypes were first preprocessed by running PHESANT with the 'save' option.
  
### 15/09/2025: Trying out another approach to compare Progressors and Regressors
 
# Load libraries
library(dplyr)
library(psych)
library(readr)
library(broom)
library(car) # for Anova()
library(emmeans)

# Load data
merged_extended_progressors <- read_tsv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended_progressors.tsv",
                                        show_col_types = FALSE)
# Progressors
progressors <- merged_extended_progressors %>%
  mutate(group = "progressor")

merged_extended_regressors <- read_tsv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended_regressors.tsv",
                                       show_col_types = FALSE)
# Regressors
regressors  <- merged_extended_regressors %>%
  mutate(group = "regressor")

# Make sure no overlap in userIds
common_ids <- intersect(progressors$userId, regressors$userId)
length(common_ids)   # should be 0; it is 0


# Check variables that are not harmonised (numeric vs character)
diff_types <- purrr::map2_chr(progressors, regressors, ~{
  if (class(.x)[1] != class(.y)[1]) paste(class(.x)[1], "vs", class(.y)[1]) else NA_character_
}) 

diff_types <- diff_types[!is.na(diff_types)]
diff_types

# Quick look at each variable in both data frames
table(progressors$x4526, useNA = "ifany")
table(regressors$x4526,  useNA = "ifany")

# Harmonise across files
# Clean and recode in both datasets
progressors <- progressors %>%
  mutate(
    x4526 = as.numeric(x4526)
  )

regressors <- regressors %>%
  mutate(
    x4526 = case_when(
      x4526 == "Extremely unhappy" ~ 1,
      x4526 == "Extremely happy" ~ 6,
      TRUE ~ suppressWarnings(as.numeric(x4526))
    )
  )

table(progressors$x4526, useNA = "ifany")
table(regressors$x4526,  useNA = "ifany")

# ALL GOOD WITH HAPPINESS VARIABLE NOW

# Next one
table(progressors$x4570, useNA = "ifany")
table(regressors$x4570,  useNA = "ifany")

# Fix
progressors <- progressors %>%
  mutate(
    x4570 = case_when(
      x4570 == "Extremely unhappy" ~ 1,
      x4570 == "Extremely happy" ~ 6,
      TRUE ~ suppressWarnings(as.numeric(x4570))
    )
  )

regressors <- regressors %>%
  mutate(
    x4570 = case_when(
      x4570 == "Extremely unhappy" ~ 1,
      x4570 == "Extremely happy" ~ 6,
      TRUE ~ suppressWarnings(as.numeric(x4570))
    )
  )

# Sanity check after transformation
table(progressors$x4570, useNA = "ifany")
table(regressors$x4570,  useNA = "ifany")

# Last one
table(progressors$x22150, useNA = "ifany")
table(regressors$x22150,  useNA = "ifany")

# Keep as numeric
progressors <- progressors %>%
  mutate(x22150 = as.numeric(x22150))

regressors <- regressors %>%
  mutate(x22150 = as.numeric(x22150))

# Now back to bind
merged_extended <- bind_rows(regressors, progressors) %>%
  mutate(group = factor(group, levels = c("progressor", "regressor")))

# Quick sanity check
table(merged_extended$group)
merged_extended %>%
  group_by(group) %>%
  summarise(
    n   = n(),
    mean_outcome = mean(WMH_TP2_adj_std, na.rm = TRUE),
    sd_outcome   = sd(WMH_TP2_adj_std,   na.rm = TRUE)
  )

# All good

#group       n mean_outcome sd_outcome
#<fct>      <int>        <dbl>      <dbl>
#1 progressor  2750       0.540     0.672
#2 regressor   1579       -0.941    0.750

# Double-check dataset
View(merged_extended)

# Save
write.csv(
  merged_extended,
  "merged_extended_with_group.csv",
  row.names = FALSE
)

# Save
write.csv(merged_extended, "merged_extended_with_group.csv", row.names = FALSE)

# Load later
merged_extended_group <- read.csv("merged_extended_with_group.csv")

# Make sure group is a factor
merged_extended_group$group <- factor(
  merged_extended_group$group,
  levels = c("regressor", "progressor")  # progressor = reference
)

table(merged_extended_group$group)

# =====================================================
# Outcome & covariates
# =====================================================
outcome <- "WMH_TP2_adj_std"

covars_min <- c("x31_0_0", "x21022_0_0", "xICV_2_0")             # sex, age, ICV
covars_ext <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "x6150.4")  # + hypertension

# =======================================================
# Function for binary predictors with group interaction
# =======================================================
run_bin_models <- function(data, outcome, predictors, labels, covars, model_type, outfile) {
  results <- list()
  
  for (var in predictors) {
    form <- as.formula(
      paste(outcome, "~", var, "* group",
            if (length(covars) > 0) paste("+", paste(covars, collapse = " + ")) else "")
    )
    
    fit <- try(lm(form, data = data), silent = TRUE)
    if (inherits(fit, "try-error")) next
    
    tidy_fit <- broom::tidy(fit, conf.int = TRUE)
    
    # --- Progressor effect (reference group)
    prog <- tidy_fit %>%
      filter(term == var) %>%
      transmute(
        term, estimate, std.error, conf.low, conf.high, p.value,
        effect_type = "progressor_effect"
      )
    
    # --- Interaction effect (difference regressor vs. progressor)
    int <- tidy_fit %>%
      filter(term == paste0(var, ":groupregressor")) %>%
      transmute(
        term, estimate, std.error, conf.low, conf.high, p.value,
        effect_type = "interaction_regressor_vs_progressor"
      )
    
    # --- Regressor effect (using emmeans)
    reg <- try({
      slopes <- emtrends(fit, ~ group, var = var)
      as.data.frame(summary(slopes)) %>%
        filter(group == "regressor") %>%
        transmute(
          term = paste0(var, "_regressor_effect"),
          estimate = emmean,
          std.error = SE,
          conf.low = lower.CL,
          conf.high = upper.CL,
          p.value = p.value,
          effect_type = "regressor_effect"
        )
    }, silent = TRUE)
    
    if (inherits(reg, "try-error")) reg <- NULL
    
    # --- Combine
    res <- bind_rows(prog, reg, int) %>%
      mutate(phenotype = var, model = model_type)
    
    results[[var]] <- res
  }
  
  results_df <- bind_rows(results) %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, model, effect_type, term,
           estimate, std.error, conf.low, conf.high, p.value)
  
  # --- FDR correction across predictors
  results_df <- results_df %>%
    mutate(pval_fdr = p.adjust(p.value, method = "fdr"))
  
  # --- Save
  write.csv(results_df, outfile, row.names = FALSE)
  return(results_df)
}

# =====================================================
# Run models for binary predictors only
# =====================================================

res_bin_unadj <- run_bin_models(merged_extended_group, outcome, binary_vars, binary_labels,
                                covars = NULL, model_type = "unadjusted",
                                outfile = "binary_group_unadj.csv")

res_bin_min <- run_bin_models(merged_extended_group, outcome, binary_vars, binary_labels,
                              covars = covars_min, model_type = "min_adj",
                              outfile = "binary_group_minadj.csv")

res_bin_ext <- run_bin_models(merged_extended_group, outcome, binary_vars, binary_labels,
                              covars = covars_ext, model_type = "ext_adj",
                              outfile = "binary_group_extadj.csv")

# =====================================================
# Combine all outputs
# =====================================================
res_bin_all <- bind_rows(res_bin_unadj, res_bin_min, res_bin_ext)

write.csv(res_bin_all, "binary_group_allmodels.csv", row.names = FALSE)
head(res_bin_all)

-------------------------------------------------------------------------------------------------
### 21/09/2025: Run with GROUP as the outcome - what predicts WMH progression or regression?  
-------------------------------------------------------------------------------------------------
  
# Make group outcome binary
merged_extended_group <- merged_extended_group %>%
  mutate(group_binary = ifelse(group == "regressor", 1, 0))

# Sanity check
table(merged_extended_group$group_binary)
# 0 = progressor = 2750
# 1 = regressor = 1579

# Test a single regression to make sure it runs alright
fit <- glm(group_binary ~ x6148.4 + x31_0_0 + x21022_0_0, 
           data = merged_extended_group, family = binomial)

summary(fit)

# Odds ratios with CIs
exp(cbind(OR = coef(fit), confint(fit)))

# Run for all phenotypes

# =====================================================
# Outcome & covariates
# =====================================================
outcome <- "group_binary"   # 0 = progressor, 1 = regressor

covars_min <- c("x31_0_0", "x21022_0_0", "xICV_2_0")             # sex, age, ICV
covars_ext <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "x6150.4")  # + hypertension

# =====================================================
# Logistic regression wrapper (Wald CIs only)
# =====================================================

run_logit_models <- function(data, outcome, predictors, labels, covars, model_type, outfile) {
  results <- list()
  
  for (var in predictors) {
    form <- as.formula(
      paste(outcome, "~", var,
            if (length(covars) > 0) paste("+", paste(covars, collapse = " + ")) else "")
    )
    
    # --- Fit model and capture warnings
    w <- NULL
    fit <- withCallingHandlers(
      try(glm(form, data = data, family = binomial), silent = TRUE),
      warning = function(wrn) {
        w <<- conditionMessage(wrn) # store warning
        invokeRestart("muffleWarning")
      }
    )
    
    if (inherits(fit, "try-error")) next
    
    # --- Tidy results (without CIs)
    tidy_fit <- broom::tidy(fit, conf.int = FALSE, exponentiate = FALSE)
    
    # --- Add Wald logit CIs
    tidy_fit <- tidy_fit %>%
      mutate(
        conf.low_logit  = estimate - 1.96 * std.error,
        conf.high_logit = estimate + 1.96 * std.error
      )
    
    # --- ORs and Wald CIs
    or_fit <- tidy_fit %>%
      mutate(
        OR      = exp(estimate),
        OR_low  = exp(conf.low_logit),
        OR_high = exp(conf.high_logit)
      )
    
    # --- Assemble results
    res <- or_fit %>%
      transmute(
        phenotype = var,
        term,
        estimate_logit = estimate,
        std.error,
        conf.low_logit,
        conf.high_logit,
        OR,
        OR_low,
        OR_high,
        p.value,
        model = model_type,
        warning_flag = ifelse(is.null(w), FALSE, TRUE)
      )
    
    results[[var]] <- res
  }
  
  results_df <- bind_rows(results) %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, model, term,
           estimate_logit, std.error, conf.low_logit, conf.high_logit,
           OR, OR_low, OR_high,
           p.value, warning_flag)
  
  # --- FDR correction
  results_df <- results_df %>%
    mutate(pval_fdr = p.adjust(p.value, method = "fdr")) %>%
    arrange(pval_fdr)
  
  # --- Save to CSV
  write.csv(results_df, outfile, row.names = FALSE)
  return(results_df)
}


# =====================================================
# Prepare standardised continuous dataset
# =====================================================
std_data <- merged_extended_group %>%
  mutate(across(all_of(cont_vars), ~ as.numeric(scale(.))))

# =====================================================
# Run models
# =====================================================

## ------------------- Binary + Ordered + Unordered -------------------------------------------------
bin_ord_unord <- c(binary_vars, ord_vars, unord_vars)

bin_ord_unord_labels <- bind_rows(binary_labels, ord_labels, unord_labels)

res_bin_unadj <- run_logit_models(merged_extended_group, outcome, bin_ord_unord, bin_ord_unord_labels,
                                  covars = NULL, model_type = "unadjusted",
                                  outfile = "logit_binary_ord_unord_unadj.csv")

# Check warnings for this model
warnings()
res_bin_unadj %>% arrange(desc(abs(OR)))
print(res_bin_unadj, n = Inf)   # show all rows


res_bin_min <- run_logit_models(merged_extended_group, outcome, bin_ord_unord, bin_ord_unord_labels,
                                covars = covars_min, model_type = "min_adj",
                                outfile = "logit_binary_ord_unord_min.csv")

res_bin_ext <- run_logit_models(merged_extended_group, outcome, bin_ord_unord, bin_ord_unord_labels,
                                covars = covars_ext, model_type = "ext_adj",
                                outfile = "logit_binary_ord_unord_ext.csv")

## ------------------- Continuous RAW -----------------------------------------------------------
res_cont_unadj_raw <- run_logit_models(merged_extended_group, outcome, cont_vars, cont_labels,
                                       covars = NULL, model_type = "unadjusted_raw",
                                       outfile = "logit_cont_raw_unadj.csv")

res_cont_min_raw <- run_logit_models(merged_extended_group, outcome, cont_vars, cont_labels,
                                     covars = covars_min, model_type = "min_adj_raw",
                                     outfile = "logit_cont_raw_min.csv")

res_cont_ext_raw <- run_logit_models(merged_extended_group, outcome, cont_vars, cont_labels,
                                     covars = covars_ext, model_type = "ext_adj_raw",
                                     outfile = "logit_cont_raw_ext.csv")

## ------------------- Continuous STANDARDISED -------------------------------------------------
res_cont_unadj_std <- run_logit_models(std_data, outcome, cont_vars, cont_labels,
                                       covars = NULL, model_type = "unadjusted_std",
                                       outfile = "logit_cont_std_unadj.csv")

res_cont_min_std <- run_logit_models(std_data, outcome, cont_vars, cont_labels,
                                     covars = covars_min, model_type = "min_adj_std",
                                     outfile = "logit_cont_std_min.csv")

res_cont_ext_std <- run_logit_models(std_data, outcome, cont_vars, cont_labels,
                                     covars = covars_ext, model_type = "ext_adj_std",
                                     outfile = "logit_cont_std_ext.csv")

# Check some specific variables
table(merged_extended_group$x6148.4)

# Key predictors of interest for unadjusted - check prevalence

check_vars <- c("x3393", "x6148.4", "x6148.1")  # Hearing aid, Cataract, Diabetes-related eye disease
check_labels <- c("Hearing aid user", "Cataract", "Diabetes-related eye disease")

# Create prevalence tables
for (i in seq_along(check_vars)) {
  var <- check_vars[i]
  label <- check_labels[i]
  
  cat("\n==========", label, "==========\n")
  
  tab <- table(merged_extended_group[[var]], merged_extended_group$group)
  print(tab)
  
  # Add proportions by group
  prop_tab <- prop.table(tab, margin = 2) * 100
  print(round(prop_tab, 1))
  
  # Fisher's Exact Test (works well even with small cell counts)
  test <- fisher.test(tab)
  print(test)
}


