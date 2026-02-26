
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date/period: September 2025
--------------------------------------------------------------------------
  
### Phenotypes were first preprocessed by running PHESANT with the 'save' option.
  
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

-----------------------------------------------------------------------
### Baseline comparison between WMH Progressors and Regressors 
-----------------------------------------------------------------------

# =====================================================
# Categorical group comparison wrapper (Chi-squared version)
# =====================================================

run_cat_group_comparisons <- function(data, outcome, predictors, labels, outfile) {
  results <- list()
  
  for (var in predictors) {
    # Cross-tab
    tab <- table(data[[var]], data[[outcome]])
    
    # Run Chi-squared test
    test <- tryCatch({
      suppressWarnings(chisq.test(tab))
    }, error = function(e) NULL)
    
    if (is.null(test)) next
    
    # Extract results
    res <- data.frame(
      phenotype = var,
      statistic = test$statistic,
      df        = test$parameter,
      p.value   = test$p.value,
      stringsAsFactors = FALSE
    )
    
    results[[var]] <- res
  }
  
  results_df <- bind_rows(results) %>%
    left_join(labels, by = "phenotype") %>%
    mutate(
      pval_fdr = p.adjust(p.value, method = "fdr")
    ) %>%
    arrange(pval_fdr)
  
  # Save results
  write.csv(results_df, outfile, row.names = FALSE)
  return(results_df)
}

# =====================================================
# Wrapper for continuous variables
# =====================================================

compare_continuous <- function(data, predictors, labels, outcome) {
  results <- list()
  
  for (var in predictors) {
    grp0 <- data %>% filter(.data[[outcome]] == 0) %>% pull(var)
    grp1 <- data %>% filter(.data[[outcome]] == 1) %>% pull(var)
    
    # t-test (Welch’s)
    tt <- t.test(grp0, grp1)
    
    res <- data.frame(
      phenotype = var,
      term = var,
      mean_regressor = mean(grp1, na.rm = TRUE),
      sd_regressor   = sd(grp1, na.rm = TRUE),
      mean_progressor = mean(grp0, na.rm = TRUE),
      sd_progressor   = sd(grp0, na.rm = TRUE),
      estimate = tt$estimate[1] - tt$estimate[2],
      conf_low = tt$conf.int[1],
      conf_high = tt$conf.int[2],
      p_value = tt$p.value,
      test_type = "t-test"
    )
    
    results[[var]] <- res
  }
  
  results_df <- bind_rows(results) %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, everything())
  
  return(results_df)
}

# =====================================================
# Run comparisons: categorical + continuous
# =====================================================

# --- Categorical predictors (binary + ordered + unordered)
cat_predictors <- c(binary_vars, ord_vars, unord_vars)
cat_labels <- bind_rows(binary_labels, ord_labels, unord_labels)

res_categorical <- run_cat_group_comparisons(
  data = merged_extended_group,
  outcome = "group_binary",
  predictors = cat_predictors,
  labels = cat_labels,
  outfile = "group_comparison_categorical.csv"
)


# --- Continuous predictors
res_continuous <- compare_continuous(
  data = merged_extended_group,
  predictors = cont_vars,
  labels = cont_labels,
  outcome = "group_binary"
) %>%
  mutate(pval_fdr = p.adjust(p_value, method = "fdr")) %>%
  arrange(pval_fdr)

# Save to CSV
write.csv(res_categorical, "group_comparison_categorical.csv", row.names = FALSE)
write.csv(res_continuous, "group_comparison_continuous.csv", row.names = FALSE)

# Sanity check for regressors
summary(merged_extended_regressors$WMH_TP2_adj_std)

hist(merged_extended_regressors$WMH_TP2_adj_std, breaks = 50,
     main = "Distribution of WMH residuals (regressors only)",
     xlab = "Residualised WMH change (standardized)")

View(merged_extended_regressors)


