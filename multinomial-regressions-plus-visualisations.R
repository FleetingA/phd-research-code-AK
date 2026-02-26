
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date/period: October 2025
--------------------------------------------------------------------------
  
### Phenotypes were first preprocessed by running PHESANT with the 'save' option.
  
# Load libraries
library(dplyr)
library(psych)
library(readr)
library(broom) # tidy results
library(car) # for Anova()
library(emmeans)
library(nnet) # multinom()
library(tidyr)

# Load data
merged_extended_progressors <- read_tsv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended_progressors.tsv",
                                        show_col_types = FALSE)

---------------------------------------------------------------------------------------
# Running multinomial regressions with group membership (three groups) as the outcome 
---------------------------------------------------------------------------------------

# Change reference category to Stable
merged_extended <- merged_extended %>%
  left_join(df %>% select(userId, WMH_change_cat), by = "userId") %>%
  mutate(WMH_change_cat = factor(WMH_change_cat,
                                 levels = c("Stable", "Regression", "Progression")))

levels(merged_extended$WMH_change_cat)
View(merged_extended)

# Define adjustment sets
covars_min <- c("x31_0_0", "x21022_0_0.y.y", "xICV_2_0", "xYears_2")
covars_ext <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "xYears_2", "x4079", "x4080")

## Preparation
# Warn if minimal adjustment results are empty
if(nrow(results_min) == 0) {
  warning("results_min is empty: Check your model fits and predictor variables!")
}

# Diagnostics
merged_extended %>%
  summarise(across(c(WMH_change_cat, x31_0_0, x21022_0_0.y.y, xICV_2_0, xYears_2), 
                   ~ sum(is.na(.)), .names = "n_miss_{col}"))

sum(!is.na(merged_extended$xYears_2)) # 4201 complete cases

# Check missing for follow-up
merged_extended %>%
  group_by(WMH_change_cat) %>%
  summarise(
    n_total = n(),
    n_missing = sum(is.na(xYears_2)),
    n_complete = sum(!is.na(xYears_2)),
    mean_years = mean(xYears_2, na.rm = TRUE),
    sd_years = sd(xYears_2, na.rm = TRUE)
  )

# Check missing across covariates
merged_extended %>%
  summarise(across(c(x31_0_0, x21022_0_0.y.y, xICV_2_0, xYears_2, x4079_2_0.x, x4080_2_0.x),
                   ~ sum(is.na(.)), .names = "n_miss_{col}"))

# Check if missingness in SBP and DBP is systematic
merged_extended %>%
  group_by(WMH_change_cat) %>%
  summarise(
    n_total = n(),
    n_missing_SBP = sum(is.na(x4079_2_0.x)),
    n_missing_DBP = sum(is.na(x4080_2_0.x)),
    pct_missing_SBP = 100 * mean(is.na(x4079_2_0.x)),
    pct_missing_DBP = 100 * mean(is.na(x4080_2_0.x))
  )

# Collect predictor sets
all_predictors <- list(
  binary      = binary_vars,
  ordered     = ord_vars,
  unordered   = unord_vars,
  continuous  = cont_vars
)

all_labels <- bind_rows(
  binary_labels %>% mutate(predictor_type = "binary"),
  ord_labels %>% mutate(predictor_type = "ordered"),
  unord_labels %>% mutate(predictor_type = "unordered"),
  cont_labels %>% mutate(predictor_type = "continuous")
)

# =========================================================
# Helper function to run multinomial regression models
# =========================================================

run_multinom <- function(predictors, predictor_type, covars, model_label, data) {
  
  map_dfr(predictors, function(v) {
    formula_str <- paste("WMH_change_cat ~", v,
                         if (length(covars) > 0) paste("+", paste(covars, collapse = " + ")) else "")
    fmla <- as.formula(formula_str)
    
    # Drop rows with missing values in outcome, predictor, or covariates
    data_sub <- data %>%
      select(all_of(c("WMH_change_cat", v, covars))) %>%
      drop_na()
    
    # Skip if outcome has fewer than 2 levels
    if (n_distinct(data_sub$WMH_change_cat) < 2) {
      message(paste("Skipping", v, "- not enough outcome levels after dropping NAs"))
      return(NULL)
    }
    
    # Fit model with Hessian for SEs
    fit <- tryCatch(
      nnet::multinom(fmla, data = data_sub, trace = FALSE, Hess = TRUE),
      error = function(e) {
        message(paste0("Model failed for predictor: ", v, " - ", e$message))
        return(NULL)
      }
    )
    if (is.null(fit)) return(NULL)
    
    # Extract coefficients and SEs
    coefs <- summary(fit)$coefficients
    ses   <- summary(fit)$standard.errors
    
    # Build tidy-like output
    res <- as.data.frame(as.table(coefs), stringsAsFactors = FALSE)
    names(res) <- c("outcome_level", "term", "estimate")
    
    se_df <- as.data.frame(as.table(ses))
    names(se_df) <- c("outcome_level", "term", "std.error")
    
    res <- left_join(res, se_df, by = c("outcome_level", "term")) %>%
      mutate(
        conf.low  = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error,
        OR        = exp(estimate),
        OR_lower  = exp(conf.low),
        OR_upper  = exp(conf.high),
        phenotype = v,
        predictor_type = predictor_type,
        model = model_label
      )
    
    return(res)
  })
}

# Define complete-case datasets
merged_extended_min <- merged_extended %>%
  drop_na(WMH_change_cat, x31_0_0, x21022_0_0.y.y, xICV_2_0, xYears_2)

merged_extended_full <- merged_extended %>%
  drop_na(WMH_change_cat, x31_0_0, x21022_0_0.y.y, xICV_2_0, xYears_2, x4079_2_0.x, x4080_2_0.x)

# ===============================================================
# Run models for all predictor types and three adjustment sets
# ===============================================================

results_unadj <- map_dfr(names(all_predictors), 
                         ~ run_multinom(all_predictors[[.x]], .x, covars = NULL,
                                        model_label = "unadjusted",
                                        data = merged_extended))

results_min <- map_dfr(names(all_predictors), 
                       ~ run_multinom(all_predictors[[.x]], .x, covars = covars_min,
                                      model_label = "minimally_adjusted",
                                      data = merged_extended_min))

results_full <- map_dfr(names(all_predictors), 
                        ~ run_multinom(all_predictors[[.x]], .x, covars = covars_ext,
                                       model_label = "fully_adjusted",
                                       data = merged_extended_full))

# =========================================
# Harmonize key variables for joining
# =========================================
standardize_keys <- function(df) {
  df %>%
    mutate(
      phenotype = as.character(phenotype),
      predictor_type = as.character(predictor_type)
    )
}

results_unadj <- standardize_keys(results_unadj)
results_min   <- standardize_keys(results_min)
results_full  <- standardize_keys(results_full)
all_labels    <- standardize_keys(all_labels)

# ===================================================================
# Combine all results and compute p-values + FDR correction
# ===================================================================

add_pvals <- function(df) {
  if (!"estimate" %in% names(df) | !"std.error" %in% names(df)) return(df)
  
  df %>%
    mutate(
      z_value = estimate / std.error,
      p_value = 2 * (1 - pnorm(abs(z_value)))
    ) %>%
    group_by(model, predictor_type) %>%
    mutate(p_fdr = p.adjust(p_value, method = "fdr")) %>%
    ungroup()
}

results_unadj <- add_pvals(results_unadj)
results_min   <- add_pvals(results_min)
results_full  <- add_pvals(results_full)

# ===============================
# Compute Odds Ratios and 95%CI
# ===============================
add_or_ci <- function(df) {
  df %>%
    mutate(
      OR       = exp(estimate),
      OR_lower = exp(conf.low),
      OR_upper = exp(conf.high)
    )
}

results_unadj <- add_or_ci(results_unadj)
results_min   <- add_or_ci(results_min)
results_full  <- add_or_ci(results_full)

# ====================================================
# Compute p-values and FDR within each results table
# ====================================================
add_pvals <- function(df) {
  if (!"estimate" %in% names(df) | !"std.error" %in% names(df)) return(df)
  
  df %>%
    mutate(
      z_value = estimate / std.error,
      p_value = 2 * (1 - pnorm(abs(z_value)))
    ) %>%
    group_by(model, predictor_type) %>%
    mutate(p_fdr = p.adjust(p_value, method = "fdr")) %>%
    ungroup()
}

results_unadj <- add_pvals(results_unadj)
results_min   <- add_pvals(results_min)
results_full  <- add_pvals(results_full)

# ===============================
# Compute Odds Ratios and 95%CI
# ===============================
add_or_ci <- function(df) {
  df %>%
    mutate(
      OR       = exp(estimate),
      OR_lower = exp(conf.low),
      OR_upper = exp(conf.high)
    )
}

results_unadj <- add_or_ci(results_unadj)
results_min   <- add_or_ci(results_min)
results_full  <- add_or_ci(results_full)

# =================================================
# Combine all results (optional convenience table)
# =================================================
results_all <- bind_rows(results_unadj, results_min, results_full)

# ===============================
# Save results to CSV files
# ===============================
write_csv(results_unadj, "multinom_unadjusted.csv")
write_csv(results_min,   "multinom_minadj.csv")
write_csv(results_full,  "multinom_fulladj.csv")
write_csv(results_all,   "multinom_allmodels.csv")

# ======================================
# Report sample sizes for transparency
# ======================================
cat("\nSample sizes used per model level:\n")
cat("Unadjusted: ", nrow(merged_extended), "participants\n")
cat("Minimally adjusted: ", nrow(merged_extended_min), "participants\n")
cat("Fully adjusted: ", nrow(merged_extended_full), "participants\n")

--------------------------------------------
### 06/10/2025: Test net WMH volume change
--------------------------------------------

# % of participants classified as "Progression"
prop_progression <- mean(df$WMH_change_cat == "Progression", na.rm = TRUE) * 100
prop_progression
prop_progression <- mean(df$WMH_change_cat == "Regression", na.rm = TRUE) * 100
prop_progression
prop_progression <- mean(df$WMH_change_cat == "Stable", na.rm = TRUE) * 100
prop_progression

chisq.test(table(df$WMH_change_cat))
tab <- table(df$WMH_change_cat)

# Get category names
cats <- names(tab)

# Create all pairwise combinations
pairs <- combn(cats, 2, simplify = FALSE)

# Run pairwise prop tests
pairwise_results <- lapply(pairs, function(p) {
  x <- tab[p]             # counts for each category
  n <- rep(sum(tab), 2)   # same total N for both groups
  res <- prop.test(x = x, n = n)
  data.frame(
    comparison = paste(p, collapse = " vs "),
    p_value = res$p.value
  )
}) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(p_adj_fdr = p.adjust(p_value, method = "fdr"))

pairwise_results

# Double-check hip pain +3 months
table(merged_extended$x6159.6) # 0=4157; 1=153
table(merged_extended$x924)

----------------------------------------------------------------------------------------
### 07/10/2025: Robust Implementation for Partially and Fully Adjusted Analyses 
----------------------------------------------------------------------------------------

# Additional libraries
library(VGAM)
library(dplyr)
library(purrr)
library(broom)

run_multinom_robust <- function(predictors, predictor_type, covars, model_label, data) {
  
  map_dfr(predictors, function(v) {
    formula_str <- paste("WMH_change_cat ~", v,
                         if (length(covars) > 0) paste("+", paste(covars, collapse = " + ")) else "")
    fmla <- as.formula(formula_str)
    
    data_sub <- data %>%
      select(all_of(c("WMH_change_cat", v, covars))) %>%
      drop_na()
    
    if (n_distinct(data_sub$WMH_change_cat) < 2) {
      message(paste("Skipping", v, "- not enough outcome levels after dropping NAs"))
      return(NULL)
    }
    
    # Fit robust multinomial model (VGAM)
    fit <- tryCatch(
      VGAM::vglm(fmla, family = VGAM::multinomial(refLevel = "Stable"), data = data_sub),
      error = function(e) {
        message(paste("Model failed for", v, "-", e$message))
        return(NULL)
      }
    )
    if (is.null(fit)) return(NULL)
    
    # Extract coefficients and covariance matrix
    coefs <- coef(fit)
    se <- sqrt(diag(vcov(fit)))  # Robust SEs
    
    coef_df <- tibble(
      term = names(coefs),
      estimate = as.numeric(coefs),
      std.error = as.numeric(se)
    )
    
    # Identify outcome comparisons
    out_levels <- levels(data_sub$WMH_change_cat)
    n_levels <- length(out_levels) - 1
    comparisons <- rep(paste(out_levels[-1], "vs Stable"), each = length(coefs) / n_levels)
    
    coef_df <- coef_df %>%
      mutate(
        Comparison = comparisons,
        conf.low = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error,
        OR = exp(estimate),
        OR_lower = exp(conf.low),
        OR_upper = exp(conf.high),
        z_value = estimate / std.error,
        p_value = 2 * (1 - pnorm(abs(z_value))),
        phenotype = v,
        predictor_type = predictor_type,
        model = model_label
      )
    
    return(coef_df)
  })
}


# Now run models
results_min <- map_dfr(names(all_predictors), 
                       ~ run_multinom_robust(all_predictors[[.x]], .x, covars = covars_min,
                                             model_label = "minimally_adjusted",
                                             data = merged_extended_min))

results_full <- map_dfr(names(all_predictors), 
                        ~ run_multinom_robust(all_predictors[[.x]], .x, covars = covars_ext,
                                              model_label = "fully_adjusted",
                                              data = merged_extended_full))
# Add FDR
add_pvals <- function(df) {
  df %>%
    group_by(model, predictor_type) %>%
    mutate(p_fdr = p.adjust(p_value, method = "fdr")) %>%
    ungroup()
}

results_min <- add_pvals(results_min)
results_full <- add_pvals(results_full)

# ============================================
# Add phenotype labels (for min + full only)
# ============================================
if (exists("all_labels")) {
  results_min <- left_join(results_min, all_labels, by = c("phenotype", "predictor_type"))
  results_full <- left_join(results_full, all_labels, by = c("phenotype", "predictor_type"))
}

# ==================================
# Combine for convenience (optional)
# ==================================
results_all <- bind_rows(results_min, results_full)

# ===============================
# Save results to CSV files
# ===============================
write_csv(results_min,  "multinom_robust_minadj.csv")
write_csv(results_full, "multinom_robust_fulladj.csv")
write_csv(results_all,  "multinom_robust_min_and_full.csv")


# ===============================
# Save results to CSV files
# ===============================
write_csv(results_unadj, "multinom_robust_unadjusted.csv")
write_csv(results_min,   "multinom_robust_minadj.csv")
write_csv(results_full,  "multinom_robust_fulladj.csv")
write_csv(results_all,   "multinom_robust_allmodels.csv")

---------------------------------------------------------------------------------------------------
# NB!!!

# Bear in mind that the comparison group output is wrong (that column in the resulting tables). 

# Look at the term column to infer groups/comparisons!

# For interpretation purposes, Stable is the reference, 1 is Regression, 2 is Progression. 
---------------------------------------------------------------------------------------------------

### 16/10/2025: Re-running with corrected FDR p-values
  
correct_pvalues_FDR <- function() {
  # Create a numeric vector of p-values
  p_values <- c(0.101309,
                0.636244,
                0.101941,
                0.000064,
                0.073130,
                0.068900,
                0.120890,
                0.004000
              
  )
  
  # Apply FDR correction (Benjamini-Hochberg)
  p_adjusted <- p.adjust(p_values, method = "fdr")
  
  # Return a data frame with raw and adjusted p-values
  result <- data.frame(
    raw_p = p_values,
    FDR_corrected_p = p_adjusted
  )
  
  return(result)
}

# Run the function
corrected_results <- correct_pvalues_FDR()

# View the results
cat(corrected_results$FDR_corrected_p, sep = "\n")

### 17/10/2025: More visualisations - MLR


# ---- Libraries ----
library(readxl)
library(dplyr)
library(ggplot2)

# ---- Load your file ----
BINARY <- read_excel(
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Multinomial Reg Group as Outcome/SIGNIFICANT only/BINARY.xlsx"
)

# ---- Prepare data ----
alpha <- 0.05

BINARY <- BINARY %>%
  mutate(
    OR = as.numeric(OR),
    OR_lower = as.numeric(`OR lower`),
    OR_upper = as.numeric(`OR upper`),
    p_nom = as.numeric(P),
    p_fdr = as.numeric(`P FDR`),
    sig_cat = case_when(
      !is.na(p_fdr) & p_fdr < alpha ~ "FDR < 0.05",
      !is.na(p_nom) & p_nom < alpha ~ "Nominal < 0.05",
      TRUE ~ "Not significant"
    ),
    sig_cat = factor(sig_cat, levels = c("FDR < 0.05", "Nominal < 0.05", "Not significant"))
  )

# ---- Plot ----
pd <- position_dodge(width = 0.6)

ggplot(BINARY, aes(x = OR, y = `UKB Phenotype`, color = Model, shape = sig_cat)) +
  geom_point(position = pd, size = 3.2, stroke = 1.2) +
  geom_errorbarh(aes(xmin = OR_lower, xmax = OR_upper),
                 position = pd, height = 0.2, linewidth = 0.6) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4),
                labels = c("0.25","0.5","1","2","4")) +
  facet_wrap(~ Comparison, ncol = 1, scales = "free_y") +
  scale_shape_manual(
    values = c("FDR < 0.05" = 19, "Nominal < 0.05" = 1, "Not significant" = 2),
    drop = FALSE
  ) +
  guides(
    shape = guide_legend(override.aes = list(color = "black", size = 4)),
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  ) +
  labs(
    x = "Odds ratio (log scale)",
    y = "UKB Phenotype",
    title = "Binary Predictors of WMH Change",
    subtitle = "OR with 95% CI by comparison (coloured by model, shaped by significance)",
    shape = "Significance"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 12)
  )

### This code works; replace per predictor type. ###

---------------------------------------------------------------
### 27/10/2025: NEW VISUALISATIONS - Linear Regression Results
---------------------------------------------------------------

# ---- packages ----
library(tidyverse)
library(readxl)
library(janitor)
library(readr)    
library(tidytext) 
library(tidyr)    
library(ggplot2)
library(dplyr)
library(forcats)

# ---- 1) Load Excel (sheet 2) ----
path <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/ALL.xlsx"

df_raw <- read_excel(path, sheet = 2) %>%
  janitor::clean_names()

names(df_raw)

# Version 1 of figure
library(ggplot2)
library(dplyr)
library(forcats)
library(viridis)

df_raw <- df_raw %>%
  mutate(
    model = factor(model, levels = c("Unadjusted", "Partially adjusted", "Fully adjusted")),
    phenotype = factor(phenotype, levels = unique(phenotype)),
    fdr_signif = ifelse(adjusted_p < 0.05, "FDR-adjusted P < 0.05", "FDR-adjusted P ≥ 0.05"),
    predictor_type = factor(predictor_type, levels = unique(predictor_type))
  )

palette_okabe_ito <- c(
  "Unadjusted" = "#56B4E9",
  "Partially adjusted" = "#E69F00",
  "Fully adjusted" = "#009E73"
)

fill_fdr <- c(
  "FDR-adjusted P < 0.05" = "#000000",
  "FDR-adjusted P ≥ 0.05" = "#FFFFFF"
)

ggplot(df_raw, aes(
  x = estimate,
  y = fct_rev(phenotype),
  xmin = ci_lower,
  xmax = ci_upper,
  color = model,
  fill = fdr_signif
)) +
  geom_point(
    shape = 21,
    size = 5,
    stroke = 0.9,
    position = position_dodge(width = 0.7)
  ) +
  geom_errorbarh(height = 0.25, position = position_dodge(width = 0.7), size = 1) +
  scale_color_manual(values = palette_okabe_ito, name = "Covariate Adjustment") +
  scale_fill_manual(values = fill_fdr, name = "FDR Adjustment") +
  labs(
    x = "Estimate (95% CI)",
    y = "Phenotype",
    title = "Forest Plot of Clinical Associations with WMH Change"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(rows = vars(predictor_type), space = "free", scales = "free_y") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "right",
    strip.text.y = element_text(angle = 0, size = 13, face = "bold")
  )

# Version 2 of figure
library(tidyverse)
library(tidytext) # reorder_within / scale_y_reordered
library(forcats)

# assuming df_raw is already read + clean_names()'d
# columns: phenotype, predictor_type, model, estimate, se, ci_lower, ci_upper, p, adjusted_p

plot_df <- df_raw %>%
  mutate(
    model      = factor(model, levels = c("Unadjusted","Partially adjusted","Fully adjusted")),
    fdr_signif = if_else(adjusted_p < 0.05, "FDR-adjusted P < 0.05", "FDR-adjusted P \u2265 0.05"),
    # order phenotypes within each predictor_type by max |estimate| (helps readability)
    abs_est    = abs(estimate)
  ) %>%
  group_by(predictor_type, phenotype) %>%
  mutate(max_abs_est = max(abs_est, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(phenotype_in_facet = tidytext::reorder_within(phenotype, max_abs_est, predictor_type))

# palettes
palette_okabe_ito <- c(
  "Unadjusted"         = "#D55E92",   # blue
  "Partially adjusted" = "cyan3",   # orange
  "Fully adjusted"     = "green4"    # green
)
fill_fdr <- c(
  "FDR-adjusted P < 0.05" = "#000000",  # black fill = sig
  "FDR-adjusted P \u2265 0.05" = "#FFFFFF"  # white fill = non-sig
)

pos <- position_dodge(width = 0.7)

ggplot(
  plot_df,
  aes(
    x    = estimate,
    y    = phenotype_in_facet,
    xmin = ci_lower,
    xmax = ci_upper,
    color = model,
    fill  = fdr_signif,
    group = model        # ensure proper dodging for lines & points
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  # CI bars
  geom_errorbarh(height = 0.25, position = pos, linewidth = 1) +
  # points (shape 21 so we can outline by model, fill by FDR)
  geom_point(shape = 21, size = 3.8, stroke = 1, position = pos) +
  # one panel per predictor type; free_y so each panel has its own list of phenotypes
  facet_wrap(~ predictor_type, ncol = 1, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_color_manual(values = palette_okabe_ito, name = "Model") +
  scale_fill_manual(values = fill_fdr, name = "Adjusted P (FDR)") +
  labs(
    x = "Estimate (95% CI)",
    y = NULL,
    title = "Clinical Associations with WMH Change",
    subtitle = "Faceted by predictor type. Colour = Model; filled (black) = FDR < 0.05; hollow (white) = FDR ≥ 0.05."
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle= element_text(size = 12, hjust = 0.5),
    strip.text   = element_text(face = "bold"),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

# Make order consistent with legend
plot_df <- plot_df %>%
  mutate(
    # keep legend order you want
    model = factor(model, levels = c("Unadjusted","Partially adjusted","Fully adjusted")),
    # use the reversed order ONLY for dodging so rows appear top→bottom like the legend
    model_dodge = forcats::fct_rev(model),
    fdr_signif  = if_else(adjusted_p < 0.05, "FDR-adjusted P < 0.05", "FDR-adjusted P \u2265 0.05"),
    abs_est     = abs(estimate)
  ) %>%
  group_by(predictor_type, phenotype) %>%
  mutate(max_abs_est = max(abs_est, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(phenotype_in_facet = tidytext::reorder_within(phenotype, max_abs_est, predictor_type))

pos <- position_dodge(width = 0.6)

ggplot(
  plot_df,
  aes(
    x    = estimate,
    y    = phenotype_in_facet,
    xmin = ci_lower,
    xmax = ci_upper,
    color = model,          # legend order
    fill  = fdr_signif,
    group = model_dodge     # dodge order for alignment
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_errorbarh(height = 0.25, position = pos, linewidth = 1) +
  geom_point(shape = 21, size = 3.8, stroke = 1, position = pos) +
  facet_wrap(~ predictor_type, ncol = 1, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_color_manual(
    values = c(
      "Unadjusted"         = "#56B4E9",
      "Partially adjusted" = "cyan3",
      "Fully adjusted"     = "#D55E92"
    ),
    breaks = c("Unadjusted","Partially adjusted","Fully adjusted"),
    name   = "Model"
  ) +
  scale_fill_manual(
    values = c(
      "FDR-adjusted P < 0.05"  = "#000000",
      "FDR-adjusted P \u2265 0.05" = "#FFFFFF"
    ),
    name = "Adjusted P (FDR)"
  ) +
  labs(
    x = "Estimate (95% CI)",
    y = "UKB Phenotype",  # ✅ added back here
    title = "Clinical Associations with WMH Change",
    subtitle = "Faceted by predictor type. Top-to-bottom per phenotype: Unadjusted, Partially adjusted, Fully adjusted."
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text    = element_text(face = "bold"),
    axis.text.y   = element_text(size = 12),
    axis.title.y  = element_text(size = 14, face = "bold"),
    axis.title.x  = element_text(size = 14, face = "bold"), 
    legend.title  = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

# ---Multinomial Regression Results: more plots--- 

# Load additional libraries (if required)
library(readxl)
library(dplyr)
library(janitor)
library(forcats)
library(tidytext)
library(ggplot2)
library(scales)
library(readr)

# Read data (change accordingly, if needed)
path <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Multinomial Reg Group as Outcome/SIGNIFICANT only/ALL COMBINED SIGN.xlsx"

df_raw <- read_excel(path, sheet = 2) %>%
  clean_names() %>%
  mutate(
    # standardize model labels
    model = case_when(
      tolower(model) %in% c("unadjusted","un adj","un-adj") ~ "Unadjusted",
      tolower(model) %in% c("partially adjusted","partial") ~ "Partially adjusted",
      tolower(model) %in% c("fully adjusted","full")        ~ "Fully adjusted",
      TRUE ~ model
    ),
    # standardize comparison labels (ensure exactly two)
    comparison = case_when(
      grepl("progression", comparison, ignore.case = TRUE) ~ "Progression vs Stable",
      grepl("regression", comparison, ignore.case = TRUE)  ~ "Regression vs Stable",
      TRUE ~ comparison
    )
  ) %>%
  filter(model %in% c("Unadjusted","Partially adjusted","Fully adjusted"),
         comparison %in% c("Progression vs Stable", "Regression vs Stable"))

# numeric safety
num_cols <- c("estimate","or","or_lower","or_upper","p","p_fdr")
df_raw <- df_raw %>% mutate(across(any_of(num_cols), ~ suppressWarnings(parse_number(as.character(.)))))

# Prep for plotting
plot_df <- df_raw %>%
  transmute(
    phenotype,
    predictor_type,
    comparison,
    model = factor(model, levels = c("Unadjusted","Partially adjusted","Fully adjusted")),
    model_dodge = forcats::fct_rev(model),        # for row order = top→bottom: Unadj, Partial, Full
    OR      = or,
    OR_low  = or_lower,
    OR_high = or_upper,
    p_fdr   = p_fdr
  ) %>%
  mutate(
    fdr_signif = if_else(p_fdr < 0.05, "FDR-adjusted P < 0.05", "FDR-adjusted P \u2265 0.05"),
    abs_est    = abs(log(OR))
  ) %>%
  group_by(predictor_type, comparison, phenotype) %>%
  mutate(max_abs_est = max(abs_est, na.rm = TRUE)) %>%
  ungroup() %>%
  # order phenotypes within EACH (predictor_type x comparison)
  mutate(phenotype_in_facet = tidytext::reorder_within(phenotype, max_abs_est, interaction(predictor_type, comparison, drop = TRUE)))

# palettes
palette_models <- c(
  "Unadjusted"         = "#56B4E9",
  "Partially adjusted" = "cyan3",
  "Fully adjusted"     = "#D55E92"
)
fill_fdr <- c(
  "FDR-adjusted P < 0.05"    = "#000000",
  "FDR-adjusted P \u2265 0.05" = "#FFFFFF"
)

pos <- position_dodge(width = 0.6)

p <- ggplot(
  plot_df,
  aes(
    x     = OR,
    y     = phenotype_in_facet,
    xmin  = OR_low,
    xmax  = OR_high,
    color = model,         # legend order
    fill  = fdr_signif,
    group = model_dodge    # dodge order for alignment
  )
) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey70") +
  geom_errorbarh(height = 0.25, position = pos, linewidth = 1) +
  geom_point(shape = 21, size = 3.8, stroke = 1, position = pos) +
  # Two stacked rows by comparison; columns by predictor_type. Free y per panel.
  facet_grid(rows = vars(comparison), cols = vars(predictor_type), scales = "free_y", space = "free_y") +
  tidytext::scale_y_reordered() +
  scale_x_log10(breaks = c(0.5, 0.67, 1, 1.5, 2, 3), labels = label_number(accuracy = 0.01)) +
  scale_color_manual(values = palette_models, name = "Model") +
  scale_fill_manual(values = fill_fdr, name = "Adjusted P (FDR)") +
  labs(
    x = "Odds Ratio (95% CI), log scale",
    y = "Phenotype",
    title = "Clinical Associations with WMH Change",
    subtitle = "Top: Progression vs Stable • Bottom: Regression vs Stable. Colour = Model; filled = FDR < 0.05."
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text    = element_text(face = "bold"),
    axis.text.y   = element_text(size = 11),
    axis.title.y  = element_text(size = 13, face = "bold"),
    axis.title.x  = element_text(size = 13, face = "bold"),
    legend.title  = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

p

make_plot <- function(dat, comp_label) {
  ggplot(
    dat %>% filter(comparison == comp_label),
    aes(
      x     = OR, y = phenotype_in_facet,
      xmin  = OR_low, xmax = OR_high,
      color = model, fill = fdr_signif, group = model_dodge
    )
  ) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey70") +
    geom_errorbarh(height = 0.25, position = pos, linewidth = 1) +
    geom_point(shape = 21, size = 3.8, stroke = 1, position = pos) +
    facet_wrap(~ predictor_type, ncol = 1, scales = "free_y") +
    tidytext::scale_y_reordered() +
    scale_x_log10(breaks = c(0.5, 0.67, 1, 1.5, 2, 3), labels = label_number(accuracy = 0.01)) +
    scale_color_manual(values = palette_models, name = "Model") +
    scale_fill_manual(values = fill_fdr, name = "Adjusted P (FDR)") +
    labs(
      x = "Odds Ratio (95% CI), log scale",
      y = "Phenotype",
      title = comp_label
    ) +
    theme_bw(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(size = 11),
      panel.grid.major.y = element_blank()
    )
}

p_prog <- make_plot(plot_df, "Progression vs Stable")
p_regr <- make_plot(plot_df, "Regression vs Stable")

# Show one after the other
p_prog
p_regr

# Another option - Keep this for manuscript submission to Neurology

# Read data
path <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Multinomial Reg Group as Outcome/SIGNIFICANT only/ALL COMBINED SIGN.xlsx"

library(readxl)
library(dplyr)
library(janitor)
library(forcats)
library(tidytext)
library(ggplot2)
library(scales)
library(readr)
install.packages("cowplot")
library(cowplot)   # <-- to combine plots

df_raw <- read_excel(path, sheet = 2) %>%
  clean_names() %>%
  mutate(
    model = case_when(
      tolower(model) %in% c("unadjusted","un adj","un-adj") ~ "Unadjusted",
      tolower(model) %in% c("partially adjusted","partial") ~ "Partially adjusted",
      tolower(model) %in% c("fully adjusted","full")        ~ "Fully adjusted",
      TRUE ~ model
    ),
    comparison = case_when(
      grepl("progression", comparison, ignore.case = TRUE) ~ "Progression vs Stable",
      grepl("regression", comparison, ignore.case = TRUE)  ~ "Regression vs Stable",
      TRUE ~ comparison
    )
  ) %>%
  filter(model %in% c("Unadjusted","Partially adjusted","Fully adjusted"),
         comparison %in% c("Progression vs Stable","Regression vs Stable"))

# numeric safety
num_cols <- c("estimate","or","or_lower","or_upper","p","p_fdr")
df_raw <- df_raw %>% mutate(across(any_of(num_cols), ~ suppressWarnings(parse_number(as.character(.)))))

# Prepare
plot_df <- df_raw %>%
  transmute(
    phenotype,
    predictor_type,
    comparison,
    model = factor(model, levels = c("Unadjusted","Partially adjusted","Fully adjusted")),
    model_dodge = forcats::fct_rev(model),
    OR      = or,
    OR_low  = or_lower,
    OR_high = or_upper,
    p_fdr   = p_fdr
  ) %>%
  mutate(
    fdr_signif = if_else(p_fdr < 0.05, "FDR-adjusted P < 0.05", "FDR-adjusted P \u2265 0.05"),
    abs_est    = abs(log(OR))
  ) %>%
  group_by(predictor_type, comparison, phenotype) %>%
  mutate(max_abs_est = max(abs_est, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(phenotype_in_facet =
           tidytext::reorder_within(phenotype, max_abs_est, interaction(predictor_type, comparison, drop = TRUE)))

palette_models <- c(
  "Unadjusted"         = "#56B4E9",
  "Partially adjusted" = "cyan3",
  "Fully adjusted"     = "#D55E92"
)
fill_fdr <- c(
  "FDR-adjusted P < 0.05"    = "#000000",
  "FDR-adjusted P \u2265 0.05" = "#FFFFFF"
)

pos <- position_dodge(width = 0.6)

# Make plot (can re-use)
make_plot <- function(dat, comp_label) {
  ggplot(
    dat %>% filter(comparison == comp_label),
    aes(
      x     = OR, y = phenotype_in_facet,
      xmin  = OR_low, xmax = OR_high,
      color = model, fill = fdr_signif, group = model_dodge
    )
  ) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey70") +
    geom_errorbarh(height = 0.25, position = pos, linewidth = 1) +
    geom_point(shape = 21, size = 3.8, stroke = 1, position = pos) +
    facet_wrap(~ predictor_type, ncol = 1, scales = "free_y") +
    tidytext::scale_y_reordered() +
    scale_x_log10(breaks = c(0.5, 0.67, 1, 1.5, 2, 3),
                  labels = label_number(accuracy = 0.01)) +
    scale_color_manual(values = palette_models, name = "Model") +
    scale_fill_manual(values = fill_fdr, name = "Adjusted P (FDR)") +
    labs(
      x = "Odds Ratio (95% CI)",
      y = "Phenotype",
      title = comp_label
    ) +
    theme_bw(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    ) + scale_x_log10(
      limits = c(0.4, 3.2),  # widen the range here
      breaks = c(0.5, 0.67, 1, 1.5, 2, 3),
      labels = label_number(accuracy = 0.01)
    )
}

# Two subplots for the two group comparisons
p_prog <- make_plot(plot_df, "Progression vs Stable")
p_regr <- make_plot(plot_df, "Regression vs Stable")

# Combine vertically in one fig
combined_plot <- p_prog / p_regr + 
  plot_annotation(
    title = "Clinical Associations with WMH Change",
    subtitle = "Odds ratios shown for each adjustment level per phenotype, separated by comparison type",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

combined_plot_side <- cowplot::plot_grid(
  p_prog + theme(legend.position = "none"),
  p_regr + theme(legend.position = "bottom"),
  ncol = 2,         # ⬅️ side-by-side
  rel_widths = c(1, 1.1)  # optional, gives a bit more width to right panel
)

combined_plot_side

ggsave("combined_side_by_side.png", combined_plot_side,
       width = 16, height = 8, dpi = 300)

library(cowplot)
library(ggplot2)
library(grid)     # for unit()

# 1) Make the two plots a bit roomier and consistent
p_prog_aligned <- p_prog +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(6, 6, 6, 6),
    plot.margin = margin(10, 10, 6, 12),
    panel.spacing.x = unit(0.8, "lines")
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE, title = "Model"),
    fill   = guide_legend(nrow = 1, byrow = TRUE, title = "Adjusted P (FDR)")
  )

p_regr_aligned <- p_regr +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(6, 6, 6, 6),
    plot.margin = margin(10, 12, 6, 10),
    panel.spacing.x = unit(0.8, "lines")
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE, title = "Model"),
    fill   = guide_legend(nrow = 1, byrow = TRUE, title = "Adjusted P (FDR)")
  )

# 2) Extract a single shared legend (use one of the plots)
shared_legend <- cowplot::get_legend(p_prog_aligned)

# 3) Remove legends from the panels themselves
p_prog_noleg <- p_prog_aligned + theme(legend.position = "none")
p_regr_noleg <- p_regr_aligned + theme(legend.position = "none")

# 4) Align the two plots so y-axes/strips/panels match exactly
aligned <- cowplot::align_plots(
  p_prog_noleg, p_regr_noleg,
  align = "hv", axis = "tblr"
)

# 5) Put them side-by-side with equal widths, then add the legend below
side_by_side <- cowplot::plot_grid(
  aligned[[1]], aligned[[2]],
  ncol = 2, rel_widths = c(1, 1)
)

final_plot <- cowplot::plot_grid(
  side_by_side,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.12)  # a little extra space for the legend
)

# Add title to plot
final_plot_titled <- cowplot::ggdraw() +
  draw_label(
    "Clinical Associations with WMH Change according to Group Membership",
    fontface = "bold",
    size = 18,
    x = 0.5, y = 0.99, hjust = 0.5
  ) +
  draw_plot(final_plot, y = 0, height = 0.95)

final_plot_titled

# Save the titled version
ggsave(
  "wmh_progression_regression_side_by_side_titled.png",
  final_plot_titled,
  width = 16, height = 9, dpi = 300
)

final_plot_titled

### Another version of the figure (to use in manuscript after requested edits from Neurology) ### 

# Data 
path <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Multinomial Reg Group as Outcome/SIGNIFICANT only/ALL COMBINED SIGN.xlsx"

df_raw <- read_excel(path, sheet = 2) %>%
  clean_names() %>%
  mutate(
    model = case_when(
      tolower(model) %in% c("unadjusted","un adj","un-adj") ~ "Unadjusted",
      tolower(model) %in% c("partially adjusted","partial") ~ "Partially adjusted",
      tolower(model) %in% c("fully adjusted","full")        ~ "Fully adjusted",
      TRUE ~ model
    ),
    comparison = case_when(
      grepl("progression", comparison, ignore.case = TRUE) ~ "Progression vs Stable",
      grepl("regression", comparison, ignore.case = TRUE)  ~ "Regression vs Stable",
      TRUE ~ comparison
    )
  ) %>%
  filter(
    model %in% c("Unadjusted","Partially adjusted","Fully adjusted"),
    comparison %in% c("Progression vs Stable","Regression vs Stable")
  )

# numeric safety (parsing from strings, etc.)
num_cols <- c("estimate","or","or_lower","or_upper","p","p_fdr")
df_raw <- df_raw %>%
  mutate(across(any_of(num_cols), ~ suppressWarnings(parse_number(as.character(.)))))

# Prepare plotting data 
plot_df <- df_raw %>%
  transmute(
    phenotype,
    predictor_type,
    comparison,
    model = factor(model, levels = c("Unadjusted","Partially adjusted","Fully adjusted")),
    model_dodge = forcats::fct_rev(model),
    OR      = or,
    OR_low  = or_lower,
    OR_high = or_upper,
    p_fdr   = p_fdr
  ) %>%
  # log-scale safety: make sure all values are strictly positive
  mutate(across(c(OR, OR_low, OR_high),
                ~ ifelse(is.na(.), NA_real_, pmax(., 1e-8)))) %>%
  mutate(
    fdr_signif = if_else(p_fdr < 0.05, "FDR-adjusted P < 0.05", "FDR-adjusted P \u2265 0.05"),
    abs_est    = abs(log(OR))
  ) %>%
  group_by(predictor_type, comparison, phenotype) %>%
  mutate(max_abs_est = max(abs_est, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(phenotype_in_facet =
           tidytext::reorder_within(phenotype, max_abs_est, interaction(predictor_type, comparison, drop = TRUE)))

palette_models <- c(
  "Unadjusted"         = "#56B4E9",
  "Partially adjusted" = "cyan3",
  "Fully adjusted"     = "#D55E92"
)
fill_fdr <- c(
  "FDR-adjusted P < 0.05"      = "#000000",
  "FDR-adjusted P \u2265 0.05" = "#FFFFFF"
)

pos <- position_dodge(width = 0.6)

# Plot factory
make_plot <- function(dat, comp_label) {
  ggplot(
    dat %>% filter(comparison == comp_label),
    aes(
      x     = OR, y = phenotype_in_facet,
      xmin  = OR_low, xmax = OR_high,
      color = model, fill = fdr_signif, group = model_dodge
    )
  ) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey70") +
    geom_errorbarh(height = 0.25, position = pos, linewidth = 1) +
    geom_point(shape = 21, size = 3.8, stroke = 1, position = pos) +
    facet_wrap(~ predictor_type, ncol = 1, scales = "free_y") +
    tidytext::scale_y_reordered() +
    scale_x_log10(
      limits = c(0.2, 3),  # manually define range of odds ratios
      breaks = c(0.25, 0.5, 0.67, 1, 1.5, 2, 3),
      labels = scales::label_number(accuracy = 0.01),
      expand = expansion(mult = c(0, 0))  # avoid extra padding
    ) +
    coord_cartesian(clip = "off")
}


# Build both panels 
p_prog <- make_plot(plot_df, "Progression vs Stable")
p_regr <- make_plot(plot_df, "Regression vs Stable")

# Styling/alignment for combined figure
p_prog_aligned <- p_prog +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(6, 6, 6, 6),
    plot.margin = margin(10, 10, 6, 12),
    panel.spacing.x = unit(0.8, "lines")
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE, title = "Model"),
    fill   = guide_legend(nrow = 1, byrow = TRUE, title = "Adjusted P (FDR)")
  )

p_regr_aligned <- p_regr +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(6, 6, 6, 6),
    plot.margin = margin(10, 12, 6, 10),
    panel.spacing.x = unit(0.8, "lines")
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE, title = "Model"),
    fill   = guide_legend(nrow = 1, byrow = TRUE, title = "Adjusted P (FDR)")
  )

# Shared legend
shared_legend <- cowplot::get_legend(p_prog_aligned)

# Remove legends from individual panels
p_prog_noleg <- p_prog_aligned + theme(legend.position = "none")
p_regr_noleg <- p_regr_aligned + theme(legend.position = "none")

# Align and combine
aligned <- cowplot::align_plots(
  p_prog_noleg, p_regr_noleg,
  align = "hv", axis = "tblr"
)

side_by_side <- cowplot::plot_grid(
  aligned[[1]], aligned[[2]],
  ncol = 2, rel_widths = c(1, 1)
)

final_plot <- cowplot::plot_grid(
  side_by_side,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

final_plot_titled <- cowplot::ggdraw() +
  draw_label(
    "Clinical Associations with WMH Change according to Group Membership",
    fontface = "bold",
    size = 18,
    x = 0.5, y = 0.99, hjust = 0.5
  ) +
  draw_plot(final_plot, y = 0, height = 0.95)

# Print/Save 
final_plot_titled

# ---- Additional Packages ----
library(readxl)
library(dplyr)
library(janitor)
library(forcats)
library(tidytext)
library(ggplot2)
library(scales)
library(readr)
library(cowplot)
library(grid) # for unit()

# Load data 
path <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Multinomial Reg Group as Outcome/SIGNIFICANT only/ALL COMBINED SIGN.xlsx"

df_raw <- read_excel(path, sheet = 2) %>%
  clean_names() %>%
  mutate(
    model = case_when(
      tolower(model) %in% c("unadjusted","un adj","un-adj") ~ "Unadjusted",
      tolower(model) %in% c("partially adjusted","partial") ~ "Partially adjusted",
      tolower(model) %in% c("fully adjusted","full")        ~ "Fully adjusted",
      TRUE ~ model
    ),
    comparison = case_when(
      grepl("progression", comparison, ignore.case = TRUE) ~ "Progression vs Stable",
      grepl("regression", comparison, ignore.case = TRUE)  ~ "Regression vs Stable",
      TRUE ~ comparison
    )
  ) %>%
  filter(
    model %in% c("Unadjusted","Partially adjusted","Fully adjusted"),
    comparison %in% c("Progression vs Stable","Regression vs Stable")
  )

# numeric safety
num_cols <- c("estimate","or","or_lower","or_upper","p","p_fdr")
df_raw <- df_raw %>%
  mutate(across(any_of(num_cols), ~ suppressWarnings(parse_number(as.character(.)))))

# Prepare plotting data 
plot_df <- df_raw %>%
  transmute(
    phenotype,
    predictor_type,
    comparison,
    model = factor(model, levels = c("Unadjusted","Partially adjusted","Fully adjusted")),
    model_dodge = forcats::fct_rev(model),
    OR      = or,
    OR_low  = or_lower,
    OR_high = or_upper,
    p_fdr   = p_fdr
  ) %>%
  # log-scale safety: strictly positive
  mutate(across(c(OR, OR_low, OR_high),
                ~ ifelse(is.na(.), NA_real_, pmax(., 1e-8)))) %>%
  mutate(
    fdr_signif = if_else(p_fdr < 0.05, "FDR-adjusted P < 0.05", "FDR-adjusted P \u2265 0.05"),
    abs_est    = abs(log(OR))
  ) %>%
  group_by(predictor_type, comparison, phenotype) %>%
  mutate(max_abs_est = max(abs_est, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(phenotype_in_facet =
           tidytext::reorder_within(phenotype, max_abs_est, interaction(predictor_type, comparison, drop = TRUE)))

# Aesthetics
palette_models <- c(
  "Unadjusted"         = "#56B4E9",
  "Partially adjusted" = "cyan3",
  "Fully adjusted"     = "#D55E92"
)
fill_fdr <- c(
  "FDR-adjusted P < 0.05"      = "#000000",
  "FDR-adjusted P \u2265 0.05" = "#FFFFFF"
)
pos <- position_dodge(width = 0.6)

# Plot function
make_plot <- function(dat, comp_label) {
  ggplot(
    dat %>% filter(comparison == comp_label),
    aes(
      x     = OR, y = phenotype_in_facet,
      xmin  = OR_low, xmax = OR_high,
      color = model, fill = fdr_signif, group = model_dodge
    )
  ) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey70") +
    geom_errorbarh(height = 0.25, position = pos, linewidth = 1) +
    geom_point(shape = 21, size = 3.8, stroke = 1, position = pos) +
    facet_wrap(~ predictor_type, ncol = 1, scales = "free_y") +
    tidytext::scale_y_reordered() +
    scale_x_log10(
      breaks = c(0.1, 0.2, 0.4, 0.8, 1.5, 3),
      labels = scales::label_number(accuracy = 0.01),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(xlim = c(0.05, 3), clip = "off") +
    scale_color_manual(values = palette_models, name = "Model") +
    scale_fill_manual(values = fill_fdr, name = "Adjusted P (FDR)") +
    labs(
      x = "OR (95% CI)",
      y = "Phenotype",
      title = comp_label
    ) +
    theme_bw(base_size = 13) +
    theme(
      plot.title        = element_text(face = "bold", size = 16, hjust = 0.5),
      strip.text        = element_text(face = "bold", size = 12),  # bold facet titles
      strip.background  = element_rect(fill = "white", colour = "black"), # white bg, dark border
      axis.text.y       = element_text(size = 10),
      axis.title.x      = element_text(size = 12, face = "bold"),
      axis.title.y      = element_text(size = 12, face = "bold"),
      legend.position   = "bottom",
      panel.grid.major.y = element_blank()
    )
}


# Build both panels 
p_prog <- make_plot(plot_df, "Progression vs Stable")
p_regr <- make_plot(plot_df, "Regression vs Stable")

# Styling/alignment for combined figure 
p_prog_aligned <- p_prog +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(6, 6, 6, 6),
    plot.margin = margin(10, 10, 6, 12),
    panel.spacing.x = unit(0.8, "lines")
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE, title = "Model"),
    fill   = guide_legend(nrow = 1, byrow = TRUE, title = "Adjusted P (FDR)")
  )

p_regr_aligned <- p_regr +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(6, 6, 6, 6),
    plot.margin = margin(10, 12, 6, 10),
    panel.spacing.x = unit(0.8, "lines")
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE, title = "Model"),
    fill   = guide_legend(nrow = 1, byrow = TRUE, title = "Adjusted P (FDR)")
  )

# Shared legend, align, combine 
shared_legend <- cowplot::get_legend(p_prog_aligned)

p_prog_noleg <- p_prog_aligned + theme(legend.position = "none")
p_regr_noleg <- p_regr_aligned + theme(legend.position = "none")

aligned <- cowplot::align_plots(
  p_prog_noleg, p_regr_noleg,
  align = "hv", axis = "tblr"
)

side_by_side <- cowplot::plot_grid(
  aligned[[1]], aligned[[2]],
  ncol = 2, rel_widths = c(1, 1)
)

final_plot <- cowplot::plot_grid(
  side_by_side,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

final_plot_titled <- cowplot::ggdraw() +
  draw_label(
    "Clinical Associations with WMH Change according to Group Membership",
    fontface = "bold",
    size = 18,
    x = 0.5, y = 0.99, hjust = 0.5
  ) +
  draw_plot(final_plot, y = 0, height = 0.95)

# Print/Save
final_plot_titled

