
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
library(broom)
library(car) # for Anova()
library(emmeans)

# Load data
merged_extended_progressors <- read_tsv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended_progressors.tsv",
                                        show_col_types = FALSE)


# ==================================================================================
# 01/10/2025: New Form of Analysis - Adding follow-up time as an extra covariate
# This analysis will go into the paper for publication
# ==================================================================================

# First, check all covariates again
describe(df$x31_0_0) # 4329
describe(df$x21022_0_0) # 4329
describe(df$xICV_2_0) # 4329
describe(df$x6150.1) # 336
describe(df$HTN_med) # 217
describe(df$xYears_2) # 4201

# Check unique values and counts
table(merged_extended$x6150.4, useNA = "ifany")

# Check how many are coded as "4"
sum(merged_extended$x6150.4 == 4, na.rm = TRUE)

# Proportion missing (if 4 = missing)
mean(merged_extended$x6150.4 == 4, na.rm = TRUE)
describe(df$x6150.100) # 336
describe(merged_extended$x6177.100) # 1950

# New BP covariates
describe(merged_extended$x4079) # 3129; automated
describe(merged_extended$x4080) # 3129; automated

describe(merged_extended$x93) # 842; manual
describe(merged_extended$x94) # 842; manual 

# =========================
# Setup
# =========================

# Outcome variable
outcome <- "WMH_TP2_adj_std"

# =========================
# Setup covariates
# =========================

# Covariates
covars_min <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "xYears_2") # age, sex, ICV, follow-up
covars_ext <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "xYears_2", "x4079", "x4080") # + SBP, DBP

# Results folder
results_dir <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas"

# Loops for regression analyses 

# =========================================
# Binary phenotype regressions (unadjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# =========================================

# Binary predictors
binary_vars <- c(
  "x1210","x1920","x1930","x1940","x1950","x1960","x1970","x1980","x1990",
  "x2000","x2010","x2020","x2030","x2040","x2188","x2207","x2227","x2316",
  "x2335","x2443","x2956","x3005","x3393","x3404","x3414","x3571","x3606",
  "x3741","x3799","x4067","x4598","x4631","x4642","x4653","x4717","x4728",
  "x4792","x5663","x6145.3","x6145.6","x6145.2","x6145.1","x6145.4","x6145.5",
  "x6148.5","x6148.4","x6148.1","x6148.6","x6148.2","x6148.3","x6149.1",
  "x6149.3","x6149.6","x6149.4","x6149.5","x6149.2","x6150.100","x6150.4",
  "x6150.2","x6150.1","x6150.3","x20116","x20117","x20122","x20123",
  "x20419","x20425","x20426","x20427","x20437","x20449","x20450","x20532",
  "x20533","x20534","x20535","x20540","x20541","x21024","x21064","x21065"
)

# Labels lookup table
binary_labels <- data.frame(
  phenotype = binary_vars,
  label = c(
    "Snoring","Mood swings","Miserableness","Irritability","Sensitivity/hurt feelings",
    "Fed-up feelings","Nervous feelings","Worrier/anxious","Tense/highly strung",
    "Worry too long after embarrassment","Suffer from nerves","Loneliness/isolation",
    "Guilty feelings","Risk taking","Long-standing illness/disability","Wears glasses/contacts",
    "Other eye problems","Wheeze in chest","Chest pain/discomfort","Diabetes diagnosed",
    "General pain 3+ months","Fracture from fall","Hearing aid user","Neck/shoulder pain 3+ months",
    "Hip pain 3+ months","Back pain 3+ months","Chest pain when walking","Abdominal pain 3+ months",
    "Headaches 3+ months","Facial pain 3+ months","Ever depressed ≥1 week","Ever disinterested ≥1 week",
    "Ever hyper/manic ≥2 days","Ever highly irritable","Shortness of breath on level ground",
    "Leg pain on walking","Cochlear implant","Longest manic/irritable episode",
    "Stress: death of relative","Stress: financial","Stress: serious illness/assault relative",
    "Stress: serious illness/assault self","Stress: death of spouse","Stress: divorce/separation",
    "Macular degeneration","Cataract","Diabetes-related eye disease","Other serious eye condition",
    "Glaucoma","Vision loss from injury","Mouth ulcers","Bleeding gums","Dentures",
    "Loose teeth","Toothache","Painful gums","Any vascular/heart problem",
    "High blood pressure","Angina","Heart attack","Stroke",
    "Smoking status","Alcohol drinker status","Bipolar disorder",
    "Probable major depression (single episode)","Difficulty concentrating (anxiety)",
    "Ever worried more than others","Restless (anxiety)","Sleep problems (anxiety)",
    "Thoughts of death (depression)","Tiredness (depression)","Worthlessness (depression)",
    "Sleep change","Trouble falling asleep","Sleeping too much","Waking too early",
    "Multiple worries","Difficulty stopping worrying","IBS diagnosis","Sensitive stomach",
    "Family history of IBS"
  )
)

# =========================
# Run regressions
# =========================
results <- list()

for (var in binary_vars) {
  form <- as.formula(paste(outcome, "~", var))
  
  fit <- try(lm(form, data = merged_extended), silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    tidy_fit <- broom::tidy(fit, conf.int = TRUE)
    
    # Only proceed if the predictor exists in the model results
    if (var %in% tidy_fit$term) {
      predictor_res <- tidy_fit %>% filter(term == var)
      
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

results_df <- bind_rows(results)

# Combine results
results_df <- bind_rows(results)

# Add descriptive labels
results_df <- results_df %>%
  left_join(binary_labels, by = "phenotype") %>%
  select(phenotype, label, everything())

# FDR correction
results_df$pval_fdr <- p.adjust(results_df$pval, method = "fdr")

# =========================
# Save results
# =========================

write.csv(
  results_df,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/binary_unadjusted_results.csv",
  row.names = FALSE
)

# ========================================================
# Binary phenotype regressions (min adjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# Covariates = sex, age, ICV, follow-up time; then add BP
# ========================================================

# =========================
# Run adjusted regressions
# =========================
results_adj <- list()

# change from covars_min to covars_ext
for (var in binary_vars) {
  # Build formula with covariates
  form <- as.formula(paste(outcome, "~", var, "+", paste(covars_min, collapse = " + ")))
  
  fit <- try(lm(form, data = merged_extended), silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    tidy_fit <- broom::tidy(fit, conf.int = TRUE)
    
    if (var %in% tidy_fit$term) {
      predictor_res <- tidy_fit %>% filter(term == var)
      
      results_adj[[var]] <- data.frame(
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

# Combine results
results_adj_df <- bind_rows(results_adj)

# Add descriptive labels
results_adj_df <- results_adj_df %>%
  left_join(binary_labels, by = "phenotype") %>%
  select(phenotype, label, everything())

# FDR correction + sort
results_adj_df$pval_fdr <- p.adjust(results_adj_df$pval, method = "fdr")
results_adj_df <- results_adj_df %>% arrange(pval_fdr)

# =========================
# Save results
# =========================
write.csv(results_adj_df,
          "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/binary_adj_min_FDR.csv",
          row.names = FALSE)

write.csv(results_adj_df,
          "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/binary_adj_ext_FDR.csv",
          row.names = FALSE)

# Peek
head(results_adj_df)

# =========================================================
# Ordered categorical phenotype regressions (unadjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# =========================================================

ord_vars <- c(
  "x884","x894","x904","x924","x991","x1001","x1021","x1200","x1220","x1239",
  "x1558","x2050","x2060","x2070","x2080","x2110","x2178","x2296","x4526",
  "x4620","x6015","x6016","x6373","x20016","x20127","x20240","x20458",
  "x20459","x21048"
)

ord_labels <- data.frame(
  phenotype = ord_vars,
  label = c(
    "Moderate activity days/weeks","Moderate activity duration",
    "Vigorous activity days/weeks","Walking pace","Strenuous sports frequency",
    "Strenuous sports duration","Light DIY duration","Sleeplessness/insomnia",
    "Daytime dozing","Current smoking","Alcohol intake frequency",
    "Depressed mood (2 weeks)","Unenthusiasm (2 weeks)","Tenseness/restlessness (2 weeks)",
    "Tiredness/lethargy (2 weeks)","Able to confide","Overall health rating",
    "Falls in last year","Happiness","Depression episodes",
    "Chest pain during activity","Chest pain outside activity","Puzzle score",
    "Fluid intelligence","Neuroticism","Max digits remembered",
    "General happiness","Happiness with health","Back pain (3 months)"
  )
)

run_ord_models <- function(data, outcome, predictors, covars, labels, outfile) {
  results <- list()
  
  for (var in predictors) {
    form <- as.formula(paste(outcome, "~", var, 
                             if(length(covars) > 0) paste("+", paste(covars, collapse = " + ")) else ""))
    
    fit <- try(lm(form, data = data), silent = TRUE)
    
    if (!inherits(fit, "try-error")) {
      tidy_fit <- broom::tidy(fit, conf.int = TRUE)
      
      if (var %in% tidy_fit$term) {
        predictor_res <- tidy_fit %>% filter(term == var)
        
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
  
  results_df <- bind_rows(results)
  
  # Add labels
  results_df <- results_df %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, everything())
  
  # FDR correction + sort
  results_df$pval_fdr <- p.adjust(results_df$pval, method = "fdr")
  results_df <- results_df %>% arrange(pval_fdr)
  
  # Save
  write.csv(results_df, outfile, row.names = FALSE)
  
  return(results_df)
}

# Run models
outcome <- "WMH_TP2_adj_std"

# Unadjusted
res_ord_unadj <- run_ord_models(
  merged_extended, outcome, ord_vars, c(), ord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/orderedcat_unadjusted_FDR.csv"
)

# Minimal adjusted
res_ord_min <- run_ord_models(
  merged_extended, outcome, ord_vars, covars_min, ord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/orderedcat_minadj_FDR.csv"
)

# Extended adjusted
res_ord_ext <- run_ord_models(
  merged_extended, outcome, ord_vars, covars_ext, ord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/orderedcat_extadj_FDR.csv"
)

# =======================================================
# Unordered categorical phenotype regressions (unadjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# =======================================================

unord_vars <- c("x1538", "x20126", "x20536")

unord_labels <- data.frame(
  phenotype = unord_vars,
  label = c(
    "Major dietary changes (5 years)",
    "Bipolar and major depression status",
    "Weight change during worst depression episode"
  )
)

library(dplyr)
library(broom)
library(car)   # for Anova()

run_unord_models <- function(data, outcome, predictors, covars, labels, outfile) {
  results <- list()
  
  for (var in predictors) {
    form <- as.formula(paste(outcome, "~ factor(", var, ")", 
                             if(length(covars) > 0) paste("+", paste(covars, collapse = " + ")) else ""))
    
    fit <- try(lm(form, data = data), silent = TRUE)
    
    if (!inherits(fit, "try-error")) {
      tidy_fit <- broom::tidy(fit, conf.int = TRUE)
      anova_p <- try(Anova(fit, type = 3)$`Pr(>F)`[1], silent = TRUE)  # overall p-value
      
      # Store results (one row per coefficient + overall p)
      coef_results <- tidy_fit %>%
        filter(term != "(Intercept)") %>%
        mutate(phenotype = var, overall_p = ifelse(!inherits(anova_p, "try-error"), anova_p, NA))
      
      results[[var]] <- coef_results
    } else {
      message("Skipping variable: ", var, " (fit failed)")
    }
  }
  
  results_df <- bind_rows(results)
  
  # Add labels
  results_df <- results_df %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, term, estimate, std.error, conf.low, conf.high, p.value, overall_p)
  
  # FDR correction on per-coefficient p-values
  results_df$pval_fdr <- p.adjust(results_df$p.value, method = "fdr")
  
  # Save
  write.csv(results_df, outfile, row.names = FALSE)
  
  return(results_df)
}


# Unadjusted
res_unord_unadj <- run_unord_models(
  merged_extended, outcome, unord_vars, c(), unord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/unorderedcat_unadj_FDR.csv"
)

# Minimal adjusted
res_unord_min <- run_unord_models(
  merged_extended, outcome, unord_vars, covars_min, unord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/unorderedcat_mindadj_FDR.csv"
)

# Extended adjusted
res_unord_ext <- run_unord_models(
  merged_extended, outcome, unord_vars, covars_ext, unord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/unorderedcat_extadj_FDR.csv"
)

# =============================================
# Continuous phenotype regressions (unadjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# =============================================

cont_vars <- c(
  "x93","x94","x95","x874","x914","x4079_2_0.x","x4080_2_0.x","x4609","x5375",
  "x20023","x20420","x21001","x21002"
)

cont_labels <- data.frame(
  phenotype = cont_vars,
  label = c(
    "Systolic BP (manual)",
    "Diastolic BP (manual)",
    "Pulse rate",
    "Duration of walks",
    "Duration of vigorous activity",
    "Diastolic BP (automated)",
    "Systolic BP (automated)",
    "Longest depression period",
    "Longest unenthusiasm/disinterest period",
    "Reaction time (mean)",
    "Longest worried/anxious period",
    "BMI",
    "Weight"
  )
)

# Loop
library(dplyr)
library(broom)

run_cont_models <- function(data, outcome, predictors, covars, labels, outfile) {
  results <- list()
  
  for (var in predictors) {
    form <- as.formula(paste(outcome, "~", var, 
                             if(length(covars) > 0) paste("+", paste(covars, collapse = " + ")) else ""))
    
    fit <- try(lm(form, data = data), silent = TRUE)
    
    if (!inherits(fit, "try-error")) {
      tidy_fit <- broom::tidy(fit, conf.int = TRUE)
      
      if (var %in% tidy_fit$term) {
        predictor_res <- tidy_fit %>% filter(term == var)
        
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
  
  results_df <- bind_rows(results)
  
  # Add labels
  results_df <- results_df %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, everything())
  
  # FDR correction + sort
  results_df$pval_fdr <- p.adjust(results_df$pval, method = "fdr")
  results_df <- results_df %>% arrange(pval_fdr)
  
  # Save
  write.csv(results_df, outfile, row.names = FALSE)
  
  return(results_df)
}


# Unadjusted
res_cont_unadj <- run_cont_models(
  merged_extended, outcome, cont_vars, c(), cont_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/continuous_unadj_FDR.csv"
)

# Minimal adjusted
res_cont_min <- run_cont_models(
  merged_extended, outcome, cont_vars, covars_min, cont_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/continuous_minadj_FDR.csv"
)

# Extended adjusted
res_cont_ext <- run_cont_models(
  merged_extended, outcome, cont_vars, covars_ext, cont_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/continuous_extadj_FDR.csv"
)

# Save dataset
write.table(
  merged_extended,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended_final.tsv",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)


## -----Create tables and figures based on new results----- ##

### Binary predictors 

# Path to your binary results folder
bin_path <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/Binary"

# Required libraries
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)

# Adjust path and sheet name if needed
BINARY_ALL <- read_excel("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/Binary/BINARY_ALL.xlsx")

# Plot
library(dplyr)
library(ggplot2)
alpha <- 0.05

BINARY_ALL <- BINARY_ALL %>%
  mutate(
    p_nom = as.numeric(`P-value`),
    p_fdr = as.numeric(`P-value FDR`),
    sig_cat = case_when(
      !is.na(p_fdr) & p_fdr < alpha ~ "FDR < 0.05",
      !is.na(p_nom) & p_nom < alpha & (is.na(p_fdr) | p_fdr >= alpha) ~ "Nominal < 0.05",
      TRUE ~ "Not significant"
    ),
    sig_cat = factor(sig_cat, levels = c("FDR < 0.05", "Nominal < 0.05", "Not significant"))
  )

# Plot
ggplot(BINARY_ALL, aes(x = Estimate, y = `UKB Phenotype`, color = Model, shape = sig_cat)) +
  geom_point(position = position_dodge(width = 0.6), size = 3, stroke = 1.1) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "Difference in WMH change (standardized units)", 
       y = "UKB Phenotype",
       title = "Binary Predictors of WMH Change",
       shape = "Significance") +
  scale_shape_manual(
    values = c("FDR < 0.05" = 19,        # solid circle
               "Nominal < 0.05" = 1,     # open circle
               "Not significant" = 2),   # open triangle
    breaks = c("FDR < 0.05", "Nominal < 0.05", "Not significant"),
    drop = FALSE
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

### Continuous predictors

library(dplyr)
library(readr)

# Adjust path and sheet name if needed
CONT_ALL <- read_excel("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/Continuous/CONT_ALL.xlsx")

# Add significance category
CONT_ALL <- CONT_ALL %>%
  mutate(
    p_nom = as.numeric(`P-value`),
    p_fdr = as.numeric(`P-value FDR`),
    sig_cat = case_when(
      !is.na(p_fdr) & p_fdr < alpha ~ "FDR < 0.05",
      !is.na(p_nom) & p_nom < alpha & (is.na(p_fdr) | p_fdr >= alpha) ~ "Nominal < 0.05",
      TRUE ~ "Not significant"
    ),
    sig_cat = factor(sig_cat, levels = c("FDR < 0.05", "Nominal < 0.05", "Not significant"))
  )

# Plot
ggplot(CONT_ALL, aes(x = Estimate, y = `UKB Phenotype`, color = Model, shape = sig_cat)) +
  geom_point(position = position_dodge(width = 0.6), size = 3, stroke = 1.1) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "Standardized difference in WMH change per 1SD increase in predictor", 
       y = "UKB Phenotype",
       title = "Continuous Predictors of WMH Change",
       shape = "Significance") +
  scale_shape_manual(
    values = c("FDR < 0.05" = 19,        # solid circle
               "Nominal < 0.05" = 1,     # open circle
               "Not significant" = 2),   # open triangle
    breaks = c("FDR < 0.05", "Nominal < 0.05", "Not significant"),
    drop = FALSE
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

### Ordered categorical predictors

# Adjust path and sheet name if needed
ORDCAT_ALL <- read_excel("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/Ordered categorical/ORDCAT_ALL.xlsx")

# Add significance category
ORDCAT_ALL <- ORDCAT_ALL %>%
  mutate(
    p_nom = as.numeric(`P-value`),
    p_fdr = as.numeric(`P-value FDR`),
    sig_cat = case_when(
      !is.na(p_fdr) & p_fdr < alpha ~ "FDR < 0.05",
      !is.na(p_nom) & p_nom < alpha & (is.na(p_fdr) | p_fdr >= alpha) ~ "Nominal < 0.05",
      TRUE ~ "Not significant"
    ),
    sig_cat = factor(sig_cat, levels = c("FDR < 0.05", "Nominal < 0.05", "Not significant"))
  )

# Plot
ggplot(ORDCAT_ALL, aes(x = Estimate, y = `UKB Phenotype`, color = Model, shape = sig_cat)) +
  geom_point(position = position_dodge(width = 0.6), size = 3, stroke = 1.1) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "Standardized difference in WMH change per one-step increase in category", 
       y = "UKB Phenotype",
       title = "Ordered Categorical Predictors of WMH Change",
       shape = "Significance") +
  scale_shape_manual(
    values = c("FDR < 0.05" = 19,        # solid circle
               "Nominal < 0.05" = 1,     # open circle
               "Not significant" = 2),   # open triangle
    breaks = c("FDR < 0.05", "Nominal < 0.05", "Not significant"),
    drop = FALSE
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

### Unordered categorical predictors

# Adjust path and sheet name if needed
UNORDCAT_ALL <- read_excel("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/Any Change PheWas/Unordered categorical/UNORDCAT_ALL.xlsx")

# Add significance category
UNORDCAT_ALL <- UNORDCAT_ALL %>%
  mutate(
    p_nom = as.numeric(`P-value`),
    p_fdr = as.numeric(`P-value FDR`),
    sig_cat = case_when(
      !is.na(p_fdr) & p_fdr < alpha ~ "FDR < 0.05",
      !is.na(p_nom) & p_nom < alpha & (is.na(p_fdr) | p_fdr >= alpha) ~ "Nominal < 0.05",
      TRUE ~ "Not significant"
    ),
    sig_cat = factor(sig_cat, levels = c("FDR < 0.05", "Nominal < 0.05", "Not significant"))
  )

# Plot
ggplot(UNORDCAT_ALL, aes(x = Estimate, y = `UKB Phenotype`, color = Model, shape = sig_cat)) +
  geom_point(position = position_dodge(width = 0.6), size = 3, stroke = 1.1) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "Standardized difference in WMH change compared to reference category", 
       y = "UKB Phenotype",
       title = "Unordered Categorical Predictors of WMH Change",
       shape = "Significance") +
  scale_shape_manual(
    values = c("FDR < 0.05" = 19,        # solid circle
               "Nominal < 0.05" = 1,     # open circle
               "Not significant" = 2),   # open triangle
    breaks = c("FDR < 0.05", "Nominal < 0.05", "Not significant"),
    drop = FALSE
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

