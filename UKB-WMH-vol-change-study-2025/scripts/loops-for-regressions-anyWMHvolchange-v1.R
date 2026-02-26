
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

# Load data to work with
merged_extended <- read_tsv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv",
                            sep = "\t", row.names = FALSE, quote = FALSE)

## LOOPS FOR REGRESSION ANALYSES

# =========================================
# Binary phenotype regressions (unadjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# =========================================

# Outcome
outcome <- "WMH_TP2_adj_std"

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
write.csv(results_df, "binary_unadjusted_results.csv", row.names = FALSE)

# Peek at top results
head(results_df)

# Sort by FDR p-value (smallest first)
results_df <- results_df %>%
  arrange(pval_fdr)

# Save results (sorted)
write.csv(results_df,
          "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/binary_unadjusted.csv",
          row.names = FALSE)

write.csv(results_df,
          "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/binarypred_unadjustedFDR_results.csv",
          row.names = FALSE)

# ===========================================
# Binary phenotype regressions (adjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# Covariates = sex, age, ICV, hypertension
# ===========================================

# Outcome and covariates
outcome <- "WMH_TP2_adj_std"
covars  <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "x6150.4")

# Now, add covariates
confounders <- read.csv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/confounderfile_partial.csv",
                        stringsAsFactors = FALSE)
names(confounders)

# Merge confounders into merged_extended, keeping only the 4329 participants
merged_extended <- merged_extended %>%
  left_join(confounders %>% select(userId, x31_0_0, x21022_0_0), by = "userId")

table(merged_extended$x31_0_0) # sex; 0 (female)=2362; 1 (male)=1967
summary(merged_extended$x21022_0_0) # age; median=52
hist(merged_extended$x21022_0_0)

# Minimal adjustment
covars_min <- c("x31_0_0", "x21022_0_0", "xICV_2_0")

# Binary predictors (same as before)
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

# Labels table (same as before, shortened here)
binary_labels <- data.frame(
  phenotype = binary_vars,
  label = c("Snoring","Mood swings","Miserableness","Irritability","Sensitivity/hurt feelings",
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
            "Family history of IBS")
)

# =========================
# Run adjusted regressions
# =========================
results_adj <- list()

# change from covars to covars_min
for (var in binary_vars) {
  # Build formula with covariates
  form <- as.formula(paste(outcome, "~", var, "+", paste(covars, collapse = " + ")))
  
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
          "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/binary_adjustedextFDR.csv",
          row.names = FALSE)

write.csv(results_adj_df,
          "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/binary_adjustedminFDR.csv",
          row.names = FALSE)

# Peek
head(results_adj_df)

# =======================================================
# Ordered categorical phenotype regressions (unadjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# =======================================================

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

library(dplyr)
library(broom)

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

# Covariates
covars_min <- c("x31_0_0", "x21022_0_0", "xICV_2_0")   # age, sex, ICV
covars_ext <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "x6150.4")  # + HTN

# Unadjusted
res_ord_unadj <- run_ord_models(
  merged_extended, outcome, ord_vars, c(), ord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/orderedcat_unadjustedFDR.csv"
)

# Minimal adjusted
res_ord_min <- run_ord_models(
  merged_extended, outcome, ord_vars, covars_min, ord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/orderedcat_adjustedFDRmin.csv"
)

# Extended adjusted
res_ord_ext <- run_ord_models(
  merged_extended, outcome, ord_vars, covars_ext, ord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/orderedcat_adjustedFDRext.csv"
)

# =============================================================
# Unordered categorical phenotype regressions (unadjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# =============================================================

unord_vars <- c("x1538", "x20126", "x20536")

unord_labels <- data.frame(
  phenotype = unord_vars,
  label = c(
    "Major dietary changes (5 years)",
    "Bipolar and major depression status",
    "Weight change during worst depression episode"
  )
)

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

# Run models
outcome <- "WMH_TP2_adj_std"

# Covariates
covars_min <- c("x31_0_0", "x21022_0_0", "xICV_2_0")
covars_ext <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "x6150.4")

# Unadjusted
res_unord_unadj <- run_unord_models(
  merged_extended, outcome, unord_vars, c(), unord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/unorderedcat_unadjustedFDR.csv"
)

# Minimal adjusted
res_unord_min <- run_unord_models(
  merged_extended, outcome, unord_vars, covars_min, unord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/unorderedcat_adjustedFDR_min.csv"
)

# Extended adjusted
res_unord_ext <- run_unord_models(
  merged_extended, outcome, unord_vars, covars_ext, unord_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/unorderedcat_adjustedFDR_ext.csv"
)

# ================================================
# Continuous phenotype regressions (unadjusted)
# Outcome = WMH_TP2_adj_std (continuous)
# ================================================

cont_vars <- c(
  "x93","x94","x95","x874","x914","x4079","x4080","x4609","x5375",
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

outcome <- "WMH_TP2_adj_std"

# Covariates
covars_min <- c("x31_0_0", "x21022_0_0", "xICV_2_0")   # age, sex, ICV
covars_ext <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "x6150.4")   # age, sex, ICV, HTN

# Unadjusted
res_cont_unadj <- run_cont_models(
  merged_extended, outcome, cont_vars, c(), cont_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/continuous_unadjustedFDR.csv"
)

# Minimal adjusted
res_cont_min <- run_cont_models(
  merged_extended, outcome, cont_vars, covars_min, cont_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/continuous_adjustedFDR_min.csv"
)

# Extended adjusted
res_cont_ext <- run_cont_models(
  merged_extended, outcome, cont_vars, covars_ext, cont_labels,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/continuous_adjustedFDR_ext.csv"
)

# Save dataset
write.table(
  merged_extended,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended_final.tsv",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

