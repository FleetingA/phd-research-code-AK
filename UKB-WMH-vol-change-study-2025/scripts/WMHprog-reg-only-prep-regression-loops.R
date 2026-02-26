
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
library(car) # for Anova(); unordered categorical regressions

# Load data to work with
merged_extended <- read_tsv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv",
                            sep = "\t", row.names = FALSE, quote = FALSE)

# ==========================================
# WMH Progressors & WMH Regressors only
# ==========================================

# Paths
base_path <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/New Files to work with"

# Read the big preprocessed dataset
merged_extended <- read.delim(file.path(base_path, "merged_extended.tsv"),
                              sep = "\t", stringsAsFactors = FALSE)
# Read subgroup files (CSV)
merged_prog <- read_csv(file.path(base_path, "merged_prog.csv"), show_col_types = FALSE)
merged_reg  <- read_csv(file.path(base_path, "merged_reg.csv"), show_col_types = FALSE)

# Make sure IDs line up
merged_prog <- merged_prog %>% rename(userId = any_of(c("userId","eid")))
merged_reg  <- merged_reg  %>% rename(userId = any_of(c("userId","eid")))

# Keep only progressors from merged_extended
merged_extended_progressors <- merged_extended %>%
  semi_join(merged_prog, by = "userId")

# Keep only regressors from merged_extended
merged_extended_regressors <- merged_extended %>%
  semi_join(merged_reg, by = "userId")

# Save files for prog and reg
write.table(merged_extended_progressors,
            file.path(base_path, "merged_extended_progressors.tsv"),
            sep = "\t", row.names = FALSE, quote = FALSE)

write.table(merged_extended_regressors,
            file.path(base_path, "merged_extended_regressors.tsv"),
            sep = "\t", row.names = FALSE, quote = FALSE)

# Quick checks
cat("Full dataset:", nrow(merged_extended), "\n") # 4329
cat("Progressors:", nrow(merged_extended_progressors), "\n") # 2750
cat("Regressors:", nrow(merged_extended_regressors), "\n") # 1579

----------------------------------------
# LOOPS FOR PROGRESSORS AND REGRESSORS
----------------------------------------

# =========================
# Progressors
# =========================
outcome <- "WMH_TP2_adj_std"

# Run unadjusted regressions
results_prog_unadj <- list()

for (var in binary_vars) {
  form <- as.formula(paste(outcome, "~", var))
  
  fit <- try(lm(form, data = merged_extended_progressors), silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    tidy_fit <- broom::tidy(fit, conf.int = TRUE)
    
    if (var %in% tidy_fit$term) {
      predictor_res <- tidy_fit %>% filter(term == var)
      results_prog_unadj[[var]] <- data.frame(
        phenotype = var,
        beta      = predictor_res$estimate,
        se        = predictor_res$std.error,
        ci_low    = predictor_res$conf.low,
        ci_high   = predictor_res$conf.high,
        pval      = predictor_res$p.value
      )
    }
  }
}

results_prog_unadj <- bind_rows(results_prog_unadj) %>%
  left_join(binary_labels, by = "phenotype") %>%
  select(phenotype, label, everything()) %>%
  mutate(pval_fdr = p.adjust(pval, method = "fdr"))

# Save
write.csv(results_prog_unadj,
          "results_progressors_binary_unadjusted.csv", row.names = FALSE
)


# =========================
# Regressors
# =========================
results_reg_unadj <- list()

for (var in binary_vars) {
  form <- as.formula(paste(outcome, "~", var))
  
  fit <- try(lm(form, data = merged_extended_regressors), silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    tidy_fit <- broom::tidy(fit, conf.int = TRUE)
    
    if (var %in% tidy_fit$term) {
      predictor_res <- tidy_fit %>% filter(term == var)
      results_reg_unadj[[var]] <- data.frame(
        phenotype = var,
        beta      = predictor_res$estimate,
        se        = predictor_res$std.error,
        ci_low    = predictor_res$conf.low,
        ci_high   = predictor_res$conf.high,
        pval      = predictor_res$p.value
      )
    }
  }
}

results_reg_unadj <- bind_rows(results_reg_unadj) %>%
  left_join(binary_labels, by = "phenotype") %>%
  select(phenotype, label, everything()) %>%
  mutate(pval_fdr = p.adjust(pval, method = "fdr"))

# Save
write.csv(results_reg_unadj,
          "results_regressors_binary_unadjusted.csv", row.names = FALSE
)

library(dplyr)
library(broom)

# =====================================================
# Outcome + Covariates
# =====================================================

outcome <- "WMH_TP2_adj_std"

# Minimal and extended covariate sets
covars_min <- c("x31_0_0", "x21022_0_0", "xICV_2_0")   # sex, age, ICV
covars_ext <- c("x31_0_0", "x21022_0_0", "xICV_2_0", "x6150.4") # + HTN


# =====================================================
# Binary predictors + labels (same as before)
# =====================================================

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

# =====================================================
# Function for running models
# =====================================================
run_models <- function(data, outcome, predictors, labels, covars = NULL, model_type = "unadjusted") {
  results <- list()
  
  for (var in predictors) {
    rhs <- c(var, covars)
    form <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
    
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
          pval = predictor_res$p.value,
          model = model_type
        )
      }
    }
  }
  
  results_df <- bind_rows(results) %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, everything())
  
  results_df$pval_fdr <- p.adjust(results_df$pval, method = "fdr")
  results_df <- results_df %>% arrange(pval_fdr)
  
  return(results_df)
}

# =====================================================
# Run for PROGRESSORS
# =====================================================

res_bin_prog_unadj <- run_models(merged_extended_progressors, outcome, binary_vars, binary_labels, covars = NULL, model_type = "unadjusted")
res_bin_prog_min   <- run_models(merged_extended_progressors, outcome, binary_vars, binary_labels, covars = covars_min, model_type = "min_adj")
res_bin_prog_ext   <- run_models(merged_extended_progressors, outcome, binary_vars, binary_labels, covars = covars_ext, model_type = "ext_adj")

write.csv(res_bin_prog_unadj, "binary_progressors_unadj.csv", row.names = FALSE)
write.csv(res_bin_prog_min,   "binary_progressors_minadj.csv", row.names = FALSE)
write.csv(res_bin_prog_ext,   "binary_progressors_extadj.csv", row.names = FALSE)

# =====================================================
# Run for REGRESSORS
# =====================================================

res_bin_reg_unadj <- run_models(merged_extended_regressors, outcome, binary_vars, binary_labels, covars = NULL, model_type = "unadjusted")
res_bin_reg_min   <- run_models(merged_extended_regressors, outcome, binary_vars, binary_labels, covars = covars_min, model_type = "min_adj")
res_bin_reg_ext   <- run_models(merged_extended_regressors, outcome, binary_vars, binary_labels, covars = covars_ext, model_type = "ext_adj")

write.csv(res_bin_reg_unadj, "binary_regressors_unadj.csv", row.names = FALSE)
write.csv(res_bin_reg_min,   "binary_regressors_minadj.csv", row.names = FALSE)
write.csv(res_bin_reg_ext,   "binary_regressors_extadj.csv", row.names = FALSE)


# ---DO THE SAME FOR THE REST OF THE PREDICTOR TYPES--- #

# =====================================================
# Ordered categorical
# =====================================================

run_ord_models <- function(data, outcome, predictors, labels, covars = NULL, model_type = "unadjusted") {
  results <- list()
  for (var in predictors) {
    rhs <- c(var, covars)
    form <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
    fit <- try(lm(form, data = data), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      tidy_fit <- broom::tidy(fit, conf.int = TRUE)
      if (var %in% tidy_fit$term) {
        predictor_res <- tidy_fit %>% filter(term == var)
        results[[var]] <- data.frame(
          phenotype = var,
          beta      = predictor_res$estimate,
          se        = predictor_res$std.error,
          ci_low    = predictor_res$conf.low,
          ci_high   = predictor_res$conf.high,
          pval      = predictor_res$p.value,
          model     = model_type
        )
      }
    }
  }
  results_df <- bind_rows(results) %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, everything())
  results_df$pval_fdr <- p.adjust(results_df$pval, method = "fdr")
  results_df %>% arrange(pval_fdr)
}

# PROGRESSORS
res_ord_prog_unadj <- run_ord_models(merged_extended_progressors, outcome, ord_vars, ord_labels, covars = NULL, model_type = "unadjusted")
res_ord_prog_min   <- run_ord_models(merged_extended_progressors, outcome, ord_vars, ord_labels, covars = covars_min, model_type = "min_adj")
res_ord_prog_ext   <- run_ord_models(merged_extended_progressors, outcome, ord_vars, ord_labels, covars = covars_ext, model_type = "ext_adj")

write.csv(res_ord_prog_unadj, "ordered_progressors_unadj.csv", row.names = FALSE)
write.csv(res_ord_prog_min,   "ordered_progressors_minadj.csv", row.names = FALSE)
write.csv(res_ord_prog_ext,   "ordered_progressors_extadj.csv", row.names = FALSE)

# REGRESSORS
res_ord_reg_unadj <- run_ord_models(merged_extended_regressors, outcome, ord_vars, ord_labels, covars = NULL, model_type = "unadjusted")
res_ord_reg_min   <- run_ord_models(merged_extended_regressors, outcome, ord_vars, ord_labels, covars = covars_min, model_type = "min_adj")
res_ord_reg_ext   <- run_ord_models(merged_extended_regressors, outcome, ord_vars, ord_labels, covars = covars_ext, model_type = "ext_adj")

write.csv(res_ord_reg_unadj, "ordered_regressors_unadj.csv", row.names = FALSE)
write.csv(res_ord_reg_min,   "ordered_regressors_minadj.csv", row.names = FALSE)
write.csv(res_ord_reg_ext,   "ordered_regressors_extadj.csv", row.names = FALSE)


# =====================================================
# Unordered categorical
# =====================================================

run_unord_models <- function(data, outcome, predictors, labels, covars = NULL, model_type = "unadjusted") {
  results <- list()
  for (var in predictors) {
    rhs <- c(paste0("factor(", var, ")"), covars)
    form <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
    fit <- try(lm(form, data = data), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      tidy_fit <- broom::tidy(fit, conf.int = TRUE)
      anova_p  <- try(Anova(fit, type = 3)$`Pr(>F)`[1], silent = TRUE)
      coef_results <- tidy_fit %>%
        filter(term != "(Intercept)") %>%
        mutate(phenotype = var,
               overall_p = ifelse(!inherits(anova_p, "try-error"), anova_p, NA),
               model = model_type)
      results[[var]] <- coef_results
    }
  }
  results_df <- bind_rows(results) %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, term, estimate, std.error, conf.low, conf.high, p.value, overall_p, model)
  results_df$pval_fdr <- p.adjust(results_df$p.value, method = "fdr")
  results_df
}

# PROGRESSORS
res_unord_prog_unadj <- run_unord_models(merged_extended_progressors, outcome, unord_vars, unord_labels, covars = NULL, model_type = "unadjusted")
res_unord_prog_min   <- run_unord_models(merged_extended_progressors, outcome, unord_vars, unord_labels, covars = covars_min, model_type = "min_adj")
res_unord_prog_ext   <- run_unord_models(merged_extended_progressors, outcome, unord_vars, unord_labels, covars = covars_ext, model_type = "ext_adj")

write.csv(res_unord_prog_unadj, "unordered_progressors_unadj.csv", row.names = FALSE)
write.csv(res_unord_prog_min,   "unordered_progressors_minadj.csv", row.names = FALSE)
write.csv(res_unord_prog_ext,   "unordered_progressors_extadj.csv", row.names = FALSE)

# REGRESSORS
res_unord_reg_unadj <- run_unord_models(merged_extended_regressors, outcome, unord_vars, unord_labels, covars = NULL, model_type = "unadjusted")
res_unord_reg_min   <- run_unord_models(merged_extended_regressors, outcome, unord_vars, unord_labels, covars = covars_min, model_type = "min_adj")
res_unord_reg_ext   <- run_unord_models(merged_extended_regressors, outcome, unord_vars, unord_labels, covars = covars_ext, model_type = "ext_adj")

write.csv(res_unord_reg_unadj, "unordered_regressors_unadj.csv", row.names = FALSE)
write.csv(res_unord_reg_min,   "unordered_regressors_minadj.csv", row.names = FALSE)
write.csv(res_unord_reg_ext,   "unordered_regressors_extadj.csv", row.names = FALSE)


# =====================================================
# Continuous
# =====================================================

run_cont_models <- function(data, outcome, predictors, labels, covars = NULL, model_type = "unadjusted") {
  results <- list()
  for (var in predictors) {
    rhs <- c(var, covars)
    form <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
    fit <- try(lm(form, data = data), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      tidy_fit <- broom::tidy(fit, conf.int = TRUE)
      if (var %in% tidy_fit$term) {
        predictor_res <- tidy_fit %>% filter(term == var)
        results[[var]] <- data.frame(
          phenotype = var,
          beta      = predictor_res$estimate,
          se        = predictor_res$std.error,
          ci_low    = predictor_res$conf.low,
          ci_high   = predictor_res$conf.high,
          pval      = predictor_res$p.value,
          model     = model_type
        )
      }
    }
  }
  results_df <- bind_rows(results) %>%
    left_join(labels, by = "phenotype") %>%
    select(phenotype, label, everything())
  results_df$pval_fdr <- p.adjust(results_df$pval, method = "fdr")
  results_df %>% arrange(pval_fdr)
}

# PROGRESSORS
res_cont_prog_unadj <- run_cont_models(merged_extended_progressors, outcome, cont_vars, cont_labels, covars = NULL, model_type = "unadjusted")
res_cont_prog_min   <- run_cont_models(merged_extended_progressors, outcome, cont_vars, cont_labels, covars = covars_min, model_type = "min_adj")
res_cont_prog_ext   <- run_cont_models(merged_extended_progressors, outcome, cont_vars, cont_labels, covars = covars_ext, model_type = "ext_adj")

write.csv(res_cont_prog_unadj, "continuous_progressors_unadj.csv", row.names = FALSE)
write.csv(res_cont_prog_min,   "continuous_progressors_minadj.csv", row.names = FALSE)
write.csv(res_cont_prog_ext,   "continuous_progressors_extadj.csv", row.names = FALSE)

# REGRESSORS
res_cont_reg_unadj <- run_cont_models(merged_extended_regressors, outcome, cont_vars, cont_labels, covars = NULL, model_type = "unadjusted")
res_cont_reg_min   <- run_cont_models(merged_extended_regressors, outcome, cont_vars, cont_labels, covars = covars_min, model_type = "min_adj")
res_cont_reg_ext   <- run_cont_models(merged_extended_regressors, outcome, cont_vars, cont_labels, covars = covars_ext, model_type = "ext_adj")

write.csv(res_cont_reg_unadj, "continuous_regressors_unadj.csv", row.names = FALSE)
write.csv(res_cont_reg_min,   "continuous_regressors_minadj.csv", row.names = FALSE)
write.csv(res_cont_reg_ext,   "continuous_regressors_extadj.csv", row.names = FALSE)

# Saved all files

# Some results checks 
table(merged_extended$x6149.6) # 0=3938; 1=366
table(merged_extended$x6148.4) # 0=3893; 1=376

# Visualise results

# ---Modify file names and titles as required; change file paths as required--- #

# Load packages
library(readxl)
library(ggplot2)
library(dplyr)

# Load file
df <- read_excel("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/WMH Regressors/ALL REG FULLY ADJ sign.xlsx")

# Make sure columns match your naming
names(df) <- c("Phenotype", "Beta", "SE", "CI_lower", "CI_upper", "P", "P_FDR")

# Add -log10(p) for bubble size
df <- df %>%
  mutate(
    logP = -log10(P),
    Phenotype = factor(Phenotype, levels = rev(unique(Phenotype))) # reverse order for nice plotting
  )

# Bubble plot
bubble_plot <- ggplot(df, aes(x = Beta, y = Phenotype)) +
  geom_point(aes(size = logP, color = P), alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_size_continuous(name = "-log10(p)", range = c(2, 8)) +
  scale_color_gradient(name = "Raw P-value", low = "purple", high = "green3") +
  labs(
    title = "Fully Adjusted Associations for WMH Regressors",
    x = "Beta (effect size)",
    y = "Phenotype"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 14),
    legend.position = "left",
    plot.title = element_text(hjust = 0.5)  # right-align title
  )

# Find min and max of Beta with a little padding
#beta_min <- min(df$Beta, na.rm = TRUE) - 0.1
#beta_max <- max(df$Beta, na.rm = TRUE) + 0.1

# Add coord_cartesian to tighten the plot
#bubble_plot <- bubble_plot +
#coord_cartesian(xlim = c(beta_min, beta_max))

# Show
print(bubble_plot)

#ggsave("bubble_adj_ext_prog.png", bubble_plot, width = 8, height = 8, dpi = 300)  # smaller width means narrower

ggsave(
  "bubble_adj_ext_reg.png",
  bubble_plot,
  width = 12,   # increase for wider figure
  height = 6,   # adjust height if needed
  dpi = 300
)

