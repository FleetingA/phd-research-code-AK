
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 
  
# Date: 08/08/2025
----------------------------------------------------------------------------------
  
# Load libraries
library(readr)
library(psych)
library(ggplot2)
library(dplyr)
library(pROC)

### Additional Subgroup Analyses - MCI vs CN and Probable AD vs CN 
  
# --- Step 1: Recode QVAL to dx3 ---
dataset <- dataset %>%
  mutate(
    dx3 = case_when(
      QVAL == "Cohort 1 (Healthy)"     ~ "CN",
      QVAL == "Cohort 2 (MCI)"         ~ "MCI",
      QVAL == "Cohort 3 (Probable AD)" ~ "mildAD",
      TRUE ~ NA_character_
    )
  )

biomarkers <- c("AB4240_ratio_c", "pTau181_logtr_c", 
                "PTAU217_logtr_c", "ApoE4_sqrt_c")
cvd <- "ASCVD_cont_score_sqrtc"

# --- Step 2: Function to run models, print full summaries, and return ROC ---
run_and_plot <- function(df, bm, contrast) {
  df_sub <- df %>%
    filter(dx3 %in% contrast) %>%
    mutate(outcome = ifelse(dx3 == contrast[2], 1, 0))
  
  # Main model
  f_main <- as.formula(paste0("outcome ~ ", bm, " + ", cvd))
  m_main <- glm(f_main, data = df_sub, family = binomial)
  cat("\n------------------------------------------------\n")
  cat("Main effects model:", bm, "-", paste(contrast, collapse = " vs "), "\n")
  print(summary(m_main))
  
  # Interaction model
  f_int <- as.formula(paste0("outcome ~ ", bm, " * ", cvd))
  m_int <- glm(f_int, data = df_sub, family = binomial)
  cat("\nInteraction model:", bm, "-", paste(contrast, collapse = " vs "), "\n")
  print(summary(m_int))
  
  # ROC curves
  roc_main <- roc(df_sub$outcome, fitted(m_main))
  roc_int  <- roc(df_sub$outcome, fitted(m_int))
  
  plot(roc_main, col = "blue", lwd = 2,
       main = paste("ROC:", bm, "-", paste(contrast, collapse = " vs ")))
  lines(roc_int, col = "red", lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  legend("bottomright",
         legend = c(
           paste("Main (AUC =", round(auc(roc_main), 3), ")"),
           paste("Interaction (AUC =", round(auc(roc_int), 3), ")")
         ),
         col = c("blue", "red"), lwd = 2)
}

# --- Step 3: Run for all biomarkers and both contrasts ---
for (bm in biomarkers) {
  run_and_plot(dataset, bm, c("CN", "MCI"))
  run_and_plot(dataset, bm, c("CN", "mildAD"))
}

# Run descriptives for predictors for power analysis
describe(dataset$AB4240_ratio_c)
describe(dataset$pTau181_logtr_c)
describe(dataset$PTAU217_logtr_c)
describe(dataset$ApoE4_sqrt_c)
describe(dataset$ASCVD_cont_score_sqrtc)

## --- Prospective power for biomarker × ASCVD interaction (lean, no files) ---

suppressPackageStartupMessages({
  library(MASS)      # mvrnorm
  library(dplyr)
  library(ggplot2)
})

set.seed(123)

# ---- Inputs ----
N_total   <- 745L
prev_case <- 444/745         # ≈ 0.596
alpha     <- 0.05
nsim      <- 500             # ↑ for smoother estimates, ↓ for speed

# Standardized predictors (mean=0, sd=1)
# Correlation (biomarker ↔ ASCVD)
rho_map <- c(
  "AB4240_ratio_c"  = -0.16,
  "pTau181_logtr_c" =  0.30,
  "PTAU217_logtr_c" =  0.21,
  "ApoE4_sqrt_c"    = -0.03
)

# Main effects (log-ORs) from your fitted models
beta_map <- list(
  "AB4240_ratio_c"  = list(beta_bio = log(0.78), beta_cvd = log(1.97)),
  "pTau181_logtr_c" = list(beta_bio = log(1.83), beta_cvd = log(1.43)),
  "PTAU217_logtr_c" = list(beta_bio = log(2.33), beta_cvd = log(1.47)),
  "ApoE4_sqrt_c"    = list(beta_bio = log(1.33), beta_cvd = log(1.71))
)

# Interaction OR grid (keep small for speed; expand later if you like)
interaction_ors   <- c(0.75, 0.80, 0.85, 0.90, 1.00, 1.10, 1.20)
interaction_betas <- log(interaction_ors)

# ---- Helpers ----
rmv2 <- function(n, rho){
  Sigma <- matrix(c(1, rho, rho, 1), ncol = 2)
  MASS::mvrnorm(n = n, mu = c(0,0), Sigma = Sigma)
}

solve_intercept <- function(beta_bio, beta_cvd, beta_int, rho, target_prev, n_approx = 8e4){
  X <- rmv2(n_approx, rho)
  bio <- X[,1]; cvd <- X[,2]
  f <- function(b0){
    p <- plogis(b0 + beta_bio*bio + beta_cvd*cvd + beta_int*(bio*cvd))
    mean(p) - target_prev
  }
  uniroot(f, interval = c(-8, 8))$root
}

power_for_one <- function(N, beta_bio, beta_cvd, beta_int, rho, prev_case, nsim = 1000, alpha = 0.05){
  b0 <- solve_intercept(beta_bio, beta_cvd, beta_int, rho, prev_case, n_approx = 6e4)
  sig <- 0L
  for (i in seq_len(nsim)){
    X <- rmv2(N, rho)
    bio <- X[,1]; cvd <- X[,2]
    y <- rbinom(N, 1, plogis(b0 + beta_bio*bio + beta_cvd*cvd + beta_int*(bio*cvd)))
    fit <- glm(y ~ bio*cvd, family = binomial())
    pval <- summary(fit)$coefficients["bio:cvd","Pr(>|z|)"]
    if (!is.na(pval) && pval < alpha) sig <- sig + 1L
  }
  sig / nsim
}

power_grid_for_biomarker <- function(biomarker){
  rho       <- unname(rho_map[biomarker])
  beta_bio  <- beta_map[[biomarker]]$beta_bio
  beta_cvd  <- beta_map[[biomarker]]$beta_cvd
  tibble(
    Biomarker = biomarker,
    Interaction_OR = interaction_ors,
    Power = vapply(interaction_betas, function(bi){
      power_for_one(N_total, beta_bio, beta_cvd, bi, rho, prev_case, nsim = nsim, alpha = alpha)
    }, numeric(1))
  )
}

# ---- Run ----
biomarkers <- names(rho_map)
power_results <- dplyr::bind_rows(lapply(biomarkers, power_grid_for_biomarker))

mde_summary <- power_results %>%
  group_by(Biomarker) %>%
  summarize(
    MDE80 = Interaction_OR[which.min(abs(Power - 0.80))],
    Power_at_MDE80 = Power[which.min(abs(Power - 0.80))],
    MDE90 = Interaction_OR[which.min(abs(Power - 0.90))],
    Power_at_MDE90 = Power[which.min(abs(Power - 0.90))]
  ) %>%
  ungroup()

# ---- Print clean summaries ----
cat("\n=== Power grid (rounded) ===\n")
print(power_results %>% mutate(Power = round(Power, 3)))

cat("\n=== Approx. Minimum Detectable Interaction ORs ===\n")
print(mde_summary %>% mutate(across(starts_with("Power"), ~round(.x, 3))))

# ---- Plot to the graphics window ----
for (bm in biomarkers){
  df <- dplyr::filter(power_results, Biomarker == bm)
  p <- ggplot(df, aes(x = Interaction_OR, y = Power)) +
    geom_line() + geom_point() +
    geom_hline(yintercept = 0.80, linetype = "dashed") +
    geom_hline(yintercept = 0.90, linetype = "dashed") +
    labs(title = paste("Prospective Power Curve:", bm),
         x = "Interaction OR (biomarker × ASCVD)",
         y = "Power") +
    theme_minimal(base_size = 12)
  print(p)
}

cat("\nDone. Increase `nsim` or widen `interaction_ors` for more precision.\n")

library(dplyr)
library(broom)
library(ggplot2)
library(forcats)

-------------------------------------------------
### 09/08/2025: Visualisations for Supplements
  
## CV Risk Stratification: 
  
# Load additional libraries
library(tidyverse)

# Data from table
dataset <- tribble(
  ~Group, ~Predictor, ~OR, ~CI_low, ~CI_high,
  "Incident CVD", "AB4240_ratio_c", 1.14, 0.77, 1.69,
  "Incident CVD", "ASCVD_cont_score_sqrt", 2.71, 1.44, 5.04,
  "No CVD", "AB4240_ratio_c", 0.79, 0.66, 0.94,
  "No CVD", "ASCVD_cont_score_sqrt", 1.57, 1.31, 1.87,
  
  "Incident CVD", "pTau181_logtr_c", 1.06, 0.65, 1.73,
  "Incident CVD", "ASCVD Score", 2.57, 1.37, 4.79,
  "No CVD", "pTau181_logtr_c", 1.97, 1.62, 2.40,
  "No CVD", "ASCVD_cont_score_sqrt", 1.37, 1.15, 1.64,
  
  "Incident CVD", "PTAU217_logtr_c", 1.79, 0.92, 3.48,
  "Incident CVD", "ASCVD Score", 2.45, 1.34, 4.52,
  "No CVD", "PTAU217_logtr_c", 2.40, 1.92, 2.96,
  "No CVD", "ASCVD_cont_score_sqrt", 1.41, 1.19, 1.69,
  
  "Incident CVD", "ApoE4_sqrt_c", 0.99, 0.62, 1.59,
  "Incident CVD", "ASCVD Score", 2.62, 1.42, 4.80,
  "No CVD", "ApoE4_sqrt_c", 1.37, 1.14, 1.63,
  "No CVD", "ASCVD_cont_score_sqrt", 1.66, 1.40, 1.99
)

# Order predictors within facets
dataset <- dataset %>%
  mutate(Predictor = factor(Predictor,
                            levels = rev(c("AB4240_ratio_c", "pTau181_logtr_c", "PTAU217_logtr_c", "ApoE4_sqrt_c", "ASCVD_cont_score_sqrt"))))

# Forest plot
ggplot(dataset, aes(x = OR, y = Predictor)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey50") +
  scale_x_log10() +
  facet_wrap(~ Group) +
  labs(x = "Odds Ratio (log scale)", y = NULL,
       title = "Odds Ratios for Biomarkers and ASCVD Score",
       subtitle = "Models stratified by CVD group") +
  theme_minimal()

# Version 2:

# Data from table
dataset <- tribble(
  ~Group, ~Predictor, ~OR, ~CI_low, ~CI_high,
  "Incident CVD", "Aβ42/Aβ40", 1.14, 0.77, 1.69,
  "Incident CVD", "ASCVD Score", 2.71, 1.44, 5.04,
  "No CVD", "Aβ42/Aβ40", 0.79, 0.66, 0.94,
  "No CVD", "ASCVD Score", 1.57, 1.31, 1.87,
  
  "Incident CVD", "P-tau181", 1.06, 0.65, 1.73,
  "Incident CVD", "ASCVD Score", 2.57, 1.37, 4.79,
  "No CVD", "P-tau181", 1.97, 1.62, 2.40,
  "No CVD", "ASCVD Score", 1.37, 1.15, 1.64,
  
  "Incident CVD", "P-tau217", 1.79, 0.92, 3.48,
  "Incident CVD", "ASCVD Score", 2.45, 1.34, 4.52,
  "No CVD", "P-tau217", 2.40, 1.92, 2.96,
  "No CVD", "ASCVD Score", 1.41, 1.19, 1.69,
  
  "Incident CVD", "ApoE4", 0.99, 0.62, 1.59,
  "Incident CVD", "ASCVD Score", 2.62, 1.42, 4.80,
  "No CVD", "ApoE4", 1.37, 1.14, 1.63,
  "No CVD", "ASCVD Score", 1.66, 1.40, 1.99
)

# Order predictors within facets
dataset <- dataset %>%
  mutate(Predictor = factor(Predictor,
                            levels = rev(c("Aβ42/Aβ40", "P-tau181", "P-tau217", "ApoE4", "ASCVD Score"))))

# Forest plot
ggplot(dataset, aes(x = OR, y = Predictor)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0) +
  geom_vline(xintercept = 1, linetype = 2, color = "red4") +
  scale_x_log10() +
  facet_wrap(~ Group) +
  labs(x = "Odds Ratio (log scale)", y = NULL,
       title = "Odds Ratios for Biomarkers and ASCVD Score",
       subtitle = "Models stratified by CVD group") +
  theme_minimal()

# Filter only biomarkers (exclude ASCVD Score)
bio_df <- dataset %>% filter(Predictor != "ASCVD Score")

# Group comparison plot
ggplot(bio_df, aes(x = Predictor, y = OR, color = Group)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey50") +
  scale_y_log10() +
  labs(x = "Biomarker", y = "Odds Ratio (log scale)",
       title = "Biomarker Associations with CI",
       subtitle = "Comparison between Incident CVD and No CVD groups",
       color = "Group") +
  theme_minimal()

# Load additional library
library(scales)

ggplot(bio_df, aes(x = Predictor, y = OR, color = Group)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey50") +
  scale_y_log10(
    breaks = c(0.5, 1, 2, 3),
    labels = c("0.5", "1", "2", "3")
  ) +
  labs(x = "Plasma Biomarker", y = "Odds Ratio",
       title = "Biomarker Associations with Cognitive Status",
       subtitle = "Comparison between Incident CV Risk and No CV Risk Groups",
       color = "Group") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # Center title
  )

-----------------------
  # Sex Stratification:
  
  # ---------- Enter results ----------
df <- tribble(
  ~Biomarker, ~Sex,     ~Model,         ~Term,            ~OR,  ~CI_low, ~CI_high,
  # p-tau181 — Males
  "p-tau181", "Male",   "Main",         "Biomarker",      1.67, 1.27,    2.19,
  "p-tau181", "Male",   "Main",         "ASCVD",          1.15, 0.84,    1.57,
  "p-tau181", "Male",   "Interaction",  "Biomarker",      1.89, 1.41,    2.54,
  "p-tau181", "Male",   "Interaction",  "ASCVD",          1.19, 0.87,    1.62,
  "p-tau181", "Male",   "Interaction",  "Interaction",    0.68, 0.49,    0.94,
  
  # p-tau181 — Females
  "p-tau181", "Female", "Main",         "Biomarker",      1.97, 1.56,    2.50,
  "p-tau181", "Female", "Main",         "ASCVD",          1.51, 1.21,    1.87,
  "p-tau181", "Female", "Interaction",  "Biomarker",      1.91, 1.48,    2.47,
  "p-tau181", "Female", "Interaction",  "ASCVD",          1.49, 1.20,    1.85,
  "p-tau181", "Female", "Interaction",  "Interaction",    0.93, 0.72,    1.20,
  
  # p-tau217 — Males
  "p-tau217", "Male",   "Main",         "Biomarker",      2.03, 1.52,    2.73,
  "p-tau217", "Male",   "Main",         "ASCVD",          1.26, 0.92,    1.72,
  "p-tau217", "Male",   "Interaction",  "Biomarker",      2.53, 1.71,    3.75,
  "p-tau217", "Male",   "Interaction",  "ASCVD",          1.22, 0.89,    1.67,
  "p-tau217", "Male",   "Interaction",  "Interaction",    0.62, 0.42,    0.92,
  
  # p-tau217 — Females
  "p-tau217", "Female", "Main",         "Biomarker",      2.58, 1.97,    3.40,
  "p-tau217", "Female", "Main",         "ASCVD",          1.49, 1.20,    1.85,
  "p-tau217", "Female", "Interaction",  "Biomarker",      2.53, 1.89,    3.40,
  "p-tau217", "Female", "Interaction",  "ASCVD",          1.46, 1.16,    1.85,
  "p-tau217", "Female", "Interaction",  "Interaction",    0.93, 0.69,    1.25
)

# helper: a minimal log-OR axis with intuitive ticks
scale_or <- scale_x_log10(breaks = c(0.5, 1, 2, 3, 4),
                          labels = c("0.5","1","2","3","4"))

base_theme <- theme_minimal() +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# ---------- Plot A: Biomarker term ----------
pA <- df %>%
  filter(Term == "Biomarker") %>%
  ggplot(aes(x = OR, y = Sex, color = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.5), height = 0) +
  geom_vline(xintercept = 1, linetype = 2) +
  facet_wrap(~ Biomarker, nrow = 1) +
  scale_or +
  labs(x = "Odds Ratio", y = NULL,
       title = "Biomarker effect (p-tau × CI), by Sex and Model") +
  base_theme

# ---------- Plot B: ASCVD term ----------
pB <- df %>%
  filter(Term == "ASCVD") %>%
  ggplot(aes(x = OR, y = Sex, color = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.5), height = 0) +
  geom_vline(xintercept = 1, linetype = 2) +
  facet_wrap(~ Biomarker, nrow = 1) +
  scale_or +
  labs(x = "Odds Ratio", y = NULL,
       title = "ASCVD effect in the same models, by Sex and Model") +
  base_theme

# ---------- Plot C: Interaction term ----------
pC <- df %>%
  filter(Term == "Interaction") %>%
  ggplot(aes(x = OR, y = Sex)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0) +
  geom_vline(xintercept = 1, linetype = 2) +
  facet_wrap(~ Biomarker, nrow = 1) +
  scale_or +
  labs(x = "Odds Ratio", y = NULL,
       title = "Interaction (p-tau × ASCVD) term, by Sex") +
  base_theme

pA; pB; pC

# Stratification by separate ASCVD components:

# ---- Data (for eAppendices) ----

# ---- Data (p-tau181 + separate ASCVD components) ---- # Replace with values for
# corresponding biomarker

# ---- Data (ApoE4 + separate ASCVD components) ----
abeta <- tribble(
  ~Model, ~Term,                 ~OR,    ~CI_low, ~CI_high, ~Type,
  "Age",  "ApoE4",               1.319,   1.126,   1.546,  "ApoE4",
  "Age",  "Age (per year)",      1.077,   1.053,   1.102,  "ASCVD component",
  
  "Gender","ApoE4",              1.286,   1.102,   1.502,  "ApoE4",
  "Gender","Male (vs ref)",      1.702,   1.258,   2.304,  "ASCVD component",
  
  "Ethnicity","ApoE4",           1.286,   1.104,   1.499,  "ApoE4",
  "Ethnicity","Black (vs ref)",  0.660,   0.259,   1.680,  "ASCVD component",
  "Ethnicity","Other (vs ref)",  0.972,   0.146,   6.452,  "ASCVD component",
  "Ethnicity","White (vs ref)",  0.581,   0.181,   1.865,  "ASCVD component",
  
  "HDL cholesterol","ApoE4",     1.285,   1.103,   1.499,  "ApoE4",
  "HDL cholesterol","HDL (mmol/L)",0.780, 0.543,   1.120,  "ASCVD component",
  
  "Total cholesterol","ApoE4",   1.310,   1.128,   1.521,  "ApoE4",
  "Total cholesterol","Total (mmol/L)",0.883,0.764,1.021,  "ASCVD component",
  
  "LDL cholesterol","ApoE4",     1.304,   1.123,   1.515,  "ApoE4",
  "LDL cholesterol","LDL (mg/dL)",0.997,  0.993,   1.002,  "ASCVD component",
  
  "Systolic BP","ApoE4",         1.290,   1.111,   1.497,  "ApoE4",
  "Systolic BP","SBP (mmHg)",    1.010,   1.001,   1.019,  "ASCVD component",
  
  "Hypertension medication","ApoE4",1.293,1.111,   1.505,  "ApoE4",
  "Hypertension medication","On HBP meds",1.412,1.050,1.898,"ASCVD component",
  
  "Current smoking","ApoE4",     1.287,   1.106,   1.499,  "ApoE4",
  "Current smoking","Smoker",    1.459,   0.756,   2.818,  "ASCVD component",
  
  "Diabetes","ApoE4",            1.285,   1.105,   1.498,  "ApoE4",
  "Diabetes","Diabetes",         0.964,   0.544,   1.708,  "ASCVD component"
) %>%
  mutate(
    Row  = factor(paste(Model, "-", Term), levels = rev(unique(paste(Model, "-", Term)))),
    Type = factor(Type, levels = c("ApoE4","ASCVD component"))
  )

# ---- Plot (ApoE4) ----
unique(abeta$Type)  # should be: "ApoE4" "ASCVD component"

p <- ggplot(abeta, aes(x = OR, y = Row, color = Type)) +
  geom_point(size = 1.6) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0, size = 0.5) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey50") +
  scale_x_log10(breaks = c(0.5, 1, 1.5, 2, 3),
                labels = c("0.5","1","1.5","2","3")) +
  scale_color_manual(
    values = c("ApoE4" = "#1E88E5", "ASCVD component" = "#D55E00"),
    breaks = c("ApoE4", "ASCVD component"),
    labels = c("ApoE4 term", "ASCVD component term"),
    name   = "Term"
  ) +
  labs(x = "Odds Ratio (log scale)", y = NULL,
       title = "ApoE4 with separate ASCVD components") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.y = element_text(size = 6, lineheight = 1.0),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )