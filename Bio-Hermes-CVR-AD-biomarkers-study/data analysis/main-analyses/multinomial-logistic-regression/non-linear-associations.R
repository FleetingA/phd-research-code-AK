
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
# Author: Angelina Kancheva 
  
# Date: 29/10/2025
----------------------------------------------------------------------------------
  
  
# Load libraries
library(readr)
library(psych)
library(ggplot2)
library(dplyr)

### Exploring non-linear associations - Response to JAHA reviewers 
  
## ----------------------------
## Setup
## ----------------------------

y  <- "AD_status"
z  <- "ASCVD_cont_score_sqrtc"

# Biomarkers
biomarkers <- c("AB4240_ratio_c", "pTau181_logtr_c", "PTAU217_logtr_c", "ApoE4_sqrt_c")

## ----------------------------
## Helper: fit models & LRTs
## ----------------------------
fit_mods_and_tests <- function(x, data) {
  # 0) MAIN EFFECTS (no interaction)
  f_main <- as.formula(paste0(y, " ~ ", x, " + ", z))
  m_main <- glm(f_main, data = data, family = binomial)
  
  # 1) LINEAR INTERACTION
  f_int <- as.formula(paste0(y, " ~ ", x, " * ", z))
  m_int <- glm(f_int, data = data, family = binomial)
  
  # 2) QUADRATIC MAIN EFFECTS (keep interaction linear)
  f_quad_main <- as.formula(paste0(
    y, " ~ ", x, " * ", z,
    " + I(", x, "^2) + I(", z, "^2)"
  ))
  m_qmain <- glm(f_quad_main, data = data, family = binomial)
  
  # 3) NONLINEAR MODERATION (allow squared terms to interact)
  f_quad_int <- as.formula(paste0(
    y, " ~ ", x, " * ", z,
    " + I(", x, "^2) + I(", z, "^2)",
    " + I(", x, "^2):", z,
    " + ", x, ":I(", z, "^2)"
  ))
  m_qint <- glm(f_quad_int, data = data, family = binomial)
  
  # Likelihood-ratio tests
  lrt_int        <- anova(m_main,  m_int,   test = "LRT")  # does interaction help?
  lrt_quad_main  <- anova(m_int,   m_qmain, test = "LRT")  # do x^2 & z^2 help?
  lrt_quad_int   <- anova(m_qmain, m_qint,  test = "LRT")  # does nonlinear moderation help?
  
  # Extract p-values neatly
  get_p <- function(aov_obj) {
    as.numeric(tail(aov_obj$`Pr(>Chi)`, 1))
  }
  
  out <- data.frame(
    biomarker = x,
    p_interaction_LRT          = get_p(lrt_int),
    p_quad_main_LRT            = get_p(lrt_quad_main),
    p_quad_interaction_LRT     = get_p(lrt_quad_int),
    AIC_linear_interaction     = AIC(m_int),
    AIC_quad_main              = AIC(m_qmain),
    AIC_quad_interaction       = AIC(m_qint),
    row.names = NULL
  )
  
  # Return models and summary row
  list(models = list(
    main    = m_main,
    int     = m_int,
    q_main  = m_qmain,
    q_int   = m_qint
  ),
  lrt_row = out)
}

## ----------------------------
## Run for all biomarkers
## ----------------------------
all_results <- lapply(biomarkers, fit_mods_and_tests, data = dataset)

# Collect p-values/AIC in a single table
lrt_table <- do.call(rbind, lapply(all_results, `[[`, "lrt_row"))
print(lrt_table)

## ----------------------------
## (Optional) Quick odds ratios for the linear interaction model
## ----------------------------
or_tables <- lapply(all_results, function(res) {
  m <- res$models$int
  data.frame(
    term = names(coef(m)),
    OR   = exp(coef(m)),
    lower = exp(coef(m) - 1.96 * sqrt(diag(vcov(m)))),
    upper = exp(coef(m) + 1.96 * sqrt(diag(vcov(m))))
  )
})
names(or_tables) <- biomarkers
# Example: view ORs for AB42/40 interaction model
or_tables$AB4240_ratio_c



