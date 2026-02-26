
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

### 12/09/2025: Standardise betas to complement results 

### 07/10/25 Update: No more need for this analysis!

### THIS WILL BE REDUNDANT - NO NEED FOR FURTHER STANDARDISATION

# ------------------------------------------------------
# 1. Compute SDs for all predictors from merged_extended
# ------------------------------------------------------
sds <- merged_extended %>%
  summarise(across(c(binary_vars, ord_vars, unord_vars, cont_vars), sd, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "phenotype", values_to = "SD_pred")

# ----------------------------------------------------
# 2. File handling
# ----------------------------------------------------
# Folder containing results

# Change names for any change, WMH progressors and WMH regressors

results_dir <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/WMH Regressors"

#results_dir <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/WMH Progressors"
#results_dir <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/ALL_RESULTS_ANY_CHANGE"

# Vector of result filenames
files <- c(
  "binary_regressors_unadj.csv",
  "binary_regressors_minadj.csv",
  "binary_regressors_extadj.csv",
  "ordered_regressors_unadj.csv",
  "ordered_regressors_minadj.csv",
  "ordered_regressors_extadj.csv",
  "unordered_regressors_unadj.csv",
  "unordered_regressors_minadj.csv",
  "unordered_regressors_extadj.csv",
  "continuous_regressors_unadj.csv",
  "continuous_regressors_minadj.csv",
  "continuous_regressors_extadj.csv"
)

# ----------------------------------------------------
# 3. Loop through files, standardise betas, and save
# ----------------------------------------------------
for (f in files) {
  
  infile <- file.path(results_dir, f)
  outfile <- file.path(results_dir, gsub(".csv$", "_std.csv", f))  # add _std to name
  
  # Read results file
  results <- read.csv(infile, stringsAsFactors = FALSE)
  
  # Merge SDs + compute standardized betas
  results_std <- results %>%
    left_join(sds, by = "phenotype") %>%
    mutate(
      beta       = as.numeric(beta),
      se         = as.numeric(se),
      ci_low     = as.numeric(ci_low),
      ci_high    = as.numeric(ci_high),
      SD_pred    = as.numeric(SD_pred),
      beta_std   = beta * SD_pred,
      se_std     = se * SD_pred,
      ci_low_std = ci_low * SD_pred,
      ci_high_std= ci_high * SD_pred
    )
  
  # Save with new filename
  write.csv(results_std, outfile, row.names = FALSE)
  
  message("Processed: ", infile, " --> ", outfile)
}

# Check files
test <- read.csv(file.path(results_dir, "orderedcat_adjustedFDRext.csv"),
                 stringsAsFactors = FALSE)

names(test)
str(test)
head(test)

# This file is fine; check remainder.

for (f in files) {
  infile <- file.path(results_dir, f)
  dat <- read.csv(infile, stringsAsFactors = FALSE)
  
  cat("\n---", f, "---\n")
  print(names(dat))
  print(str(dat$beta))
  print(head(dat))
}

# Now fix loop and rerun
for (f in files) {
  
  infile <- file.path(results_dir, f)
  outfile <- file.path(results_dir, gsub(".csv$", "_std.csv", f))
  
  results <- read.csv(infile, stringsAsFactors = FALSE)
  
  # Case 1: standard files with beta/se
  if ("beta" %in% names(results)) {
    results_std <- results %>%
      left_join(sds, by = "phenotype") %>%
      mutate(
        beta       = as.numeric(beta),
        se         = as.numeric(se),
        ci_low     = as.numeric(ci_low),
        ci_high    = as.numeric(ci_high),
        SD_pred    = as.numeric(SD_pred),
        beta_std   = beta * SD_pred,
        se_std     = se * SD_pred,
        ci_low_std = ci_low * SD_pred,
        ci_high_std= ci_high * SD_pred
      )
  }
  
  # Case 2: unordered categorical files with estimate/std.error
  else if ("estimate" %in% names(results)) {
    results_std <- results %>%
      left_join(sds, by = "phenotype") %>%
      mutate(
        estimate     = as.numeric(estimate),
        std.error    = as.numeric(std.error),
        conf.low     = as.numeric(conf.low),
        conf.high    = as.numeric(conf.high),
        SD_pred      = as.numeric(SD_pred),
        estimate_std = estimate * SD_pred,
        se_std       = std.error * SD_pred,
        ci_low_std   = conf.low * SD_pred,
        ci_high_std  = conf.high * SD_pred
      )
  }
  
  # Save with new filename
  write.csv(results_std, outfile, row.names = FALSE)
  message("Processed: ", infile, " --> ", outfile)
}

# CREATE NEW VISUALISATIONS - standardised betas 

# Load packages
library(readxl)
library(ggplot2)
library(dplyr)

# Load file
df <- read_excel("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/WMH Progressors/Standardized betas added/Unadj progressors/Unadj_stand_prog_sign.xlsx")

names(df) <- c("Phenotype", "Beta_std", "SE_std", "CI_lower_std", "CI_upper_std", "P", "P_FDR")

# Add -log10(p) for bubble size
df <- df %>%
  mutate(
    logP = -log10(P),
    Phenotype = factor(Phenotype, levels = rev(unique(Phenotype))) # reverse order for nice plotting
  )

df <- df %>%
  mutate(P_plot = ifelse(P <= 1e-10, 1e-10, P))  # cap tiny p-values

bubble_plot <- ggplot(df, aes(x = Beta_std, y = Phenotype)) +
  geom_point(aes(size = abs(Beta_std), color = P_plot), alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_size_continuous(name = "Effect size (|Beta|)", range = c(2, 8)) +
  scale_color_gradient(
    name = "Raw P-value",
    low = "orange", high = "cyan2",
    trans = "log10",                       # spread across log scale
    breaks = c(0.01, 0.0001),
    labels = c("0.01", "0.0001"),
    #limits = c(1e-4, 0.1) 
  ) +
  guides(
    color = guide_colorbar(label.theme = element_text(angle = 45))
  ) +
  labs(
    title = "Unadjusted Associations for WMH Progressors",
    x = "Standardized Beta",
    y = "Phenotype"
  ) +
  coord_cartesian(xlim = c(-0.20, 0.8)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 14),
    legend.position = "left",
    plot.title = element_text(hjust = 0.5)
  )

print(bubble_plot)

ggsave(
  "bubble_adj_min_any.png",
  bubble_plot,
  width = 12, height = 6, dpi = 300
)


