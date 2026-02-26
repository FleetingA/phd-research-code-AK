
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

### 02/10/2025: Percentile approach to quantifying WMH change (as in Jochems et al.)

hist(merged_extended$x25781_2_0_log) # normal
hist(merged_extended$x25781_3_0_log) # normal
hist(merged_extended$xICV_2_0) # normal

# =========================
# Setup
# =========================
library(dplyr)
library(ggplot2)

# merged_extended with:
#   x25781_2_0 = baseline WMH volume (mm3)
#   x25781_3_0 = follow-up WMH volume (mm3)
#   xICV_2_0   = proxy ICV (mm3)

df <- merged_extended %>%
  mutate(
    WMH_bl_raw = x25781_2_0, # baseline raw
    WMH_fu_raw = x25781_3_0, # follow-up raw
    ICV_proxy  = xICV_2_0
  ) %>%
  filter(is.finite(WMH_bl_raw), is.finite(WMH_fu_raw), is.finite(ICV_proxy))

# =========================
# Express as %ICV and compute net change
# =========================
df <- df %>%
  mutate(
    WMH_pct_bl = 100 * WMH_bl_raw / ICV_proxy, # baseline
    WMH_pct_fu = 100 * WMH_fu_raw / ICV_proxy, # follow-up
    dWMH_pct   = WMH_pct_fu - WMH_pct_bl,   # net change (%ICV)
    dWMH_raw   = WMH_fu_raw - WMH_bl_raw    # net change (mm3)
  )

# ========================================================
# Percentile-based categorisation (like Angela's paper)
# ========================================================

# Work on raw mm3 change (dWMH_raw) for comparability
delta_var <- df$dWMH_raw

F_delta <- ecdf(delta_var)
p0 <- F_delta(0) * 100            # percentile of no-change
p0_round <- round(p0)

lower_pct <- max(0, p0_round - 10)
upper_pct <- min(100, p0_round + 10)

lower_cut <- as.numeric(quantile(delta_var, probs = lower_pct/100, na.rm = TRUE))
upper_cut <- as.numeric(quantile(delta_var, probs = upper_pct/100, na.rm = TRUE))

message(sprintf("No-change percentile p0 = %.1f (rounded to %d)", p0, p0_round))
message(sprintf("Stable band: %dth–%dth percentiles", lower_pct, upper_pct))
message(sprintf("Stable cutpoints on Δ WMH raw (mm3): [%.1f, %.1f]", lower_cut, upper_cut))

# Categorise Regression/Stable/Progression
df <- df %>%
  mutate(
    WMH_change_cat = case_when(
      dWMH_raw <  lower_cut ~ "Regression",
      dWMH_raw >  upper_cut ~ "Progression",
      TRUE                  ~ "Stable"
    ),
    WMH_change_cat = factor(WMH_change_cat,
                            levels = c("Regression","Stable","Progression"),
                            ordered = TRUE)
  )

# ===============================
# Quintiles of Δ WMH for plots
# ===============================
df <- df %>%
  mutate(
    dWMH_quint = cut(dWMH_raw,
                     breaks = quantile(dWMH_raw, probs = seq(0, 1, 0.2), na.rm = TRUE),
                     include.lowest = TRUE,
                     labels = paste0("Q", 1:5))
  )

table(df$WMH_change_cat)
table(df$dWMH_quint)

#Q1  Q2  Q3  Q4  Q5 
#867 867 863 867 865 
# for a total n of 4329

summary(df$WMH_change) # your delta variable
quantile(df$WMH_change, probs = c(0.26, 0.36, 0.46))
table(df$WMH_change_cat)

# Diagnostic check
# Ensure we're looking at the numeric Δ WMH
df <- df %>%
  mutate(dWMH_raw = as.numeric(dWMH_raw))

# Summary of raw changes
summary(df$dWMH_raw)

# Check where 0 falls and the stable cutoffs
quantile(df$dWMH_raw, probs = c(0.26, 0.36, 0.46), na.rm = TRUE)

# Frequency table of categories
table(df$WMH_change_cat, useNA = "ifany")

### Summary stats for the three groups

library(dplyr)

# First, read in the native file with the original phenotypes

# =============================
# 1. Read phenotypes-all.tsv
# =============================

# Read the raw phenotype file
df_raw <- read.csv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/data02092025/phenotypes_260325.csv",
                   header = TRUE, stringsAsFactors = FALSE)

# Quick check of structure
str(df_raw)

# Look at first few rows
head(df_raw[, 1:10])   # first 10 columns only so it’s not overwhelming

# Inspect all column names
colnames(df_raw)[1:50]  # first 50 column names

# Inspect column names
colnames(df_raw)[1:50]  # just look at first 50 to find the exact names

# =========================
# 2. Select raw variables
# =========================
vars_keep <- c("userId", 
               "x21022_0_0",  # age
               "x31_0_0",     # sex
               "x4080_2_0",   # systolic BP
               "x4079_2_0",   # diastolic BP
               "x21001_2_0",  # BMI
               "x1239_2_0",   # smoking
               "x25781_2_0",  # WMH baseline
               "x25781_3_0")  # WMH follow-up

df_raw_sel <- df_raw[, vars_keep]

# ================================
# 3. Merge with your current df
# ================================
df <- df %>%
  left_join(df_raw_sel, by = "userId")

# Now df has both PHESANT-processed and raw UKB fields

names(df_raw)[1:100]   # show first 100 column names
names(df)

# Check summaries now
library(dplyr)

# Pick continuous variables
cont_vars <- c("userId", 
               "x21022_0_0.y.y",  # age
               "x31_0_0.y",     # sex
               "x4080_2_0.x",   # systolic BP
               "x4079_2_0.x",   # diastolic BP
               "x21001_2_0",  # BMI
               "x1239_2_0",   # smoking
               "x25781_2_0.y.y",  # WMH baseline
               "x25781_3_0.y.y")   # WMH follow-up

# Summary stats per group
desc_cont <- df %>%
  select(WMH_change_cat, all_of(cont_vars)) %>%
  group_by(WMH_change_cat) %>%
  summarise(across(all_of(cont_vars),
                   list(
                     n      = ~sum(!is.na(.)),
                     mean   = ~mean(., na.rm = TRUE),
                     sd     = ~sd(., na.rm = TRUE),
                     median = ~median(., na.rm = TRUE),
                     IQR    = ~IQR(., na.rm = TRUE)
                   ),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

print(desc_cont)

# =========================
# Categorical descriptives
# =========================
desc_cat_sex <- df %>%
  group_by(WMH_change_cat, x31_0_0.y.y) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = 100 * n / sum(n))

desc_cat_smoking <- df %>%
  group_by(WMH_change_cat, x1239_2_0) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = 100 * n / sum(n))

# Combine categorical tables
desc_cat <- bind_rows(
  desc_cat_sex %>% mutate(variable = "Sex", category = as.character(x31_0_0.y.y)) %>% select(-x31_0_0.y.y),
  desc_cat_smoking %>% mutate(variable = "Smoking", category = as.character(x1239_2_0)) %>% select(-x1239_2_0)
)

# =========================
# Save to CSVs
# =========================
out_dir <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression"

write.csv(desc_cont,
          file = file.path(out_dir, "descriptives_continuous.csv"),
          row.names = FALSE)

write.csv(desc_cat,
          file = file.path(out_dir, "descriptives_categorical.csv"),
          row.names = FALSE)

message("Descriptive stats saved in: ", out_dir)

# Counts only
table(df$WMH_change_cat)

# Counts + percentages
df %>%
  count(WMH_change_cat) %>%
  mutate(percent = 100 * n / sum(n))

# ===============================================
# PLOTS: WMH change vs baseline WMH, by quintile
# ===============================================

# Baseline WMH distribution across quintiles

# Make sure quintiles are ordered from Q1 (regression) to Q5 (progression)
df$dWMH_quint <- factor(df$dWMH_quint,
                        levels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                        ordered = TRUE)

# Plot
# Make sure quintiles are ordered from Q1 (regression) to Q5 (progression)
df$dWMH_quint <- factor(df$dWMH_quint,
                        levels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                        ordered = TRUE)

# Plot
p1 <- ggplot(df, aes(x = dWMH_quint, y = WMH_pct_bl, fill = dWMH_quint)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_fill_brewer(palette = "YlGnBu", direction = 1) +  # Q1 light → Q5 dark
  labs(
    x = expression(Delta~"WMH Quintile (Raw mm"^3*")"),
    y = "Baseline WMH (%ICV)",
    title = "Baseline WMH (%ICV) across Quintiles of Raw Δ"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # removes redundant legend since x-axis already labels
print(p1)

# Make sure categories are ordered
df$WMH_change_cat <- factor(df$WMH_change_cat,
                            levels = c("Regression", "Stable", "Progression"))

p2 <- ggplot(df, aes(x = WMH_change_cat, y = WMH_pct_bl, fill = WMH_change_cat)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_fill_brewer(palette = "Set2") +  # distinct colours for 3 categories
  labs(
    x = "WMH Change Group (percentile method)",
    y = "Baseline WMH (%ICV)",
    title = "Baseline WMH (%ICV) across Regression, Stable, and Progression Groups"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p2)


# =========================
# Regression templates
# =========================

# Overall association
fit_A <- lm(dWMH_pct ~ WMH_pct_bl, data = df)
summary(fit_A)

# Interaction with quintiles
fit_B <- lm(dWMH_pct ~ WMH_pct_bl * dWMH_quint, data = df)
summary(fit_B)

# Regression with 3-class outcome (Regression/Stable/Progression) if needed
# library(nnet)
# fit_rsp <- nnet::multinom(WMH_change_cat ~ WMH_pct_bl, data = df, trace = FALSE)
# summary(fit_rsp)


# Check new outcome
table(df$WMH_change_cat, useNA = "ifany")

# Make sure it is a factor with Stable as reference
df$WMH_change_cat <- factor(df$WMH_change_cat,
                            levels = c("Stable", "Regression", "Progression"))

summary(df$WMH_change_cat)

# Group comparison with the three new groups
library(dplyr)
library(purrr)

## Continuous phenotypes
kw_res <- map_dfr(cont_vars, function(v) {
  out <- kruskal.test(df[[v]] ~ df$WMH_change_cat)
  tibble(phenotype = v,
         test = "Kruskal-Wallis",
         stat = unname(out$statistic),
         df = unname(out$parameter),
         p = out$p.value)
}) %>%
  group_by(test) %>%
  mutate(p_FDR = p.adjust(p, "fdr")) %>%
  ungroup()

## Categorical phenotypes (binary, ordered, unordered)
chi_res <- map_dfr(c(binary_vars, ord_vars, unord_vars), function(v) {
  tab <- table(df$WMH_change_cat, df[[v]])
  out <- suppressWarnings(chisq.test(tab))
  tibble(phenotype = v,
         test = "Chi-squared",
         stat = unname(out$statistic),
         df = unname(out$parameter),
         p = out$p.value)
}) %>%
  group_by(test) %>%
  mutate(p_FDR = p.adjust(p, "fdr")) %>%
  ungroup()

## Combine
group_comparisons <- bind_rows(kw_res, chi_res) %>%
  arrange(p_FDR)
head(group_comparisons)

# Combine all label tables
labels_tbl <- bind_rows(
  binary_labels,
  ord_labels,
  unord_labels,
  cont_labels
) %>%
  distinct()  # in case of duplicates

# Add labels to group comparisons
group_comparisons_labeled <- group_comparisons %>%
  left_join(labels_tbl, by = c("phenotype" = "phenotype")) %>%
  mutate(label = ifelse(is.na(label), phenotype, label)) %>%
  select(phenotype, label, everything())  # put label next to code

out_path <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master_050925/PHESANT-master/MyWas/results02092025/Progression Stable Regression/group_comparisons_labeled.csv"

write.csv(group_comparisons_labeled, file = out_path, row.names = FALSE)

message("File with labels saved to: ", out_path)

hist(df$WMH_TP2_adj_std)
library(psych)
describe(df$WMH_TP2_adj_std)

# Association with raw WMH change (%ICV and mm3)
m1 <- lm(dWMH_pct ~ xYears_2, data = df)
m2 <- lm(dWMH_raw ~ xYears_2, data = df)

summary(m1)   # WMH %ICV change ~ years
summary(m2)   # WMH raw mm3 change ~ years

# Summary statistics for follow-up var
summary(df$xYears_2)
describe(df$xYears_2)

# Histogram
hist(df$xYears_2, breaks=30, col="skyblue", main="Distribution of Follow-up (years)",
     xlab="Years between baseline and follow-up")

# Density plot
plot(density(df$xYears_2, na.rm=TRUE), main="Density of Follow-up (years)", xlab="Years")

# Boxplot by WMH group
boxplot(xYears_2 ~ WMH_change_cat, data=df,
        xlab="WMH change group", ylab="Follow-up time (years)",
        col=c("lightblue","lightgreen","lightpink"))

# Scatter with smoothing line
plot(df$xYears_2, df$dWMH_raw, pch=16, col=rgb(0,0,1,0.3),
     xlab="Follow-up time (years)", ylab="Δ WMH raw (mm³)")
abline(lm(dWMH_raw ~ xYears_2, data=df), col="red", lwd=2)
lines(lowess(df$xYears_2, df$dWMH_raw), col="darkgreen", lwd=2)
legend("topleft", legend=c("Linear fit","Lowess smoother"),
       col=c("red","darkgreen"), lty=1, bty="n")


### Group comparisons with posthocs (with labels) ###

library(dplyr)
library(FSA) # for Dunn test

# Check normality
hist(df$x21022_0_0.y.y) # no
hist(df$x4080_2_0.x) # yes
hist(df$x4079_2_0.x) # yes
hist(df$x21001_2_0) # mostly
hist(df$x25781_2_0.y.y) # no
hist(df$x25781_3_0.y.y) # no

df$WMH_change_cat
levels(df$WMH_change_cat)

# ==============================
# Extra libraries
# ==============================
library(FSA) # Dunn test

# ==============================
# Variable groups
# ==============================
kw_vars   <- c("x21022_0_0.y.y", "x21001_2_0", "x25781_2_0.y.y", "x25781_3_0.y.y")
anova_vars <- c("x4080_2_0.x", "x4079_2_0.x")
cat_vars   <- c("x31_0_0.y", "x1239_2_0")

# ============================
# Kruskal-Wallis + Dunn
# ============================

# Make sure outcome is a factor
df <- df %>%
  mutate(WMH_change_cat = as.factor(WMH_change_cat))
class(df$WMH_change_cat)
table(df$WMH_change_cat)

# Set ordered to FALSE
df$WMH_change_cat <- factor(df$WMH_change_cat, 
                            levels = levels(df$WMH_change_cat), 
                            ordered = FALSE)

# Age
kruskal.test(x21022_0_0 ~ WMH_change_cat, data = df)
dunnTest(x21022_0_0 ~ WMH_change_cat, data = df, method = "holm")

# BMI
kruskal.test(x21001 ~ WMH_change_cat, data = df)
dunnTest(x21001 ~ WMH_change_cat, data = df, method = "holm")

# WMH baseline
kruskal.test(x25781_2_0 ~ WMH_change_cat, data = df)
dunnTest(x25781_2_0 ~ WMH_change_cat, data = df, method = "holm")

# WMH follow-up
kruskal.test(x25781_3_0 ~ WMH_change_cat, data = df)
dunnTest(x25781_3_0 ~ WMH_change_cat, data = df, method = "holm")

# ============================
# ANOVA + Tukey
# ============================

# SBP
fit_sbp <- aov(x4080_2_0.x ~ WMH_change_cat, data = df)
summary(fit_sbp)
TukeyHSD(fit_sbp)

# DBP
fit_dbp <- aov(x4079_2_0.x ~ WMH_change_cat, data = df)
summary(fit_dbp)
TukeyHSD(fit_dbp)

# ============================
# Chi-squared (categorical)
# ============================

# Sex
tab_sex <- table(df$WMH_change_cat, df$x31_0_0.y)
chisq.test(tab_sex) # significant

# Posthoc
# Sex ~ WMH_change_cat pairwise comparisons with Holm correction
pairwise_sex <- pairwise.prop.test(
  x = table(df$WMH_change_cat, df$x31_0_0.y),
  p.adjust.method = "holm"
)
pairwise_sex
table(df$x31_0_0.y) # female=0, male=1

# Frequency table: counts of females and males in each WMH group
table(df$WMH_change_cat, df$x31_0_0.y)

# Proportion table: row-wise proportions (within each WMH group)
round(prop.table(table(df$WMH_change_cat, df$x31_0_0.y), 1), 2)

# Smoking
tab_smoke <- table(df$WMH_change_cat, df$x1239_2_0)
chisq.test(tab_smoke) # not significant 

# ============================
# Kruskal-Wallis + Dunn
# ============================

# Age
kruskal.test(x21022_0_0.y.y ~ WMH_change_cat, data = df)
install.packages("FSA")   # if not already installed
library(FSA)
dunnTest(x21022_0_0.y.y ~ WMH_change_cat, data = df, method = "holm")

# BMI
kruskal.test(x21001_2_0 ~ WMH_change_cat, data = df)
dunnTest(x21001_2_0 ~ WMH_change_cat, data = df, method = "holm")

# WMH baseline
kruskal.test(x25781_2_0.y.y ~ WMH_change_cat, data = df)
dunnTest(x25781_2_0.y.y ~ WMH_change_cat, data = df, method = "holm")

levels(df$WMH_change_cat)
# If needed, enforce a consistent order:
df$WMH_change_cat <- factor(df$WMH_change_cat,
                            levels = c("Regression","Stable","Progression"))

# WMH follow-up
kruskal.test(x25781_3_0.y.y ~ WMH_change_cat, data = df)
dunnTest(x25781_3_0.y.y ~ WMH_change_cat, data = df, method = "holm")

# ============================
# ANOVA + Tukey
# ============================

# SBP
fit_sbp <- aov(x4080_2_0.x ~ WMH_change_cat, data = df)
summary(fit_sbp)
TukeyHSD(fit_sbp)

# DBP
fit_dbp <- aov(x4079_2_0.x ~ WMH_change_cat, data = df)
summary(fit_dbp)
TukeyHSD(fit_dbp)

# Net WMH change
library(tidyr)
df <- df %>%
  mutate(WMH_change_raw = x25781_3_0.y.y - x25781_2_0.y.y)
summary(df$WMH_change_raw)
median(df$WMH_change_raw, na.rm = TRUE)
mean(df$WMH_change_raw, na.rm = TRUE)

# Is it statistically significant?
wilcox.test(df$x25781_3_0.y.y, df$x25781_2_0.y.y, paired = TRUE)

boxplot(df$x25781_2_0.y.y, df$x25781_3_0.y.y,
        names = c("Baseline", "Follow-up"),
        main = "WMH volume change",
        ylab = "WMH volume (mm³)")

fit <- lm(WMH_change_raw ~ xYears_2, data = df)
summary(fit)

fit <- lm(WMH_change_raw ~ xYears_2, data = df)
confint(fit, level = 0.95) # 95% confidence intervals

# Visualise this
library(ggplot2)

# Boxplot of net WMH change across groups
ggplot(df, aes(x = WMH_change_cat, y = dWMH_raw, fill = WMH_change_cat)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Net WMH Change (mm³) across Regression, Stable, and Progression Groups",
    x = "WMH Change Group (percentile method)",
    y = "Net WMH Change (mm³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
geom_jitter(width = 0.2, alpha = 0.3, size = 0.5)


