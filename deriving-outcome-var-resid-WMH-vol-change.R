
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

# Load data to work with
merged_extended <- read_tsv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv",
                            sep = "\t", row.names = FALSE, quote = FALSE)

## PREPARE OUTCOME VARIABLE - WMH residualised change

# Fit regression of follow-up WMH on baseline WMH to derive outcome variable
wmh_resid_model <- lm(x25781_3_0_log ~ x25781_2_0_log, data = merged_extended)
merged_extended$WMH_TP2_adj <- resid(wmh_resid_model)

describe(merged_extended$WMH_TP2_adj)
# Check distribution
hist(merged_extended$WMH_TP2_adj) # not too bad

# Scale WMH outcome var
merged_extended$WMH_TP2_adj_std <- scale(merged_extended$WMH_TP2_adj)

# Save file again
write.table(merged_extended,
            "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE
)

