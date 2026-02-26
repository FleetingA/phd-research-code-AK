
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes

# Author: Angelina Kancheva 
# Date/period: September 2025
--------------------------------------------------------------------------  
  
### Phenotypes were preprocessed by running PHESANT with the 'save' option.

# Load libraries
library(dplyr)
library(psych)
library(readr)

# Define base path
base_path <- "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results02092025"

# Read in each pre-processed phenotype file
bin   <- read.csv(file.path(base_path, "data-binary-all.txt"),   stringsAsFactors = FALSE)
ord   <- read.csv(file.path(base_path, "data-catord-all.txt"),   stringsAsFactors = FALSE)
unord <- read.csv(file.path(base_path, "data-catunord-all.txt"), stringsAsFactors = FALSE)
cont  <- read.csv(file.path(base_path, "data-cont-all.txt"),     stringsAsFactors = FALSE)

# Merge them all by userID
phenos <- bin %>%
  full_join(ord,   by = "userID") %>%
  full_join(unord, by = "userID") %>%
  full_join(cont,  by = "userID")

# Save combined TSV in the same folder
write.table(phenos, file.path(base_path, "phenotypes-all.tsv"),
            sep = "\t", row.names = FALSE, quote = FALSE)

# Load phenotypes again
phenos <- read.table(file.path(base_path, "phenotypes-all.tsv"),
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)
View(phenos)

# Quick sanity checks 
dim(phenos)              # 502359 participants × 241 variables
head(phenos[,1:10])      # preview first 10 columns

# A few additional sanity checks
describe(phenos$X25781) # 45013; means this is UKB instance 2 
class(phenos$X25781) # numeric 

# Append other important variables to this dataset
merged_anychange <- read_csv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_anychange.csv")
View(merged_anychange)
nrow(merged_anychange) # 4329 participants, as it should be

# Save only those values from phenofile that correspond to the userIds present in the merged file
# Add the WMH time point 2 variable, 25781_3 

# Load preprocessed phenotypes (TSV)
phenos <- read.table(
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/results02092025/phenotypes-all.tsv",
  header = TRUE, sep = "\t", stringsAsFactors = FALSE
)

# Load merged file (CSV)
merged_anychange <- read_csv(
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_anychange.csv"
)

# Check the ID column names in both
names(phenos)[1]          # should be "userID"
names(merged_anychange)[1]  # check if it's "userID" or "eid", etc.

# If merged_anychange has "eid", rename to "userID" to match phenos
phenos <- phenos %>%
  rename(userId = userID)
View(phenos)

# Keep only participants from merged_anychange (4329 IDs)
merged_060925 <- phenos %>%
  semi_join(merged_anychange, by = "userId")

# Save the clean pre-processed file for those 4329 people
write.table(merged_060925,
            "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/phenotypes-4329.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE
)

# Quick check
dim(merged_060925) # 4329 x 241
View(merged_060925)

# Inspect WMH instance 2 variable
describe(merged_060925$X25781) # 4329
hist(merged_060925$X25781) # pretty normal

# Add missing variables from merged dataset

# First, check values for each of them
describe(merged_anychange$x25781_3_0.x) # 4329
describe(merged_anychange$xYears_2) # 4201
describe(merged_anychange$xMonths_2) # 4201
describe(merged_anychange$x6153_2_0) # 126 # consider dropping
describe(merged_anychange$HTN_med) # 217
table(merged_anychange$HTN_med) # 179=0; 1=38 


# Vector of variable IDs to add
extra_vars <- c("x25781_2_0", "x25781_3_0.x", "x53_2_0", "x53_3_0", "x53_0_0",
                "xYears_2", "xMonths_2", "xDays_2", "x6153_2_0", "HTN_med")

# Subset those columns + userId from merged_anychange
extra_data <- merged_anychange %>%
  select(userId, all_of(extra_vars))

# Append them to merged_060925
merged_extended <- merged_060925 %>%
  left_join(extra_data, by = "userId")

# Save the extended dataset
write.table(merged_extended,
            "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE
)

# Quick check that your new variables are in there
names(merged_extended)[names(merged_extended) %in% extra_vars]
View(merged_extended)

# Make all column names lowercase
names(merged_extended) <- sub("^X", "x", names(merged_extended))

# Rename x25781_3_0.x → x25781_3_0
names(merged_extended)[names(merged_extended) == "x25781_3_0.x"] <- "x25781_3_0"

# Check WMH variable instance 3 renaming
describe(merged_extended$x25781_3_0) # 4329; good!

# Check WMH variable instance 2 again
describe(merged_extended$x25781_2_0) # 4329, good!

# Make sure only complete rows for the imaging vars are included
merged_extended070925 <- merged_extended %>%
  filter(!is.na(x25781) & !is.na(x25781_3_0))
nrow(merged_extended070925) # 4329 remaining, as it should be!

# Save this new version of the dataset
write.table(
  merged_extended070925,
  "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended070925.tsv",
  sep = "\t", row.names = FALSE, quote = FALSE
)

# Load data again
library(readr)
merged_extended <- read_tsv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv")

# WMHs vars are currently raw - inspect and preprocess manually
describe(merged_extended$x25781_2_0)
describe(merged_extended$x25781_3_0)

hist(merged_extended$x25781_2_0) # very skewed
hist(merged_extended$x25781_3_0) # very skewed 

# Log transform variables
merged_extended$x25781_2_0_log <- log1p(merged_extended$x25781_2_0)
merged_extended$x25781_3_0_log <- log1p(merged_extended$x25781_3_0)

# Check again
describe(merged_extended$x25781_2_0_log)
describe(merged_extended$x25781_3_0_log)

hist(merged_extended$x25781_2_0_log) # pretty normal, much improved
hist(merged_extended$x25781_3_0_log) # pretty normal, much improved

# Save the clean preprocessed file for those 4329 people
write.table(merged_extended,
            "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE
)

# Add the ICV variable (not added yet)

# Load the confounder file (CSV)
confounders <- read_csv("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/confounderfile_partial.csv")

# Subset just userId and ICV
icv_data <- confounders %>%
  select(userId, xICV_2_0)

# Merge into your main dataset
merged_extended <- merged_extended %>%
  left_join(icv_data, by = "userId")

# Quick check
summary(merged_extended$xICV_2_0)
hist(merged_extended$xICV_2_0) # fairly normal
describe(merged_extended$xICV_2_0) # 4329
nrow(merged_extended) # 4329
View(merged_extended)

# Save file again
write.table(merged_extended,
            "C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT_050925_UKBWMH-study/PHESANT-master/MyWas/data02092025/New Files to work with/merged_extended.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE
)

##### Some folder names/paths might have changed. Double-check before loading again. ##### 

