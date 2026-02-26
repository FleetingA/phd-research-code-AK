
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date: 11/03/2025
--------------------------------------------------------------------------

### Comparing UserIds across files

# Load libraries
library(readr) 
  
# Load data
TESTTEST_confounderfile_plus_icv <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data23022025/TESTTEST_confounderfile_plus_icv.csv")
View(TESTTEST_confounderfile_plus_icv)

TESTTEST_traitofinterest_230225_no_outliers <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data23022025/TESTTEST_traitofinterest_230225_no_outliers.csv")
View(TESTTEST_traitofinterest_230225_no_outliers)

# Check structure of both files
print(class(TESTTEST_confounderfile_plus_icv))  # Should return "tbl_df" "tbl" "data.frame"
print(class(TESTTEST_traitofinterest_230225_no_outliers))

# Check if all column names are properly read
print(colnames(TESTTEST_confounderfile_plus_icv))
print(colnames(TESTTEST_traitofinterest_230225_no_outliers))

if ("userId" %in% names(TESTTEST_confounderfile_plus_icv) & "userId" %in% names(TESTTEST_traitofinterest_230225_no_outliers)) {
  ids1 <- TESTTEST_confounderfile_plus_icv$userId
  ids2 <- TESTTEST_traitofinterest_230225_no_outliers$userId
  
  # Compare UserIds
  same_ids <- setequal(ids1, ids2)
  
  if (same_ids) {
    print("Both files have the same UserId participants.")
  } else {
    print("The files have different UserId participants.")
  }
} else {
  print("UserId column is missing in one or both files.")
}
