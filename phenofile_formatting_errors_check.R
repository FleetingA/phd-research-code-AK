
#### Investigating formatting problems in phenofile - 10/03/2025 ####

# Load data
library(readr)
phenotypes_151223 <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data23022025/phenotypes_151223.csv")
View(phenotypes_151223)

X <- as.matrix(df)  # Convert dataframe to matrix
mode(X) <- "numeric"  # Force numeric mode

# Check columns with errors
cols_to_check <- c("x924_2_0", "x1160_2_0", "x1200_2_0", "x1220_2_0", "x1239_2_0", 
                   "x1249_2_0", "x1558_2_0", "x2060_2_0", "x2070_2_0", "x2080_2_0", 
                   "x2178_2_0", "x2296_2_0", "x4079_2_0", "x4080_2_0", "x4620_2_0", 
                   "x5507_2_0", "x6349_2_0", "x6351_2_0", "x6373_2_0", "x20016_2_0", 
                   "x20127_0_0", "x20128_2_0", "x20537_0_0", "x20539_0_0")

colSums(is.na(phenotypes_151223[cols_to_check]))

colSums(is.infinite(as.matrix(phenotypes_151223[cols_to_check])))

class(phenotypes_151223)

phenotypes_151223 <- as.data.frame(df)
rm(phenotypes_151223)  # Remove if it was mistakenly assigned
phenotypes_151223 <- read_csv("C:/Users/angel/Desktop/PHEWAS_PhD_Project_2023/291123_PheWas_Paper_Results/MyWas/data23022025/phenotypes_151223.csv")
View(phenotypes_151223)
str(phenotypes_151223)

# Load as data frame and replace NA and Inf values
phenotypes_151223 <- as.data.frame(phenotypes_151223)
cols_to_check %in% names(phenotypes_151223)
colSums(is.na(phenotypes_151223))
colSums(is.infinite(as.matrix(phenotypes_151223)))

df_selected <- phenotypes_151223[, cols_to_check, drop = FALSE]
colSums(is.na(df_selected))
colSums(is.infinite(as.matrix(df_selected)))

df_selected[] <- lapply(df_selected, function(x) as.numeric(as.character(x)))

df_selected[] <- lapply(df_selected, function(x) {
  x[is.na(x)] <- 0
  x[is.infinite(x)] <- max(x[!is.infinite(x)], na.rm = TRUE)
  return(x)
})

X <- as.matrix(phenotypes_151223)
svd_result <- svd(X)

cols_to_check %in% names(phenotypes_151223)
View(phenotypes_151223)

# Change NA to blank spaces
phenotypes_151223[is.na(phenotypes_151223)] <- ""

# Save new file
write.csv(phenotypes_151223, "cleaned_data.csv", row.names = FALSE, na = "")

