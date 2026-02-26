
--------------------------------------------------------------------------
# PhD Project 2025: UKB WMH volume change over time + clinical phenotypes
  
# Author: Angelina Kancheva 
# Date/period: September 2025
--------------------------------------------------------------------------

# Load packages
library(readxl)
library(psych)
library(dplyr)

# Read in progressors
prog_outliers <- read_xlsx("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master/PHESANT-master/MyWas/data16042025/Only_Prog_Outliers-030226.xlsx")
# Read in regressors
reg_outliers <- read_xlsx("C:/Users/angel/Desktop/PRECISION_MEDICINE/PHESANT-master/PHESANT-master/MyWas/data16042025/Only_Reg_Outliers-030226.xlsx")

# Describe groups
describe(prog_outliers)
describe(reg_outliers)

class(prog_outliers)
class(reg_outliers)

