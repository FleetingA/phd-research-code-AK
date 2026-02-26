
--------------------------------------------------------------------------
# PhD Project 2025: Lothian Birth Cohort + WMH volume SEM analysis 
  
# Author: Angelina Kancheva 
# Date: 28/05/2025
--------------------------------------------------------------------------

# Install the haven package to transfer data from SPSS
install.packages("haven")

# Load libraries
library(readr)
library(haven)

# Read the SPSS .sav file
lbc_data <- read_sav("C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/LBC1936_PhenotypesAssociatedWithCerebralSVD_AK_14APR2025.sav")
View(lbc_data)

# Or load alternatively (depending on where dataset is stored)
load("C:/Users/angel/Desktop/PRECISION_MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.RData")
ls()

# Save data in R
save(lbc_data, file = "C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.RData")
# Save as csv also
write.csv(lbc_data, file = "C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.csv", row.names = FALSE)

# Load data (as per 15/08/2025)
library(readr)
lbc_data <- read_csv("C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.csv")
View(lbc_data)

# Check variables

## Kidney health
# eGFR
library(psych)
describe(lbc_data$bld_eGFR_w2)
hist(lbc_data$bld_eGFR_w2)
hist(lbc_data$bld_eGFR_w2, main = "eGFR Distribution", xlab = "eGFR", col = "lightblue", breaks = 20)

plot(density(lbc_data$bld_eGFR_w2, na.rm = TRUE), main = "Density Plot of eGFR in LBC1936", col = "cyan3", 
     xlab = "eGFR")

boxplot(lbc_data$bld_eGFR_w2, main = "Boxplot of eGFR in LBC1936", ylab = "eGFR", col = "lightgreen")

# Summary stats for eGFR
install.packages("psych")
library(psych)
describe(lbc_data$bld_eGFR_w2) # median=65; range=54; mean=63.12; SD=5.96; min=11; max=65.
# All values 65.00 designate eGFR>60ml/min.

max_density <- max(density(lbc_data$bld_eGFR_w2, na.rm = TRUE)$y)
print(max_density)

# Normality test for eGFR
hist(lbc_data$bld_eGFR_w2, prob = TRUE,
     main = "eGFR Distribution with Normal Curve",
     xlab = "eF=GFR",
     xlim = c(30, 110),    # adjust as needed for horizontal space
     ylim = c(0, 0.25),    # adjust to control vertical space
     col = "lightgray", border = "white")

lines(density(lbc_data$bld_eGFR_w2, na.rm = TRUE), col = "blue", lwd = 2)

curve(dnorm(x,
            mean = mean(lbc_data$bld_eGFR_w2, na.rm = TRUE),
            sd = sd(lbc_data$bld_eGFR_w2, na.rm = TRUE)),
      col = "red", lwd = 2, add = TRUE)

# Who attended Wave 2?
table(lbc_data$attend73) # 0=225; 1=809 

# Sex
table(lbc_data$sex) # 1=518 (Male); 2=516 (Female)
library(dplyr)

# Recode sex to binary instead of categorical
library(tidyverse)
lbc_data <- lbc_data %>%
  mutate(sex_recode = case_when(
    sex == 1 ~ 0,
    sex == 2 ~ 1,
    TRUE ~ NA_real_
  ))

lbc_data <- read_csv("C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.csv")
View(lbc_data)
# Check recoding - new variable created (sex_recode)
table(lbc_data$sex_recode) # 0=518 (Male); 1=516 (Female)

## Age 
# Age in days on taking SMS MHT aged 11
hist(lbc_data$agedays_SMS)
summary(lbc_data$agedays_SMS) # mean=3993.18 
describe(lbc_data$agedays_SMS) # SD=102.7; median=4001; range=365

# Age in days when tested at the WTCRF - Wave 2
hist(lbc_data$agedays_w2) # pretty much normally distributed
summary(lbc_data$agedays_w2) # mean=26475.05 
describe(lbc_data$agedays_w2) # SD=257.5; median=26482; range=1186

# Current smoking status 
table(lbc_data$smokcurr_w2) # 0 (Never)=392; 1 (Ex)=352; 2 (Current)=65  
describe(lbc_data$smokcurr_w2) # checking for implausible values

# Alcohol consumption (Yes or No)
table(lbc_data$alcohol_w2) # 0 (No)=97; 1 (Yes)=712
describe(lbc_data$alcohol_w2) # checking for implausible values

# Health self-report category - Wave 2
health_labels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")

# Original histogram, but suppress the default x-axis
hist(lbc_data$hlthcat_w2,
     col = "pink3",
     main = "Self-Rated Health at Wave 2",
     xlab = "Health Category",
     ylab = "Frequency",
     xaxt = "n")  # Turn off default x-axis labels

# Add new x-axis labels at positions 1 through 5
axis(1, at = 1:5, labels = health_labels)

table(lbc_data$hlthcat_w2)

# Health compared to one year ago - Wave 2
table(lbc_data$hlthyrago_w2)

# Health self-report category - Wave 2
health_labels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")

# Another figure (slightly more detailed) - 15/08/2025:

# Health self-report category - Wave 2
health_labels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")

# Save histogram data
h <- hist(lbc_data$hlthcat_w2,
          col = "pink3",
          main = "Self-Rated Health at Wave 2",
          xlab = "Health Category",
          ylab = "Frequency",
          xaxt = "n")

# Add new x-axis labels
axis(1, at = 1:5, labels = health_labels)

# Add counts above bars (excluding zeros)
for (i in seq_along(h$counts)) {
  if (h$counts[i] > 0) {
    text(x = h$mids[i],
         y = h$counts[i] + 11,  # slightly above bar
         labels = paste0("N=", h$counts[i]),
         font = 2,               # bold text
         cex = 0.9)               # slightly larger
  }
}

sum(!is.na(lbc_data$hlthcat_w2)) # N=807

health_labels <- c("Much worse than one year ago", "Somewhat worse than one year ago", 
                   "About the same as one year ago", "Somewhat better than one year ago", 
                   "Much better now than one year ago")

# Original histogram, but suppress the default x-axis
hist(lbc_data$hlthyrago_w2,
     col = "lightblue3",
     main = "Health Compared to One Year Ago at Wave 2",
     xlab = "Health Category",
     ylab = "Frequency",
     xaxt = "n")  # Turn off default x-axis labels

# Add new x-axis labels at positions 1 through 5
axis(1, at = 1:5, labels = health_labels)

# High blood pressure
table(lbc_data$hibp_w2) # 0 (No)=423; 1 (Yes)=386

## Continuous BP variables
# Sitting - DBP
hist(lbc_data$dbp1sit_w2) # mostly normal
hist(lbc_data$dbp2sit_w2) # mostly normal
hist(lbc_data$dbp3sit_w2) # mostly normal

describe(lbc_data$dbp1sit_w2) # mean=79.47; SD=10.46 (max=130; min=47)
describe(lbc_data$dbp2sit_w2) # mean=77.81; SD=10.19 (max=114; min=48) N for all is 866
describe(lbc_data$dbp3sit_w2) # mean=76.87; SD=10.06 (max=111; min=48)

# Average sitting DBP
library(psych)
lbc_data$dbp_avr_sitt <- rowMeans(lbc_data[, c("dbp1sit_w2", "dbp2sit_w2", "dbp3sit_w2")], na.rm = TRUE)
hist(lbc_data$dbp_avr_sitt) # mostly normal
describe(lbc_data$dbp_avr_sitt) # mean=78.07; SD=9.86 (max=114.3; min=50.33). N=866

# Sitting - SBP 
hist(lbc_data$sbp1sit_w2) # mostly normal
hist(lbc_data$sbp2sit_w2) # mostly normal
hist(lbc_data$sbp3sit_w2) # mostly normal

describe(lbc_data$sbp1sit_w2) # mean=152.29; SD=20.5 (max=240; min=75). N=866
describe(lbc_data$sbp2sit_w2) # mean=148.59; SD=19.58 (max=216; min=87). N=866
describe(lbc_data$sbp3sit_w2) # mean=145.46; SD=18.33 (max=219; min=85). N=863

# Average sitting SBP
lbc_data$sbp_avr_sitt <- rowMeans(lbc_data[, c("sbp1sit_w2", "sbp2sit_w2", "sbp3sit_w2")], na.rm = TRUE)
hist(lbc_data$sbp_avr_sitt) # mostly normal
describe(lbc_data$sbp_avr_sitt) # mean=148.81; SD=18.95 (max=220.3; min=82.33). N=866

# Standing - DBP
hist(lbc_data$dbp1std_w2) # mostly normal
hist(lbc_data$dbp2std_w2) # mostly normal
hist(lbc_data$dbp3std_w2) # mostly normal
 
describe(lbc_data$dbp1std_w2) # mean=81.49; SD=10.43 (max=115; min=50). N=863
describe(lbc_data$dbp2std_w2) # mean=81; SD=10.39 (max=117; min=51). N=861
describe(lbc_data$dbp3std_w2) # mean=80.85; SD=10.17 (max=116; min=53). N=857

# Average standing DBP
lbc_data$dbp_avr_stand <- rowMeans(lbc_data[, c("dbp1std_w2", "dbp2std_w2", "dbp3std_w2")], na.rm = TRUE)
hist(lbc_data$dbp_avr_stand) # mostly normal
describe(lbc_data$dbp_avr_stand) # mean=81.13; SD=9.93 (max=115.67; min=52.67). N=863

# Standing - SBP
hist(lbc_data$sbp1std_w2) # mostly normal
hist(lbc_data$sbp2std_w2) # mostly normal
hist(lbc_data$sbp3std_w2) # mostly normal

describe(lbc_data$sbp1std_w2) # mean=144.94; SD=19.93 (max=231; min=73). N=863
describe(lbc_data$sbp2std_w2) # mean=144.94; SD=19.56 (max=240; min=79). N=861
describe(lbc_data$sbp3std_w2) # mean=143.89; SD=18.65 (max=211; min=87). N=857

# Average standing SBP
library(psych)
lbc_data$sbp_avr_stand <- rowMeans(lbc_data[, c("sbp1std_w2", "sbp2std_w2", "sbp3std_w2")], na.rm = TRUE)
hist(lbc_data$sbp_avr_stand) # mostly normal
describe(lbc_data$sbp_avr_stand) # mean=144.61; SD=18.81 (max=225.33; min=79.67). N=863
View(lbc_data$sbp_avr_stand)

# High cholesterol
table(lbc_data$hichol_w2) # 0 (No)=496; 1 (Yes)=313

# Diagnosis of diabetes
table(lbc_data$diab_w2) # 0 (No)=727; 1 (Yes)=82

# CVD history
table(lbc_data$cvdhist_w2) # 0 (No)=579; 1 (Yes)=230

# Create a frequency table first
cvd_counts <- table(lbc_data$cvdhist_w2)

# Define custom labels
health_labels <- c("No", "Yes")

# Create the bar plot
barplot(cvd_counts,
        names.arg = health_labels,
        col = "orange",
        main = "Cardiovascular Disease History",
        xlab = "CVD Category",
        ylab = "Frequency",
        border = "black",
        space = 0)  # Removes space between bars

-------------------------------------------------------------------------------------------
# NOTE THAT SOME VARIABLES ARE CURRENTLY SUMMARISED BEFORE EXCLUSIONS AND OTHERS - AFTER!
# ALWAYS RE-LOAD CORRECT VERSION OF DATASET AND RE-RUN DESCRIPTIVES IF NEEDED!
-------------------------------------------------------------------------------------------

# History of stroke
table(lbc_data$stroke_w2) # 0 (No)=811; 1 (Yes)=55

# Family history of heart disease, stroke, or problems with blood vessels - Wave 2
table(lbc_data$famhist_w2) # 0 (No)=256; 1 (Yes)=550

# Dementia - Wave 2
table(lbc_data$dement_w2) # 0 (No)=864; 1 (Yes)=2

# Family history of Dementia, Alzheimer's or memory problems - Wave 2
table(lbc_data$famhistdement_w2) # 0 (No)=588; 1 (Yes)=193; 2 (Not asked)=27

# On medication or not
table(lbc_data$onmeds_w2) # 0 (No)=103; 1 (Yes)=706

# Body mass index
hist(lbc_data$bmi_w2) # skewed
describe(lbc_data$bmi_w2) # median=27.3; range=34.34; max=51.01; min=16.67

# Townsend deprivation
library(psych)
describe(lbc_data$adl_w2)

## Oral health
# Number of teeth
hist(lbc_data$numteeth_w2)
describe(lbc_data$numteeth_w2) # n=741; median=19; range=35; min=0; max=35

## Imaging Variables
# Age in days of Wave 2 MRI
hist(lbc_data$ageMRI_w2) # somewhat normal
describe(lbc_data$ageMRI_w2) # mean=26546.84; SD=263.89

# Save data in R
save(lbc_data, file = "C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.RData")
# Save as csv also
write.csv(lbc_data, file = "C:/Users/angel/Desktop/PRECISION MEDICINE/Lothian Birth Cohort Dataset/Dataset and Scripts/lbc_data.csv", row.names = FALSE)



### 19/08/2025: Run additional descriptives on the data after exclusions

hist(lbc_data$visr_u_sqrtz) # transformed already
describe(lbc_data$visr_u_sqrtz)

hist(lbc_data$Recoded_visr_u_w2) # not transformed
describe(lbc_data$Recoded_visr_u_w2)

hist(lbc_data$Recoded_visl_u_w2) # not transformed
describe(lbc_data$Recoded_visl_u_w2)

hist(lbc_data$visl_u_sqrtz) # transformed already
describe(lbc_data$visl_u_sqrtz)

hist(lbc_data$demspn_w2) # not transformed; pretty normal
describe(lbc_data$demspn_w2)

hist(lbc_data$sixmwk_w2) # not transformed; a bit skewed
describe(lbc_data$sixmwk_w2) 

### 16/06/2025: More work on the data

## Imaging Data

# WM damage metric
hist(lbc_data$WMdamage_metric_w2) # highly skewed
describe(lbc_data$WMdamage_metric_w2)

# WMH volume in mm3
hist(lbc_data$wmh_mm3_w2) # also highly skewed
describe(lbc_data$wmh_mm3_w2)

# WMH volume corrected for ICV
hist(lbc_data$wmhIcv_ratio_w2) # also highly skewed
describe(lbc_data$wmhIcv_ratio_w2)

# Transform brain measures due to very high kurtosis
lbc_data$log_WMdamage_metric <- log10(lbc_data$WMdamage_metric_w2 + 1e-6)  # add tiny constant to avoid log(0)
lbc_data$log_WMH_mm3 <- log10(lbc_data$wmh_mm3_w2 + 0.1)
lbc_data$log_WMHICV_ratio <- log10(lbc_data$wmhIcv_ratio_w2 + 1e-6)

# Check if transformation has improved skewness
hist(lbc_data$log_WMdamage_metric) # much improved
hist(lbc_data$log_WMH_mm3) # looks better
hist(lbc_data$log_WMHICV_ratio) # also looks better 

# Check mental health/wellbeing variables + mobility + visual acuity
library(psych)

# Satisfaction With Life

hist(lbc_data$swls1_w2) # In most ways my life is close to ideal
describe(lbc_data$swls1_w2) # N=855; median=6; min=1; max=7; range=6; mean=5.06; SD=1.43

hist(lbc_data$swls2_w2) # The conditions of my life are excellent
describe(lbc_data$swls2_w2) # N=855; median=6; min=1; max=7; range=6; mean=5.09; SD=1.42

hist(lbc_data$swls3_w2) # I am satisfied with my life
describe(lbc_data$swls3_w2) # N=855; median=6; min=1; max=7; range=6; mean=5.58; SD=1.22

hist(lbc_data$swls4_w2) # So far I have gotten the important things I want in life
describe(lbc_data$swls4_w2) # N=855; median=6; min=1; max=7; range=6; mean=5.43; SD=1.29

hist(lbc_data$swls5_w2) # If I could live my life again, I would change almost nothing
describe(lbc_data$swls5_w2) # N=854; median=5; min=1; max=7; range=6; mean=4.38; SD=1.75

# Warwick-Edinburgh Mental Wellbeing scale

hist(lbc_data$wemwbs1_w2) # I've been feeling optimistic about the future
describe(lbc_data$wemwbs1_w2) # N=854; median=3; min=1; max=5; range=4; mean=3.38; SD=0.83

hist(lbc_data$wemwbs2_w2) # I've been feeling useful
describe(lbc_data$wemwbs2_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.64; SD=0.78

hist(lbc_data$wemwbs3_w2) # I've been feeling relaxed
describe(lbc_data$wemwbs3_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.56; SD=0.77

hist(lbc_data$wemwbs4_w2) # I've been feeling interested in other people
describe(lbc_data$wemwbs4_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.8; SD=0.84

hist(lbc_data$wemwbs5_w2) # I've had energy to spare
describe(lbc_data$wemwbs5_w2) # N=854; median=3; min=1; max=5; range=4; mean=3.02; SD=0.84

hist(lbc_data$wemwbs6_w2) # I've been dealing with problems well
describe(lbc_data$wemwbs6_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.68; SD=0.72

hist(lbc_data$wemwbs7_w2) # I've been thinking clearly 
describe(lbc_data$wemwbs7_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.91; SD=0.75

hist(lbc_data$wemwbs8_w2) # I've been feeling good about myself 
describe(lbc_data$wemwbs8_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.55; SD=0.79
 
hist(lbc_data$wemwbs9_w2) # I've been feeling close to other people
describe(lbc_data$wemwbs9_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.68; SD=0.82

hist(lbc_data$wemwbs10_w2) # I've been feeling confident
describe(lbc_data$wemwbs10_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.62; SD=0.8

hist(lbc_data$wemwbs11_w2) # I've been able to make up my own mind about things
describe(lbc_data$wemwbs11_w2) # N=854; median=4; min=1; max=5; range=4; mean=4.09; SD=0.77

hist(lbc_data$wemwbs12_w2) # I've been feeling loved
describe(lbc_data$wemwbs12_w2) # N=854; median=4; min=1; max=5; range=4; mean=4.03; SD=0.91

hist(lbc_data$wemwbs13_w2) # I've been interested in new things
describe(lbc_data$wemwbs13_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.68; SD=0.94

hist(lbc_data$wemwbs14_w2) # I've been feeling cheerful
describe(lbc_data$wemwbs14_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.78; SD=0.72

# Life Orientation Test-Revised
hist(lbc_data$lotr1_w2) # In uncertain times, I usually expect the best
describe(lbc_data$lotr1_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.55; SD=1.06

hist(lbc_data$lotr2_w2) # It's easy for me to relax
describe(lbc_data$lotr2_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.73; SD=1.16

hist(lbc_data$lotr3_w2) # If something can go wrong with me, it will 
describe(lbc_data$lotr3_w2) # N=854; median=2; min=1; max=5; range=4; mean=2.44; SD=1.2

hist(lbc_data$lotr4_w2) # I'm always optimistic about my future
describe(lbc_data$lotr4_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.77; SD=1.02

hist(lbc_data$lotr5_w2) # I enjoy my friends a lot
describe(lbc_data$lotr5_w2) # N=854; median=5; min=1; max=5; range=4; mean=4.53; SD=0.8

hist(lbc_data$lotr6_w2) # It's important for me to keep busy
describe(lbc_data$lotr6_w2) # N=854; median=5; min=1; max=5; range=4; mean=4.22; SD=0.97

hist(lbc_data$lotr7_w2) # I hardly ever expect things to go my way
describe(lbc_data$lotr7_w2) # N=854; median=2; min=1; max=5; range=4; mean=2.23; SD=1.14

hist(lbc_data$lotr8_w2) # I don't get upset too easily
describe(lbc_data$lotr8_w2) # N=854; median=4; min=1; max=5; range=4; mean=3.55; SD=1.2

hist(lbc_data$lotr9_w2) # I rarely count on good things happening to me
describe(lbc_data$lotr9_w2) # N=854; median=2; min=1; max=5; range=4; mean=2.39; SD=1.17

hist(lbc_data$lotr10_w2) # Overall, I expect more good things to happen to me than bad
describe(lbc_data$lotr10_w2) # N=854; median=4; min=1; max=5; range=4; mean=4.1; SD=0.97

# Sense of Coherence 
hist(lbc_data$socunderstand_w2) # Do you usually feel that the things that happen to you in your daily life 
# are hard to understand?
describe(lbc_data$socunderstand_w2) # N=855; median=2; min=0; max=2; range=2; mean=1.65; SD=0.52

hist(lbc_data$sochopeless_w2) # Do you usually see a solution to problems and difficulties 
# that other people find hopeless?
describe(lbc_data$sochopeless_w2) # N=854; median=1; min=0; max=2; range=2; mean=0.79; SD=0.51

hist(lbc_data$socsatisfaction_w2) # Do you usually feel that your daily life is a source 
# of personal satisfaction?
describe(lbc_data$socsatisfaction_w2) # N=855; median=0; min=0; max=2; range=2; mean=0.49; SD=0.62

# Perceived Social Support
hist(lbc_data$socsupporthappy_w2) # There are people I know amongst my family or friends who do 
# things to make me feel happy
describe(lbc_data$socsupporthappy_w2) # N=855; median=3; min=1; max=3; range=2; mean=2.81; SD=0.43

hist(lbc_data$socsupportloved_w2) # There are people I know amongst my family and friends who 
# make me feel loved
describe(lbc_data$socsupportloved_w2) # N=855; median=3; min=1; max=3; range=2; mean=2.83; SD=0.41
     
hist(lbc_data$socsupportrelied_w2) # There are people I know amongst my family and friends who can be 
# relied upon no matter what happens
describe(lbc_data$socsupportrelied_w2) # N=855; median=3; min=1; max=3; range=2; mean=2.87; SD=0.37

hist(lbc_data$socsupportcare_w2) # There are people I know amongst my family and friends who 
# would see that I am taken care of if I needed to be
describe(lbc_data$socsupportcare_w2) # N=855; median=3; min=1; max=3; range=2; mean=2.82; SD=0.43

hist(lbc_data$socsupportaccept_w2) # There are people I know amongst my family and friends 
# who accept me just as I am
describe(lbc_data$socsupportaccept_w2) # N=855; median=3; min=1; max=3; range=2; mean=2.87; SD=0.36

hist(lbc_data$socsupportimpor_w2) # There are people I know amongst my family and friends 
# who make me feel an important part of their lives
describe(lbc_data$socsupportimpor_w2) # N=2.71; median=3; min=1; max=3; range=2; mean=2.71; SD=0.51
 
hist(lbc_data$socsupportencour_w2) # There are people I know amongst my family and friends 
# who give me support and encouragement 
describe(lbc_data$socsupportencour_w2) # N=855; median=3; min=1; max=3; range=2; mean=2.76; SD=0.49

# European Social Survey
# Define Variable names
eur_vars <- c("eurlonely_w2", "eurcapable_w2", "euraccomp_w2", "eurvaluable_w2",
              "eurlearning_w2", "eurnormal_w2", "eurclose_w2", 
              "eurrespect_w2", "eurunfairly_w2")

# Initialize a list to store results
desc_list_eur <- list()

# Loop over each variable and compute descriptive statistics
for (var in eur_vars) {
  stats <- describe(lbc_data[[var]])
  desc_list_eur[[var]] <- stats
}

# Combine into a data frame
eur_desc_table <- do.call(rbind, desc_list_eur)

# Add variable names
eur_desc_table$variable <- rownames(eur_desc_table)
rownames(eur_desc_table) <- NULL

# Reorder columns
eur_desc_table <- eur_desc_table[, c("variable", "n", "mean", "sd", "median", "trimmed", 
                                     "mad", "min", "max", "range", "skew", "kurtosis", "se")]

# Save to CSV
write.csv(eur_desc_table, "eur_descriptive_statistics.csv", row.names = FALSE)

# Experiences & Attitudes to Ageing Questionnaire
# Create list of variable names
eaaq_vars <- paste0("eaaq", 1:24, "_w2")

# Collect descriptive stats into a list
desc_list <- list()

for (var in eaaq_vars) {
  stats <- describe(lbc_data[[var]])
  desc_list[[var]] <- stats
}

# Combine all into a data frame
desc_table <- do.call(rbind, desc_list)

# Add variable names as a new column
desc_table$variable <- rownames(desc_table)
rownames(desc_table) <- NULL

# Reorder columns for clarity
desc_table <- desc_table[, c("variable", "n", "mean", "sd", "median", "trimmed", 
                             "mad", "min", "max", "range", "skew", "kurtosis", "se")]

# Save to CSV
write.csv(desc_table, "eaaq_descriptive_statistics.csv", row.names = FALSE)

# Age in days when completed questionnaire
hist(lbc_data$agedays_eaaq_w2)
describe(lbc_data$agedays_eaaq_w2) # Around 74 years old;
# N=813; median=27023; range=450; mean=27018.97; SD=103.95

# Mobility - Physical Performance Battery

# Define variable list
mobility_vars <- c(
  "sidest_w2", "sidestsec_w2", "semtanst_w2", "semtanstsec_w2", 
  "tanst_w2", "tanstsec_w2", "tanstsecs_w2", 
  "chairst_w2", "chairstsec_w2", "chairstifnosec_w2", "chairstone_w2"
)

# Initialize empty list to store descriptive stats
desc_list_mobility <- list()

# Loop to compute describe() for each variable
for (var in mobility_vars) {
  stats <- describe(lbc_data[[var]])
  desc_list_mobility[[var]] <- stats
}

# Combine into one data frame
mobility_desc_table <- do.call(rbind, desc_list_mobility)

# Add variable names
mobility_desc_table$variable <- rownames(mobility_desc_table)
rownames(mobility_desc_table) <- NULL

# Reorder columns
mobility_desc_table <- mobility_desc_table[, c("variable", "n", "mean", "sd", "median", "trimmed", 
                                               "mad", "min", "max", "range", "skew", "kurtosis", "se")]
# Visual Acuity Data
hist(lbc_data$Recoded_visr_c_w2) # highly skewed
describe(lbc_data$Recoded_visr_c_w2) # N=594; median=0.2; range=1.2; min=-0.1; max=1.1; mean=0.16; SD=0.2

hist(lbc_data$Recoded_visl_c_w2) # highly skewed
describe(lbc_data$Recoded_visl_c_w2) # N=595; median=0.2; range=1.4; min=-0.1; max=1.3; mean=0.14; SD=0.2

