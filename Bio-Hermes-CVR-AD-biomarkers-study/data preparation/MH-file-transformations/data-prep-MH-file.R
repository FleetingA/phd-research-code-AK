
----------------------------------------------------------------------------------
# PhD Project 2024: Bio-Hermes Cardiovascular Risk + Plasma AD Biomarkers Project
  
# Author: Angelina Kancheva 
# Date: 16/10/2024
----------------------------------------------------------------------------------
  
### Data Preparation Steps
## MH stands for 'medical history'
  
# Load libraries
library(readr)
library(dplyr)

# Load data
MH <- read_csv("Bio_Hermes_Data/Bio_Hermes_Clinical_Data/MH.csv")
View(MH)

# Check all disease entries
unique_entries <- unique(MH$MHTERM)
options(max.print = 10000)
# Show the unique entries
print(unique_entries, max.print=TRUE)

# Select only columns needed for analysis
MH <- MH %>% select(USUBJID, MHTERM, MHENRTPT, MHENRTPT_num, Diabetes_status)
View(MH)

# Recode values in the MHENRTPT column
table(MH$MHENRTPT)
# Recode the MHENRTPT variable
MH$MHENRTPT_num <- ifelse(is.na(MH$MHENRTPT), 0, ifelse(MH$MHENRTPT == "ONGOING", 1, MH$MHENRTPT))

# Select just columns that start with a 'd'
d_entries <- grep("^d", MH$MHTERM, value = TRUE, ignore.case = TRUE)

# Display the entries that start with 'd'
print(d_entries)

# List of diabetes-related terms
diabetes_terms <- c(
  "Diabetes",                                            
  "Diabetes (Type 2)",                                  
  "DIABETES Type 2",                                     
  "Diabetes Mellitus - type 2",                         
  "Diabetic retinopathy",                                 
  "Diabetes Type 2",                                   
  "Diabetes Type II",                                   
  "Diabetes Type II",                                   
  "Diabetic Nephropathy",                                
  "Diabetes Type II",                                   
  "Diabetes Mellitus Type II",                         
  "Diabetes Mellitus Type 2",                           
  "Diabetic Neuropathy",                                
  "Diabetes Mellitus, Type 2",                         
  "Diabetes Mellitus Type 2",                           
  "Diabetic Ketoacidosis",                            
  "Diabetic Neuropathy",                                
  "Diabetes Mellitus Type II",                           
  "Diabetes Type II",                                   
  "Diabetes",                                           
  "Diabetes - type II",                                  
  "Diabetes II",                                         
  "Diabetes Type 2",                                    
  "Diabetes Type II",                                   
  "Diabetes II",                                        
  "Diabetes Type II",                                  
  "Diabetic Neuropathy",                               
  "Diabetic Retinopathy",                               
  "Diabetes Type II",                                   
  "Diabetes I",                                        
  "Diabetes mellitus II",                             
  "Diabetes Mellitus II",                             
  "Diabetes Mellitus II",                               
  "Diabetes mellitus II",                               
  "Diabetes Mellitus II",                               
  "diabetes mellitus II",                              
  "Diabetes Mellitus II",                               
  "Diabetes Mellitus II",                               
  "Diabetes Mellitus II",                               
  "Diabetes Mellitus Type II",                          
  "Diabetes Mellitus II",                               
  "Diabetes Type II",                                    
  "Diabetes Mellitus II",                               
  "Diabetes Mellitus I",                                
  "Diabetes Mellitus II",                               
  "Diabetes Mellitus",                                  
  "Diabetes Mellitus",                                 
  "Diabetes Mellitus, Type 2",                          
  "Diabetes Mellitus",                                  
  "Diabetes Mellitus",                                  
  "Diabetes Mellitus Type 2",                           
  "Diabetes Mellitus",                                  
  "Diabetes Mellitus, Type 2",                          
  "diabetes mellitus",                                  
  "Diabetes Type 2",                                    
  "Diabetes Mellitus, Type 2",                          
  "Diabetes type 2",                                    
  "Diabetic neuropathy",                                 
  "Diabetes type 2",                                   
  "Diabetes type 2",                                    
  "Diabetes type 2",                                     
  "Diabetes Type 2",                                    
  "Diabetes type 2",                                    
  "Diabetes Type 2",                                   
  "Diabetes",                                           
  "Diabetes Type II",                                   
  "Diabetes",                                           
  "Diabetes",                                           
  "Diabetes type 2",                                   
  "Diabetes Type 2",                                    
  "Diabetes Type 2",                                    
  "DIABETES",                                           
  "Diabetes Type 2",                                     
  "Diabetes Type 2",                                    
  "Diabetes",                                           
  "Diabetes Mellitus Type 2",                           
  "Diabetes type 2",                                    
  "diabetes type 2",                                     
  "diabetes type 2",                                    
  "diabetes mellitus type 2",                           
  "diabetes type 2",                                    
  "diabetic neuropathy",                                
  "diabetes type II",                                   
  "diabetes type 2",                                    
  "diabetes type 2",                                    
  "diabetes",                                            
  "diabetes type 2",                                    
  "Diabetes Type 2",                                    
  "Diabetes Mellitus",                                  
  "Diabetes Mellitus",                                  
  "Diabetes Mellitus",                                  
  "Diabetes Mellitus",                                  
  "Diabetes type II",                                   
  "Diabetes type II",                                   
  "Diabetes type II",                                   
  "Diabetes Type 2",                                    
  "Diabetes Type 1",                                    
  "Diabetes",                                           
  "Diabetes",                                           
  "Diabetes"
)

View(MH)

# Save new diabetes file 
write.csv(MH, "MH_diabetes_status_added.csv", row.names = FALSE)

# Check unique disease entries in MH file
unique_MHTERM <- unique(MH$MHTERM)
options(max.print = 5000)
print(unique_MHTERM)

# Select just columns that start with 'a' for ASCVD
a_entries <- grep("^a", MH$MHTERM, value = TRUE, ignore.case = TRUE)

# Display the entries that start with 'a'
print(a_entries)

# Select just columns that start with 's' for stroke
s_entries <- grep("^str", MH$MHTERM, value = TRUE, ignore.case = TRUE)

# Display the entries that start with 's'
print(s_entries)

# Select just columns that start with 'cor' for CHD
chd_entries <- grep("^cor", MH$MHTERM, value = TRUE, ignore.case = TRUE)

# Display the entries that start with 'chd'
print(chd_entries)

## ---Exclude participants whose current MH indicates ongoing atherosclerosis (only exclusion to be applied!)---

# Define the vector of exclusion conditions
excl_conditions <- c(
  "atherosclerosis of coronary artery",
  "Atherosclerosis of aorta",
  "Atherosclerosis of Aorta",
  "Atherosclerosis of coronary artery without angina pectoris",
  "Atherosclerosis of aorta",
  "Atherosclerosis of aorta",
  "Atherosclerotic Heart Disease of Native coronary artery",
  "Aorta Atherosclerosis",
  "arteriosclerosis of aorta"
)

# Filter the dataset to exclude participants with the specified conditions
MH_noAS <- MH %>%
  filter(!(MHTERM %in% excl_conditions & MHENRTPT_num == 1))

# View the filtered dataset
View(MH_noAS)
table(MH_noAS$Diabetes_status)

---------------------------------------------------
### 12/12/2024: More data cleaning and preparation

# Exclude rows where Diabetes_status is NA
MH_noAS_nomiss <- MH_noAS %>%
  filter(!is.na(Diabetes_status))
View(MH_noAS_nomiss)
table(MH_noAS_nomiss$Diabetes_status)

# Keep a single row of diabetes status
MH_noAS_nomiss <- MH_noAS_nomiss %>%
  group_by(USUBJID) %>%
  # For each participant, keep one row with 1 if any, otherwise keep one row with 0
  summarize(
    USUBJID = first(USUBJID),
    MHTERM = first(MHTERM),
    MHENRTPT = first(MHENRTPT),
    MHENRTPT_num = first(MHENRTPT_num),
    Diabetes_status = max(Diabetes_status)
  ) %>%
  ungroup()

# View the new data frame
View(MH_noAS_nomiss)

# Save current work
write.csv(MH_noAS, "MH_noAS.csv", row.names = FALSE)
write.csv(MH_noAS_nomiss, "MH_noAS_nomiss_longformat.csv", row.names = FALSE)

# Further trim data frame contents
MH_noAS_nomiss_longformat <- MH_noAS_nomiss_longformat %>% select(USUBJID, MHTERM, MHENRTPT, MHENRTPT_num, Diabetes_status_new)
View(MH_noAS_nomiss_longformat)
table(MH_noAS_nomiss_longformat$Diabetes_status_new)

# Save file
write.csv(MH_noAS_nomiss_longformat, "MH_final_131224.csv", row.names = FALSE)

----------------------------------------
### 07/03/2025: CV Risk weighting

MH_noAS_nomiss_longformat <- read_csv("Angelina_Bio_Hermes_R_code/MH_file_Transformations/MH_noAS_nomiss_longformat.csv")
View(MH_noAS_nomiss_longformat)

unique(MH_noAS_nomiss_longformat$MHTERM)
options(max.print = 5000)  # Increase to a high enough value
print(unique(MH_noAS_nomiss_longformat$MHTERM))

# Store all unique diagnoses from MHTERM into a vector
diagnosis_vector <- unique(MH_noAS_nomiss_longformat$MHTERM)
options(max.print = 999999)  # Increase print limit
print(diagnosis_vector)
print(diagnosis_vector[1162:1164])

# Replace variations of 'Angina' with 'Angina' in the MHTERM column
MH_noAS_nomiss_longformat$MHTERM <- ifelse(
  MH_noAS_nomiss_longformat$MHTERM %in% c("Chronic stable angina", "Angina Pectoris", "Stable angina pectoris", "Angina"),
  "Angina",
  MH_noAS_nomiss_longformat$MHTERM
)

# Check that all angina occurrences have the same name now
unique(MH_noAS_nomiss_longformat$MHTERM[grepl("angin", MH_noAS_nomiss_longformat$MHTERM, ignore.case = TRUE)])

# Replace variations of 'Myocardial Infarction' with 'MI' in the MHTERM column
MH_noAS_nomiss_longformat$MHTERM <- ifelse(
  MH_noAS_nomiss_longformat$MHTERM %in% c("Myocardial Infarction", "myocardial infarction", "MI"),
  "MI",
  MH_noAS_nomiss_longformat$MHTERM
)

# Check that all MI occurrences have the same name now
unique(MH_noAS_nomiss_longformat$MHTERM[grepl("myo", MH_noAS_nomiss_longformat$MHTERM, ignore.case = TRUE)])

# Replace variations of 'Stroke/TIA' with 'Stroke' in the MHTERM column
MH_noAS_nomiss_longformat$MHTERM <- ifelse(
  MH_noAS_nomiss_longformat$MHTERM %in% c("Stroke/TIA", "Stroke", "Embolic Stroke", 
                                          "Sequela of Thrombotic Stroke", "TIA"),
  "Stroke",
  MH_noAS_nomiss_longformat$MHTERM
)

# Check that all stroke occurrences have the same name now
unique(MH_noAS_nomiss_longformat$MHTERM[grepl("stroke", MH_noAS_nomiss_longformat$MHTERM, ignore.case = TRUE)])

# Replace variations of 'Heart Disease/Heart failure' with 'Heart Disease' in the MHTERM column
MH_noAS_nomiss_longformat$MHTERM <- ifelse(
  MH_noAS_nomiss_longformat$MHTERM %in% c("Atherosclerotic Heart Disease", "Rheumatic heart disease (valve disorder)", 
   "Hypertensive heart disease", "Coronary Heart Disease", "Heart Disease", 
   "Heart disease", "Congestive Heart Disease", "Diastolic Congestive Heart Failure", 
   "HIstory of congestive heart failure", "Congestive Heart Failure", 
   "CONGESTIVE HEART FAILURE", "Hypertensive heart and renal disease", 
   "Congestive heart failure", "congestive heart failure", "Chronic Diastolic Heart Failure", 
   "Cardiovascular heart failure", "Chronic disease heart failure", 
   "chronic diastolic heart failure" , "Heart disease", "heart disease", 
   "Diastolic Congestive Heart Failure", "Congestive Heart Disease", "Coronary Disease",
   "Coronary artery Disease", "Single coronary vessel disease", "Coronary Artery DIsease",
   "Coronary Arteriosclerosis", "Coronary Atherosclerosis due to calcified coronary lesion",
   "Coronary Atherosclerosis", "Coronary arteriosclerosis", "coronary artery disease",
   "Coronary artery disease", "Coronary atherosclerosis", "Coronary Artery Disease",
   "Multiple Coronary Artery Disease Factors", "CORONARY ARTERY DISEASE", "Coronary Heart Disease",
   "Coronary Artery Bypass Surgery", "coronary disease","coronary artery desease",
   "Coronary Disease", "Coronary artery Disease", "Single coronary vessel disease",
   "Presence of coronary angioplasty implant and graft",
   "Coronary Arteriosclerosis", "Cornonary Artery Atherosclerosis", "Heart Attacks",
   "Heart valve insufficiency", "heart Stent"),
  "HeartDisease",
  MH_noAS_nomiss_longformat$MHTERM
)

# Check that all heart disease occurrences have the same name now
unique(MH_noAS_nomiss_longformat$MHTERM[grepl("heart", MH_noAS_nomiss_longformat$MHTERM, ignore.case = TRUE)])

# Replace variations of 'Peripheral artery/vascular disease' with 'Peripheral Disease' in the MHTERM column
MH_noAS_nomiss_longformat$MHTERM <- ifelse(
  MH_noAS_nomiss_longformat$MHTERM %in% c("peripheral artery occlusion disease", "Peripheral artery disease", 
"Peripheral Artery Disease", "Peripheral Vascular Disease", 
"Peripheral Vascular Disease (Managed)", "Peripheral vascular disease", 
"peripheral artery occlusion disease"),
"PeripheralDisease",
MH_noAS_nomiss_longformat$MHTERM
)

# Check that all peripheral disease occurrences have the same name now
unique(MH_noAS_nomiss_longformat$MHTERM[grepl("periph", MH_noAS_nomiss_longformat$MHTERM, ignore.case = TRUE)])

# Replace variations of 'CV disease (any)' with 'CVD' in the MHTERM column
MH_noAS_nomiss_longformat$MHTERM <- ifelse(
  MH_noAS_nomiss_longformat$MHTERM %in% c("Cardiovascular heart failure", "Cardiovascular disease", "Cardiovascular Disease"),
  "CVD",
  MH_noAS_nomiss_longformat$MHTERM
)

# Check that all CVD occurrences have the same name now
unique(MH_noAS_nomiss_longformat$MHTERM[grepl("cvd", MH_noAS_nomiss_longformat$MHTERM, ignore.case = TRUE)])

# Replace variations of any of the CVD diseases with 'prevalentcvdany' in the MHTERM column
MH_noAS_nomiss_longformat$MHTERM <- ifelse(
  MH_noAS_nomiss_longformat$MHTERM %in% c("Angina", "Stroke", "HeartDisease",
                                          "PeripheralDisease", "CVD", "CVDprevalent"),
  "PrevalentCVDany",
  MH_noAS_nomiss_longformat$MHTERM
)

# Check that all CVD occurrences have the same name now
unique(MH_noAS_nomiss_longformat$MHTERM[grepl("cvd", MH_noAS_nomiss_longformat$MHTERM, ignore.case = TRUE)])


### Create weighting for people with prevalent CV Risk ###

# Create a new weighting variable
MH_noAS_nomiss_longformat$weight <- ifelse(MH_noAS_nomiss_longformat$MHTERM == "PrevalentCVDany", 1, 0)
table(MH_noAS_nomiss_longformat$weight)

CVD_weighting <- MH_noAS_nomiss_longformat %>% select(USUBJID, MHTERM, weight)
View(CVD_weighting)

# Extract USUBJIDs where weight = 1
participants_with_CVD <- MH_noAS_nomiss_longformat$USUBJID[MH_noAS_nomiss_longformat$weight == 1]

# Print or inspect the vector
print(participants_with_CVD)
