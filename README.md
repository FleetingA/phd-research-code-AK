# PhD Research Code (2022–2026)

This repository contains scripts and documentation used for the analyses included in my PhD thesis (2022–2026).

## Included Projects

1. **UK Biobank PheWAS (PHESANT-based)**  
   A phenome-wide association study exploring clinical phenotypes associated with white matter hyperintensities in 45,013 UKB participants.

2. **UK Biobank WMH Volume Change Study**  
   A study investigating baseline phenotypes associated with white matter hyperintensity volume change in 4329 UKB participants.

3. **Lothian Birth Cohort 1936 Study**  
   An analysis examining associations between white matter hyperintensity volume and clinical phenotypes in the LBC1936 using a structural equation modelling framework.

4. **Bio-Hermes Project**  
   A study exploring cardiovascular risk as a potential moderator of the relationship between Alzheimer’s disease-associated blood plasma biomarkers and cognitive status.

Separate README.md have been provided within individual folders.

## Requirements
#### R Version

This code was developed and run using R versions 4.2.x – 4.3.x
(Tested on R 4.3.1; earlier 4.x versions should also work.)

It is recommended to use a recent R 4.x release.

## Core Packages

The following R packages are required for most analyses in this repository:

- tidyverse (data wrangling and visualisation)

- data.table (efficient data manipulation)

- haven (importing SPSS/Stata files)

- readr (data import)

- ggplot2 (visualisation)

- dplyr (data manipulation)

- tidyr (data reshaping)

## Statistical/Modelling Packages

Some projects additionally require:

- lavaan (structural equation modelling)

- car (regression diagnostics)

- psych (psychometrics and descriptive statistics)

## Installing Required Packages

You can install the main packages using:

```r
install.packages(c(
  "tidyverse", "data.table", "haven", "readr",
  "lavaan", "lme4", "nlme", "survival",
  "glmnet", "car", "psych", "here",
  "janitor", "broom", "knitr", "rmarkdown"
))
```

## Data Availability

No raw data are shared in this repository.  
UK Biobank, Lothian Birth Cohort, and Bio-Hermes data are subject to access and governance restrictions.

## Reproducibility

Where possible, scripts are structured to allow reproducibility given appropriate data access.  
Users will need to adapt file paths and local directory structures to match their own computing environments and project needs.

