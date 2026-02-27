**Bio-Hermes Study Analysis Repository** 
---

This folder contains scripts and supporting documentation for analyses conducted as part of the Bio-Hermes study, which investigated cardiovascular risk as a potential moderator of the association between Alzheimer's Disease (AD)–related blood plasma biomarkers and cognitive status.

#### Project Overview

The primary objective of this analysis was to examine whether cardiovascular risk modifies the relationship between AD plasma biomarkers and cognitive status.

Biomarker data were provided by multiple collaborating companies and research entities. Company names are retained within scripts and variable naming conventions to maintain traceability to source datasets.

#### Contents

- Data processing scripts

- Statistical modelling scripts (e.g., multinomial regression analyses)

- Software Requirements

Analyses were conducted in R (several versions, as required).

Key packages include:

- dplyr
- psych
- ggplot2
- RiskScorescvd
- car
- pscl
- DescTools
- sjstats
- pROC
- caret
- lmtest
- lmtest

#### Data Availability

Raw biomarker data are not included in this repository due to data sharing restrictions. Scripts assume access to the Bio-Hermes dataset.

Variable names reflect the original (“native”) naming conventions used in the source datasets. Users adapting these scripts to other datasets will need to modify variable names accordingly.




