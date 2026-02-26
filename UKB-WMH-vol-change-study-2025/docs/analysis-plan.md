### Analysis Plan for Second UKB Study - WMH Volume Change + Clinical Phenotypes:
### Author: Angelina Kancheva 

### Period: 10/07/2024-26/03/2025 
---

#### Steps:

- Take the repeat imaging data, compute change scores from the first imaging assessment, and examine if phenotypes measured at first imaging assessment predict WMH change;

- Use phenotypes measured in 2014 as outcomes and WMH change as the predictor variable;

- How to calculate WMH change? 

- Evaluate the difference between baseline and follow-up images --> the longitudinal WMH volume change would be the difference in z-score between baseline and follow-up images.


#### Slicing the data:


use n_eid <add other UKB vars of interest> using "PATH_TO_FILE"


#### Export newly prepared file as csv:


export delimited using "PATH_TO_FILE/transformed-data.csv"

#### Convert all string instances to their numeric counterparts as per UKB Data fields.


#### Variable transformations:


* Diabetes diagnosed by a doctor (confounder):
* 
Converted -3s (Prefer not to answer) and -1s (Do not know) to 0s (No).

* Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones:

Converted -7s (None of the above) to 0s. Converted -3s and -1s to \[blank]. 

* Similar for the other medication variable excluding hormonal therapy:

Converted -7s (None of the above) to 0s. Converted -3s and -1s to \[blank]. 

* Fluid intelligence/Reasoning 4: positional arithmetic:

-3s and -1s set to \[blank].

* Eye problems/disorders:

All -7s recoded to 0, and -1s and -3s left \[blank].

* Duration to complete numeric path (trail #1):

Recoded 'trail not completed' to 0 for all such instances. 

* FI10 : arithmetic sequence recognition:

Variable recoded according to:

Coding	Meaning

1	96

2	95

3	94

4	93

5	92

-121	Do not know

-818	Prefer not to answer

#### More variable transformations:

--> Covariates file: Made variables 6153 and 6177 binary --> only BP took a value of 1, all else = 0. 


