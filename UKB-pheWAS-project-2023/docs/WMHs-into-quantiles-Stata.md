**Steps to divide the WMH variable from UKB into quantiles (10 subgroups)**

**Date:** December 2023 

---

#### Load UKB data:

use "PATH_TO_FILE/filename.dta", clear

#### Inspecting and summarizing the WMH variable:


browse x25781_2_0 / # Total volume of white matter hyperintensities (from T1 and T2\_FLAIR images) 

sum x25781_2_0


#### Dividing the WMH variable into ten quantiles and inspecting the results:


xtile dec= x25781_2_0, nq(10)


sort x25781_2_0


sort dec


by dec: summarize x25781_2_0, detail


#### Dichotomizing the WMH variable into 0s and 1s:

browse x25781_2_0dec


. replace  x25781_2_0dec = 0 if x25781_2_0dec == 1

(4,504 real changes made)


. replace x25781_2_0dec = 0 if x25781_2_0dec == 2

(4,503 real changes made)


.  replace x25781_2_0dec = 0 if x25781_2_0dec == 3

(4,501 real changes made)


.  replace x25781_2_0dec = 0 if x25781_2_0dec == 4

(4,505 real changes made)


.  replace x25781_2_0dec = 0 if x25781_2_0dec == 5

(4,499 real changes made)


.  replace x25781_2_0dec = 0 if x25781_2_0dec == 6

(4,501 real changes made)


.  replace x25781_2_0dec = 0 if x25781_2_0dec == 7

(4,500 real changes made)


.  replace x25781_2_0dec = 1 if x25781_2_0dec == 10

(4,501 real changes made)


.  replace x25781_2_0dec = 0 if x25781_2_0dec == 8

(4,499 real changes made)


.  replace x25781_2_0dec = 0 if x25781_2_0dec == 9

(4,500 real changes made)


. tab x25781_2_0dec


#### Exporting dataset into a .csv file:

export delimited using "PATH_TO_FILE/filename.csv", replace


##### OLD WMH VARIABLE - RESULTS FROM JULY 2023

#### Dividing the WMH variable into ten quantiles and inspecting the results:


. xtile UKB_WMHs = x25781_2_0, n(10)


. browse UKB_WMHs


. xtile UKB_WMHs2 = x25781_2_0, nq(10) # this command performs the same transformation as n(10) 


. browse UKB_WMHs2


. sort UKB_WMHs UKB_WMHs2


. sort x25781_2_0


. browse x25781_2_0 UKB_WMHs UKB_WMHs2


. sum UKB_WMHs2


#### Interrogating each quantile category separately:

. sort UKB_WMHs2

. by UKB_WMHs2: summarize x25781_2_0, detail


-> UKB_WMHs2 = 1
-> UKB\_WMHs2 = 2
-> UKB\_WMHs2 = 3

... etc.


. rename UKB_WMHs2 x25781_new




