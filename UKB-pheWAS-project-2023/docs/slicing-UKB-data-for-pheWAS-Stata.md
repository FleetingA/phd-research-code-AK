#### Slicing UKB Data for UKB PheWAS PhD Project 

### Author: Angelina Kancheva 

### Date: 1st sept 2023 

---

use "PATH_TO_DATA/file.dta"

merge 1:1 n_eid using "PATH_TO_DATA/newfile.dta",

keep if _merge==3

drop _merge


merge 1:1 n_eid using "PATH_TO_DATA/newfile.dta",

keep if _merge==3

drop _merge


destring, replace

compress

save "PATH_TO_OUTPUT/Angelina/01sept23.dta", replace

export delimited using "PATH_TO_OUTPUT/Angelina_01sept23.csv", delimiter(tab) replace

---

#### Slicing the UKB data in Stata:

use n_eid n_<variables_to_be_added_from_UKB_showcase> using PATH_TO_FILE
save PATH_TO_FILE, replace






