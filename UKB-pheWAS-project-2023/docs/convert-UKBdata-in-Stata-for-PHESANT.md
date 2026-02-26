
## STATA code to rename and transform original UKB data to make compatible with PHESANT 

**Date:** July 2023  
**Author:** Angelina Kancheva  

---
                                
#### Get information about all vars and store them in a macro:

ds, has(type numeric)
local varlist `r(varlist)'

#### Rename all vars to replace 'n_' in the var header with 'x' (no _ or spaces):

foreach var of varlist n_* {
    local newname = subinstr("`var'", "n_", "x", .)
    rename `var' `newname'
}

#### Generate an indexing variable ordering the entire dataset from start to finish (might be redundant later on):

egen index_var = seq(), from(1)
label variable index_var "userId"

#### Placing that variable at the beginning of the dataset and dropping the original index_var:

generate long userId = ., before(xeid)
drop index_var
label variable userId "Index Ordering Variable"

#### Dropping irrelevant variables (that are not the phenotypes that we have selected for the primary analysis):

drop s_* /* vars starting with 's_' */
drop x_* /* vars starting with 'x_' */

#### Saving the cleaned dataset:

save "C:\Users\angel\Desktop\UKB Data July 2023\UKB Data 2023\Angelina_21Jul23.dta", replace

File then manually renamed to "phenofile_UKB.dta".

#### Exporting Stata .dta file to .csv:

use "phenofile_UKB.dta"
export delimited using "phenofile_UKB.csv", replace

Take a similar approach to prepare the trait-of-interest and confounder files.

---

### Transformations on new/more detailed UKB dataset *****
**Date:** 23rd January 2024

---
     
#### Transform variables (to make string values appear as numeric) - example(s): 

capture tostring x34_0_0, replace force
capture tostring x93_2_0, replace force
capture tostring x94_2_0, replace force...

... etc. 

#### Dealing with "SKIP Integer type but not numeric" error (in Excel):

Using Find and Replace:

1. Press Ctrl + H to open the "Find and Replace" dialog.
2. In the "Find what" box, you can either leave it blank (to represent empty cells) or type a period "." if missing data is represented this way.
3. In the "Replace with" box, type the numeric value you want to replace the missing value with (e.g., -2).
4. Click "Replace All".

Changing from <blank> to -3 as <blank> did not solve the issue for 20122-20193.


## 15/12/2023: Update

#### Adding a few more phenotypes and creating an ICV variable - 15/12/2023:

. sort userId x31_0_0 x2207_2_0 x2227_2_0 x2316_2_0 x2634_2_0 x25004_2_0 x25006_2_0 x25008_2_0

. browse

. tab <var of interest 1>

. tab <var of interest 2>

etc...

. gen ICV = x25004_2_0 + x25006_2_0 + x25008_2_0







