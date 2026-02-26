
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

### 25/09/2023: More checks on the faulty variables

4067_2|| CAT-SINGLE || Inc(>=10): 0(228) || Inc(>=10): 1(246) || CAT-SINGLE-BINARY || BINARY-LOGISTIC-SKIP-500 (474) || >  Facial pains for 3+ months >> After excluding variables with <500 participants, only one value remains (-3/-1), hence skipped.

20123_0|| CAT-SINGLE || reassignments: 9=NA || Inc(>=10): 1(944) || SKIP (only one value) || 
20124_0|| CAT-SINGLE || reassignments: 9=NA || Inc(>=10): 1(1461) || SKIP (only one value) || 
20125_0|| CAT-SINGLE || reassignments: 9=NA || Inc(>=10): 1(743) || SKIP (only one value) ||

. tab x20123_0_0, mis

     Single |
 episode of |
   probable |
      major |
 depression |      Freq.     Percent        Cum.
------------+-----------------------------------
          . |    492,609       98.06       98.06
          1 |      9,750        1.94      100.00
------------+-----------------------------------
      Total |    502,359      100.00

. tab x20124_0_0, mis

   Probable |
  recurrent |
      major |
 depression |
 (moderate) |      Freq.     Percent        Cum.
------------+-----------------------------------
          . |    487,122       96.97       96.97
          1 |     15,237        3.03      100.00
------------+-----------------------------------
      Total |    502,359      100.00

. tab x20125_0_0, mis

   Probable |
  recurrent |
      major |
 depression |
   (severe) |      Freq.     Percent        Cum.
------------+-----------------------------------
          . |    492,963       98.13       98.13
          1 |      9,396        1.87      100.00
------------+-----------------------------------
      Total |    502,359      100.00

20189_0|| CAT-SINGLE || Inc(>=10): 5(97) || Inc(>=10): 3(41) || Inc(>=10): 1(62) || Inc(>=10): 4(23) || Inc(>=10): 2(67) || CAT-SINGLE-UNORDERED || CATUNORD-SKIP-500 (290) || 
20193_0|| CAT-SINGLE || Inc(>=10): 4(13) || Inc(>=10): 3(33) || Inc(>=10): 5(19) || Inc(>=10): 2(13) || Inc(>=10): 1(15) || CAT-SINGLE-UNORDERED || CATUNORD-SKIP-500 (93) || 

. tab x20189_0_0, mis

     FI13 : |
     subset |
  inclusion |
      logic |      Freq.     Percent        Cum.
------------+-----------------------------------
          . |    500,915       99.71       99.71
          1 |        291        0.06       99.77
          2 |        358        0.07       99.84
          3 |        203        0.04       99.88
          4 |        138        0.03       99.91
          5 |        454        0.09      100.00
------------+-----------------------------------
      Total |    502,359      100.00

. tab x20193_0_0, mis

     FI14 : |
alphanumeri |
          c |
substitutio |
          n |      Freq.     Percent        Cum.
------------+-----------------------------------
          . |    501,834       99.90       99.90
          1 |         83        0.02       99.91
          2 |         79        0.02       99.93
          3 |        175        0.03       99.96
          4 |         80        0.02       99.98
          5 |        108        0.02      100.00
------------+-----------------------------------
      Total |    502,359      100.00

22150_0|| INTEGER || CONTINUOUS || IRNT || CONTINUOUS-SKIP-500 (228) || > Age COPD diagnosed by doctor >> 99.65% missing values, hence skipped 

---

## 15/12/2023: Update

#### Adding a few more phenotypes and creating an ICV variable - 15/12/2023:

. sort userId x31_0_0 x2207_2_0 x2227_2_0 x2316_2_0 x2634_2_0 x25004_2_0 x25006_2_0 x25008_2_0

. browse

. tab <var of interest 1>

. tab <var of interest 2>

etc...

. gen ICV = x25004_2_0 + x25006_2_0 + x25008_2_0






