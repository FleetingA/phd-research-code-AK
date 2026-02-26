
# STATA code to rename and transform original UKB data to make compatible with PHESANT 

**Date:** July 2023  
**Author:** Angelina Kancheva  

---
                                
### Get information about all vars and store them in a macro:

ds, has(type numeric)
local varlist `r(varlist)'

### Rename all vars to replace 'n_' in the var header with 'x' (no _ or spaces):

foreach var of varlist n_* {
    local newname = subinstr("`var'", "n_", "x", .)
    rename `var' `newname'
}

### Generate an indexing variable ordering the entire dataset from start to finish (might be redundant later on):

egen index_var = seq(), from(1)
label variable index_var "userId"

### Placing that variable at the beginning of the dataset and dropping the original index_var:

generate long userId = ., before(xeid)
drop index_var
label variable userId "Index Ordering Variable"

### Dropping irrelevant variables (that are not the phenotypes that we have selected for the primary analysis):

drop s_* /* vars starting with 's_' */
drop x_* /* vars starting with 'x_' */

### Saving the cleaned dataset:

save "C:\Users\angel\Desktop\UKB Data July 2023\UKB Data 2023\Angelina_21Jul23.dta", replace

File then manually renamed to "phenofile_UKB.dta".

### Exporting Stata .dta file to .csv:

use "phenofile_UKB.dta"
export delimited using "phenofile_UKB.csv", replace

Take a similar approach to prepare the trait-of-interest and confounder files.

***** Transformations on new/more detailed UKB dataset *****
     **** 23rd January 2024 ****

### Transform variables (to make string values appear as numeric) - example(s): 

capture tostring x34_0_0, replace force
capture tostring x93_2_0, replace force
capture tostring x94_2_0, replace force...

... etc. 

### Dealing with "SKIP Integer type but not numeric" error (in Excel):

Using Find and Replace:

1. Press Ctrl + H to open the "Find and Replace" dialog.
2. In the "Find what" box, you can either leave it blank (to represent empty cells) or type a period "." if missing data is represented this way.
3. In the "Replace with" box, type the numeric value you want to replace the missing value with (e.g., -2).
4. Click "Replace All".

874_2_0 > 433,613 values changed from . to -3 (i.e., 'Prefer not to answer')
1160_2_0 > 432,486 values changed from . to -3 (i.e., 'Prefer not to answer')
2956_2_0 > 501,659 values changed from . to -3 (i.e., 'Prefer not to answer')
3786_2_0 > 494,405 values changed from . to -3 (i.e., 'Prefer not to answer') 
3894_2_0 > 500,787 values changed from . to -3 (i.e., 'Prefer not to answer') 
4012_2_0 > 501,110 values changed from . to -3 (i.e., 'Prefer not to answer') 
4022_2_0 > 501,735 values changed from . to -3 (i.e., 'Prefer not to answer') 
4056_2_0 > 501,275 values changed from . to -3 (i.e., 'Prefer not to answer') 
4067_2_0 > 501,614 values changed from . to -3 (i.e., 'Prefer not to answer') 
4079_2_0 > 446,751 values changed from . to -3 (i.e., 'Prefer not to answer')
4080_2_0 > 446,786 values changed from . to -3 (i.e., 'Prefer not to answer')
4609_2_0 > 467,952 values changed from . to -3 (i.e., 'Prefer not to answer')
4620_2_0 > 467,952 values changed from . to -3 (i.e., 'Prefer not to answer')
6348_2_0 > 449,024 values changed from . to -3 (i.e., 'Prefer not to answer')
6349_2_0 > 449,024 values changed from . to -3 (i.e., 'Prefer not to answer')
6350_2_0 > 449,024 values changed from . to -3 (i.e., 'Prefer not to answer')
6351_2_0 > 449,024 values changed from . to -3 (i.e., 'Prefer not to answer')
6373_2_0 > 449,556 values changed from . to -3 (i.e., 'Prefer not to answer')


20122_0_0 > 500,744 values changed from . to <blank>
20123_0_0 > 492,609 values changed from . to <blank>
20124_0_0 > 487,122 values changed from . to <blank>
20125_0_0 > 492,963 values changed from . to <blank>
20127_0_0 > 100,899 values changed from . to <blank>
20189_0_0 > 500,915 values changed from . to <blank>
20191_0_0 > 378,778 values changed from . to <blank>
20193_0_0 > 501,834 values changed from . to <blank>

Changing from <blank> to -3 as <blank> did not solve the issue for 20122-20193.

More checks on the faulty variables - 25/09/2023:

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

### Adding a few more phenotypes and creating an ICV variable - 15/12/2023:

. sort userId x31_0_0 x2207_2_0 x2227_2_0 x2316_2_0 x2634_2_0 x25004_2_0 x25006_2_0 x25008_2_0

. browse

. tab x2634_2_0

        Duration of heavy DIY |      Freq.     Percent        Cum.
------------------------------+-----------------------------------
         Prefer not to answer |        190        0.58        0.58
                  Do not know |      1,596        4.88        5.47
         Less than 15 minutes |      1,858        5.69       11.15
    Between 15 and 30 minutes |      5,653       17.30       28.45
Between 30 minutes and 1 hour |      8,786       26.88       55.33
      Between 1 and 1.5 hours |      5,788       17.71       73.04
      Between 1.5 and 2 hours |      3,792       11.60       84.65
        Between 2 and 3 hours |      3,004        9.19       93.84
                 Over 3 hours |      2,013        6.16      100.00
------------------------------+-----------------------------------
                        Total |     32,680      100.00

. tab x2207_2_0

    Wears glasses or |
      contact lenses |      Freq.     Percent        Cum.
---------------------+-----------------------------------
Prefer not to answer |         78        0.11        0.11
                  No |      3,967        5.68        5.79
                 Yes |     65,828       94.21      100.00
---------------------+-----------------------------------
               Total |     69,873      100.00

.  tab x2227_2_0

  Other eye problems |      Freq.     Percent        Cum.
---------------------+-----------------------------------
Prefer not to answer |        268        0.38        0.38
                  No |     55,718       79.74       80.13
                 Yes |     13,887       19.87      100.00
---------------------+-----------------------------------
               Total |     69,873      100.00

.  tab x2316_2_0

 Wheeze or whistling |
in the chest in last |
                year |      Freq.     Percent        Cum.
---------------------+-----------------------------------
Prefer not to answer |         51        0.07        0.07
         Do not know |      1,161        1.66        1.73
                  No |     57,449       82.22       83.95
                 Yes |     11,212       16.05      100.00
---------------------+-----------------------------------
               Total |     69,873      100.00


. gen ICV = x25004_2_0 + x25006_2_0 + x25008_2_0
(455,961 missing values generated)

. sum ICV

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
         ICV |     46,398     1194629    115795.3   797554.5    1810530




