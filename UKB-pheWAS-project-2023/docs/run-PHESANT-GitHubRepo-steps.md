## Steps to prepare my device for running PheWas with PHESANT

**July 2023**

**Author:** Angelina Kancheva

---

Since I am a Windows user:


1. Install Git Bash (to be able to run Linux-like commands).
2. Install wget (to be able to download files from the internet, such as UKB's data dictionary).

On this device, PHESANT-master is located in C:\\Users\\angel\\Documents\\PHESANT-master\\PHESANT-master.

UKB Data Dictionary Steps that PHESANT carries out under the hood
---

1. To obtain the latest version of the UKB Data Dictionary file: wget http://biobank.ctsu.ox.ac.uk/%7Ebbdatan/Data\_Dictionary\_Showcase.csv.
2. To convert the resulting data showcase file from csv to tsv: cat Data\_Dictionary\_Showcase.csv | sed 's/,/\\t/g' | tr -d '\\r' > Data\_Dictionary\_Showcase.tsv.
3. The two file versions are stored (e.g., in C:\\Users\\angel\\Documents\\PHESANT-master\\PHESANT-master\\variable-info\\update-outcome-info).
4. The command "sh addNewFields.sh" updates the "outcome-info-new" file by adding any new fields available with the latest UKB showcase release.
5. The two data showcase files are removed:
   rm Data\_Dictionary\_Showcase20200806.csv
   rm Data\_Dictionary\_Showcase20200806.tsv
6. Script to run PheWas with current folder structure in PHESANT-master-fork2 (this is the only script that needs to be modified code-wise; the rest is being executed in the background):


**Part 1:**

cd ../WAS/

Rscript phenomeScan.r   
--phenofile="../MyWas/data200923/phenotypes\_1512.csv"   
--traitofinterestfile="../MyWAS/data200923/traitofinterest.csv"   
--confounderfile="../MyWas/data200923/confounderfile\_plus\_icv.csv"   
--variablelistfile="../variable-info/outcome-info.tsv"   
--datacodingfile="../variable-info/data-coding-ordinal-info.txt"   
--traitofinterest="x25781\_2\_0"   
--resDir="../MyWas/results200923/"   
--userId="userId"   
--genetic FALSE


Updated as of 23/01/2024 with renewed dataset:

Rscript phenomeScan.r   
--phenofile="../MyWas/data\_ICV/phenotypes.csv"   
--traitofinterestfile="../MyWAS/data\_ICV/traitofinterest\_ICV\_nonmissingonly.csv"   
--confounderfile="../MyWas/data\_ICV/confounders\_ICV\_nonmissingonly.csv"   
--variablelistfile="../variable-info/outcome-info.tsv"   
--datacodingfile="../variable-info/data-coding-ordinal-info.txt"   
--traitofinterest="x25781\_2\_0"   
--resDir="../MyWas/resultsICVtofintnonmissing/"   
--userId="userId"   
--genetic FALSE


Updated as of 23/02/2025 with WMH volume change scores:

Rscript phenomeScan.r   
--phenofile="../MyWas/data16042025/phenotypes\_260325.csv"   
--traitofinterestfile="../MyWAS/data16042025/RAW\_SCORES\_tofi.csv"   
--confounderfile="../MyWas/data16042025/RAW\_SCORES\_confounderfile\_fully\_adjusted.csv"   
--variablelistfile="../variable-info/outcome-info.tsv"   
--datacodingfile="../variable-info/data-coding-ordinal-infoNEW.txt"   
--traitofinterest="x25781\_difference"   
--resDir="../MyWas/results\_RAW\_SCORES\_fully\_adj\_250425/"   
--userId="userId"   
--genetic=FALSE


### Second UKB Study: WMH Volume Change + Clinical Phenotyping Project - Different Analysis Approach:
---

Updated as of 02/09/2025 with the SAVE option:

Rscript phenomeScan.r   
--phenofile="../MyWas/data02092025/phenotypes\_260325.csv"   
--confounderfile="../MyWas/data02092025/confounderfile\_partial.csv"   
--variablelistfile="../variable-info/outcome-info.tsv"   
--datacodingfile="../variable-info/data-coding-ordinal-info.txt"   
--resDir="../MyWas/results02092025/"   
--userId="userId"   
--genetic=FALSE   
--save

phenotypes in total: 230


DIAGNOSTICS:

Rscript phenomeScan.r   
--phenofile="../MyWas/data/phenotypes.csv"   
--confounderfile="../MyWas/data/confounder\_file.csv"   
--variablelistfile="../variable-info/outcome-info.tsv"   
--datacodingfile="../variable-info/data-coding-ordinal-infonew.txt"   
--traitofinterest="x100\_2\_0"   
--resDir="../MyWas/results/"   
--userId="userId"   
--genetic FALSE

TEST:

testDir="../testWAS/"

Rscript phenomeScan.r   
--phenofile="../testWAS/data/phenotypes.csv"   
--confounderfile="../testWAS/data/exposure.csv"   
--variablelistfile="../testWAS/variable-lists/outcome-info.tsv"   
--datacodingfile="../testWAS/variable-lists/data-coding-ordinal-info.txt"   
--traitofinterest="exposure"   
--resDir="../testWAS/results/"   
--userId="userId" \\


Part 2:

cd ../resultsProcessing/

Rscript mainCombineResults.r   
--resDir="../MyWas/results\_RAW\_SCORES\_fully\_adj\_250425/"   
--variablelistfile="../variable-info/outcome-info.tsv"

Part 3:

cd PHESANT-viz/bin
java -cp ".;C:/Users/angel/Desktop/TEST-280723-PHESANT-master/PHESANT-viz/jar/json-simple-1.1 2.jar" ResultsToJSON "C:/Users/angel/Desktop/TEST-280723-PHESANT-master/MyWas/results/results-combined.txt" "C:/Users/angel/Desktop/TEST-280723-PHESANT-master/PHESANT-viz/node-positions.csv" "C:/Users/angel/Desktop/TEST-280723-PHESANT-master/PHESANT-viz/java-json.json"

cd ..
cd web
python -m http.server
localhost:8000


Note that some fields are defined as related fields in the data-coding-ordinal-info file, which means they are made use of when processing other variables. If there are variables in the phenotype dataset (i.e., phenofile) with a data code that has a specific field as a required field, but these are not found, the code will throw an error. Example:

[1] "Required variable: Field  x20082\_0\_0 is a data code related field (default\_related\_field column in data code information file) but was not found in phenotype data"
[1] "!!! PHESANT has stopped - add required variables to phenotype file or remove relevant phenotypes (so that required variables are not needed)."

The variables that are causing an issue in light of the above error are:

* Food weight > field ID 100001;
* Energy > field ID 100002;
* Protein > field ID 100003;
* Fat > field ID 100004;
* Carbohydrate > field ID 100005;
* Saturated fat > field ID 100006;
* Polyunsaturated fat > field ID 100007;
* Englyst dietary fibre > field ID 100009;
* Potassium > field ID 100016;
* Magnesium > field ID 100017;

Notes/Reminders
-

1. Check participant withdrawal notification emails > make sure to exclude people who have withdrawn from UKB >> we no longer have their consent!
2. Make sure you create your own confounder file and add a --confounderfile argument to the main phenome scan script.
3. Re-run the code with a significantly smaller subset of phenotypes to make sure you understand what you are looking at!




