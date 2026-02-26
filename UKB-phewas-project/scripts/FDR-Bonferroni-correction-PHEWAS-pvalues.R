
--------------------------------------------------------------------------
# PhD Project 2023: UKB PheWas cSVD Clinical Phenotypes Project
  
# Author: Angelina Kancheva 
# Date/period: December 2023
--------------------------------------------------------------------------
  
### Calculating FDR-corrected p-values from uncorrected p-values after running PheWas with PHESANT 

# Create a vector of uncorrected p-values
unc_pvalues <- c(0.0081,
                 0.0165,
                 0.0168,
                 0.0214,
                 0.0282,
                 0.0300,
                 0.0365,
                 0.0412
)
                 
print(unc_pvalues)

# Calculate FDR-corrected p-values
adj_values <- p.adjust(unc_pvalues, method="fdr")

adj_rounded_values <- round(adj_values, 4)
print(adj_rounded_values)

