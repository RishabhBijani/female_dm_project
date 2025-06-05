# Female DM Project

This repository contains all code and cleaned data files associated with the Female DM Project, which investigates the relationship between female District Magistrates (DMs) and the provisioning of public goods at the district level in India. The analysis spans the Census years 2001 and 2011.

## Directory Structure

```
├── cleaned_data/                   # Final cleaned datasets used for analysis
│   ├── 2001_panel_clean.csv
│   ├── 2011_panel_clean.csv
│   ├── ias_analysed_merged_70.csv
│   ├── ias_analysed_merged_80.csv
│   ├── ias_analysed_merged_90.csv
│   └── updated_coi_wide_1.csv
│
├── scripts/                        # R and STATA scripts used to clean and analyze data
│   ├── cleaning_compilation_final.R
│   ├── ias_matching.R
│   ├── panel_creation.R
│   └── econometric_analysis.do
│
├── README.md                       # This documentation file
```

## Description of Contents

### 1. `cleaned_data/`

* **2001\_panel\_clean.csv** and **2011\_panel\_clean.csv**: Cleaned panel data files for 2001 and 2011, respectively. These include information on village-level public service outcomes aggregated at the district level.
* **ias\_analysed\_merged\_70.csv**, **ias\_analysed\_merged\_80.csv**, **ias\_analysed\_merged\_90.csv**: District-wise aggregated IAS officer postings by decade, used to compute female service shares.
* **updated\_coi\_wide\_1.csv**: Final merged dataset containing political controls and additional variables used in regression specifications.

### 2. `scripts/`

* **cleaning\_compilation\_final.R**: Primary cleaning script for SHRID, PCA, and village-level datasets.
* **ias\_matching.R**: Code for merging cleaned IAS officer profile and experience data with district identifiers.
* **panel\_creation.R**: Code used to construct wide and long panel datasets by combining service delivery outcomes with IAS data.
* **econometric\_analysis.do**: STATA `.do` file containing all regression specifications, standard error clustering, and robustness checks.

## Raw Data Sources

Due to GitHub's file size limitations, raw data files are not stored in the repository. Instead, they can be downloaded via the following Google Drive links:

| File Name                  | Download Link                                                                                  |
| -------------------------- | ---------------------------------------------------------------------------------------------- |
| `pc11_pca_clean_shrid.csv` | [Download](https://drive.google.com/file/d/1jzYMK8JwoMxiYXYr__d2H7DFjvLB0i2L/view?usp=sharing) |
| `shrid_loc_names.csv`      | [Download](https://drive.google.com/file/d/12XUpA9z2EzTg7lgylNkIeJsQeNDqyGzC/view?usp=sharing) |
| `dist_key.csv`             | [Download](https://drive.google.com/file/d/1kTSmlGtv2ouEpucVP5enVoxSGQffSyFw/view?usp=sharing) |
| `pc11r_shrid_key.csv`      | [Download](https://drive.google.com/file/d/1iqtTYDT2k7lNxXgHhNF1MGmmLIqAwOGX/view?usp=sharing) |
| `state_key.csv`            | [Download](https://drive.google.com/file/d/1LWPyejsPud4lPfeWRRXjP9ts9PH8798M/view?usp=sharing) |

Each of these datasets is essential for replicating the data cleaning pipeline.

## Instructions for Replication

1. Download the raw data files from the above links and place them in a local `raw_data/` directory.
2. Run `scripts/cleaning_compilation_final.R` to generate village-level cleaned outputs.
3. Run `scripts/ias_matching.R` to prepare the IAS officer service files.
4. Execute `scripts/panel_creation.R` to merge panel data across years.
5. Final regressions and diagnostic tests can be executed via `scripts/econometric_analysis.do` in STATA.

## Notes

* All data cleaning steps rely on `tidyverse`, `lubridate`, and `stringr` packages in R.
* STATA regressions cluster standard errors at the district level and use `robust` options where appropriate.
* Ensure your R environment uses UTF-8 encoding and your STATA version supports panel regressions and robust SEs.

## References

* Asher, Sam, Tobias Lunt, Ryu Matsuura, and Paul Novosad. 2020. Development Research at High Geographic Resolution: An Analysis of Night Lights, Firms, and Poverty in India Using the SHRUG Open Data Platform. September 2020.
  
* Banerjee, Abhijit, and Rohini Somanathan. 2007. “The Political Economy of Public Goods: Some Evidence from India.” Journal of Development Economics 82 (2): 287–314.
  
* Bhavnani, Rikhil R., and Alexander Lee. 2017. “Local Embeddedness and Bureaucratic Performance: Evidence from India.” Journal of Politics (Forthcoming).
  
* Iyer, Lakshmi, and Anandi Mani. 2012. “Traveling Agents: Political Change and Bureaucratic Turnover in India.” Review of Economics and Statistics 94 (3): 723–739.
  
## License

This repository is made freely available for research and academic use. If using this repository, please cite appropriately.

---

For any queries or reproducibility issues, please contact [rishabhbijani@gmail.com](mailto:rishabhbijani@gmail.com)
