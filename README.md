# powered-dif-analysis_traditional-students
An ongoing applied psychometric study investigating Differential Item Functioning (DIF) and measurement invariance in DAACS assessments for traditional college-age students (17-18). This research has just commenced and is currently in the power analysis and data preparation phase.
# Evaluating Differential Item Functioning in Traditional College-Age Students (17-18)

This repository contains an applied psychometric research study investigating item fairness within the DAACS Mathematics and Reading assessments, specifically targeting the "Traditional College-Age" (17-18 years old) demographic subgroup.

The repository highlights the complete lifecycle of a targeted fairness study: from *a priori* statistical power analyses and sample size justifications, to targeted data extraction, and finally to multivariable logistic regression Differential Item Functioning (DIF) modeling.

## Key Highlights

* **Evidence-Based Study Design:** Includes rigorous *a priori* statistical power analyses calculating Cohen's *h* log-odds approximations and McFadden's pseudo-R² thresholds to justify cohort merging and validate sample size sufficiency for DIF detection.
* **Targeted Demographic Subsetting:** Clean, reproducible scripts to extract and describe the specific 17-18 year old cohort from broader, multi-wave operational assessment databases.
* **Applied Fairness Research:** Translates operational data pipelines into focused, subgroup-specific psychometric research answering direct questions about measurement invariance.

## Repository Structure

    powered-dif-analysis_traditional-students/
    ├── README.md
    ├── src/
    │   ├── 01_study_design/
    │   │   └── Power_Analysis_Math_Read_17-18_2022.Rmd 
    │   ├── 02_data_preparation/
    │   │   ├── subset_math_17_18_describe.R             
    │   │   └── subset_read_17_18_describe.R             
    │   └── 03_dif_analysis/
    │       └── (DIF execution scripts for the targeted subset)

## Methodology Overview

1. **Study Design (`01_study_design/`):** Before running statistical tests, the project evaluates the minimum sample sizes required to detect moderate DIF effects (e.g., Odds Ratio = 1.5). The power analysis proves that by merging the 2022 and 2023 cohorts, the study achieves a sufficient sample size (~540-560) to fit a multivariable logistic model with stable estimates and a low risk of overfitting.
2. **Data Preparation (`02_data_preparation/`):** Ingests the finalized, fully-cleaned operational DAACS datasets and securely subsets the populations to isolate students aged 17 and 18. It generates subset-specific missingness diagnostics, demographic distributions, and item-exposure rates.
3. **DIF Analysis (`03_dif_analysis/`):** Evaluates the measurement properties of the items within this specific age bracket to ensure the assessments operate fairly across intersecting demographics (e.g., gender, financial aid status) within the young adult population.

## Requirements

* **R (>= 4.1.0)**
* **Core Packages:** `dplyr`, `tidyr`, `ggplot2`
* **Study Design:** `pwr`, `rmarkdown`

## Author
**Oxana Rosca, PhD** *Educational Psychology *
