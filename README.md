# Information Sources and Adoption of Emerging Care Practices Among Emergency Clinicians During the COVID-19 Pandemic

This repository contains the analysis code for a national survey study examining how emergency department (ED) clinicians used information sources early in the COVID-19 pandemic, and how those sources were associated with adoption of emerging clinical practices.

The goal of this repository is to support transparency, reproducibility, and reuse of the analytic approach.

## Study Overview

Early in the COVID-19 pandemic, doctors and nurses had to make care decisions under extreme uncertainty. Guidance changed rapidly, and strong clinical evidence was often unavailable.

In this study, we asked:
* Where did ED clinicians get their information early in the pandemic?
* Were some information sources linked to safer or more evidence-aligned decisions than others?

Using survey data from over 1,600 U.S. emergency physicians and nurses, we examined how reliance on different information sources related to adoption of three early-pandemic practices: PPE reuse, high-flow oxygen, and ivermectin. We also examined the role of clinician decision-making autonomy.

## Key Findings

Clinicians relied on distinct clusters of information sources, including professional networks and authorities, academic publications, news and social media, and blogs and podcasts.

Professional networks and academic literature were more strongly associated with adoption of practices later supported by evidence (e.g., high-flow oxygen). News and social media use was associated with higher adoption of practices that later showed limited or mixed evidence (e.g., ivermectin, PPE reuse). Greater decision-making autonomy was associated with lower adoption of PPE reuse and ivermectin and higher adoption of high-flow oxygen among physicians.

## Code Overview

This code processes raw survey data from emergency department clinicians to produce an analysis-ready dataset and replicate the study’s primary findings. The pipeline first filters respondents to eligible cases and cleans and recodes demographic and geographic variables, including derivation of practice setting classifications. It then constructs composite measures of information source use and related constructs, and applies principal component analysis with varimax rotation to summarize patterns of information use and visualize factor loadings. Using these derived measures, the code fits multivariable logistic regression models to examine associations between information environments, decision-making autonomy, and adoption of early-pandemic clinical practices, restricting analyses to clinicians with at least some flexibility to make care decisions. Finally, the pipeline generates publication-ready tables and figures, including descriptive summaries, PCA visualizations, and adjusted odds ratio plots.

Pipeline (script order)

* 01_filter_data.R → filter respondents, save intermediate dataset
* 02_clean_data.R → clean/recode/join RUCA, save data/clean_data.rds
* 03_handle_scales.R → create subscale/composite averages
* 04_table_one.R → export results/table_one.docx
* 05_eda.R → descriptive plots + group comparisons
* 06_pca.R → PCA + loadings/scree/heatmap plots
* 07_logistic_models.R → adjusted logistic models + forest plots
* 08_innovation_factors.R → justification heatmaps/bars + unadjusted ORs
* 09_key_visualizations.Rmd → assemble key figures (driver file)

Please note that raw survey data are not publicly available due to confidentiality and data use agreements.

## Outputs

Running the full pipeline will generate:
* data/clean_data.rds (analysis-ready dataset)
* results/table_one.docx (Table 1 demographics)
* figures including PCA loadings, adjusted odds ratio forest plots, and justification bar charts

## How to Run the Analysis

After cloning the repository and installing required packages, run the scripts sequentially. The analysis assumes the working directory is set to the project root.

## Required Packages

All analyses were conducted in R (≥ 4.2). The following R packages are required to run the full pipeline:

### Data manipulation and tidying
tidyverse
janitor

### Statistical analysis
psych        # PCA, factor analysis, reliability
stats        # Base statistical models (glm, PCA utilities)
broom        # Tidying model outputs
car          # Model utilities
survey       # Survey-related helpers (if applicable to extensions)

### Visualization
ggplot2
patchwork
ggrepel
scales
viridis

### Tables and reporting
gtsummary
gt
flextable
officer
kableExtra

### File I/O
readxl
writexl
haven
here

### Utility / helpers
forcats
stringr
purrr

You can install all required packages with:

install.packages(c(
  "tidyverse", "janitor", "psych", "broom", "car", "survey",
  "ggplot2", "patchwork", "ggrepel", "scales", "viridis",
  "gtsummary", "gt", "flextable", "officer", "kableExtra",
  "readxl", "writexl", "haven", "here",
  "forcats", "stringr", "purrr"
))

## Citation

If you use this code, please cite:

Huber C, Hassler G, Cohen CC, Qureshi N, Vazquez E, Huilgol SS, Berdahl CT, Mendel P, Fischer SH.  
*Information Sources and Adoption of Emerging Care Practices Among Emergency Clinicians During the COVID-19 Pandemic.*  
Manuscript in preparation.

## License

This repository is shared under the MIT License.
