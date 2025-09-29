# Estimation-processes-in-real-world-domains







```bash
.
├── Data/
│   ├── data_raw_estimation_*.csv - Raw data, processed in script `data_preparation_and_inital_analysis.R`
│   ├── data_analysis_*.csv   - Data used for model comparison analysis, created in `data_preparation_and_inital_analysis.R`
|   ├── data_tidy_combined.csv - Combine tidy data for all domains (i.e., after filterting etc.), created in `data_preparation_and_inital_analysis.R`
|   ├── ID_dictionaries.csv - Contains mapping of numeric IDs (used in the results of the model comparisons) and ID-strings (used in the estimation data)
│   └── Multidimensional Scaling/
│       └── MDS_config_*.csv - Final MDS configurations used as inputs for all models (producing script can be found here: https://github.com/dizyd/Similarity-Datasets)
├── Figures
|
├── Materials/
│   ├── images/
│   │   ├── Countries/
│   │   ├── Food/
│   │   ├── Mammals/
│   │   └── LICENSE
│   └── design_data_*.csv - lables, criterion values, image names and MDS coordinates for all stimuli
|
├── Results
|
└── Scripts/
    ├── data_preparation_and_inital_analysis.R  - R code for data processing (filtering etc.), demographics and descriptive analysis 
    │
    ├── plot_settings.R     - R code for the default ggplot2 theme and settings
    │
    ├── Model Comparison/
    │    ├── BF2_Personwise_MC_*.ipynb - Python code for the model comparison analysis 
    │    ├── figures_and_tables_model_comparison.R - R Code to make the results figures and tables 
    │    └── make_tidy_df_best_mod.R - Creates a tidy data.frame of the model comparison results, with the best fitting model per person
    │
    ├── Parameter Estimation/
    │    └── BF2_Personwise_E_*.ipynb - Python code to compute posterior parameter estimates. 
    │
    ├── Posterior Predictions/
    │    ├── BF2_gen_predictions__*.ipynb - Python code to generate posterior predictions.
    │    └── analysis_Rsq_predictions.R - R code to analyze the posterior predictions.
    │
    └── Appendix/ - Contains scripts to produce tables and figures reported in the Appendix (e.g., lists of items, figure of priors, etc.)
     
```


## Session Info


```
R version 4.4.2 (2024-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

attached packages:
 [1] afex_1.4-1        lme4_1.1-36       Matrix_1.7-1     
 [4] psych_2.4.12      viridis_0.6.5     viridisLite_0.4.2
 [7] extrafont_0.19    patchwork_1.3.0   kableExtra_1.4.0 
[10] lubridate_1.9.4   forcats_1.0.0     stringr_1.5.1    
[13] dplyr_1.1.4       purrr_1.0.2       readr_2.1.5      
[16] tidyr_1.3.1       tibble_3.2.1      ggplot2_3.5.2    
[19] tidyverse_2.0.0 
```