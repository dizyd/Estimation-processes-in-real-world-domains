# Unpacking Cognitive Processes of Estimation in Real-World Domains

<br>

## About this repository

This repository contains the data, materials, results, and code to reproduce all analyses and figures in the project *Unpacking Cognitive Processes of Estimation in Real-World Domains: A Competition of Computational Models*. The structure of this repository is as follows:

<br>

## Folder Structure:


```bash
.
├── Data/
│   ├── `codebooks.html`             - Shows information about the variables in each of the available data sets (file created by codebooks.qmd). To display the file correctly, it must be downloaded.
│   ├── `data_raw_estimation_*.csv`  - Raw data, processed in script `data_preparation_and_initial_analysis.R`
│   ├── `data_analysis_*.csv`        - Data used for model comparison analysis, created in `data_preparation_and_initial_analysis.R`
│   ├── `data_tidy_combined.csv`     - Combine tidy data for all domains (i.e., after filtering etc.), created in `data_preparation_and_initial_analysis.R`
│   ├── `ID_dictionaries.csv`        - Contains mapping of numeric IDs (used in the results of the model comparisons) and ID-strings (used in the estimation data)
│   └── Multidimensional Scaling/
│       └── `MDS_config_*.csv`       - Final MDS configurations used as inputs for all models (producing script can be found here: https://github.com/dizyd/Similarity-Datasets)
├── Figures
│
├── Materials/
│  ├── images/
│  │   ├── Countries/
│  │   ├── Food/
│  │   ├── Mammals/
│  │   └── LICENSE
│  └── `design_data_*.csv` - labels, criterion values, image names, and MDS coordinates for all stimuli
│
├── Results - Intermediate result files, such as trained networks as `.keras` files, estimated posterior model probabilities (in `pmp_*.csv`), parameters (`par_ests_*.csv`), and posterior predictions (`pred_*.csv`)
│
└── Scripts/
    ├── `data_preparation_and_inital_analysis.R`  - R code for data processing (filtering, etc.), demographics, and descriptive analysis 
    │
    ├── `plot_settings.R` - R code for the default ggplot2 theme and settings
    │
    ├── Model Comparison/
    │    ├── `BF2_Personwise_MC_*.ipynb`             - Python code for the model comparison analysis 
    │    ├── `figures_and_tables_model_comparison.R` - R Code to make the results figures and tables 
    │    └── `make_tidy_df_best_mod.R`               - Creates a tidy data.frame of the model comparison results, with the best-fitting model per person
    │
    ├── Parameter Estimation/
    │    └── `BF2_Personwise_E_*.ipynb` - Python code to compute posterior parameter estimates. 
    │
    ├── Posterior Predictions/
    │    ├── `BF2_gen_predictions__*.ipynb` - Python code to generate posterior predictions.
    │    └── `analysis_Rsq_predictions.R`   - R code to analyze the posterior predictions.
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


Python 3.11.9 (tags/v3.11.9:de54cf5, Apr  2 2024, 10:12:12) [MSC v.1938 64 bit (AMD64)]
Windows-10-10.0.26100-SP0

absl-py 2.2.2          jaxlib 0.6.1            pure-eval 0.2.3
arviz 0.21.0           jedi 0.19.2             pygments 2.19.1
asttokens 3.0.0        jupyter-client 8.6.3    pyparsing 3.2.3
bayesflow 2.0.3        jupyter-core 5.7.2      python-dateutil 2.9.0.post0
colorama 0.4.6         keras 3.10.0            pytz 2025.2
comm 0.2.2             kiwisolver 1.4.8        pywin32 310
contourpy 1.3.2        llvmlite 0.44.0         pyzmq 26.4.0
cycler 0.12.1          markdown-it-py 3.0.0    rich 14.0.0
debugpy 1.8.14         matplotlib 3.10.3       scipy 1.15.3
decorator 5.2.1        matplotlib-inline 0.1.7 seaborn 0.13.2
executing 2.2.0        mdurl 0.1.2             setuptools 65.5.0
fonttools 4.58.0       mizani 0.13.5           six 1.17.0
h5netcdf 1.6.1         ml-dtypes 0.5.1         stack-data 0.6.3
h5py 3.13.0            namex 0.0.9             statsmodels 0.14.4
ipykernel 6.29.5       nest-asyncio 1.6.0      tornado 6.5.1
ipython 9.2.0          numba 0.61.2            tqdm 4.67.1
ipython-pygments-lexers 1.1.1  numpy 1.26.4     traitlets 5.14.3
jax 0.6.1              opt-einsum 3.4.0        typing-extensions 4.13.2
patsy 1.0.1            optree 0.15.0           tzdata 2025.2
pillow 11.2.1          packaging 25.0          wcwidth 0.2.13
pip 25.1.1             pandas 2.2.3            xarray 2025.4.0
platformdirs 4.3.8     parso 0.8.4             xarray-einstats 0.9.0
plotnine 0.14.5        prompt-toolkit 3.0.51   psutil 7.0.0                 
                  
                            
```


This work, including all figures, is licensed under a <a rel="license" href=" https://creativecommons.org/licenses/by-nc-sa/4.0/ ">CC BY-NC-SA 4.0</a>.  

<br>

## Contributing Authors
David Izydorczyk & Arndt Bröder


## Abstract
Will be added soon.

## Publication
(work in progress)

## Funding
This research was funded by Grant IZ 96/1-1 provided to David Izydorczyk from the German Research Foundation (DFG) and supported by the University of Mannheim’s Graduate School of Economic and Social Sciences.
