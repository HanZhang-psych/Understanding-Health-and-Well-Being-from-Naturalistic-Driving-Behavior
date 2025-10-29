# User Guide: Understanding Health and Well-Being from Naturalistic Driving Behavior

This guide explains the project structure and provides step-by-step instructions to reproduce all analyses reported in the manuscript.

## Project Overview

This project analyzes the relationship between naturalistic driving behavior and health/well-being outcomes using data from the LongROAD study. The analysis includes:
- Bivariate relationships between 24 driving variables and 12 health outcomes
- Multivariate specificity analyses using structural equation modeling
- Nested cross-validation to assess out-of-sample prediction performance
- Additional/supplemental scripts including power analysis, plotting functions, and descriptive statistics

## Folder Structure

### `/data/shareable`
Contains shareable data files that allow reproducibility without access to raw data:
- `correlation_matrix.csv`: Correlation matrix of all variables (24 driving variables, 12 health outcomes, demographic variables). This matrix can be used to reproduce bivariate and multivariate analyses without needing raw data files.
- `aggregate_descriptive_stats.csv`: Means and standard deviations for all variables. Used in conjunction with the correlation matrix for to simulate data for nested CV. 

### `/preprocessing`
Contains scripts to process raw data into analysis-ready format. Due to controlled access, the raw data or the final, processed, participant-level data used for analysis are not publicly available. These preprocessing scripts are shared for transparency, and for future use if other researchers want to obtain the raw data from AAA.

- **Data processing scripts:**
  - `D.R`: Demographics processing
  - `DataLogger.R`: Driving data logger processing
  - `DR.CH.R`, `DR.MH.R`, `DR.PH.R`, `DR.SH.R`: Processing for cognitive, mental, physical, and social health data respectively
  - `aggregate_data.R`: Merges all datasets and produce a final,participant-level dataset for analysis.
  
- **Correlation matrix production:**
  - `produce_cor.R`: Creates the correlation matrix and aggregated descriptive statistics from participant-level data (stored in `data/shareable/`)

### `/analysis`
Contains all analysis scripts:
- **Core analysis scripts:**
  - `funcs.R`: Shared functions used across analyses (regression functions, variable definitions, color schemes)
  - `bivariate.R`: Tests all bivariate relationships between driving variables and health outcomes
  - `multivariate.R`: Tests specificity of relationships using structural equation models
- `nestedCV_sim.ipynb`: Nested cross-validation analysis using simulated data (Python/Jupyter)
  
- **Visualization scripts:**
  - `plot_corplots.R`: Creates correlation plots (if used)
  - `plot_chorddiagram.R`: Creates chord diagrams (if used)
  - `plot_nestedCV.R`: Creates plots for nested CV results
  
- **Additional analyses:**
  - `power analysis.R`: Statistical power analysis (if used)
  - `descriptives.R`: Generates descriptive statistics and histograms

### `/analysis/cv_errors`
Contains cross-validation results:
- `simulated/` subfolder: Contains CV results from simulated data analysis, with individual CSV files for each outcome variable (e.g., `LIFESATISFACTION.csv`, `cog.csv`, `phys.csv`, etc.)

### `/figs`
Contains all generated figures:
- Histograms: `Driving_Histogram.png`, `Health_Histogram.png`
- Correlation matrices: `Driving_Correlation_Matrix.png`, `Health_Correlation_Matrix.png`
- Analysis results: `cv_results_sim.png`, `multivariate_combined.png`, `specificity_score.png`, `dominance_matrix.png`

## Reproducing the Analysis

### Step 1: Bivariate Analysis

#### Understanding the `USE_RAW` Parameter

Several R analysis scripts (`bivariate.R`, `multivariate.R`, and `plot_nestedCV.R`) include a `USE_RAW` parameter at the top of the file. 

**For all analyses using the provided shareable data, set `USE_RAW = FALSE`** (this is the default setting). When `USE_RAW = FALSE`:
- Uses the correlation matrix from `data/shareable/correlation_matrix.csv`
- Uses descriptive statistics from `data/shareable/aggregate_descriptive_stats.csv`
- Provides statistically equivalent results to using raw data for regression analyses

1. **Open and run the bivariate analysis script:**
   - Open `analysis/bivariate.R` in RStudio (or your R IDE)
   - **Before running:** Ensure `USE_RAW = FALSE` is set at the top of the script (around line 16). This uses the correlation matrix from `data/shareable/`.
   - Run the entire script (Ctrl+Shift+Enter in RStudio, or use the Source button)

   This script:
   - Tests all pairwise relationships between 24 driving variables and 12 health outcomes
   - Controls for demographics (age, education, income, site, gender, race, work status, marital status)
   - Uses Bonferroni correction for multiple comparisons
   - Saves results to:
     - `analysis/all_bivariate_relationships.csv`: All relationships
     - `analysis/significant_bivariate_relationships.csv`: Only significant relationships (p < 0.05 after Bonferroni correction)

2. **Visualize bivariate analysis results:**

    - Open and run `analysis/plot_chorddiagram.R` in RStudio (or your R environment).

    This script:
    - Loads the output from the bivariate analysis (`significant_bivariate_relationships.csv`).
    - Plots a chord diagram, where:
        - Each driving variable and health outcome is arranged as a segment around a circle.
        - A line (chord) connects pairs with statistically significant associations.
        - The resulting plot visually summarizes which variables are most interconnected.
    - Saves results to:
        - `figs/chord_diagram.png`.

### Step 2: Multivariate Specificity Analysis

1. **Open and run the multivariate analysis script:**
   - Open `analysis/multivariate.R` in RStudio (or your R IDE)
   - **Before running:** Ensure `USE_RAW = FALSE` is set at the top of the script. This uses the correlation matrix from `data/shareable/`.
   - Run the entire script (Ctrl+Shift+Enter in RStudio, or use the Source button)
   
   This script:
   - Tests specificity of relationships for driving variables with multiple significant associations
   - Uses structural equation modeling (SEM) via `lavaan` package
   - Compares regression coefficients across health outcomes to identify specificity
   - Saves results to `analysis/multivariate_tests.csv`
   - Generates visualizations:
     - `figs/specificity_score.png`: Specificity scores for each driving variable
     - `figs/dominance_matrix.png`: Heatmap showing which driving variables predict which outcomes
     - `figs/multivariate_combined.png`: Combined figure

**Note:** This script requires `significant_bivariate_relationships.csv` from Step 3.

### Step 3: Nested Cross-Validation Analysis

1. **Open the simulated data notebook:**
   - Open `analysis/nestedCV_sim.ipynb` in Jupyter Notebook, JupyterLab, or VS Code with Jupyter extension

2. **Run all cells in order:**
   - In Jupyter: Use Cell → Run All, or run cells sequentially with Shift+Enter
   - In VS Code: Right-click and select "Run All Cells"
   
   This notebook:
   - Uses the correlation matrix and descriptive statistics from `data/shareable/` to simulate data
   - Implements nested k-fold cross-validation (10 outer folds)
   - Compares two models for each health outcome:
     - **Reduced model:** Demographics only
     - **Full model:** Demographics + all 24 driving variables
   - Uses Ridge regression with inner CV for hyperparameter tuning
   - Saves participant-level prediction errors to `analysis/cv_errors/simulated/[outcome].csv` for each outcome
   - Prints t-test results comparing model performance
   - **Note:** The simulated data preserves the correlation structure and descriptive statistics from the original dataset, allowing reproduction of the nested CV procedure. While not identical, the simulated results replicate the general pattern seen in the real data.

### Step 4: Visualize Cross-Validation Results

1. **Open and run the plotting script:**
   - Open `analysis/plot_nestedCV.R` in RStudio (or your R IDE)
   - **Before running:** Ensure `USE_RAW = FALSE` is set at the top of the script to load CV results from the `cv_errors/simulated/` folder
   - Run the entire script (Ctrl+Shift+Enter in RStudio, or use the Source button)
   
   This script:
   - Loads CV error files from Step 5
   - Performs paired t-tests comparing full vs. reduced models
   - Calculates Cohen's d effect sizes
   - Creates visualization: `figs/cv_results_sim.png`
   - Saves a table of results to `Table_S2.docx`

## Troubleshooting

### Common Issues

1. **Missing data files:**
   - Ensure the shareable data files are in `data/shareable/`:
     - `correlation_matrix.csv`
     - `aggregate_descriptive_stats.csv`
   - All scripts should have `USE_RAW = FALSE` set by default

2. **Path issues:**
   - All R scripts automatically set the working directory to their location using `rstudioapi::getActiveDocumentContext()$path`
   - If running outside RStudio, you may need to manually set the working directory or modify the script's path-setting code
