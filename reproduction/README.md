# Reproduction Code

This folder contains all code for reproducing the results in the report.

## Files

### Data Preparation
- `data_prep_gerber.R`: Prepares GerberGreen dataset (Z matrix, V matrix)
- `data_prep_lalonde.R`: Prepares LaLonde dataset

### FindIt Analysis
- `findit_gerber_official.R`: FindIt analysis on GerberGreen (factorial design)
- `findit_lalonde.R`: FindIt analysis on LaLonde

### Simulation Study
- `simulation_data_gen.R`: Data generation function for simulation study
- `simulation_setup.R`: Simulation parameters and setup
- `run_simulation_final_fixed.R`: Main simulation study script (300 replications)
- `simulation_results_final_fixed.RData`: Pre-computed simulation results

## Usage

### 1. Data Preparation
```r
# GerberGreen
source("reproduction/data_prep_gerber.R")
# Output: gerber_prepared.RData

# LaLonde
source("reproduction/data_prep_lalonde.R")
# Output: lalonde_prepared.RData
```

### 2. FindIt Analysis
```r
# GerberGreen (factorial design)
source("reproduction/findit_gerber_official.R")
# Output: findit_gerber_factorial_official.RData

# LaLonde
source("reproduction/findit_lalonde.R")
# Output: findit_lalonde_results.RData
```

### 3. Simulation Study
```r
# Run simulation (takes ~45 minutes)
source("reproduction/run_simulation_final_fixed.R")
# Output: simulation_results_final_fixed.RData

# Or load pre-computed results
load("reproduction/simulation_results_final_fixed.RData")
```

## Requirements

- R (>= 4.0)
- Required packages:
  - `FindIt`
  - `glmnet` (dependency of FindIt)

Install packages:
```r
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("FindIt")
```

## Notes

- All scripts use relative paths and should be run from project root
- Simulation results are pre-computed for reproducibility
- To regenerate results, run the scripts (may take significant time)


