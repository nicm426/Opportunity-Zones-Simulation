# Opportunity Zone Simulation: Causal Forest vs Regression Discontinuity

Monte Carlo simulations comparing regression discontinuity and causal forest estimators for Opportunity Zone policy evaluation.

## Overview

This repository contains R code for the simulation study in:

> McWilliams, W. (2025). "Heterogeneous Treatment Effects in Opportunity Zone Evaluation: A Causal Forest Alternative to Regression Discontinuity."

The simulation framework is adapted from [Gulen, Jensen, and Page (2020)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3489377), modified to reflect the Opportunity Zone context with fuzzy RD treatment assignment and a hybrid identification strategy.

## Key Features

- **Fuzzy RD design**: Mirrors OZ selection where ~25% of eligible tracts are designated
- **Three estimators**: RD (rdrobust), standard causal forest, and hybrid causal forest
- **Hybrid approach**: Includes running variable as covariate in causal forest to leverage quasi-experimental variation
- **Four scenarios**: Baseline, heterogeneous effects, unobserved confounding, and combined
- **Parallel processing**: Supports multi-core execution with checkpointing

## Requirements

```r
install.packages(c("MASS", "grf", "rdrobust", "dplyr", "parallel"))
```

## Usage

1. Open `oz_simulation_robust.R`
2. Adjust parameters at the top of the file:
   ```r
   NUM_SIMULATIONS <- 1000    # Number of simulations per scenario
   NUM_OBS <- 5000            # Observations per simulation
   NUM_CORES <- "auto"        # Parallel cores ("auto" or specific number)
   ```
3. Run the script:
   ```r
   source("oz_simulation_robust.R")
   ```

Results are saved to `./oz_simulation_output/` with summary statistics and full results for each scenario.

## Runtime Estimates

Tested on a 56-core workstation (using 54 cores):

| Simulations | Time per Scenario | Total (4 scenarios) |
|-------------|-------------------|---------------------|
| 100 | ~1.5 minutes | ~6 minutes |
| 1,000 | ~13 minutes | ~1 hour |
| 10,000 | ~2 hours 10 min | ~8.5 hours |

**Scaling notes:**
- Runtime scales approximately linearly with number of simulations
- With fewer cores, expect proportionally longer runtimes
- Checkpointing saves progress every 10 simulations, so interrupted runs can resume
- Memory usage: ~2-4 GB with parallel processing

## Generating Figures

After simulations complete:

```r
source("generate_figures.R")
```

This creates publication-ready figures in `./figures/`.

## File Structure

```
├── oz_simulation_robust.R    # Main simulation script
├── generate_figures.R        # Figure generation script
├── oz_simulation_output/     # Results (created at runtime)
│   ├── scenario1_baseline_final.rds
│   ├── scenario1_baseline_summary.csv
│   └── ...
└── figures/                  # Figures (created by generate_figures.R)
    ├── fig1_bias_comparison.pdf
    ├── fig2_rmse_comparison.pdf
    └── ...
```

## Citation

If you use this code, please cite:

```bibtex
@unpublished{mcwilliams2025oz,
  author = {McWilliams, William},
  title = {Heterogeneous Treatment Effects in Opportunity Zone Evaluation: A Causal Forest Alternative to Regression Discontinuity},
  year = {2025},
  institution = {Virginia Tech}
}
```

## Acknowledgments

Simulation framework adapted from:

> Gulen, H., Jensen, T., & Page, B. (2020). "The Heterogeneous Effects of Default on Investment: An Application of Causal Random Forests in Corporate Finance."

## License

MIT
