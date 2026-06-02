# Falen et al. (2026) - Analytical pipeline

R code and scoring data accompanying:

> Falen, L., Aide, T. M. & Alonso, A. (2026). Ecological baselines and
> co-governance as foundational design choices for the voluntary
> biodiversity credit market. *Communications Sustainability*.

This repository contains everything needed to reproduce the figures and
statistical results reported in the manuscript and Supplementary Information.

## Repository structure

```
falen-2026/
├── README.md                 (this file)
├── LICENSE                   MIT license
├── run_all.R                 master script that runs the full pipeline
├── R/                        modular analysis scripts
│   ├── 00_setup.R            packages, colour palette, ggplot theme, helpers
│   ├── 01_load_and_score.R   load data, compute Kendall's W (Suppl. Table 2)
│   ├── 02_figure1_dimensions.R   Figure 1 (dimension bar charts)
│   ├── 03_figure2_nmds_n13.R     Figure 2 (NMDS, N = 13, primary analysis)
│   ├── 04_figS2_nmds_n20.R       Supplementary Figure 2 (NMDS, N = 20)
│   ├── 05_figS3_clusters.R       Supplementary Figure 3 (cluster diagnostics)
│   ├── 06_doc_gradient.R         documentation-gradient diagnostic
│   └── 07_bootstrap_means.R      bootstrap CIs and Wilcoxon tests
├── data/
│   └── scoring_data.xlsx     scoring matrix (20 schemes x 15 criteria)
└── outputs/                  generated on each run; not version-controlled
```

## Data

`data/scoring_data.xlsx` contains four sheets:

- **Mean**: arithmetic mean across the three independent evaluators
  (used as input for all multivariate analyses).
- **Evaluator_1**, **Evaluator_2**, **Evaluator_3**: per-evaluator scores
  (used to compute Kendall's coefficient of concordance, Supplementary
  Table 2).

Each sheet has 20 rows (schemes) and 16 columns (scheme name + 15
evaluation criteria scored on a 0-3 rubric). The criteria are grouped
in three dimensions (5 criteria each): Governance, Rights & Equity;
Credit Integrity; Biodiversity Monitoring.

## Requirements

- R version 4.2.0 or later
- R packages: `readxl`, `dplyr`, `tidyr`, `vegan` (>= 2.7-2),
  `cluster`, `ggplot2`, `ggrepel`, `patchwork`, `scales`

Install the dependencies with:

```r
install.packages(c("readxl", "dplyr", "tidyr", "vegan", "cluster",
                   "ggplot2", "ggrepel", "patchwork", "scales"))
```

## How to reproduce

Clone the repository and run the master script from the repository root:

```bash
git clone https://github.com/LourdesF19/falen-2026.git
cd falen-2026
Rscript run_all.R
```

Or, from an interactive R session:

```r
setwd("path/to/falen-2026")
source("run_all.R")
```

All figures and tables will be written to the `outputs/` directory.
Individual scripts can also be run independently, as each one sources
`00_setup.R` and `01_load_and_score.R` first.

## Reproducibility notes

- A global random seed (`set.seed(42)`) is set in `00_setup.R` and at the
  start of every script that uses stochastic procedures (NMDS, k-means,
  bootstrap, permutation tests).
- NMDS is computed with `vegan::metaMDS()` using Bray-Curtis dissimilarities,
  two dimensions (`k = 2`), and `trymax = 200` random starts.
- `vegan::envfit()` is run with 9,999 permutations.
- Multiple-testing correction across the 15 criterion-level tests follows
  the Benjamini-Hochberg false-discovery-rate procedure
  (Benjamini & Hochberg 1995) at alpha = 0.05.
- The gap statistic uses the Tibshirani 1-SE rule (`firstSEmax`) with 100
  reference data sets.
- Bootstrap confidence intervals use 10,000 percentile resamples.

## Outputs

Running the full pipeline produces:

**Figures**
- `Figure1_dimension_scores.png/.pdf`
- `Figure2_NMDS_N13.png/.pdf`
- `SupplFigure2_NMDS_N20.png/.pdf`
- `SupplFigure3_cluster_diagnostics.png/.pdf`

**Tables (CSV)**
- `table_dimension_scores.csv`
- `tableS2_kendall_w.csv`
- `tableS3_mean_score_matrix.csv`
- `table_envfit_N13.csv`
- `table_envfit_N20.csv`
- `table_envfit_N20_residualised.csv`
- `table_silhouette_k2to5.csv`
- `table_gap_statistic.csv`
- `table_wss_elbow.csv`
- `table_documentation_gradient_diagnostic.csv`
- `table_bootstrap_CI_dimensions.csv`
- `table_wilcoxon_dimensions.csv`

## Citation

If you use this code or data, please cite both the article and the
archived release of this repository:

> Falen, L., Aide, T. M. & Alonso, A. (2026). Ecological baselines and
> co-governance as foundational design choices for the voluntary
> biodiversity credit market. *Communications Sustainability*. DOI: [tbd]
>
> Falen, L., Aide, T. M. & Alonso, A. (2026). Data and code for Falen
> et al. 2026 (v1.0.0). *Zenodo*. DOI: [tbd]

## License

Released under the MIT License - see `LICENSE`.

## Contact

Lourdes Falen — lourdesfalen@gmail.com / FalenL@si.edu
