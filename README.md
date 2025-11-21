# Statistical Analysis of Powder X-Ray Diffraction Data for Ni-Co Perovskites

Statistical analysis of PXRD data from Ni₍₁₋ₓ₎Coₓ perovskites using R.

**Author:** Alexandre De Cuyper (February 2024)

## Overview

This project applies exploratory data analysis, k-means clustering, and ANOVA to powder X-ray diffraction data from four Ni-Co perovskite compositions (Ni₇₅Co₂₅, Ni₅₀Co₅₀, Ni₂₅Co₇₅, pure Co). The analysis investigates composition-dependent variations in peak positions and intensities, providing evidence for systematic structural changes in the crystalline lattice.

Samples were synthesized as part of the [perovskite-dsc](https://gitlab.com/Undreak/perovskite-dsc) project.

## Structure

```
Project-R/
├── project.R                   # Main analysis script
├── data/                       # Raw XRD .dat files (4 compositions)
├── plot/                       # Generated visualizations
└── presentation/
    └── data_analysis.pdf       # Full analysis presentation
```

## Requirements

R (>= 4.0) with `tidyverse`:

```r
install.packages("tidyverse")
```

## Usage

```r
source("project.R")
```

Outputs ANOVA results to console and saves plots to `plot/`.

## Report

See [presentation/data_analysis.pdf](presentation/data_analysis.pdf) for the full analysis.

## License

[GPL-3.0-or-later](LICENSE)
