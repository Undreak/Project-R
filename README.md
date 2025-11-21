# Statistical Analysis of Powder X-Ray Diffraction Data for Ni-Co Perovskites

**Author:** Alexandre De Cuyper
**Institution:** University of A Coruña
**Date:** February 22, 2024
**License:** GNU General Public License v3.0 or later

## Table of Contents

- [Overview](#overview)
- [Background](#background)
- [Materials](#materials)
- [Objectives](#objectives)
- [Methodology](#methodology)
- [Key Findings](#key-findings)
- [Project Structure](#project-structure)
- [Dependencies](#dependencies)
- [Reproducibility](#reproducibility)
- [Limitations and Future Work](#limitations-and-future-work)
- [Tools and Resources](#tools-and-resources)

## Overview

This project presents a comprehensive statistical analysis of powder X-ray diffraction (PXRD) data from nickel-cobalt perovskite materials with varying compositions. The analysis employs experimental design principles, exploratory data analysis, k-means clustering, and analysis of variance (ANOVA) to investigate structural changes in Ni₍₁₋ₓ₎Coₓ perovskites as a function of composition.

The study demonstrates significant composition-dependent variations in both peak positions (2θ angles) and intensities, providing evidence for systematic structural changes in the crystalline lattice as the Ni:Co ratio varies.

## Background

### Material Science Context

Perovskite materials with the general formula ABO₃ are of significant interest in materials science due to their diverse physical and chemical properties. Mixed-metal perovskites, particularly those incorporating transition metals like nickel and cobalt, exhibit tunable properties that make them attractive for various applications including catalysis, energy storage, and photovoltaics.

The samples analyzed in this project were synthesized as part of a previous research effort on dye-sensitized solar cells (DSC), documented in the [perovskite-dsc repository](https://gitlab.com/Undreak/perovskite-dsc). The structural characterization via powder X-ray diffraction provides crucial information about how compositional changes affect the crystal lattice parameters.

### Powder X-Ray Diffraction

PXRD is a non-destructive analytical technique used to identify and characterize crystalline materials. When X-rays interact with a crystalline sample, they undergo constructive interference (diffraction) at specific angles that satisfy Bragg's law:

```
nλ = 2d·sin(θ)
```

where:
- n is an integer (order of reflection)
- λ is the X-ray wavelength
- d is the spacing between crystallographic planes
- θ is the angle of incidence

The resulting diffraction pattern—intensity as a function of 2θ—serves as a "fingerprint" for the material's crystal structure.

## Materials

Four nickel-cobalt compositions were analyzed:

| Sample ID | Composition | Ni Content | Co Content | Description |
|-----------|-------------|------------|------------|-------------|
| Ni75Co25 | Ni₀.₇₅Co₀.₂₅ | 75% | 25% | Nickel-rich |
| Ni50Co50 | Ni₀.₅Co₀.₅ | 50% | 50% | Equimolar |
| Ni25Co75 | Ni₀.₂₅Co₀.₇₅ | 25% | 75% | Cobalt-rich |
| Co | Co | 0% | 100% | Pure cobalt reference |

Each sample was characterized using powder X-ray diffraction, with data exported from MATCH! crystallographic software. The datasets consist of 2θ angles (typically 8° to ~35°) and corresponding intensity values.

## Objectives

### Primary Research Questions

1. **Do different compositions exhibit statistically significant differences in peak positions?**
   - Hypothesis: Compositional changes alter lattice parameters, shifting peak positions

2. **Are there significant differences in peak intensities across compositions?**
   - Hypothesis: Composition affects crystallographic plane populations and/or atomic scattering factors

3. **Is there an interaction between composition and specific peak clusters?**
   - Hypothesis: Certain diffraction peaks are more sensitive to compositional changes than others

### Experimental Design Goals

- Identify cause-and-effect relationships between composition and crystal structure
- Quantify small but significant structural changes using statistical methods
- Develop a reproducible analysis pipeline for PXRD data

## Methodology

### 1. Data Processing

**Data Import and Preparation**
```r
# Import data files for each composition
d1 <- read.table("data/AD Ni75Co25.dat")
d2 <- read.table("data/AR cO50nI50.dat")
d3 <- read.table("data/JGD Co75Ni25.dat")
d4 <- read.table("data/AD Co.dat")

# Combine into unified dataset
data <- bind_cols(d1, d2$V2, d3$V2, d4$V2)
colnames(data) <- c("2_theta", "Ni75Co25", "Ni50Co50", "Ni25Co75", "Co")
```

### 2. Exploratory Data Analysis (EDA)

**Visualization**
- Overlaid diffraction patterns using `ggplot2` with ribbon plots
- Color-coded by composition for visual comparison
- Created both full-range and zoomed views to examine peak details
- Applied intensity threshold (threshold = 40) to distinguish significant peaks from background noise

**Key Visualization Features:**
- Full diffraction pattern (8° - 35° 2θ)
- Zoomed region (20° - 25° 2θ) showing peak shifts
- Threshold line to identify significant peaks

### 3. Peak Identification

A custom peak-finding algorithm was implemented:

```r
find_peaks <- function(data) {
  if (length(data) < 2) {
    return(NULL)
  }
  peaks <- rep(0, length(data))

  # Identify local maxima
  for (i in 2:(length(data) - 1)) {
    if (data[i] > data[i - 1] && data[i] > data[i + 1]) {
      peaks[i] <- data[i]
    }
  }

  # Check boundary conditions
  if (data[1] > data[2]) peaks[1] <- data[1]
  if (tail(data, 1) > tail(data, 2)[1]) {
    peaks[length(peaks)] <- data[length(data)]
  }

  return(peaks)
}
```

**Peak Selection Criteria:**
- Local maximum condition: I(θᵢ) > I(θᵢ₋₁) AND I(θᵢ) > I(θᵢ₊₁)
- Intensity threshold: I > 40 (empirically determined)
- Results in identification of significant diffraction peaks only

### 4. Clustering Analysis

**K-means Clustering:**
- Applied to group similar peak positions across all compositions
- Clustering based solely on 2θ position (1D clustering)
- Number of clusters: k = 22
- Algorithm: k-means with 4 random starts (nstart = 4)

```r
k <- 22
cluster_assignments <- kmeans(data_for_clustering,
  centers = k,
  nstart = 4
)$cluster
```

**Rationale:**
- Groups peaks that occur at similar 2θ positions
- Facilitates comparison of "equivalent" peaks across different compositions
- Enables statistical testing of composition effects on specific peak families

### 5. Statistical Analysis

**ANOVA Model 1: Peak Position Analysis**
```r
anova_result <- aov(`2_theta` ~ Composition * cluster, data = non_zero_data_long)
```

Tests:
- Main effect of Composition on 2θ position
- Main effect of peak cluster on 2θ position
- Interaction between Composition and cluster

**ANOVA Model 2: Peak Intensity Analysis**
```r
anova_result <- aov(Intensity ~ Composition * cluster, data = non_zero_data_long)
```

Tests:
- Main effect of Composition on peak intensity
- Main effect of cluster on peak intensity
- Interaction between Composition and cluster

### 6. Visualization of Results

Multiple visualization types were generated:

1. **Heatmaps:** Peak position and intensity across compositions
2. **Boxplots:** Distribution of 2θ values by cluster
3. **Bar plots:** Mean 2θ values by composition
4. **Cluster-colored heatmaps:** Visual identification of peak families

All plots saved to `plot/` directory at 10" × 5.625" (16:9 aspect ratio).

## Key Findings

### 1. Peak Position Analysis

**ANOVA Results for 2θ Position:**

| Source | Df | Sum Sq | Mean Sq | F value | Pr(>F) | Significance |
|--------|-----|---------|---------|---------|---------|--------------|
| Composition | 3 | 156.5 | 52.15 | 81.746 | 0.000114*** | Highly significant |
| Cluster | 15 | 2896.1 | 193.08 | 302.627 | 2.33e-06*** | Highly significant |
| Composition:cluster | 31 | 0.9 | 0.03 | 0.044 | 1.000000 | Not significant |
| Residuals | 5 | 3.2 | 0.64 | - | - | - |

**Interpretation:**
- **Composition effect (p < 0.001):** Different Ni:Co ratios produce statistically significant shifts in peak positions
- **Cluster effect (p < 0.001):** Different peak families occur at distinct 2θ positions (expected)
- **No interaction (p = 1.00):** The compositional effect is consistent across all peak families

### 2. Peak Intensity Analysis

**Summary Statistics by Composition:**

| Composition | Mean Intensity | Variance |
|-------------|----------------|----------|
| Co | 99.8 | 3653.0 |
| Ni25Co75 | 116.0 | 8636.0 |
| Ni50Co50 | 104.0 | 8514.0 |
| Ni75Co25 | 109.0 | 6062.0 |

**ANOVA Results for Intensity:**

| Factor | Df | Sum Sq | Mean Sq | F value | Pr(>F) | Significance |
|--------|-----|---------|---------|---------|---------|--------------|
| Composition | 3 | 9496 | 3165 | 14.80 | 0.012440* | Significant |
| Cluster | 9 | 220856 | 24540 | 114.78 | 0.000182*** | Highly significant |
| Composition:Cluster | 21 | 31651 | 1507 | 7.05 | 0.035380* | Significant |
| Residuals | 4 | 855 | 214 | - | - | - |

**Interpretation:**
- **Composition effect (p = 0.012):** Peak intensities vary significantly with composition
- **Cluster effect (p < 0.001):** Different peaks have inherently different intensities
- **Significant interaction (p = 0.035):** The compositional effect on intensity varies by peak family

### 3. Mean 2θ by Composition

Analysis of mean peak positions reveals a systematic trend:

- **Ni75Co25:** Highest mean 2θ (~25-26°)
- **Ni50Co50:** Intermediate (~23-24°)
- **Ni25Co75:** Intermediate (~24-25°)
- **Co:** Lowest mean 2θ (~22-23°)

This trend suggests systematic lattice parameter changes as nickel is replaced with cobalt.

### 4. Structural Implications

The statistically significant differences in peak positions indicate:

1. **Lattice parameter variation:** Ni and Co have different ionic radii, causing systematic changes in unit cell dimensions
2. **Vegard's Law behavior:** Approximately linear variation in lattice parameters with composition
3. **Structural integrity:** Peaks remain distinct and well-defined across all compositions, suggesting high crystallinity

## Project Structure

```
Project-R/
├── README.md                    # This file
├── LICENSE                      # GNU GPL v3.0+
├── project.R                    # Main analysis script
├── .gitignore                   # Git ignore rules (LaTeX files, etc.)
│
├── data/                        # Raw XRD data files
│   ├── AD Ni75Co25.dat         # Ni75Co25 composition
│   ├── AR cO50nI50.dat         # Ni50Co50 composition
│   ├── JGD Co75Ni25.dat        # Ni25Co75 composition
│   └── AD Co.dat               # Pure Co reference
│
├── plot/                        # Generated visualizations
│   ├── graph_nt.png            # Full diffraction pattern
│   ├── graph_nt_zoom.png       # Zoomed view (20-25° 2θ)
│   ├── graph.png               # Pattern with threshold line
│   ├── peaks.png               # Peak position heatmap
│   ├── peak_clusters.png       # Clustered peaks visualization
│   ├── theta.png               # Boxplot of 2θ by cluster
│   ├── theta_zoom.png          # Zoomed boxplot
│   ├── intensity.png           # Intensity boxplots
│   └── mean_theta.png          # Mean 2θ bar chart
│
└── presentation/                # Project presentation
    └── data_analysis.pdf       # Full presentation (24 slides)
```

## Dependencies

### Required R Packages

```r
# Data manipulation and visualization
library(tidyverse)  # Includes:
  # - ggplot2  (visualization)
  # - tidyr    (data reshaping)
  # - dplyr    (data manipulation)
  # - readr    (data import)
```

### Installation

```r
# Install tidyverse if not already installed
if (!require("tidyverse")) {
  install.packages("tidyverse")
}
```

### R Version

Developed and tested with R version ≥ 4.0.0

## Reproducibility

### Running the Complete Analysis

1. **Clone the repository:**
```bash
git clone https://github.com/Undreak/Project-R.git
cd Project-R
```

2. **Ensure data files are present:**
```bash
ls data/*.dat
# Should show four .dat files
```

3. **Run the analysis:**
```r
# From R or RStudio
source("project.R")
```

4. **View outputs:**
   - Plots saved to `plot/` directory
   - ANOVA results printed to console
   - Statistical summaries displayed during execution

### Key Parameters

If you wish to modify the analysis:

| Parameter | Location | Default Value | Description |
|-----------|----------|---------------|-------------|
| `threshold` | Line 81 | 40 | Minimum intensity for peak detection |
| `k` | Line 186 | 22 | Number of clusters for k-means |
| `nstart` | Line 191 | 4 | Number of random starts for k-means |
| Zoom range | Line 49 | [240:340] | Subset for zoomed visualization |

### Computational Requirements

- **RAM:** < 1 GB
- **Runtime:** < 1 minute on modern hardware
- **Storage:** ~50 MB for plots

## Limitations and Future Work

### Current Limitations

1. **Nickel reference data:** Pure Ni data not included; would complete the composition series
2. **Peak finding algorithm:** Simple local maximum detection; could be improved with:
   - Gaussian/Lorentzian peak fitting
   - Signal smoothing/denoising
   - Automated threshold determination
3. **Clustering variability:** K-means results can vary between runs due to random initialization
   - Consider: hierarchical clustering or deterministic methods
4. **Sample size:** Four compositions provide limited statistical power
   - Additional intermediate compositions would strengthen conclusions

### Future Research Directions

1. **Quantitative phase analysis:**
   - Rietveld refinement for precise lattice parameters
   - Phase fraction determination if multi-phase samples exist

2. **Crystallite size analysis:**
   - Scherrer equation application to peak broadening
   - Strain vs. size effects using Williamson-Hall analysis

3. **Complete compositional series:**
   - Include pure Ni endpoint
   - Add intermediate compositions (e.g., Ni₀.₆₃Co₀.₃₇, Ni₀.₃₇Co₀.₆₃)

4. **Correlation with properties:**
   - Link structural parameters to DSC performance metrics
   - Investigate composition-property-structure relationships

5. **Advanced statistical methods:**
   - Multivariate analysis (PCA, PLS)
   - Bayesian approaches for uncertainty quantification
   - Machine learning for pattern recognition

6. **Temperature-dependent studies:**
   - In-situ XRD at various temperatures
   - Thermal expansion coefficient determination

## Tools and Resources

### Software Used

- **R** - Statistical computing environment. https://www.R-project.org/
- **tidyverse** - Collection of R packages for data manipulation and visualization (ggplot2, dplyr, tidyr). https://www.tidyverse.org/
- **MATCH!** - Phase identification software for powder diffraction data. https://www.crystalimpact.com/match/

### Related Work

- **Perovskite DSC Project:** https://gitlab.com/Undreak/perovskite-dsc
  - Original synthesis and characterization of the Ni-Co perovskite materials used in this analysis

## License

This project is licensed under the GNU General Public License v3.0 or later. See the [LICENSE](LICENSE) file for details.
