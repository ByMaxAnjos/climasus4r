# Climasus4r:: Integrated Analysis Toolkit for Health, Climate, and Environmental Data

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
<!-- badges: end -->

## Overview

**CLIMASUS4r** is an integrated R toolkit designed to streamline the analysis of health (SUS), climate, and environmental data in Brazil. Developed as part of the **INCT Conexão - Amazônia** project, this package automates the most laborious steps of data acquisition, cleaning, integration, and analysis, ensuring standardized and reproducible research workflows.

The package builds upon the excellent work provided by [`microdatasus`](https://github.com/rfsaldanha/microdatasus), adding specialized functions for climate-health research, including:

- **Parallel data acquisition** from multiple states and years
- **Intelligent encoding correction** for Brazilian Portuguese text
- **Standardized column names and values** (English, Portuguese, and Spanish translations)
- **Multilingual support** for output and messages (EN/PT/ES)
- **ICD-10 filtering** for disease-specific analyses
- **User-friendly CLI feedback** throughout the workflow

## Installation

Currently in development. Install the latest version from GitHub:

```r
# Install devtools if you haven't already
if (!require("remotes")) {
  install.packages("remotes")
}

# Install CLIMASUS4r
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never", quite = TRUE)
```
