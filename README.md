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

## Phase 1: Data Infrastructure (🔄 vs ✅)

### New Features

* **Data Import**
  * 🔄 `sus_data_import()`: Import DATASUS data with parallel processing support
  * Automatic caching to avoid redundant downloads
  * Support for multiple states and years in a single call
  * Progress tracking with informative CLI messages
  * Error handling with detailed reporting

* **Data Cleaning**
  * 🔄 `sus_data_clean_encoding()`: Automatic detection and correction of character encoding issues
  * Handles common Latin1/UTF-8 conflicts in Brazilian Portuguese text
  * Reports which columns were corrected

* **Data Standardization**
  * 🔄 `sus_data_standardize()`: Comprehensive column name and value standardization
  * Translates 80+ Portuguese column names to English
  * Standardizes categorical values (sex, race, marital status, etc.)
  * Option to keep original columns for comparison

* **Disease Filtering**
  * 🔄 `sus_data_filter_cid()`: Flexible ICD-10 code filtering
  * Support for single codes, code ranges, and entire chapters
  * Multiple matching strategies (exact, starts_with, range)
  * Auto-detection of ICD column

## Roadmap

### Phase 2: Socioeconomic Integration (Planned)
* Geographic boundary linking
* IBGE socioeconomic data integration
* Population-weighted spatial operations

### Phase 3: Environmental Integration (Planned)
* INMET meteorological data import
* Air quality data integration
* Exposure matching algorithms

### Phase 4: Spatial Analysis (Planned)
* Bayesian spatial smoothing
* Spatial cluster detection
* Local indicators of spatial association

### Phase 5: Temporal & Predictive Analysis (Planned)
* Distributed lag non-linear models (DLNM)
* Attributable fraction calculation
* Machine learning prediction wrappers

---
# Funding

Este projeto é desenvolvido no âmbito do Instituto Nacional de Ciência e Tecnologia de Pesquisa e Conhecimento de Excelência da Amazônia Ocidental (INCT-CONEXÃO), no contexto das atividades do:

* Núcleo Permanente de Climatologia (NPC) do INCT-CONEXÃO

* Núcleo Avançado de Climatologia (NAC) do INCT-CONEXÃO