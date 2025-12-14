# Climasus4r::Integrated Analysis Toolkit for Health, Climate, and Environmental Data

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
<!-- badges: end -->

## Overview

**climasus4r** is an integrated R toolkit designed to streamline the analysis of health (SUS), climate, and environmental data in Brazil. Developed as part of the **INCT Conexão - Amazônia** project, this package automates the most laborious steps of data acquisition, cleaning, integration, and analysis, ensuring standardized and reproducible research workflows.

The package builds upon the excellent work provided by [`microdatasus`](https://github.com/rfsaldanha/microdatasus), adding specialized functions for climate-health research, including:

- **Parallel data acquisition** from multiple states and years
- **Improved encoding correction** for Brazilian Portuguese text
- **Standardized column names and values** (English, Portuguese, and Spanish translations)
- **Multilingual support** for output and messages (EN/PT/ES)
- **ICD-10 filtering** for disease-specific analyses

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

### Data Acquisition

#### `sus_data_import()`

Import data from DATASUS with parallel processing support.

```r
# Single state and year
library(climasus4r)
df <- sus_data_import(uf = "RJ", year = 2022, system = "SIM")

# Multiple states and years with parallel processing
df <- sus_data_import(
  uf = c("RJ", "SP", "MG", "ES"),
  year = 2018:2022,
  system = "SIM",
  parallel = TRUE,
  workers = 4
)

# Available systems: "SIM", "SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SIH", etc.
```

### Data Cleaning

#### `sus_data_clean_encoding()`

Detect and correct character encoding issues automatically.

```r
df_clean <- sus_data_clean_encoding(df_raw)
```

**What it does:**
- Scans all text columns for encoding problems
- Corrects common Latin1/UTF-8 conflicts
- Reports which columns were corrected
- Acts as a safety net after `microdatasus` preprocessing

### Data Standardization

#### `sus_data_standardize()`

Standardize column names and categorical values for consistency.

```r
df_standard <- sus_data_standardize(
  df_clean,
  translate_columns = TRUE,    # Portuguese → English
  standardize_values = TRUE,   # Decode categorical variables
  keep_original = FALSE        # Remove original columns
)
```

**Transformations:**

| Original (Portuguese) | Standardized (English) | Example Values |
|----------------------|------------------------|----------------|
| `DTOBITO` | `death_date` | `2022-03-27` |
| `SEXO` | `sex` | `Male`, `Female` |
| `RACACOR` | `race` | `White`, `Black`, `Brown`, `Yellow`, `Indigenous` |
| `CAUSABAS` | `underlying_cause` | `I21.9`, `J18.9` |
| `CODMUNRES` | `residence_municipality_code` | `330455` |

### Disease Filtering

#### `sus_data_filter_cid()`

Filter data by ICD-10 codes with flexible matching options.

```r
# Filter by ICD-10 chapter (all respiratory diseases)
df_resp <- sus_data_filter_cid(df, icd_codes = "J00-J99")

# Filter by specific codes
df_pneumonia <- sus_data_filter_cid(
  df, 
  icd_codes = c("J12", "J13", "J14", "J15", "J16", "J17", "J18")
)

# Filter multiple disease groups
df_climate_sensitive <- sus_data_filter_cid(
  df,
  icd_codes = c("J00-J99", "I00-I99", "A00-A09")  # Resp + Cardio + Infectious
)
```

**Supported ICD-10 Chapters:**
- `A00-B99`: Infectious and parasitic diseases
- `I00-I99`: Cardiovascular diseases
- `J00-J99`: Respiratory diseases
- `K00-K99`: Digestive diseases
- `N00-N99`: Genitourinary diseases
- And all other ICD-10 chapters

### Pipeline Approach (Recommended)

```r
library(climasus4r)
library(dplyr)

# Complete pipeline in one go
df_analysis_ready <- sus_data_import(
  uf = "RJ",
  year = 2022,
  system = "SIM-DO"
) %>%
  sus_data_clean_encoding(lang = "en") %>%
  sus_data_standardize(lang = "en") %>%
  sus_data_filter_cid(icd_codes = c("J00-J99", "I00-I99"))  # Respiratory + Cardiovascular

# Proceed with your climate-health analysis
```

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

Este projeto é desenvolvido no âmbito do Instituto Nacional de Ciência e Tecnologia de Pesquisa e Conhecimento de Excelência da Amazônia Ocidental (INCT-CONEXÃO), processo no 408474/2024-6, no contexto das atividades do:

* Núcleo Permanente de Climatologia (NPC) do INCT-CONEXÃO

* Núcleo Avançado de Climatologia (NAC) do INCT-CONEXÃO