# climasus4r:: Integrated Analysis Toolkit for Health, Climate, and Environmental Data

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/ByMaxAnjos/climasus4r)
<!-- badges: end -->

## Overview

**climasus4r** is an integrated R toolkit designed to streamline the analysis of health (SUS), climate, and environmental data in Brazil. Developed as part of the **INCT Conexão - Amazônia** project, this package automates the most laborious steps of data acquisition, cleaning, integration, and analysis, ensuring standardized and reproducible research workflows.

The package builds upon the excellent work provided by [`microdatasus`](https://github.com/rfsaldanha/microdatasus), adding specialized functions for climate-health research, including:

- **Parallel data acquisition** from multiple states and years
- **Multi-system support** for all 6 major Brazilian health systems (SIM, SINASC, SINAN, SIH, SIA, CNES)
- **Improved encoding correction** for Brazilian Portuguese text
- **Standardized column names and values** with multilingual translations (EN/PT/ES)
- **Comprehensive ICD-10 filtering** with 54+ predefined disease groups
- **Climate-sensitive disease classification** for epidemiological research
- **Fully multilingual interface** for international collaboration

## Installation

Currently in development. Install the latest version from GitHub:

```r
# Install remotes if you haven't already
if (!require("remotes")) {
  install.packages("remotes")
}

# Install CLIMASUS4r
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never", quiet = TRUE)

# Upgrade often to get the latest improvements 
remove.packages("climasus4r")
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never", quiet = TRUE)
```

## Quick Start

```r
library(climasus4r)

# Complete pipeline: Import → Clean → Standardize → Filter
df_respiratory <- sus_data_import(
  uf = "SP",
  year = 2023,
  system = "SIM-DO"
) |>
  sus_data_clean_encoding() |>
  sus_data_standardize(lang = "en") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "en")

```

## Phase 1: Data Infrastructure ✅

### Core Functions

#### 1. `sus_data_import()` - Data Acquisition

Import data from DATASUS with **parallel processing** support for multiple states and years.

```r
# Single state and year
df <- sus_data_import(uf = "RJ", year = 2022, system = "SIM-DO")

# Multiple states and years with parallel processing
df <- sus_data_import(
  uf = c("RJ", "SP", "MG", "ES"),
  year = 2018:2022,
  system = "SIM-DO",
  parallel = TRUE,
  workers = 4,
  use_cache = TRUE
)
```

**Supported Systems:**
- **SIM** (Mortality): `"SIM-DO"`, `"SIM-DOEXT"`, `"SIM-DOFET"`, `"SIM-DOMAT"`
- **SINASC** (Live Births): `"SINASC"`
- **SINAN** (Notifiable Diseases): `"SINAN-DENGUE"`, `"SINAN-CHIKUNGUNYA"`, `"SINAN-ZIKA"`, `"SINAN-MALARIA"`, etc.
- **SIH** (Hospital Admissions): `"SIH-RD"`, `"SIH-SP"`
- **SIA** (Outpatient): `"SIA-PA"`, `"SIA-PS"`
- **CNES** (Health Establishments): `"CNES-ST"`, `"CNES-PF"`

**Features:**
- ✅ Automatic caching to avoid redundant downloads
- ✅ Parallel processing for faster data acquisition
- ✅ Progress bars with `cli` feedback

---

#### 2. `sus_data_clean_encoding()` - Encoding Correction

Detect and correct character encoding issues automatically.

```r
df_clean <- sus_data_clean_encoding(df_raw, lang = "en", verbose = TRUE)
```

**What it does:**
- Scans all text columns for encoding problems
- Corrects common Latin1/UTF-8 conflicts
- Reports which columns were corrected
- Acts as a safety net after `microdatasus` preprocessing
- Supports multilingual messages (EN/PT/ES)

---

#### 3. `sus_data_standardize()` - Data Standardization

Standardize column names and categorical values with **multilingual support**.

```r
# English standardization (default)
df_en <- sus_data_standardize(df_clean, lang = "en")

# Portuguese standardization
df_pt <- sus_data_standardize(df_clean, lang = "pt")

# Spanish standardization
df_es <- sus_data_standardize(df_clean, lang = "es")
```

**Transformations:**

| Original (DATASUS) | English | Portuguese | Spanish |
|--------------------|---------|------------|---------|
| `DTOBITO` | `death_date` | `data_obito` | `fecha_muerte` |
| `SEXO` | `sex` | `sexo` | `sexo` |
| `RACACOR` | `race` | `raca` | `raza` |
| `CAUSABAS` | `underlying_cause` | `causa_basica` | `causa_basica` |
| `TIPOBITO` | `death_type` | `tipo_obito` | `tipo_muerte` |

---

#### 4. `sus_data_filter_cid()` - ICD-10 Filtering

Filter data by ICD-10 codes with **54+ predefined disease groups** and flexible matching options.

```r
# Filter by disease group (easiest!)
df_respiratory <- sus_data_filter_cid(
  df,
  disease_group = "respiratory",
  lang = "en"
)

# Filter by explicit ICD codes
df_cardio <- sus_data_filter_cid(
  df,
  icd_codes = "I00-I99",
  match_type = "starts_with",
  lang = "pt"
)

# Filter by specific codes
df_ami <- sus_data_filter_cid(
  df,
  icd_codes = c("I21", "I22"),  # Acute myocardial infarction
  match_type = "starts_with",
  lang = "es"
)
```

**54+ Predefined Disease Groups:**

| Category | Groups | Examples |
|----------|--------|----------|
| **Infectious Diseases** (15) | `dengue`, `zika`, `chikungunya`, `malaria`, `tuberculosis`, `covid19`, `waterborne`, `vectorborne` | Climate-sensitive infections |
| **Cardiovascular** (6) | `cardiovascular`, `hypertensive`, `ischemic_heart`, `cerebrovascular`, `heart_failure` | Heat-related conditions |
| **Respiratory** (6) | `respiratory`, `acute_respiratory`, `chronic_respiratory`, `pneumonia`, `asthma`, `copd` | Air quality impacts |
| **Neoplasms** (2) | `neoplasms`, `malignant_neoplasms` | Cancer burden |
| **Endocrine/Metabolic** (2) | `diabetes`, `metabolic` | Chronic diseases |
| **External Causes** (6) | `external_causes`, `accidents`, `violence`, `transport_accidents`, `drowning`, `heat_exposure` | Climate disasters |
| **Climate-Health Special** (4) | `climate_sensitive_all`, `heat_related`, `cold_related`, `extreme_weather` | Epidemiological priorities |
| **Age-Specific** (2) | `pediatric_respiratory`, `elderly_cardiovascular` | Vulnerable populations |
| **Syndromic** (3) | `fever_syndrome`, `respiratory_syndrome`, `diarrheal_syndrome` | Surveillance |

**List all available groups:**
```r
# List all groups
list_disease_groups(lang = "en")

# List only climate-sensitive groups
list_disease_groups(climate_sensitive_only = TRUE, lang = "pt")

# Get details about a specific group
get_disease_group_details("dengue", lang = "es")
```
---

### Complete Pipeline Example

```r
library(climasus4r)
library(dplyr)

# Pipeline 1: Respiratory diseases in English
df_respiratory_en <- sus_data_import(
  uf = "SP",
  year = 2023,
  system = "SIM-DO",
  use_cache = TRUE
) |>
  sus_data_clean_encoding(lang = "en") |>
  sus_data_standardize(lang = "en") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "en")

# Pipeline 2: Cardiovascular diseases in Portuguese
df_cardio_pt <- sus_data_import(
  uf = c("RJ", "SP", "MG"),
  year = 2020:2023,
  system = "SIM-DO",
  parallel = TRUE
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "cardiovascular", lang = "pt")

# Pipeline 3: Climate-sensitive diseases in Spanish
df_climate_es <- sus_data_import(
  uf = "AM",
  year = 2023,
  system = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "es") |>
  sus_data_standardize(lang = "es") |>
  sus_data_filter_cid(disease_group = "climate_sensitive_all", lang = "es")

```

---

## Roadmap

### Phase 2: Socioeconomic Integration (In Progress)
* Geographic boundary linking
* IBGE socioeconomic data integration (population, GDP, HDI)
* Population-weighted spatial operations
* Census tract matching

### Phase 3: Environmental Integration (Planned)
* INMET meteorological data import
* Air quality data integration (CETESB, INPE)
* Satellite data processing (MODIS, Sentinel)
* Exposure matching algorithms

### Phase 4: Spatial Analysis (Planned)
* Bayesian spatial smoothing
* Spatial cluster detection (SaTScan, Kulldorff)
* Local indicators of spatial association (LISA)
* Spatial regression models

### Phase 5: Temporal & Predictive Analysis (Planned)
* Distributed lag non-linear models (DLNM)
* Attributable fraction calculation
* Time series decomposition
* Machine learning prediction wrappers


---

## Funding

This climasus4r project is funded by the Fundação Oswaldo Cruz Noroeste – Unidade de Rondônia (FIOCRUZ/RO) related to **Instituto Nacional de Ciência e Tecnologia de Pesquisa e Conhecimento de Excelência da Amazônia Ocidental (INCT-CONEXÃO)**,  nº of process 408474/2024-6, that include:

* **Núcleo Permanente de Climatologia (NPC)** INCT-CONEXÃO
* **Núcleo Avançado de Climatologia (NAC)** INCT-CONEXÃO

---

## Acknowledgments

- **microdatasus** team for the foundational data import infrastructure
- **DATASUS** for providing open access to Brazilian health data
- **INCT Conexão - Amazônia** for funding and institutional support

---

## Contact

- **Maintainer**: Max Anjos
- **Email**: [maxanjos@campus.ul.pt]
- **GitHub**: [https://github.com/ByMaxAnjos/climasus4r](https://github.com/ByMaxAnjos/climasus4r)
- **Issues**: [https://github.com/ByMaxAnjos/climasus4r/issues](https://github.com/ByMaxAnjos/climasus4r/issues)
