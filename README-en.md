<p align="center">
  <img src="https://github.com/ByMaxAnjos/climasus4r/blob/master/inst/figures/logo.png?raw=true"
       alt="climasus4r logo"
       width="190"/>
</p>

> **Integrated Spatiotemporal Analyses of Health, Climate, and Environment in Brazil**


<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/ByMaxAnjos/climasus4r)
<!-- badges: end -->

---

[**climasus4r**](https://bymaxanjos.github.io/climasus4r/) is an integrated R toolkit designed to streamline the analysis of health, climate, and environmental data in Brazil. Developed within the INCT Conexão – Amazônia project, it automates and standardizes critical steps in epidemiological and environmental research workflows, promoting reproducibility, efficiency, and scalability.

Built on the solid [`microdatasus`](https://github.com/rfsaldanha/microdatasus) ecosystem, climasus4r expands functionality by incorporating specialized routines for climate and health studies, significantly reducing the effort required for data acquisition, cleaning, integration, and preparation.

## 🌐 Other Languages

- [Português](../pt/index.html) | [Español](../es/index.html)

## Installation

**climasus4r** is currently under active development. The latest version can be installed directly from GitHub, ensuring access to the most up-to-date features. Before installation, you must have the remotes package, which allows the installation of packages hosted on GitHub.

```r
# Install remotes if you don't have it
if (!require("remotes")) {
  install.packages("remotes")
}

# Install CLIMASUS4r
remotes::install_github("ByMaxAnjos/climasus4r", dependencies = TRUE, upgrade = "never")
```

## 📦 Function Overview

| Category | Function | Description |
| :--- | :--- | :--- |
| **📥 Import & Export** | `sus_data_import()` | Imports and pre-processes DATASUS data with intelligent caching. |
| | `sus_data_read()` | Optimized reading of processed data with parallel support. |
| | `sus_data_export()` | Exports processed data preserving metadata. |
| **🧹 Cleaning & Standardization** | `sus_data_clean_encoding()` | Detects and corrects character encoding issues. |
| | `sus_data_standardize()` | Standardizes SUS data column names and values. |
| | `sus_create_variables()` | Creates derived variables for epidemiological analysis. |
| **🔍 Filters & Selection** | `sus_data_filter_cid()` | Filters by ICD-10 codes or disease groups (multilingual). |
| | `sus_data_filter_demographics()` | Filters data by demographic variables (age, sex, race). |
| **🗺️ Spatial & Census** | `sus_join_spatial()` | Links SUS data to Brazilian geographic boundaries. |
| | `sus_socio_add_census()` | Enriches health data with socioeconomic variables from the Census. |
| | `sus_data_aggregate()` | Aggregates health data into time series. |
| **📊 Quality & Metadata** | `sus_data_quality_report()` | Generates detailed reports on data quality. |
| | `list_disease_groups()` | Lists available disease groups for filtering. |
| | `sus_census_explore()` | Interactive explorer for Census variables. |
| **⚡ Cache** | `clear_climasus_cache()` | Manages and clears local file storage. |


## Supported Systems

**climasus4r** provides simplified and standardized access to major DATASUS information systems through integration with the **microdatasus** package. This integration automates the collection of raw data from various databases of the Brazilian health system, covering epidemiology, mortality, hospital admissions, and the healthcare network. From this data, climasus4r organizes, cleans, and structures the information, transforming complex DATASUS databases into datasets ready for statistical analysis and spatiotemporal studies.

#### **1. SIM (Mortality Information System)**

* `"SIM-DO"`: Death Certificates (Complete Dataset)
* `"SIM-DOFET"`: Fetal Deaths
* `"SIM-DOEXT"`: Deaths from External Causes
* `"SIM-DOINF"`: Infant Deaths
* `"SIM-DOMAT"`: Maternal Deaths

#### **2. SIH (Hospital Information System)**

* `"SIH-RD"`: AIH (Hospital Admission Authorizations) - General
* `"SIH-RJ"`: AIH - Specific to Rio de Janeiro
* `"SIH-SP"`: AIH - Specific to São Paulo
* `"SIH-ER"`: Emergency Records

#### **3. SINAN (Notifiable Diseases Information System)**

* `"SINAN-DENGUE"`: Dengue cases
* `"SINAN-CHIKUNGUNYA"`: Chikungunya cases
* `"SINAN-ZIKA"`: Zika virus cases
* `"SINAN-MALARIA"`: Malaria cases
* `"SINAN-CHAGAS"`: Chagas disease cases
* `"SINAN-LEISHMANIOSE-VISCERAL"`: Visceral Leishmaniasis
* `"SINAN-LEISHMANIOSE-TEGUMENTAR"`: Tegumentary Leishmaniasis
* `"SINAN-LEPTOSPIROSE"`: Leptospirosis cases

#### **4. SIA (Ambulatory Information System)**

* `"SIA-AB"`: Primary Care (Basic Attention)
* `"SIA-ABO"`: Dental Procedures
* `"SIA-ACF"`: Pharmaceutical Assistance
* `"SIA-AD"`: High Complexity/Differentiated Care
* `"SIA-AN"`: Home Care
* `"SIA-AM"`: Specialized Outpatient Clinics
* `"SIA-AQ"`: Strategic Actions
* `"SIA-AR"`: Regulation
* `"SIA-ATD"`: Urgency/Emergency
* `"SIA-PA"`: Ambulatory Procedures in Hospitals
* `"SIA-PS"`: Psychosocial Care
* `"SIA-SAD"`: Specialized Care

#### **5. CNES (National Register of Health Establishments)**

* `"CNES-LT"`: Hospital Beds
* `"CNES-ST"`: Health Professionals
* `"CNES-DC"`: Equipment (Detailed)
* `"CNES-EQ"`: Equipment (Summary)
* `"CNES-SR"`: Specialized Services
* `"CNES-HB"`: Hospital Beds (Historical)
* `"CNES-PF"`: Physical Personnel (Professionals)
* `"CNES-EP"`: Teaching Participants
* `"CNES-RC"`: Hospital Classification
* `"CNES-IN"`: Hospital Indicators
* `"CNES-EE"`: Teaching Entities
* `"CNES-EF"`: Teaching Facilities
* `"CNES-GM"`: Management and Support

#### **6. SINASC (Live Birth Information System)**

* `"SINASC"`: Live Birth Certificates


## Quick Start

```r
library(climasus4r)
library(dplyr)

# Complete pipeline: Analysis-ready data
df_analysis <- sus_data_import(
  uf = "SP",
  year = 2023,
  system = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "en") |>
  sus_data_standardize(lang = "en") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "en") |>
  sus_create_variables(create_age_groups = TRUE, lang = "en")
```
## Data Infrastructure

The infrastructure phase of **climasus4r** provides a complete end-to-end pipeline for health data preparation, from raw acquisition to analysis-ready data. With nine main functions, you can transform DATASUS data into aggregated, standardized, and modeling-ready time series in minutes.

```r
RAW DATA (DATASUS)
    ↓
[1] sus_data_import()           → Parallel acquisition
    ↓
[2] sus_data_clean_encoding()   → Encoding correction
    ↓
[3] sus_data_standardize()      → Multilingual standardization
    ↓
[4] sus_data_filter_cid()       → Filtering by disease
    ↓
[5] sus_create_variables()      → Variable creation
    ↓
[6] sus_data_filter_demographics() → Demographic filtering
    ↓
[7] sus_data_quality_report()   → Quality verification
    ↓
[8] sus_data_aggregate()        → Temporal aggregation
    ↓
[9] sus_data_export()           → Export with metadata
    ↓
DATA READY FOR ANALYSIS
```

For more information, see the [Tutorials](..en/articles/tutorials.html) and [Complete Documentation](..en/reference/index.html).

## 🌲 Have feedback or suggestions?
Do you have an idea for improvement or did you spot a mistake? We'd love to hear from you! Click the button below to create a new issue on GitHub and share your feedback directly with us.

<a href='https://github.com/ByMaxAnjos/climasus4r/issues/new'>
  <button type="button" class="btn" style="background-color: #2E7D32; color: white; padding: 8px 16px; font-size: 14px; font-weight: bold; border: none; border-radius: 6px; cursor: pointer; transition: background-color 0.3s;">
    Open an issue in the Github repository
  </button>
</a>