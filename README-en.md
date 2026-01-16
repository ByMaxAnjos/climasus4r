# climasus4r:: Integrated Spatiotemporal Analyses of Health, Climate, and Environment in Brazil

<img src="https://github.com/ByMaxAnjos/climasus4r/blob/master/inst/figures/logo.png?raw=true" 
     alt="climasus4r Logo" 
     width="140" 
     style="float: right; margin-left: 10px;">

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/ByMaxAnjos/climasus4r)
<!-- badges: end -->

---

**climasus4r** is an integrated R toolkit designed to streamline the analysis of health, climate, and environmental data in Brazil. Developed within the INCT Conexão – Amazônia project, it automates and standardizes critical steps in epidemiological and environmental research workflows, promoting reproducibility, efficiency, and scalability.

Built on the solid [`microdatasus`](https://github.com/rfsaldanha/microdatasus) ecosystem, climasus4r expands functionality by incorporating specialized routines for climate and health studies, significantly reducing the effort required for data acquisition, cleaning, integration, and preparation.

## 🌐 Other Languages

- [Português](../pt/index.html) | [Español](../es/index.html)

## Installation

```r
# Install remotes if you don't have it
if (!require("remotes")) {
  install.packages("remotes")
}

# Install CLIMASUS4r
remotes::install_github("ByMaxAnjos/climasus4r", dependencies = TRUE, upgrade = "never")
```

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

For more information, see the [Tutorials](articles/tutorials.html) and [Complete Documentation](reference/index.html).
