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

**Other languages:** [Português](README-pt.md) | [Español](README-es.md)

[**climasus4r**](https://bymaxanjos.github.io/climasus4r/) changes how researchers analyse the intersection of climate, health, and environment in Brazil. This R package integrates data from multiple sources—health information systems, weather stations, satellite imagery, and socioeconomic databases—into a unified analytical framework. What would normally take months of manual data wrangling now takes minutes, thanks to automated pipelines that ensure reproducibility and scientific rigour. With climasus4r, users can share not only results but entire analytical workflows that others can verify and build upon.

## Why use climasus4r?

> | Aspect | climasus4r | Similar packages |
> |---|---|---|
> | **Health–climate integration** | ✓ Native | ✗ Requires manual integration |
> | **Automated SUS data** | ✓ Yes (6 systems) | ✗ Requires manual download |
> | **RAPs (reproducibility)** | ✓ Built in | ✗ Requires manual documentation |
> | **Multilingual** | ✓ PT/ES/EN | ✗ Usually EN only |
> | **Parallel processing** | ✓ Native | ✗ Requires manual setup |
> | **Spatiotemporal aggregation** | ✓ Flexible (15+ options) | ✗ Limited |
> | **Focus on the Brazilian context** | ✓ Yes | ✗ Generic |


## Key Features

> - **Unified data integration:** Seamlessly combine health (SUS), climate (INMET, ERA5), environmental (MapBiomas, AQI), and socioeconomic (IBGE) data.
> - **Automated pipelines:** Import, clean, standardise, and aggregate data in a few lines of code.
> - **Reproducible workflows:** Generate RAPs (Reproducible Analytical Pipelines) that document every step of your analysis.
> - **Flexible aggregation:** Analyse data at daily, weekly, monthly, seasonal, or annual scales; at municipal, regional, or national levels.
> - **Multilingual support:** Output in Portuguese, Spanish, or English.
> - **High performance:** Parallel processing to handle large data volumes efficiently.


# Installation

`climasus4r` is available via r-universe and GitHub. We recommend **Option 1** for most users — it requires neither `remotes`/`devtools` nor a GitHub token.

---

## Option 1: Install from r-universe (recommended)

```r
install.packages("climasus4r", repos = c(
  "https://bymaxanjos.r-universe.dev",
  "https://cloud.r-project.org"
))
```

## Option 2: Install from GitHub

If you do not have the `remotes` package installed yet, run:

::: callout-tip
**Prerequisite:** If you already have `remotes` or `devtools` installed, you can skip this step.
:::

```r
if (!require("remotes")) { install.packages("remotes")}
```

```r
# Install or update the package directly from GitHub
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never")
```

::: callout-tip
**Linux (Ubuntu/Debian, Colab, Jupyter):** CRAN does not ship binaries for Linux, so `duckdb` and `arrow` get compiled from source (can take over an hour). Use Posit Package Manager binaries instead, before installing:

```r
system("grep VERSION_CODENAME /etc/os-release")  # e.g. jammy, noble
options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"),  # replace jammy with your codename
  HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),
    paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"]))
)
install.packages(c("remotes", "duckdb", "future", "furrr", "arrow"))
remotes::install_github("ByMaxAnjos/climasus4r", dependencies = NA, upgrade = "never")
```
:::

::: callout-warning
**Updates:** `climasus4r` is under active development. To update, simply run the command above again; R will overwrite the old version automatically. After updating, we recommend restarting the session (Menu: Session > Restart R).
:::

## Option 3: Install from a local file (.zip)

This option is recommended for environments with unstable connections, network restrictions, or training sessions with many users.

1. **Download the package**: Click [HERE](https://github.com/ByMaxAnjos/climasus4r/archive/refs/heads/master.zip) to download the .zip
2. **Extract the file**: Extract the contents to an easily accessible folder (e.g. Downloads or Desktop).
3. **Install locally**: Run the command below, adjusting the path to the folder where you extracted the file:

```r
# IMPORTANT: change the path below to where you saved the package
remotes::install_local(
  "C:/Path/To/Your/Folder/climasus4r-master",
  upgrade = "never"
)
```

## Loading the package

After installation, load the package whenever you start a new R session:
```r
library(climasus4r)
```

## Quick Start

```r
library(climasus4r)

# Full Phase 1 pipeline: analysis-ready data in 8 steps
df_analysis <- sus_data_import(
  uf = "SP",
  year = 2023,
  system = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "en") |>
  sus_data_standardize(lang = "en") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "en") |>
  sus_data_create_variables(
    create_age_groups = TRUE,
    create_calendar_vars = TRUE,
    lang = "en"
  ) |>
  sus_data_filter_demographics(
    age_range = c(0, 5),  # Children under 5
    sex = c("Female", "Male"),
    lang = "en"
  ) |>
  sus_data_aggregate(
    time_unit = "day",
    lang = "en"
  )
# Save
sus_data_export(df_analysis,
    file_path = "pediatric_respiratory_sp_2023.csv",
    format = "csv",
    include_metadata = TRUE,
    lang = "en"
  )
```

---

# climasus4r — Complete Function Catalogue

> Integrated health–climate–environment analysis pipeline for Brazil.
> ~65 exported functions · 81 R files · languages: `pt` (default), `en`, `es`

---

## Processing Pipeline

```
sus_data_import() / sus_data_read()
        ↓
sus_data_clean_encoding() → sus_data_standardize()
        ↓
sus_data_filter_cid() → sus_data_filter_demographics() → sus_data_create_variables()
        ↓
sus_data_aggregate()
        ↓
sus_spatial_join()  |  sus_climate_*/sus_grid_*  |  sus_socio_add_census()
        ↓
sus_mod_dlnm() / sus_mod_casecrossover() / sus_mod_ml() / sus_mod_spatial_*() ...
        ↓
sus_mod_plot_*() / sus_data_plot_*() / sus_data_export()
```

---

## 1. Metadata and Core Class

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `sus_meta()` | `climasus_meta.R` | `x` (climasus_df), `field`, `add_history`, `valid_values` | list / field / updated climasus_df | Reads or writes pipeline metadata (stage, type, system, history) |

---

## 2. Import

| Function | File | Input | Output (stage) | Description |
|--------|---------|-------|----------------|-----------|
| `sus_data_import()` | `sus_data_import.R` | `system`, `uf`, `year`, `month`, `lang` | `"import"` | Downloads data via microdatasus (SIM, SIH, SINAN, SIA, CNES, SINASC) |
| `sus_data_read()` | `sus_data_read.R` | `path`, `system`, `lang` | `"import"` | Reads local SUS files (.dbc, .parquet, .csv) |

---

## 3. Preparation and Cleaning

| Function | File | Input (stage) | Output (stage) | Description |
|--------|---------|---------------|----------------|-----------|
| `sus_data_clean_encoding()` | `sus_data_clean_encoding.R` | `"import"` | `"clean"` | Fixes Latin-1/UTF-8 encoding and malformed characters |
| `sus_data_standardize()` | `sus_data_standardize.R` | `"clean"` | `"stand"` | Standardises column names and types across SUS systems |

---

## 4. Filtering and Derivation

| Function | File | Input (stage) | Output (stage) | Description |
|--------|---------|---------------|----------------|-----------|
| `sus_data_filter_cid()` | `sus_data_filter_cid.R` | `"stand"` | `"filter_cid"` | Filters by ICD-10 chapters or codes |
| `sus_data_filter_demographics()` | `sus_data_filter_demographics.R` | `"filter_cid"` | `"filter_demo"` | Filters by age range, sex, and place of residence |
| `sus_data_create_variables()` | `sus_create_variables.R` | `"filter_demo"` | `"derive"` | Creates derived variables (age groups, weekday, seasonality) |
| `sus_filter_cid_explore()` | `sus_filter_cid_explore.R` | `df`, `lang`, `output` | viewer / tibble | Interactive ICD-10 group browser |
| `sus_data_cid_select()` | `sus_data_filter_cid.R` | `df`, `cid_group`, `lang` | filtered climasus_df | Outcome selection by predefined ICD group |

---

## 5. Aggregation and Export

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `sus_data_aggregate()` | `sus_data_aggregate.R` | climasus_df, `by`, `unit`, `lang` | `"aggregate"` | Aggregates records by municipality × date × variable |
| `sus_data_export()` | `sus_data_export.R` | climasus_df, `path`, `format` | .parquet / .duckdb file | Exports processed data |
| `sus_data_quality_report()` | `sus_data_quality_report.R` | climasus_df, `lang`, `output` | HTML/PDF | Diagnostic report on completeness and consistency |

---

## 6. Health Data Visualisation

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `sus_data_plot_demographics()` | `sus_data_plot_demographics.R` | climasus_df, `lang` | ggplot2 | Age pyramid, sex, demographic distributions |
| `sus_data_plot_aggregate_ts()` | `sus_data_plot_aggregate_ts.R` | climasus_df, `by`, `lang` | ggplot2 | Time series of aggregated outcomes |
| `sus_data_plot_aggregate_map()` | `sus_data_plot_aggregate_map.R` | climasus_df, `year`, `lang` | ggplot2 / map | Municipality map of outcomes |

---

## 7. Spatial Integration

| Function | File | Input | Output (stage) | Description |
|--------|---------|-------|----------------|-----------|
| `sus_spatial_join()` | `sus_join_spatial.R` | climasus_df, `level` (munic/state/biome), `lang`, `use_cache` | `"spatial"` + sf | Links SUS data to Brazilian administrative boundaries via geobr |

---

## 8. Climate Data — INMET Stations

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `sus_climate_inmet()` | `sus_climate_inmet.R` | `stations`, `start_date`, `end_date`, `lang` | type="inmet" | Imports hourly INMET data (temperature, humidity, rainfall, wind) |
| `sus_climate_fill_inmet()` | `sus_climate_fill_gap.R` | climasus_df (inmet), `method` | type="filled" | Fills gaps by spatial interpolation or regression |
| `sus_climate_normals()` | `sus_climate_normals.R` | `stations`, `period`, `lang` | climate normals | 30-year means per station |
| `sus_climate_normals_meta()` | `sus_climate_normals.R` | `stations`, `lang` | metadata tibble | Lists available normals per station |
| `sus_climate_anomaly()` | `sus_climate_anomaly.R` | `observed`, `normals`, `method`, `vars` | type="anomaly" | Climate anomalies (absolute / relative / standardised) |
| `sus_climate_aggregate()` | `sus_climate_aggregate.R` | health_data, climate_data, `climate_var`, `temporal_strategy`, `lag_days` | integrated climasus_df | Joins climate to health data with 10 temporal strategies |
| `sus_climate_compute_heatwaves()` | `sus_climate_compute_heatwaves.R` | climasus_df, `method`, `baseline_*`, `percentile` | climasus_hw list (`$events`, `$daily`, `$summary`) | Detects heatwaves (WHO, WMO, ECCA, local) |
| `sus_climate_compute_spi()` | `sus_climate_compute_spi.R` | climasus_df, `scale`, `lang` | SPI | Standardised Precipitation Index |
| `sus_climate_compute_spei()` | `sus_climate_compute_spei.R` | climasus_df, `scale`, `lang` | SPEI | Standardised Precipitation-Evapotranspiration Index |
| `sus_climate_compute_indicators()` | `sus_climate_compute_indicators.R` | climasus_df, `indicators`, `region`, `keep_source_vars` | wbgt_c, hi_c, utci_c, pet_c, cdd_c, hdd_c… | 15+ bioclimatic and heat-stress indices |

**Heatwave helpers:**

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `hw_get_events()` | `sus_climate_compute_heatwaves.R` | climasus_hw | events tibble | Extracts discrete heatwave events |
| `hw_count_by_year()` | `sus_climate_compute_heatwaves.R` | climasus_hw | annual tibble | Annual count of heatwave events and days |
| `hw_active_days()` | `sus_climate_compute_heatwaves.R` | climasus_hw | filtered tibble | Filters days with an active heatwave |

**Climate visualisation:**

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `sus_climate_plot_aggregate()` | `sus_climate_plot_aggregate.R` | climasus_df, `lang` | ggplot2 | Time series of climate variables |
| `sus_climate_plot_fill()` | `sus_climate_plot_fill.R` | climasus_df, `lang` | ggplot2 | Missing-data and gap-filling patterns |
| `sus_climate_plot_heatwaves()` | `sus_climate_plot_heatwaves.R` | climasus_hw, `lang` | ggplot2 | Heatwave timeline and frequency |

---

## 9. Gridded Data — Satellite / Reanalysis

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `sus_grid_chirps()` | `sus_grid_chirps.R` | `bbox`, dates, `lang` | CHIRPS precipitation | Daily rainfall at 0.05° |
| `sus_grid_era5()` | `sus_grid_era5.R` | `bbox`, `variables`, dates | ERA5 | Temperature, humidity, wind, radiation |
| `sus_grid_fires()` | `sus_grid_fires.R` | `bbox`, dates | fires | FIRMS/PRODES fire hotspots |
| `sus_grid_pdsi()` | `sus_grid_pdsi.R` | `bbox`, dates | PDSI | Palmer Drought Severity Index |
| `sus_grid_smvi()` | `sus_grid_smvi.R` | `bbox`, `indices`, dates | NDVI/NDWI/EVI | Vegetation and soil-moisture indices |
| `sus_grid_prodes()` | `sus_grid_prodes.R` | `bbox`, `year` | deforestation | Cumulative PRODES/INPE deforestation |
| `sus_grid_pollution_cams()` | `sus_grid_pollution_cams.R` | `bbox`, `pollutants`, dates | CAMS pollutants | Copernicus air quality |
| `sus_grid_pollution_ghap()` | `sus_grid_pollution_ghap.R` | `bbox`, dates | GHAP PM2.5 | Fine particulate matter |
| `sus_grid_pollution_merra2()` | `sus_grid_pollution_merra2.R` | `bbox`, `variables`, dates | MERRA-2 aerosols | NASA aerosols and gases |
| `sus_grid_join()` | `sus_grid_join.R` | health_df, grid_df, `by` | integrated climasus_df | Links gridded data to the dataset by municipality × date |

---

## 10. Socioeconomic Data — Census

| Function | File | Input | Output (stage) | Description |
|--------|---------|-------|----------------|-----------|
| `sus_census_select()` | `sus_census_explore.R` | `dataset`, `year`, `lang` | data.frame / vector | Browses available census variables (censobr) |
| `sus_socio_add_census()` | `sus_socio_add_census.R` | climasus_df, `variables`, `year` | `"census"` | Links income, education, and sanitation to the dataset |

---

## 11. Epidemiological Modelling

### 11a. Exposure–Response

| Function | File | Input | Output (S3) | Description |
|--------|---------|-------|-------------|-----------|
| `sus_mod_dlnm()` | `sus_mod_dlnm.R` | climasus_df, `outcome_col`, `climate_col`, `lag_max`, `argvar`, `arglag`, `family`, `covariates`, `ref_value`, `alpha`, `lang` | `climasus_dlnm` | DLNM — lag–exposure surface for climate–health association |
| `sus_mod_casecrossover()` | `sus_mod_casecrossover.R` | climasus_df, `outcome_col`, `exposure_col`, `stratum`, `lag`, `method`, `family`, `alpha`, `lang` | `climasus_casecrossover` | Time-stratified case-crossover; conditional Poisson or clogit |
| `sus_mod_its()` | `sus_mod_its.R` | climasus_df, `outcome_col`, `interruption_dates`, `harmonics`, `family`, `covariates`, `alpha`, `lang` | `climasus_its` | Interrupted time series for intervention evaluation |
| `sus_mod_excess()` | `sus_mod_excess.R` | climasus_df, `outcome_col`, `control_period`, `study_period`, `method`, `dof_per_year`, `family`, `alpha`, `lang` | `climasus_excess` | Excess mortality/morbidity via spline/Serfling counterfactual |

### 11b. Impact Analysis

| Function | File | Input | Output (S3) | Description |
|--------|---------|-------|-------------|-----------|
| `sus_mod_af()` | `sus_mod_af.R` | `climasus_dlnm`, `threshold`, `range`, `by`, `nsim`, `alpha`, `lang` | `climasus_af` | Attributable fraction and number for heat/cold from the DLNM |
| `sus_mod_burden()` | `sus_mod_burden.R` | list of `climasus_dlnm`, `component`, `rank_by`, `top_n`, `nsim`, `alpha`, `lang` | `climasus_burden` | Disease burden and city ranking; concentration curves |

### 11c. Multi-city Pooling

| Function | File | Input | Output (S3) | Description |
|--------|---------|-------|-------------|-----------|
| `sus_mod_pool()` | `sus_mod_pool.R` | list of `climasus_dlnm`, `exposure_range`, `n_grid`, `blup`, `method`, `lang` | `climasus_pool` | Two-stage pooling via mvmeta |
| `sus_mod_metaregression()` | `sus_mod_metaregression.R` | list of `climasus_dlnm`, `covariates`, `covariate_cols`, `blup`, `method`, `lang` | `climasus_metaregression` | Meta-regression with city-level covariates; predictive BLUPs |

### 11d. Machine Learning and Sensitivity

| Function | File | Input | Output (S3) | Description |
|--------|---------|-------|-------------|-----------|
| `sus_mod_ml()` | `sus_mod_ml.R` | climasus_df, `outcome_col`, `feature_cols`, `id_col`, `objective`, `nrounds`, `seed`, `lang` | `climasus_ml` | XGBoost for health-outcome prediction |
| `sus_mod_sensitivity()` | `sus_mod_sensitivity.R` | list of `climasus_dlnm`, `hot_percentile`, `cold_percentile`, `stratum_labels`, `alpha`, `lang` | `climasus_sensitivity` | RR comparison across vulnerability strata |

---

## 12. Spatial and Spatiotemporal Epidemiology

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `sus_mod_spatial_weights()` | `sus_mod_spatial_weights.R` | sf (climasus_df), `style`, `order` | spatial weights | Contiguity/distance matrix |
| `sus_mod_spatial_moran()` | `sus_mod_spatial_moran.R` | climasus_df (spatial), weights, `var_col` | global Moran's I and LISA | Spatial autocorrelation |
| `sus_mod_spatial_scan()` | `sus_mod_spatial_scan.R` | climasus_df, `outcome_col`, `pop_col`, `method` | clusters | Spatial scan statistic |
| `sus_mod_spatial_bayes()` | `sus_mod_spatial_bayes.R` | climasus_df (spatial), `outcome_col`, `model` | Bayesian smoothing | BYM / ICAR models |
| `sus_mod_spatial_reg()` | `sus_mod_spatial_reg.R` | climasus_df (spatial), `formula`, `type` | SAR / SEM / SDM | Spatial regression |
| `sus_mod_spacetime_bayes()` | `sus_mod_spacetime_bayes.R` | climasus_df (spatial+temporal), `model` | INLA / Stan | Bayesian spatiotemporal model |
| `sus_mod_spacetime_exceedance()` | `sus_mod_spacetime_exceedance.R` | `climasus_spacetime_bayes`, `threshold` | probabilities | Spatiotemporal exceedances |
| `sus_mod_spacetime_predict()` | `sus_mod_spacetime_predict.R` | `climasus_spacetime_bayes`, `newdata` | predictions | Spatiotemporal predictions |
| `sus_mod_vulnerability_index()` | `sus_mod_vulnerability_index.R` | climasus_df, `exposure_cols`, `sensitivity_cols`, `adaptive_cols`, `method` | composite index | Climate Vulnerability Index |
| `sus_mod_swot()` | `sus_mod_swot.R` | climasus_df, `dimensions`, `lang` | `climasus_swot` | SWOT analysis of the health system under climate change |

---

## 13. Model Visualisation

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `sus_mod_plot_dlnm()` | `sus_mod_plot_dlnm.R` | `climasus_dlnm`, `type` (surface/exposure/lag) | ggplot2 / plotly | Exposure–response, lag–response, 3D surface |
| `sus_mod_plot_af()` | `sus_mod_plot_af.R` | `climasus_af` | ggplot2 | Attributable fraction by period and threshold |
| `sus_mod_plot_burden()` | `sus_mod_plot_burden.R` | `climasus_burden` | ggplot2 | Disease-burden ranking; Lorenz curve |
| `sus_mod_plot_pool()` | `sus_mod_plot_pool.R` | `climasus_pool`, `type` | ggplot2 | Pooled curves + BLUPs; forest plot |
| `sus_mod_plot_ml()` | `sus_mod_plot_ml.R` | `climasus_ml`, `type` (importance/shap/pred) | ggplot2 | Variable importance and SHAP values |
| `sus_mod_plot_sensitivity()` | `sus_mod_plot_sensitivity.R` | `climasus_sensitivity` | ggplot2 | RR heatmap across vulnerability strata |
| `sus_mod_plot_spatial_bayes()` | `sus_mod_plot_spatial_bayes.R` | `climasus_spatial_bayes` | ggplot2 / tmap | Bayesian smoothed maps (SIR, relative risk) |
| `sus_mod_plot_spatial_moran()` | `sus_mod_plot_spatial_moran.R` | `climasus_moran` | ggplot2 | Moran scatterplot and LISA map |
| `sus_mod_plot_spatial_scan()` | `sus_mod_plot_spatial_scan.R` | `climasus_scan` | ggplot2 / tmap | Map of detected clusters |
| `sus_mod_plot_spacetime()` | `sus_mod_plot_spacetime.R` | `climasus_spacetime_bayes`, `type` | ggplot2 / animation | Spatiotemporal animation / facet map |
| `sus_mod_plot_vulnerability()` | `sus_mod_plot_vulnerability.R` | climasus_df (vulnerability) | ggplot2 / tmap | Vulnerability maps and profiles |
| `sus_mod_plot_swot()` | `sus_mod_plot_swot.R` | `climasus_swot` | ggplot2 | SWOT quadrant |

---

## 14. Infrastructure and Utilities

| Function | File | Input | Output | Description |
|--------|---------|-------|--------|-----------|
| `sus_as_arrow()` | `utils-S3.R` | climasus_df | Arrow Table (lazy) | Converts to the Arrow/Parquet backend |
| `sus_as_duckdb()` | `utils-S3.R` | climasus_df | DuckDB relation | Converts to the DuckDB backend |
| `write_parquet_climasus()` | `sus_data_export.R` | climasus_df, `path` | .parquet file | Serialises with metadata embedded in the Arrow schema |
| `write_duckdb_climasus()` | `sus_data_export.R` | climasus_df, `con`, `name` | DuckDB table | Persists with a companion metadata table |
| `sus_cache_clear()` | `utils.R` | — | logical | Clears the global download cache |
| `sus_cache_info()` | `utils.R` | — | list | Cache usage statistics |
| `sus_install_deps()` | `utils.R` | `packages`, `lang` | logical | Installs optional dependencies (sf, arrow, censobr…) |
| `%>%` | `utils-pipe.R` | — | chained object | Re-export of the magrittr pipe |

---

## Summary by Pipeline Stage

| # | Stage | Key functions | Purpose |
|---|---------|---------------|-----------|
| 1 | **Import** | `sus_data_import`, `sus_data_read` | Download/read raw SUS data |
| 2 | **Cleaning** | `sus_data_clean_encoding`, `sus_data_standardize` | Prepare data |
| 3 | **Filtering** | `sus_data_filter_cid`, `sus_data_filter_demographics` | Select subsets |
| 4 | **Derivation** | `sus_data_create_variables`, `sus_data_aggregate` | Compute variables and aggregate |
| 5 | **Spatial** | `sus_spatial_join` | Link to geometries |
| 6 | **Climate (stations)** | `sus_climate_inmet`, `sus_climate_aggregate`, `sus_climate_compute_*` | Integrate INMET |
| 7 | **Climate (gridded)** | `sus_grid_chirps`, `sus_grid_era5`, `sus_grid_pollution_*` | Integrate satellite/reanalysis |
| 8 | **Socioeconomic** | `sus_socio_add_census` | Link census |
| 9 | **Modelling** | `sus_mod_dlnm`, `sus_mod_casecrossover`, `sus_mod_ml`, `sus_mod_spatial_*` | Epidemiological analysis |
| 10 | **Visualisation** | `sus_mod_plot_*`, `sus_data_plot_*`, `sus_climate_plot_*` | Plots and maps |
| 11 | **Export** | `sus_data_export`, `write_parquet_climasus` | Persist results |

---

## Funding

The climasus4r project is funded by the Brazilian Ministry of Health, the Oswaldo Cruz Foundation Rondônia (FIOCRUZ-RO / CCSRO), and the **National Institute of Science and Technology for Research and Knowledge of Excellence in the Western Amazon - INCT-CONEXAO**. Support covers:

* **Permanent Climatology Unit (NPC)** INCT-CONEXÃO
* **Advanced Climatology Unit (NAC)** INCT-CONEXÃO


---

## Acknowledgements

- The **microdatasus** team for the foundational data-import infrastructure
- **DATASUS** for providing open access to Brazilian health data
- **INCT Conexão - Amazônia** for funding and support
- All contributors and testers who helped improve the package


---

## Inspiration

- R package [`microdatasus`](https://github.com/rfsaldanha/microdatasus)

---

## Contributing

See the contributing guide in [`vignettes-en/contributing.Rmd`](vignettes-en/contributing.Rmd) (also available in [Portuguese](vignettes-pt/contributing.Rmd) and [Spanish](vignettes-es/contributing.Rmd)).

## Contact

- **Maintainer**: Max Anjos
- **Email**: maxanjos@campus.ul.pt
- **GitHub**: [https://github.com/ByMaxAnjos/climasus4r](https://github.com/ByMaxAnjos/climasus4r)
- **Issues**: [https://github.com/ByMaxAnjos/climasus4r/issues](https://github.com/ByMaxAnjos/climasus4r/issues)
