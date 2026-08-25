---
title: 'climasus4r: An R Package for Reproducible Climate-Health Data Integration and Epidemiological Analysis in Brazil'
tags:
  - R
  - climate change
  - epidemiology
  - environmental health
  - DATASUS
  - distributed lag non-linear models
  - reproducibility
authors:
  - name: Max Anjos
    orcid: 0000-0001-6394-1324
    corresponding: true
    affiliation: 1
  - name: Thaua Menezes
    orcid: 0000-0002-3015-2270
    affiliation: 2
  - name: Marlon Faria
    orcid: 0000-0002-9485-4079
    affiliation: 3
affiliations:
  - name: Federal University of Juiz de Fora (UFJF), Brazil
    index: 1
  - name: São Paulo State University (UNESP), Brazil
    index: 2
  - name: University of São Paulo (USP), Brazil
    index: 3
date: 16 August 2026
bibliography: paper.bib
---

# Summary

`climasus4r` is an R package for reproducible climate-health research using Brazilian public health, environmental, and socioeconomic data. Based on the reproducible analytical pipeline approach,  the package integrates six Brazilian Health System DATASUS subsystems (SIM, SIH, SIA, SINAN, CNES, and SINASC) to meteorological station observations, gridded cate products, pollution data, and census indicators. It then carries the integrated dataset into epidemiological designs commonly used in environmental health, including distributed lag non-linear models (DLNM), case-crossover and interrupted time-series analyses, attributable-fraction and burden estimation, multi-city meta-analysis, and spatial-temporal risk modelling.
The package's functions  are assigned with the sistema S3, `climasus_df` a class that stores provenance metadata alongside the data. Source system, pipeline stage, spatial and temporal resolution, aggregation type, and processing history remain attached across the workflow (preparation, integration, and analysis/modelling) and can be preserved in the tibble, parquet and duckDB backends. This design makes analytic dataset auditable after intermediate storage, sharing, or reuse. Outputs and messages are available in Portuguese, Spanish, and English, supporting multilingual development teams and public-health users across Brazil, Latin America, and international collaboration. 

# Statement of need

Climate-health analyses in Brazil require more than fitting a regression model. Researchers must import administrative health records, resolve DATASUS encodings and coding conventions, harmonize municipal identifiers across time, link records to appropriate spatial units, choose exposure sources, construct biologically plausible lag windows, and document each transformation before model fitting [@bhaskaran2013; @gasparrini2010; @armstrong2006]. These steps are often repeated with project-specific scripts, making results difficult to audit, reproduce, or compare across studies.

Existing R packages solve important parts of the problem. `microdatasus` [@saldanha2019] downloads and parses DATASUS files. `dlnm` [@gasparrini2011] and `mvmeta` [@gasparrini2012] provide mature statistical machinery for distributed-lag modelling and pooling. `geobr` and `censobr` provide Brazilian geographic and census data. However, there is still a methodological gap between health-data import and model-ready exposure matrices: spatial linkage, temporal exposure construction, provenance tracking, and reproducible orchestration remain largely the user's responsibility.

`climasus4r` fills this middle layer. It formalizes the path from raw or standardized DATASUS records to climate-health analytic datasets and model objects. The package does not replace specialized tools such as `microdatasus`, `dlnm`, `mvmeta`, `geobr`, or `censobr`; instead, it composes them into a coherent workflow adapted to Brazilian data infrastructure. This is especially important because DATASUS conventions, INMET station coverage, municipality boundaries, and regional climate regimes vary substantially across Brazil. A generic pipeline can therefore produce incorrect exposure assignments or biologically implausible lag definitions without obvious errors.

The main scientific contribution of the package is to make exposure construction explicit and reproducible. Rather than hiding temporal alignment inside a preprocessing script, `climasus4r` exposes named strategies that correspond to distinct epidemiological hypotheses: same-day effects, fixed delays, cumulative moving windows, incubation windows, distributed lags, degree-days, seasonal summaries, heat and cold extremes, and decay-weighted exposure. These choices are recorded in metadata, allowing analysts and reviewers to inspect not only the final model but the construction of the analytic dataset that made the model possible.

# Software architecture

\autoref{fig:pipeline} summarizes the package architecture. **Preparation** functions import DATASUS records (`sus_data_import()`), correct character encodings (`sus_data_clean_encoding()`), standardize names and codes (`sus_data_standardize()`), filter outcomes and demographic strata (`sus_data_filter_cid()`, `sus_data_filter_demographics()`), derive variables (`sus_data_create_variables()`), and aggregate health series (`sus_data_aggregate()`). **Integration** functions add spatial structure (`sus_spatial_join()`), socioeconomic context (`sus_socio_add_census()`), station climate (`sus_climate_inmet()`, `sus_climate_fill_inmet()`), and gridded exposures (`sus_grid_era5()`, `sus_grid_chirps()`, `sus_grid_pollution_cams()`, among others). `sus_climate_aggregate()` and `sus_grid_join()` then construct model-ready exposure variables. **Analysis** functions fit DLNM, case-crossover, interrupted time-series, attributable-burden, multi-city, machine-learning, spatial, and vulnerability models, with companion plotting functions for diagnostics and communication.

The architecture follows three principles. First, public functions return standard R objects whenever possible, so users can continue with base R, `dplyr`, `ggplot2`, `sf`, `dlnm`, or other ecosystem tools. Second, `climasus_df` metadata records the pipeline state without forcing users into a closed framework. Third, reproducible analytical pipeline (RAP) functions (`sus_rap_export()`, `sus_rap_read()`, `sus_rap_targets()`) allow an analysis to be serialized, inspected, re-run with new parameters, or translated into a `targets` workflow [@landau2021].

![Three-stage `climasus4r` workflow. DATASUS records are prepared, linked to socioeconomic and environmental exposures, analysed with climate-health models, and optionally exported as a `targets`-based Reproducible Analytical Pipeline.\label{fig:pipeline}](figures/pipeline_overview.png)

# Temporal exposure construction

The central methodological function is `sus_climate_aggregate()`. It links health events or aggregated health time series to climate data using one of ten temporal-alignment strategies (\autoref{fig:strategies}). Each strategy represents a different exposure-lag assumption and produces an auditable `climasus_df` output. Thresholds and base temperatures can be set by the user and, where appropriate, adapted to broad Brazilian climate regions (tropical, subtropical, and temperate).

| Strategy | Exposure window | Epidemiological rationale |
|---|---|---|
| `exact` | Same day ($t$) | Acute effects such as heat stroke or hemorrhagic stroke |
| `discrete_lag` | Single fixed lag ($t-L$) | Known delay, for example vector-borne incubation |
| `moving_window` | Mean or sum over $(t-W, t)$ | Cumulative exposure without a specific lag hypothesis |
| `offset_window` | Mean or sum over $(t-W_2, t-W_1)$ | Incubation period with recent days excluded |
| `distributed_lag` | Lag matrix from $0$ to $L$ | Unknown lag shape; input for `dlnm::crossbasis()` |
| `degree_days` | Accumulated units above a base temperature over $W$ days | Vector, pathogen, or thermal-development processes |
| `seasonal` | Climatological season mean (DJF/MAM/JJA/SON) | Long-term or ecological associations |
| `threshold_exceedance` | Count of days above a percentile or absolute threshold over $W$ days | Heatwave or hot-day definitions |
| `cold_wave_exceedance` | Count of days below a percentile or absolute threshold over $W$ days | Cold-spell definitions, especially in southern Brazil |
| `weighted_window` | Decay-weighted mean over $(t-W, t)$ | Gradual physiological or ecological accumulation of exposure |

![Decision map for the ten temporal-alignment strategies in `sus_climate_aggregate()`. The user chooses a biological or epidemiological exposure-lag mechanism, and the function returns a `climasus_df` with the matching climate aggregation recorded in metadata.\label{fig:strategies}](figures/temporal_strategies.png)

# Validation and reproducibility

The package validation focuses on software correctness and methodological equivalence, not on claiming a new epidemiological effect from example data. The testable claim is that `climasus4r` implements established climate-health specifications in a reproducible pipeline: health records can be prepared, linked to exposure data, transformed into lagged or windowed exposure structures, passed to modelling functions, and returned with inspectable diagnostics and provenance.

Model specifications were checked against published environmental epidemiology workflows. The default DLNM structure in `sus_mod_dlnm()` follows the quasi-Poisson crossbasis approach used in temperature-mortality studies [@gasparrini2010; @gasparrini2015]. Multi-city pooling in `sus_mod_pool()` follows the multivariate meta-analytic strategy implemented by `mvmeta` [@gasparrini2012]. Attributable-fraction and burden functions follow the minimum-mortality-temperature and attributable-risk framework used in large-scale temperature-mortality assessments [@gasparrini2015; @zhao2021]. Heat and cold exceedance strategies support percentile-based definitions consistent with Brazilian heatwave applications [@geirinhas2021], while degree-day options support threshold-based vector and pathogen development applications such as those described for *Aedes aegypti* thermal suitability [@lee2021].

End-to-end examples are provided as reproducible fixtures and vignettes. They are intended to verify execution, object structure, convergence diagnostics, plotting methods, and metadata propagation across the full workflow. When example exposures are simulated or reduced for portability, the documentation treats them as software validation examples rather than substantive estimates. This separation is deliberate: the package provides an auditable computational framework, while scientific inference remains the responsibility of analyses using complete, outcome-appropriate, and adequately powered datasets.

![Six-city DLNM pooling on a reproducible package example. City-specific and pooled best linear unbiased predictions are shown with the pooled exposure-response curve. The figure demonstrates the `sus_mod_dlnm()` to `sus_mod_pool()` workflow and associated diagnostics, not a standalone epidemiological estimate.\label{fig:pooled}](figures/validation_pooled_cities.png)

# State of the field

Several R packages are indispensable for climate-health research, but they usually address one layer of the workflow. `microdatasus` [@saldanha2019] provides DATASUS access; `dlnm` [@gasparrini2011] and `mvmeta` [@gasparrini2012] provide modelling tools; `geobr` and `censobr` provide Brazilian geographic and socioeconomic inputs. `climasus4r` contributes the missing integration layer: a metadata-aware pipeline that links Brazilian health microdata to environmental exposures and carries the resulting objects into established epidemiological models.

By standardizing this integration layer, `climasus4r` lowers the cost of transparent climate-health analyses in Brazil. It also makes methodological choices visible: what data system was used, how locations were linked, which exposure source was chosen, what lag structure was assumed, and how the analysis can be re-run. These features are essential for policy-relevant research in a setting where climate hazards, health vulnerability, and administrative data quality vary sharply across regions.




# Acknowledgements

We thank the INCT Conexão – Amazônia project for supporting the development of this package, and the maintainers of `microdatasus` and `dlnm`, on whose work `climasus4r` directly depends.

# References
