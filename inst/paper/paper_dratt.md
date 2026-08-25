Summary

`climasus4r` (v.1.0.0) is an R package that unifies data preparation, multi-source integration, and climate–health analysis and modelling end-to-end in a single reproducible workflow for Brazil. Built around the reproducible analytical pipeline (RAP) paradigm, the package links six Brazilian Unified Health System (SUS/DATASUS) subsystems (SIM, SIH, SIA, SINAN, CNES, and SINASC) to meteorological point-station observations, gridded climate products, air-pollution data, land-cover and territorial coverage, and socioeconomic indicators. The integrated dataset then feeds directly into epidemiological approaches commonly used in environmental health, including distributed lag non-linear models (DLNM), case-crossover and interrupted time-series designs, attributable-fraction and burden estimation, multi-city meta-analysis, and spatial-temporal risk modelling. More than 80 functions operate on a single S3 class, `climasus_df`, which carries provenance metadata (source system, pipeline stage, spatial and temporal resolution, aggregation type, and processing history) alongside the data itself, so that every analytic dataset remains fully auditable after intermediate storage, sharing, or reuse. Native Apache Parquet and DuckDB backends let `climasus4r` scale to large national datasets without leaving R, and all outputs and messages are available in Portuguese, Spanish, and English, supporting multilingual research teams and public-health users across Brazil, Latin America, and international collaborations.

Statement of Need

Climate change is intensifying the burden of climate-sensitive diseases worldwide (Romanello et al., 2021), including vector-borne infections such as dengue, whose risk in Brazil is amplified by hydrometeorological hazards and urbanisation (Lowe et al., 2021); heat-related mortality, which already accounts for a substantial share of global temperature-attributable deaths (Zhao et al., 2021) and is projected to increase further under future climate scenarios (Gasparrini et al., 2017); and the direct and indirect health consequences of extreme weather events more broadly. Spanning five major biomes and home to more than 210 million people, Brazil is among the countries most exposed to these risks in Latin America (Confalonieri et al., 2009).

Brazil is not short of data: DATASUS, INMET, PRODES, IBGE, ERA5-Land, MERRA-2, CAMS, and CHIRPS together provide an unusually rich public health, climate, and environmental record. What limits their scientific use is technical fragmentation. Research teams routinely duplicate the same extraction, cleaning, and harmonization steps in project-specific scripts, which lowers reproducibility and makes results difficult to compare across studies. Integrating these sources is also genuinely hard, because they differ in spatial and temporal resolution: INMET, for instance, provides point-based hourly, daily, and monthly station readings, whereas ERA5-Land offers gridded daily fields at roughly 9 km resolution, and reconciling the two requires explicit, defensible choices about spatial linkage and exposure-lag construction. The gap, in other words, is not the absence of data but the absence of a toolbox that carries analysts from raw multi-source records to model-ready, reproducible climate–health datasets.

`climasus4r`'s main scientific contribution is to turn the fragmented, often irreproducible practice of climate–health research in Brazil into a single, transparent, and reusable pipeline — one general enough to extend readily to other low- and middle-income countries facing similar data barriers.

State of the Field

Existing R packages address parts of this workflow, but each covers only one layer. `microdatasus` automates the download and pre-processing of DATASUS microdata but stops short of climate integration or epidemiological modelling (Saldanha et al., 2019); `datasus` covers only pre-aggregated TABNET data (Prado Siqueira, 2024); and `geobr` (Pereira & Gonçalves, 2022) and `censobr` (Pereira & Barbosa, 2023) supply Brazilian geographic and census data without linking them to climate or health exposures. On the modelling side, `dlnm` (Gasparrini, 2011) and `mvmeta` (Gasparrini & Armstrong, 2013) provide robust machinery for distributed-lag models and multi-site pooling, but both expect an already-assembled exposure dataset as input. More recent global pipelines, such as the Python framework of Dasgupta et al. (2025), offer broad multi-country, outcome-agnostic coverage but do not integrate Brazilian health-system data or distributed-lag modelling, leaving the gap this package targets unaddressed.

`climasus4r` does not replace these tools; it composes them into a coherent workflow tailored to Brazilian data infrastructure — a design choice that matters because DATASUS coding conventions, INMET station coverage, municipal boundaries, and regional climate regimes all vary substantially across the country. Concretely, the package chains a sequence of functions that import DATASUS records, clean and standardize their categorical variables, filter by CID-10 diagnosis and demographic profile, link geographic and socioeconomic indicators, aggregate health and climate data spatiotemporally, and hand the result to epidemiological modelling functions — with every step recorded in the `climasus_df` metadata so the analysis remains self-documenting and auditable end to end. As in the Summary, native Parquet and DuckDB backends and full Portuguese/Spanish/English output make the package scalable and accessible to multilingual research teams and public-health users throughout Brazil, Latin America, and beyond.

Software design

Figure 1 presents the architecture of `climasus4r` as a reproducible workflow for climate-sensitive health research using Brazilian public health data. The package is organised around three sequential layers — data preparation, exposure and covariate integration, and epidemiological analysis and modelling — each paired with companion diagnostic and visualisation functions, and wrapped by an optional reproducible analytical pipeline (RAP) export.

The data preparation layer imports health records from DATASUS information systems, including SIM, SIH, SIA, SINAN, CNES and SINASC, using `sus_data_import()`. These records are then harmonised through encoding correction, nomenclature standardisation and administrative-code cleaning with `sus_data_clean_encoding()` and `sus_data_standardize()`. Case definitions are constructed by filtering ICD-10 outcomes and demographic strata using `sus_data_filter_cid()` and `sus_data_filter_demographics()`, a step that can be inspected with the companion diagnostic `sus_data_plot_demographics()`. Derived variables and municipality-level health time series are generated with `sus_data_create_variables()` and `sus_data_aggregate()`, and can be visualised directly with `sus_data_plot_aggregate_ts()` and `sus_data_plot_aggregate_map()`.

The exposure and covariate integration layer links health outcomes to spatial, socioeconomic and environmental information. Spatial joins between municipalities, health records and monitoring stations are handled by `sus_spatial_join()`, while census-derived socioeconomic indicators are added with `sus_census_join()`. Observed climate data from INMET stations are retrieved and gap-filled using `sus_climate_inmet()` and `sus_climate_fill_inmet()`, with fit and extremes inspected through `sus_climate_plot_fill()`, `sus_climate_plot_heatwaves()` and `sus_climate_plot_coldwaves()`. Gridded environmental exposures, including ERA5, CHIRPS, air pollution and land-cover products, are processed with `sus_grid_era5()`, `sus_grid_chirps()`, `sus_grid_pollution_cams()` and `sus_grid_join()`, and mapped with `sus_grid_plot()`. Temporally aligned exposure records and analytical windows are then constructed with `sus_climate_aggregate()` — described in the Temporal exposure construction section below — and can be checked with `sus_climate_plot_aggregate()`.

The epidemiological analysis and modelling layer supports exposure-response estimation, quasi-experimental designs, health-impact assessment, evidence synthesis and spatial inference. Distributed lag non-linear models are fitted with `sus_mod_dlnm()` and diagnosed with `sus_mod_plot_dlnm()`, while case-crossover and interrupted time-series designs are implemented through `sus_mod_casecrossover()` and `sus_mod_its()`. Attributable fractions, burden estimates and excess events are estimated with `sus_mod_af()`, `sus_mod_burden()` and `sus_mod_excess()`, and visualised with `sus_mod_plot_af()` and `sus_mod_plot_burden()`. Multi-city pooled estimates are obtained with `sus_mod_pool()`, with pooling behaviour and sensitivity checked via `sus_mod_plot_pool()` and `sus_mod_plot_sensitivity()`. Spatial risk analyses are supported by `sus_mod_spatial_moran()`, `sus_mod_spatial_scan()`, `sus_mod_spatial_bayes()` and `sus_mod_vulnerability_index()`, each with a matching diagnostic map (`sus_mod_plot_spatial_moran()`, `sus_mod_plot_spatial_scan()`, `sus_mod_plot_spatial_bayes()`, `sus_mod_plot_vulnerability()`).

All three layers share a structured `climasus_df` metadata object, accessed through `sus_meta()`. Every preparation, integration and modelling step writes its provenance — data source, processing stage, and transformation history — into this object, and the diagnostic and visualisation functions read it back to label and dispatch plots correctly. This two-way flow keeps every analytic dataset auditable end to end without forcing users into a closed framework.

Reproducible analytical pipeline functions, `sus_rap_recipe()`, `sus_rap_targets()` and `sus_rap_export()`, are an optional step built on top of this metadata: they allow a completed analysis to be serialised, inspected, re-run with updated parameters, and translated into a `targets` workflow when full pipeline reproducibility is required.

Temporal exposure construction strategies

The methodological core of the exposure and covariate integration layer is `sus_climate_aggregate()`, the function that turns raw climate series into the lagged, windowed, or thresholded exposure variables that epidemiological models actually require. Rather than leaving this choice implicit in a preprocessing script, the function exposes ten named strategies, each corresponding to a distinct exposure–lag hypothesis rather than to a generic smoothing option (Table 1). Selecting a strategy is therefore a substantive epidemiological decision, not a technical default, and the choice is recorded in the `climasus_df` metadata so that it can be audited alongside the model it feeds.

The ten strategies cover the range of exposure assumptions used in environmental epidemiology, and each is anchored to a published design rather than to a generic smoothing choice. `exact` captures same-day, acute effects such as heat stroke or cardiovascular death, consistent with same-day and case-crossover mortality designs applied to Brazilian cities (Bell et al., 2008; Son et al., 2016). `discrete_lag` and `distributed_lag` represent, respectively, a single known delay and an unconstrained lag shape suitable as input to `dlnm::crossbasis()`, following the distributed lag non-linear modelling framework developed for temperature- and pollution-mortality series (Armstrong, 2006; Gasparrini et al., 2010; Gasparrini, 2011). `moving_window` and `offset_window` construct cumulative exposure with and without excluding the most recent days, matching incubation-period assumptions used in dengue climate-suitability models for Brazil, where transmission risk depends on a window of past temperature and rainfall rather than on a single day (Lee et al., 2021). `degree_days` accumulates thermal or vector-development units above a user-defined base temperature, operationalising the developmental thermal threshold and physiological-time estimates derived experimentally for *Aedes aegypti* immature stages (Grech et al., 2015). `seasonal` summarises climatological means for long-term or ecological associations, as used in spatio-temporal analyses linking visceral leishmaniasis incidence to seasonal and interannual climate variation across Brazilian biomes (Kersul et al., 2025). `threshold_exceedance` and `cold_wave_exceedance` count days above or below a percentile or absolute threshold, giving explicit heatwave and cold-spell definitions modelled directly on cause-specific mortality risk under multiple wave definitions in São Paulo (Geirinhas et al., 2021; Moraes et al., 2022). `weighted_window` applies a decay-weighted mean for exposures that accumulate gradually, following the harvesting-resistant, weighted distributed-lag approach originally proposed for air-pollution mortality series (Bhaskaran et al., 2013; Zeger et al., 1999). Thresholds and base temperatures default to values reported for Brazilian tropical, subtropical, and temperate regions, but can be overridden by the user for a specific outcome or study population.

Because every strategy returns an auditable `climasus_df`, the same health outcome can be re-aggregated under competing lag hypotheses without re-writing the upstream import, cleaning, or linkage steps — for example, comparing a `discrete_lag` specification against a `distributed_lag` crossbasis for the same dengue time series is a one-line change in `sus_climate_aggregate()`, not a new script. This makes lag selection a transparent, testable part of the analysis rather than a hidden preprocessing choice.

| Strategy | Exposure window | Epidemiological rationale |
|---|---|---|
| `exact` | Same day ($t$) | Acute effects such as heat stroke or haemorrhagic stroke |
| `discrete_lag` | Single fixed lag ($t-L$) | Known delay, for example vector-borne incubation |
| `moving_window` | Mean or sum over $(t-W, t)$ | Cumulative exposure without a specific lag hypothesis |
| `offset_window` | Mean or sum over $(t-W_2, t-W_1)$ | Incubation period with recent days excluded |
| `distributed_lag` | Lag matrix from $0$ to $L$ | Unknown lag shape; input for `dlnm::crossbasis()` |
| `degree_days` | Accumulated units above a base temperature over $W$ days | Vector, pathogen, or thermal-development processes |
| `seasonal` | Climatological season mean (DJF/MAM/JJA/SON) | Long-term or ecological associations |
| `threshold_exceedance` | Count of days above a percentile or absolute threshold over $W$ days | Heatwave or hot-day definitions |
| `cold_wave_exceedance` | Count of days below a percentile or absolute threshold over $W$ days | Cold-spell definitions, especially in southern Brazil |
| `weighted_window` | Decay-weighted mean over $(t-W, t)$ | Gradual physiological or ecological accumulation of exposure |

Tutorial

The package includes an extensive Portuguese tutorial collection in `vignettes-pt/`, organised around the same workflow described in this paper: stepwise DATASUS import, encoding correction, filtering, variable construction, spatial and socioeconomic linkage, climate integration, and modelling; thematic tutorials on climate indicators and heat/cold events; applied climate-epidemiology case studies; and reproducible analytical pipeline examples. Together, these vignettes provide a practical bridge between the package's API and full analyses, allowing users to reproduce each stage of the `climasus4r` workflow before adapting it to their own health outcome, region, exposure source, and modelling design.

Reproducible worked example

The pipeline below is a self-contained, end-to-end illustration of the workflow described above, from raw SIM-DO import to a fitted distributed lag non-linear model. It exists purely to validate that `climasus4r`'s pipeline stages compose correctly — that the object returned by each function is exactly what the next function requires — and every number it produces should be read in that light. It is not, and is not intended to be, a substantive epidemiological finding, for reasons intrinsic to the example rather than to the software: a single municipality, a single narrow age band, and a single climate station necessarily yield low statistical power, and the fitted model below shows exactly that. The ten-year window (2010–2019) was chosen because DLNM exposure- and lag-response splines are conventionally fitted over a decade or more to stabilise estimates at the temperature-distribution tails (Gasparrini et al., 2015), and because a decade of daily station and mortality records demonstrates the package's Parquet/DuckDB scalability claim; a shorter window would only sharpen the point-versus-uncertainty contrast discussed below, not remove it. This code was executed against production DATASUS and INMET services while preparing this manuscript, and every intermediate object was inspected by hand; readers adapting it for their own analysis should still independently confirm that the study period, age stratum, climate station, and spline degrees of freedom are appropriate for their outcome and location.

```r
library(climasus4r)

# 1. Import, clean, standardise, filter respiratory ICD-10, derive variables
health_clean <- sus_data_import(uf = "SP", year = 2010:2019, system = "SIM-DO") |>
  sus_data_clean_encoding() |>
  sus_data_standardize() |>
  sus_data_filter_cid(disease_group = "respiratory") |>
  sus_data_create_variables(create_age_groups = TRUE, age_breaks = c(0, 5, 15, 60, Inf),
                             create_calendar_vars = TRUE)

# 2. Descriptive figures: children under 5, weekly, state-wide by municipality
health_df <- health_clean |>
  sus_data_filter_demographics(age_range = c(0, 5)) |>
  sus_data_aggregate(time_unit = "week", group_by = "codigo_municipio_residencia")

fig_1 <- sus_data_plot_aggregate_ts(health_df, plot_type = c("trend", "seasonal"),
                                     city = "São Paulo", log_transform = TRUE)

fig_2 <- sus_data_plot_aggregate_map(health_df, map_type = "bubble",
                                      rate_per_100k = TRUE, log_scale = TRUE,
                                      show_labels = FALSE)

# 3. Exposure integration: daily counts (São Paulo city) + INMET climate.
# station_code = "A701" (Mirante de Santana) is set explicitly because
# sus_climate_aggregate() always selects the geometrically nearest station,
# which for São Paulo city is a newer automatic station with only ~2 years
# of record; A701 is the closest station with full decade coverage.
sp_daily <- health_clean |>
  sus_data_filter_demographics(age_range = c(0, 5), city = "São Paulo") |>
  sus_data_aggregate(time_unit = "day", group_by = "codigo_municipio_residencia",
                      complete_dates = TRUE) |>
  sus_spatial_join(level = "munic")

clima_sp <- sus_climate_inmet(years = 2010:2019, uf = "SP", station_code = "A701") |>
  sus_climate_fill_inmet(target_var = "tair_dry_bulb_c")

exposicao_dl <- sus_climate_aggregate(
  health_data = sp_daily, climate_data = clima_sp, climate_var = "tair_dry_bulb_c",
  temporal_strategy = "distributed_lag", lag_days = 21
)

# 4. Fit DLNM and plot the cumulative exposure-response curve
fit_dlnm <- sus_mod_dlnm(df = exposicao_dl, lag_max = 21,
                          argvar = list(fun = "ns", df = 4),
                          arglag = list(fun = "ns", df = 3))

fig_3 <- sus_mod_plot_dlnm(fit_dlnm, type = "overall")
```

Running this pipeline end to end (N = 3,626 daily observations after the 21-day lag truncation) returns a dispersion ratio of 0.97 — close enough to 1 that the quasi-Poisson family was a conservative rather than a necessary choice — and a cumulative rate ratio comparing the 75th temperature percentile to the median of 0.84 (95% CI: 0.56–1.26), with peak lag at 11 days. The point estimate points in a biologically plausible direction (cold as the dominant respiratory stressor in young children, rather than heat), but the confidence interval spans from a strong apparent protective effect to a modest apparent harmful one — it is not statistically distinguishable from no effect. This is the expected, correct behaviour of a DLNM fitted to a single city, a single five-year age band, and one climate station over one decade, not a limitation of the software: it is precisely why this worked example is presented as a validation of the pipeline's mechanics — import through to a fitted crossbasis — and not as evidence about the temperature-respiratory mortality relationship in São Paulo's children.

Figure 1. Trend and seasonal decomposition of weekly respiratory deaths in children under five, São Paulo city, 2010–2019 (log-transformed y-axis). Left/top panel: ten-year trend, smoothed to show the long-term pattern. Bottom panel: climatological seasonal pattern across calendar months, the within-year complement to the trend panel.

Figure 2. Incidence of respiratory deaths per 100,000 children under five, by municipality, São Paulo state, 2010–2019 (bubble map, log colour/size scale). Population standardisation (rather than raw counts) avoids São Paulo city mechanically dominating the map by population size alone, so the panel reflects relative risk rather than volume.

Figure 3. Cumulative exposure-response curve from the distributed lag non-linear model (`sus_mod_dlnm()`) relating daily mean temperature to respiratory mortality in children under five, São Paulo city, 2010–2019, over a 21-day lag window (cumulative RR at the 75th vs. median temperature percentile: 0.84, 95% CI 0.56–1.26; peak lag 11 days). The shaded band shows the 95% confidence interval; the marginal histogram shows the empirical temperature distribution over the study period. As throughout this manuscript, this curve is presented solely as a software-validation example — it demonstrates that the `climasus_df` produced by `sus_climate_aggregate(temporal_strategy = "distributed_lag")` feeds directly into `sus_mod_dlnm()` and `sus_mod_plot_dlnm()`, and its wide, non-significant confidence interval is the expected signature of a single-city, single-station, narrow-age-band series rather than a substantive epidemiological result. It should not be read as evidence about the temperature-mortality relationship in São Paulo without independent re-analysis on a study designed to answer that question.

References

Armstrong, B. (2006). Models for the relationship between ambient temperature and daily mortality. *Epidemiology, 17*(6), 624–631. https://doi.org/10.1097/01.ede.0000239732.50999.8f

Bell, M. L., O'Neill, M. S., Ranjit, N., Borja-Aburto, V. H., Cifuentes, L. A., & Gouveia, N. C. (2008). Vulnerability to heat-related mortality in Latin America: A case-crossover study in São Paulo, Brazil, Santiago, Chile and Mexico City, Mexico. *International Journal of Epidemiology, 37*(4), 796–804. https://doi.org/10.1093/ije/dyn094

Bhaskaran, K., Gasparrini, A., Hajat, S., Smeeth, L., & Armstrong, B. (2013). Time series regression studies in environmental epidemiology. *International Journal of Epidemiology, 42*(4), 1187–1195. https://doi.org/10.1093/ije/dyt092

Confalonieri, U. E. C., Marinho, D. P., & Rodriguez, R. E. (2009). Public health vulnerability to climate change in Brazil. *Climate Research, 40*(2–3), 175–186. https://doi.org/10.3354/cr00808

Dasgupta, A., Perez-Fernandez, I., Huynh, T., Mills, C., Nicholls, R. C., Sambaturu, P., Choisy, M., Wallom, D., Nguyen-Duy, T., Inward, R. P. D., Brittain, J.-S., Sparrow, S., & Kraemer, M. U. G. (2025). Scalable, open-access and multidisciplinary data integration pipeline for climate-sensitive diseases. *Wellcome Open Research, 10*, 467. https://doi.org/10.12688/wellcomeopenres.24774.2

Gasparrini, A., Armstrong, B., & Kenward, M. G. (2010). Distributed lag non-linear models. *Statistics in Medicine, 29*(21), 2224–2234. https://doi.org/10.1002/sim.3940

Gasparrini, A. (2011). Distributed lag linear and non-linear models in R: The package dlnm. *Journal of Statistical Software, 43*(8), 1–20. https://doi.org/10.18637/jss.v043.i08

Gasparrini, A., & Armstrong, B. (2013). Reducing and meta-analysing estimates from distributed lag non-linear models. *BMC Medical Research Methodology, 13*, 1. https://doi.org/10.1186/1471-2288-13-1

Gasparrini, A., Guo, Y., Sera, F., et al. (2017). Projections of temperature-related excess mortality under climate change scenarios. *The Lancet Planetary Health, 1*(9), e360–e367. https://doi.org/10.1016/S2542-5196(17)30156-0

Geirinhas, J. L., Russo, A., Libonati, R., Sousa, P. M., Miralles, D. G., & Trigo, R. M. (2021). Recent increasing frequency of compound summer drought and heatwaves in Southeast Brazil. *Environmental Research Letters, 16*(3), 034034. https://doi.org/10.1088/1748-9326/abe0eb

Grech, M. G., Sartor, P. D., Almirón, W. R., & Ludueña-Almeida, F. F. (2015). Effect of temperature on life history traits during immature development of *Aedes aegypti* and *Culex quinquefasciatus* (Diptera: Culicidae) from Córdoba city, Argentina. *Acta Tropica, 146*, 1–6. https://doi.org/10.1016/j.actatropica.2015.02.010

Kersul, M. G., Donato, L. E., dos Santos, A. G., Albuquerque e Silva, R., Sousa-Gomes, M. L. de, & Galvis-Ovallos, F. (2025). Climatic, environmental, and social factors in visceral leishmaniasis: A spatio-temporal perspective in Brazilian biomes. *PLOS Neglected Tropical Diseases, 19*(12), e0013842. https://doi.org/10.1371/journal.pntd.0013842

Lee, S. A., Economou, T., de Castro Catao, R., Barcellos, C., & Lowe, R. (2021). The impact of climate suitability, urbanisation, and connectivity on the expansion of dengue in 21st century Brazil. *PLOS Neglected Tropical Diseases, 15*(12), e0009773. https://doi.org/10.1371/journal.pntd.0009773

Lowe, R., Lee, S., O'Reilly, K. M., Brady, O. J., Bastos, L., Carrasco-Escobar, G., de Castro Catão, R., Colón-González, F. J., Barcellos, C., Sá Carvalho, M., Blangiardo, M., Rue, H., & Gasparrini, A. (2021). Combined effects of hydrometeorological hazards and urbanisation on dengue risk in Brazil: A spatiotemporal modelling study. *The Lancet Planetary Health, 5*(4), e209–e219. https://doi.org/10.1016/S2542-5196(20)30292-8

Moraes, S. L. de, Almendra, R., & Barrozo, L. V. (2022). Impact of heat waves and cold spells on cause-specific mortality in the city of São Paulo, Brazil. *International Journal of Hygiene and Environmental Health, 239*, 113861. https://doi.org/10.1016/j.ijheh.2021.113861

Pereira, R. H. M., & Barbosa, R. J. (2023). *censobr: Download data from Brazil's population census* (R package version 0.2.0) [R package]. GitHub. https://github.com/ipeaGIT/censobr

Pereira, R. H. M., & Gonçalves, C. N. (2022). *geobr: Download official spatial data sets of Brazil* (R package version 1.7.0) [R package]. GitHub. https://github.com/ipeaGIT/geobr

Prado Siqueira, R. (2024). *datasus: An interface to DATASUS system* [R package]. GitHub. https://github.com/rpradosiqueira/datasus

Romanello, M., McGushin, A., Di Napoli, C., Drummond, P., Hughes, N., Jamart, L., et al. (2021). The 2021 report of the Lancet Countdown on health and climate change: Code red for a healthy future. *The Lancet, 398*(10311), 1619–1662. https://doi.org/10.1016/S0140-6736(21)01787-6

Saldanha, R. de F., Bastos, R. R., & Barcellos, C. (2019). Microdatasus: A package for downloading and preprocessing microdata from the Brazilian Health Informatics Department (DATASUS). *Cadernos de Saúde Pública, 35*(9), e00032419. https://doi.org/10.1590/0102-311x00032419

Son, J.-Y., Gouveia, N., Bravo, M. A., de Freitas, C. U., & Bell, M. L. (2016). The impact of temperature on mortality in a subtropical city: Effects of cold, heat, and heat waves in São Paulo, Brazil. *International Journal of Biometeorology, 60*(1), 113–121. https://doi.org/10.1007/s00484-015-1009-7

Zeger, S. L., Dominici, F., & Samet, J. (1999). Harvesting-resistant estimates of air pollution effects on mortality. *Epidemiology, 10*(2), 171–175.

Zhao, Q., Guo, Y., Ye, T., Gasparrini, A., Tong, S., Overcenco, A., Urban, A., Schneider, A., Entezari, A., Vicedo-Cabrera, A. M., et al. (2021). Global, regional, and national burden of mortality associated with non-optimal ambient temperatures from 2000 to 2019: A three-stage modelling study. *The Lancet Planetary Health, 5*(7), e415–e425. https://doi.org/10.1016/S2542-5196(21)00081-4
