'
test
'
library(LCZ4r)

map <- lcz_get_map(city = "Juiz de Fora, Brasil")

lcz_plot_map(
  map,
  title = "Zonas Climáticas Locais (LCZ) — Juiz de Fora, MG",
  subtitle = "Mapeamento para análise das ilhas de calor urbanas",
  caption = "Fonte: Gerado via pacote LCZ4r (Anjos et al., 2025).\nSci Rep 15, 7710. https://doi.org/10.1038/s41598-025-92000-0",
  isave = TRUE
)

library(climasus4r)

health_clean <- sus_data_import(uf = "SP", year = 2010:2019, system = "SIM-DO") |>
  sus_data_clean_encoding() |>
  sus_data_standardize() |>
  sus_data_filter_cid(disease_group = "respiratory") |>
  sus_data_create_variables(create_age_groups = TRUE, age_breaks = c(0, 5, 15, 60, Inf),
                             create_calendar_vars = TRUE)


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

sus_data_cid_select()
sus_spatial_join(df,
level = "cep"
)

fig_1 <- sus_data_plot_aggregate_ts(health_df, plot_type = c("trend", "seasonal"),
                                     city = "São Paulo", log_transform = TRUE)

fig_2 <- sus_data_plot_aggregate_map(health_df, map_type = "bubble",
                                      rate_per_100k = TRUE, log_scale = TRUE,
                                      show_labels = FALSE)

# 3. Exposure integration: daily counts (São Paulo city) + INMET climate
sp_daily <- health_clean |>
  sus_data_filter_demographics(age_range = c(0, 5), city = "São Paulo") |>
  sus_data_aggregate(time_unit = "day", group_by = "codigo_municipio_residencia", complete_dates = TRUE) |>
  sus_spatial_join(level = "munic")

clima_sp <- sus_climate_inmet(years = 2010:2019, uf = "SP", station_code = "A701") |>
  sus_climate_fill_inmet(target_var = "tair_dry_bulb_c")

exposicao_dl <- sus_climate_aggregate(
  health_data = sp_daily, climate_data = clima_sp, climate_var = "tair_dry_bulb_c",
  temporal_strategy = "distributed_lag", lag_days = 21, verbose = TRUE
)

# 4. Fit DLNM and plot the cumulative exposure-response curve
fit_dlnm <- sus_mod_dlnm(df = exposicao_dl, lag_max = 21,
                          argvar = list(fun = "ns", df = 4),
                          arglag = list(fun = "ns", df = 3))

fig_3 <- sus_mod_plot_dlnm(fit_dlnm, type = "overall")





# ---------------------------------------------------------------------------
# 1. Data preparation --------------------------------------------------------
# ---------------------------------------------------------------------------
health_clean <- sus_data_import(
   uf     = "SP",
   year   = 2010:2019,
   system = "SIM-DO"
 ) |>
 sus_data_clean_encoding() |>
 sus_data_standardize()

tibble::view(health_clean)

health_clean <- health_clean |> 
 sus_data_filter_cid(disease_group = "respiratory") |>
 sus_data_create_variables(
   create_age_groups    = TRUE,
   age_breaks           = c(0, 5, 15, 60, Inf),
   create_calendar_vars  = TRUE
 )

health_df <- health_clean |>
 # Restricts the cohort to children under 5, the age group of interest.
 sus_data_filter_demographics(age_range = c(0, 5))|>
 sus_data_aggregate(
   time_unit = "week",
    group_by  = "codigo_municipio_residencia"
 )
# Municipality-highlighted heatmap of daily respiratory deaths (fig. 1).
fig_1 <- sus_data_plot_aggregate_ts(
 health_df,
 plot_type = c("trend", "seasonal"),
 city      = "São Paulo",
 log_transform = FALSE,
 lang="pt", base_size = 18
)

# Bubble map of the 20 municipalities with the highest counts (fig. 2).
fig_2 <- sus_data_plot_aggregate_map(
 health_df,
 map_type     = "bubble",
rate_per_100k = TRUE,
log_scale = TRUE,
lang="pt", base_size = 20
)

sp_daily <- health_clean |>
  sus_data_filter_demographics(city = "São Paulo") |>
  sus_data_aggregate(time_unit = "day",
    group_by  = "codigo_municipio_residencia") |> 
  sus_spatial_join(level = "munic")

clima_sp <- sus_climate_inmet(years = 2010:2019, uf = "SP")
  #sus_climate_fill_inmet(target_var = "tair_dry_bulb_c")

exposicao_dl <- sus_climate_aggregate(
  health_data       = sp_daily,
  climate_data      = clima_sp,
  climate_var       = "tair_dry_bulb_c",
  temporal_strategy = "distributed_lag",
  lag_days          = 21
)

fit_dlnm <- sus_mod_dlnm(
  df      = exposicao_dl,
  lag_max = 21,
  argvar  = list(fun = "ns", df = 4),  # exposure-response spline (U/J shape)
  arglag  = list(fun = "ns", df = 3)   # lag-response spline (smooth decay)
)

fig_3 <- sus_mod_plot_dlnm(fit_dlnm, type = "overall", lang="pt", base_size = 20)




