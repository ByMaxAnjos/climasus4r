# ── Grupo P: Preparação inline ───────────────────────────────────────────────
cli::cli_h2("Grupo P — Preparação de dados")

merged_dlnm <- sus_climate_aggregate(
  health_df, climate_df,
  climate_var = "tair_dry_bulb_c",
  temporal_strategy = "distributed_lag",
  lag_days = 0:14,
  verbose = FALSE
)

merged_exact <- sus_climate_aggregate(
  health_df, climate_df,
  climate_var = c("tair_dry_bulb_c", "rainfall_mm"),
  temporal_strategy = "exact",
  verbose = FALSE
)

city_counts <- sort(table(merged_dlnm$code_muni), decreasing = TRUE)
top_cities  <- names(city_counts)[seq_len(min(3L, length(city_counts)))]
cli::cli_alert_info("Top 3 cidades: {paste(top_cities, collapse=', ')}")

fits_list <- setNames(
  lapply(top_cities, function(city) {
    d <- merged_dlnm[merged_dlnm$code_muni == city, ]
    sus_mod_dlnm(d, outcome_col = "n_obitos", climate_col = "tair_dry_bulb_c",
                 lag_max = 14L, dof_per_year = 4L, verbose = FALSE)
  }),
  top_cities
)

af_list <- lapply(fits_list, function(f) sus_mod_af(f, verbose = FALSE))
names(af_list) <- top_cities

munic_sf <- sf::st_as_sf(dplyr::distinct(
  dplyr::select(health_df, code_muni, geom)
))

agg_city_df <- dplyr::summarise(
  dplyr::group_by(
    dplyr::select(merged_exact, code_muni, tair_dry_bulb_c, n_obitos),
    code_muni
  ),
  tair_dry_bulb_c = mean(tair_dry_bulb_c, na.rm = TRUE),
  n_obitos        = sum(n_obitos, na.rm = TRUE),
  .groups = "drop"
)

cli::cli_alert_success("Preparação concluída: {length(top_cities)} cidades | merged_dlnm: {nrow(merged_dlnm)} rows")
