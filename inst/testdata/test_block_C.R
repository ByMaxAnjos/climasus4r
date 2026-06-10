# === GRUPO C: sus_climate_aggregate (smoke) ===

run_test("aggregate | exact smoke", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = "tair_dry_bulb_c",
    temporal_strategy = "exact",
    verbose           = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "exact",
    nrow(r) == nrow(health_df),
    "tair_dry_bulb_c" %in% names(r)
  )
  agg_df <<- r
  r
})

run_test("aggregate | moving_window 7d", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = "tair_dry_bulb_c",
    temporal_strategy = "moving_window",
    window_days       = 7L,
    verbose           = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "moving_window",
    nrow(r) > 0,
    "tair_dry_bulb_c_mean_w7" %in% names(r)
  )
  r
})

run_test("aggregate | multi-var rain+temp", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = c("tair_dry_bulb_c", "rainfall_mm"),
    temporal_strategy = "exact",
    verbose           = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "exact",
    nrow(r) > 0,
    all(c("tair_dry_bulb_c", "rainfall_mm") %in% names(r))
  )
  r
})
