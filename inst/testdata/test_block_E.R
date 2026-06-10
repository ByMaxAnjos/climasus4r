# === GRUPO E: sus_climate_anomaly ===

normals_path <- file.path(BASE, "normals_sample.rds")
if (file.exists(normals_path)) {
  normals_df <- readRDS(normals_path)
} else {
  normals_df <- tryCatch(
    sus_climate_normals(period = "1991-2020", use_cache = TRUE, verbose = FALSE),
    error = function(e) { cli::cli_alert_warning("GRUPO E: sem normals — skip"); NULL }
  )
}

if (!is.null(normals_df)) {

  run_test("anomaly | absolute monthly", {
    r <- sus_climate_anomaly(
      observed   = climate_df,
      normals    = normals_df,
      method     = "absolute",
      time_scale = "monthly",
      verbose    = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "anomaly",
      nrow(r) > 0,
      any(grepl("_anomaly$", names(r)))
    )
    r
  })

  run_test("anomaly | standardized monthly", {
    r <- sus_climate_anomaly(
      observed   = climate_df,
      normals    = normals_df,
      method     = "standardized",
      time_scale = "monthly",
      verbose    = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "anomaly",
      nrow(r) > 0,
      any(grepl("_anomaly_std", names(r)))
    )
    r
  })

  run_test("anomaly | all methods", {
    r <- sus_climate_anomaly(
      observed   = climate_df,
      normals    = normals_df,
      method     = "all",
      time_scale = "monthly",
      verbose    = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "anomaly",
      nrow(r) > 0,
      any(grepl("_anomaly$",   names(r))),
      any(grepl("_anomaly_pct", names(r)))
    )
    r
  })

} else {
  cli::cli_alert_warning("GRUPO E: skipped (normals nao disponivel)")
}
