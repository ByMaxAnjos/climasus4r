# === GRUPO N: sus_grid_pollution_cams() + sus_grid_pollution_ghap() ===

# ── Teste 1: sus_grid_pollution_cams (cache smoke) ────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/cams")
  files     <- list.files(cache_dir, pattern = "\\.parquet$", recursive = TRUE)
  if (length(files) == 0L) {
    cli::cli_alert_warning("pollution_cams | cache smoke: SKIP — cache vazio em {cache_dir}")
    return(invisible(NULL))
  }

  # detecta o primeiro ano disponível no cache
  years_found <- regmatches(files, regexpr("20[0-9]{2}", files))
  year_use    <- if (length(years_found) > 0L) as.integer(years_found[[1L]]) else NULL

  run_test("pollution_cams | cache smoke", {
    r <- sus_grid_pollution_cams(
      pollutants = c("pm25", "pm10"),
      metric     = "mean",
      years      = year_use,
      use_cache  = TRUE,
      cache_dir  = cache_dir,
      lang       = "pt",
      verbose    = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "pollution_cams",
      "code_muni" %in% names(r),
      "date"      %in% names(r),
      nrow(r) > 0
    )
    r
  })
})

# ── Teste 2: sus_grid_pollution_ghap (cache smoke) ────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/ghap")
  files     <- list.files(cache_dir, pattern = "\\.parquet$", recursive = TRUE)
  if (length(files) == 0L) {
    cli::cli_alert_warning("pollution_ghap | cache smoke: SKIP — cache vazio em {cache_dir}")
    return(invisible(NULL))
  }

  # detecta o primeiro ano disponível no cache
  years_found <- regmatches(files, regexpr("20[0-9]{2}", files))
  year_use    <- if (length(years_found) > 0L) as.integer(years_found[[1L]]) else NULL

  run_test("pollution_ghap | cache smoke", {
    r <- sus_grid_pollution_ghap(
      pollutants     = "pm25",
      resolution     = "monthly",
      years          = year_use,
      months         = 1:12,
      municipalities = NULL,
      agg_fun        = "mean",
      crop_brazil    = TRUE,
      use_cache      = TRUE,
      cache_dir      = cache_dir,
      lang           = "pt",
      verbose        = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "pollution_ghap",
      "code_muni" %in% names(r),
      "date"      %in% names(r),
      nrow(r) > 0
    )
    r
  })
})
