# === GRUPO K: sus_grid_pdsi() + sus_grid_smvi() ===

# ── Teste 1: sus_grid_pdsi (cache smoke) ──────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/pdsi")
  files     <- list.files(cache_dir, recursive = TRUE)
  if (length(files) == 0L) {
    cli::cli_alert_warning("pdsi | cache smoke: SKIP — cache vazio em {cache_dir}")
    return(invisible(NULL))
  }

  # detecta o primeiro ano disponível no cache
  years_found <- regmatches(files, regexpr("20[0-9]{2}", files))
  year_use    <- if (length(years_found) > 0L) as.integer(years_found[[1L]]) else NULL

  run_test("pdsi | cache smoke", {
    r <- sus_grid_pdsi(
      years        = year_use,
      months       = 1:12,
      source       = "terraclimate",
      municipalities = NULL,
      agg_fun      = "mean",
      crop_brazil  = TRUE,
      use_cache    = TRUE,
      cache_dir    = cache_dir,
      lang         = "pt",
      verbose      = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "pdsi",
      "code_muni" %in% names(r),
      "date"      %in% names(r),
      "pdsi"      %in% names(r),
      nrow(r) > 0
    )
    r
  })
})

# ── Teste 2: sus_grid_smvi (cache smoke) ──────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/smvi")
  files     <- list.files(cache_dir, recursive = TRUE)
  if (length(files) == 0L) {
    cli::cli_alert_warning("smvi | cache smoke: SKIP — cache vazio em {cache_dir}")
    return(invisible(NULL))
  }

  # detecta o primeiro ano disponível no cache
  years_found <- regmatches(files, regexpr("20[0-9]{2}", files))
  year_use    <- if (length(years_found) > 0L) as.integer(years_found[[1L]]) else NULL

  run_test("smvi | cache smoke", {
    r <- sus_grid_smvi(
      years        = year_use,
      municipalities = NULL,
      aggregate_by = c("year", "month"),
      brazil_only  = TRUE,
      use_cache    = TRUE,
      cache_dir    = cache_dir,
      lang         = "pt",
      verbose      = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "smvi",
      nrow(r) > 0
    )
    r
  })
})
