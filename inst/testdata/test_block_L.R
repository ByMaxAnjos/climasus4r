# === GRUPO L: sus_grid_prodes() ===

# ── Teste 1: sus_grid_prodes (cache smoke) ────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/prodes")
  cached <- list.files(cache_dir,
                       pattern   = "\\.(geojson|parquet)$",
                       recursive = TRUE,
                       full.names = TRUE)
  if (length(cached) == 0L) {
    cli::cli_alert_warning("prodes | cache smoke: SKIP — cache_dir vazio ({cache_dir})")
    return(invisible(NULL))
  }

  # Inferir um ano presente nos arquivos em cache
  year_val <- tryCatch({
    m <- regmatches(basename(cached[[1L]]),
                    regexpr("[0-9]{4}", basename(cached[[1L]])))
    if (length(m) > 0L) as.integer(m[[1L]]) else 2022L
  }, error = function(e) 2022L)

  run_test("prodes | cache smoke", {
    r <- sus_grid_prodes(
      years         = year_val,
      biomes        = "Amazon",
      municipalities = muni_sample,
      use_cache     = TRUE,
      verbose       = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "prodes",
      "deforested_area_km2" %in% names(r),
      "code_muni"           %in% names(r),
      "date"                %in% names(r),
      "year"                %in% names(r),
      "biome"               %in% names(r),
      nrow(r) > 0
    )
    r
  })
})
