# === GRUPO J: sus_grid_chirps() ===

# ── Teste 1: chirps | cache smoke ─────────────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/chirps")
  cached <- list.files(cache_dir, pattern = "\\.(parquet|tif|tiff)$",
                       recursive = TRUE, full.names = TRUE)
  if (length(cached) == 0L) {
    cli::cli_alert_warning("  chirps | cache smoke: SKIP — sem cache em {cache_dir}")
    return(invisible(NULL))
  }
  year_match <- regmatches(cached[1], regexpr("20[0-9]{2}", cached[1]))
  year_val   <- if (length(year_match) > 0L) as.integer(year_match[1]) else 2022L
  run_test("chirps | cache smoke", {
    r <- sus_grid_chirps(years = year_val, municipalities = muni_sample,
                         use_cache = TRUE, verbose = FALSE)
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "chirps",
      "rainfall_chirps_mm" %in% names(r),
      nrow(r) > 0
    )
    r
  })
})
