# === GRUPO I: sus_grid_era5() ===

# ── Teste 1: sus_grid_era5 (cache smoke) ──────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/era5")
  cached <- list.files(cache_dir, pattern = "\\.(parquet|nc)$",
                       recursive = TRUE, full.names = TRUE)
  if (length(cached) == 0L) {
    cli::cli_alert_warning(
      "  era5 | cache smoke: SKIP — sem cache em {cache_dir}"
    )
    return(invisible(NULL))
  }
  # Detectar o ano presente no cache a partir do caminho do primeiro arquivo
  year_match <- regmatches(cached[1], regexpr("20[0-9]{2}", cached[1]))
  year_val   <- if (length(year_match) > 0L) as.integer(year_match[1]) else 2020L

  run_test("era5 | cache smoke", {
    r <- sus_grid_era5(
      years        = year_val,
      municipalities = muni_sample,
      use_cache    = TRUE,
      verbose      = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "era5_land",
      nrow(r) > 0
    )
    r
  })
})
