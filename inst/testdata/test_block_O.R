# === GRUPO O: sus_grid_pollution_merra2() ===

muni_sample <- c(110001L, 110002L, 110003L, 110004L, 110005L)

# ── Teste 1: pollution_merra2 | cache smoke ───────────────────────────────────
local({
  # Skip se sem credenciais E sem cache
  has_auth  <- nzchar(Sys.getenv("EARTHDATA_USER")) &&
               nzchar(Sys.getenv("EARTHDATA_PASSWORD"))
  cache_dir <- path.expand("~/.climasus4r_cache/merra2")
  cached    <- list.files(cache_dir,
                          pattern    = "\\.(parquet|nc)$",
                          recursive  = TRUE,
                          full.names = TRUE)
  if (length(cached) == 0L && !has_auth) {
    cli::cli_alert_warning(
      "  pollution_merra2 | cache smoke: SKIP — sem cache nem credenciais EARTHDATA"
    )
    return(invisible(NULL))
  }

  year_match <- if (length(cached) > 0L)
    regmatches(cached[1L], regexpr("20[0-9]{2}", cached[1L]))
  else
    character(0)
  year_val   <- if (length(year_match) > 0L) as.integer(year_match[1L]) else 2020L

  run_test("pollution_merra2 | cache smoke", {
    r <- sus_grid_pollution_merra2(
      years         = year_val,
      municipalities = muni_sample,
      use_cache     = TRUE,
      verbose       = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "pollution_merra2",
      nrow(r) > 0
    )
    r
  })
})
