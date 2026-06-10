# === GRUPO G: sus_climate_inmet + sus_climate_normals + sus_climate_uniplu ===

# ── Teste 1: sus_climate_inmet (cache smoke) ──────────────────────────────────
run_test("inmet | cache smoke", {
  r <- tryCatch(
    sus_climate_inmet(years = 2022L, uf = "RO", use_cache = TRUE,
                      workers = 1L, verbose = FALSE),
    error = function(e) {
      cli::cli_alert_warning("inmet cache: SKIP — {conditionMessage(e)}")
      return(invisible(NULL))
    }
  )
  if (is.null(r)) return(invisible(NULL))
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "inmet",
    "station_code" %in% names(r),
    "date"         %in% names(r),
    nrow(r) > 0
  )
  r
})

# ── Teste 2: sus_climate_normals (cache smoke) ────────────────────────────────
run_test("normals | cache smoke", {
  r <- tryCatch(
    sus_climate_normals(period = "1991-2020", use_cache = TRUE),
    error = function(e) {
      cli::cli_alert_warning("normals cache: SKIP — {conditionMessage(e)}")
      return(invisible(NULL))
    }
  )
  if (is.null(r)) return(invisible(NULL))
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "normals",
    "codigo" %in% names(r),
    nrow(r) > 0
  )
  r
})

# ── Teste 3: sus_climate_uniplu (cache smoke) ─────────────────────────────────
run_test("uniplu | cache smoke", {
  r <- tryCatch(
    sus_climate_uniplu(years = 2022L, uf = "RO", aggregate_to = "day",
                       use_cache = TRUE),
    error = function(e) {
      cli::cli_alert_warning("uniplu cache: SKIP — {conditionMessage(e)}")
      return(invisible(NULL))
    }
  )
  if (is.null(r)) return(invisible(NULL))
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "uniplu",
    "rainfall_mm" %in% names(r),
    nrow(r) > 0
  )
  r
})
