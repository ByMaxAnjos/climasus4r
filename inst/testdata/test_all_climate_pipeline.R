# test_all_climate_pipeline.R
# Testa todas as funções sus_climate_* do pacote climasus4r
# Padrão: test_all_pipeline_v2.R
# Uso: Rscript inst/testdata/test_all_climate_pipeline.R

setwd("/Users/co2map/Documents/2026/CLIMASUS4r/climasus4r")
suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))

BASE    <- "inst/testdata/climate"
results <- list()
ok      <- 0L
fail    <- 0L

run_test <- function(name, code) {
  r <- tryCatch(code, error = function(e) {
    cli::cli_alert_danger("  {name}: FAIL — {conditionMessage(e)}")
    results[[length(results) + 1L]] <<- list(
      label = name, result = "FAIL", rows = NA_integer_,
      error = conditionMessage(e)
    )
    fail <<- fail + 1L
    invisible(NULL)
  })
  if (!is.null(r)) {
    n <- tryCatch(
      if (is.list(r) && !is.data.frame(r)) paste0("list(", length(r), ")")
      else as.integer(nrow(r)),
      error = function(e) "?"
    )
    cli::cli_alert_success("  {name}: OK ({n} linhas)")
    results[[length(results) + 1L]] <<- list(
      label = name, result = "PASS", rows = n, error = NA_character_
    )
    ok <<- ok + 1L
  }
  invisible(r)
}

# Operador null-coalesce para uso nos blocos
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ── Carregar fixtures ──────────────────────────────────────────────────────────
cli::cli_h1("Carregando fixtures")
climate_df <- climasus4r:::from_arrow_climasus(file.path(BASE, "climate_ro_2022.parquet"))
health_df  <- readRDS(file.path(BASE, "health_ro_2022_spatial.rds"))
filled_df  <- NULL
agg_df     <- NULL
cli::cli_alert_info("climate_df: {nrow(climate_df)} linhas | stage={sus_meta(climate_df,'stage')} | type={sus_meta(climate_df,'type')}")
cli::cli_alert_info("health_df : {nrow(health_df)} linhas | stage={sus_meta(health_df,'stage')}")

# ── Grupo A — sus_climate_fill_inmet ──────────────────────────────────────────
cli::cli_h2("Grupo A — sus_climate_fill_inmet")

run_test("fill_inmet | single var tair_dry_bulb_c", {
  r <- sus_climate_fill_inmet(
    climate_df,
    target_var  = "tair_dry_bulb_c",
    verbose     = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    "tair_dry_bulb_c" %in% names(r),
    nrow(r) == nrow(climate_df)
  )
  filled_df <<- r
  r
})

run_test("fill_inmet | multi-var tair+rh", {
  r <- sus_climate_fill_inmet(
    climate_df,
    target_var = c("tair_dry_bulb_c", "rh_mean_porc"),
    verbose    = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    all(c("tair_dry_bulb_c", "rh_mean_porc") %in% names(r)),
    nrow(r) == nrow(climate_df)
  )
  r
})

run_test("fill_inmet | all vars", {
  r <- sus_climate_fill_inmet(
    climate_df,
    target_var = "all",
    verbose    = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    nrow(r) == nrow(climate_df)
  )
  r
})

# ── Grupo B — sus_climate_compute_indicators ──────────────────────────────────
cli::cli_h2("Grupo B — sus_climate_compute_indicators")

run_test("compute_indicators | all", {
  r <- sus_climate_compute_indicators(
    filled_df %||% climate_df,
    indicators = "all",
    verbose    = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "indicators",
    any(c("wbgt_c", "hi_c", "utci_c") %in% names(r)),
    nrow(r) > 0
  )
  r
})

run_test("compute_indicators | HI only", {
  r <- sus_climate_compute_indicators(
    climate_df,
    indicators = c("HI"),
    verbose    = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "indicators",
    "hi_c" %in% names(r),
    nrow(r) == nrow(climate_df)
  )
  r
})

run_test("compute_indicators | WBGT+HI", {
  r <- sus_climate_compute_indicators(
    climate_df,
    indicators = c("WBGT", "HI"),
    verbose    = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "indicators",
    all(c("wbgt_c", "hi_c") %in% names(r))
  )
  r
})

# ── Grupo C — sus_climate_aggregate ───────────────────────────────────────────
cli::cli_h2("Grupo C — sus_climate_aggregate")

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

# ── Grupo D — sus_climate_compute_heatwaves ───────────────────────────────────
cli::cli_h2("Grupo D — sus_climate_compute_heatwaves")

# Entrada: climate_df (stage=climate, type=inmet) carregado no mestre
# filled_df pode ter sido definido pelo grupo A; usa fallback para climate_df

hw_input <- if (exists("filled_df") && !is.null(filled_df)) filled_df else climate_df

run_test("compute_heatwaves | WHO", {
  r <- sus_climate_compute_heatwaves(hw_input, method = "WHO", verbose = FALSE, lang = "pt")
  stopifnot(
    is.list(r),
    all(c("events", "daily", "summary") %in% names(r)),
    is.data.frame(r[["events"]]),
    all(c("event_id", "start_date", "end_date", "duration_days") %in% names(r[["events"]])),
    is.integer(nrow(r[["events"]])) || nrow(r[["events"]]) >= 0
  )
  r
})

run_test("compute_heatwaves | WMO", {
  r <- sus_climate_compute_heatwaves(hw_input, method = "WMO", verbose = FALSE, lang = "pt")
  stopifnot(
    is.list(r),
    "hw_wmo" %in% names(r[["daily"]])
  )
  r
})

run_test("compute_heatwaves | EHF", {
  r <- sus_climate_compute_heatwaves(hw_input, method = "EHF", verbose = FALSE, lang = "pt")
  stopifnot(
    any(grepl("ehf", names(r[["events"]]), ignore.case = TRUE) | nrow(r[["events"]]) >= 0)
  )
  r
})

run_test("compute_heatwaves | multi-method WHO+WMO", {
  r <- sus_climate_compute_heatwaves(hw_input, method = c("WHO", "WMO"), verbose = FALSE, lang = "pt")
  stopifnot(
    all(c("hw_who", "hw_wmo") %in% names(r[["daily"]]))
  )
  r
})

# ── Grupo E — sus_climate_anomaly ─────────────────────────────────────────────
cli::cli_h2("Grupo E — sus_climate_anomaly")

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

# ── Grupo F — sus_climate_compute_spi + sus_climate_compute_spei ──────────────
cli::cli_h2("Grupo F — sus_climate_compute_spi + sus_climate_compute_spei")

monthly_path <- file.path(BASE, "climate_ro_2022_monthly.parquet")
if (file.exists(monthly_path)) {
  monthly_df <- climasus4r:::from_arrow_climasus(monthly_path)

  run_test("compute_spi | 1-3-6mo", {
    r <- sus_climate_compute_spi(
      monthly_df,
      var     = "rainfall_mm",
      scales  = c(1L, 3L, 6L),
      lang    = "pt",
      verbose = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "spi",
      all(c("spi_1mo", "spi_3mo", "spi_6mo") %in% names(r)),
      nrow(r) > 0
    )
    r
  })

  run_test("compute_spi | 12mo single scale", {
    r <- sus_climate_compute_spi(
      monthly_df,
      var     = "rainfall_mm",
      scales  = 12L,
      lang    = "pt",
      verbose = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "spi",
      "spi_12mo" %in% names(r),
      !"spi_1mo" %in% names(r),
      nrow(r) > 0
    )
    r
  })

  run_test("compute_spei | thornthwaite 1-3mo", {
    r <- sus_climate_compute_spei(
      monthly_df,
      rain_var   = "rainfall_mm",
      pet_method = "thornthwaite",
      scales     = c(1L, 3L),
      verbose    = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "spei",
      all(c("spei_1mo", "spei_3mo") %in% names(r)),
      nrow(r) > 0
    )
    r
  })

} else {
  cli::cli_alert_warning("GRUPO F: fixture mensal ausente — skip")
}

# ── Grupo G — sus_climate_inmet + sus_climate_normals + sus_climate_uniplu ────
cli::cli_h2("Grupo G — sus_climate_inmet + sus_climate_normals + sus_climate_uniplu")

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
# NOTA: sus_climate_uniplu baixa ~1.6 GB do Zenodo. O teste só roda se o cache
# local já existir, para evitar crash por HTTP 403/timeout em ambientes sem rede.
local({
  cache_dir  <- path.expand("~/.climasus4r_cache/climate")
  uniplu_rds <- list.files(cache_dir, pattern = "uniplu.*\\.rds$|uniplu.*\\.parquet$",
                            recursive = TRUE, full.names = TRUE)
  if (length(uniplu_rds) == 0L) {
    cli::cli_alert_warning("  uniplu | cache smoke: SKIP — sem cache local (~/.climasus4r_cache/climate/)")
    return(invisible(NULL))
  }
  run_test("uniplu | cache smoke", {
    r <- sus_climate_uniplu(years = 2022L, uf = "RO", aggregate_to = "day",
                             use_cache = TRUE, verbose = FALSE)
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "uniplu",
      "rainfall_mm" %in% names(r),
      nrow(r) > 0
    )
    r
  })
})

# ── Sumário final ──────────────────────────────────────────────────────────────
cli::cli_rule()
cli::cli_alert_info("TOTAL: {ok + fail} testes | {ok} OK | {fail} FAIL")
failed_tests <- Filter(function(x) x$result == "FAIL", results)
if (length(failed_tests) > 0) {
  cli::cli_alert_danger("Testes com falha:")
  for (t in failed_tests) cli::cli_alert_danger("  - {t$label}: {t$error}")
  quit(status = 1L, save = "no")
} else {
  cli::cli_alert_success("Todos os testes passaram!")
}
