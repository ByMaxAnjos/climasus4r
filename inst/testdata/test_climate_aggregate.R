# test_climate_aggregate.R
# Testa sus_climate_aggregate() (10 estrategias) e sus_climate_plot_aggregate().
#
# Executar com:
#   devtools::load_all()
#   source("inst/testdata/create_climate_testdata.R")   # gera fixtures se nao existirem
#   source("inst/testdata/test_climate_aggregate.R")
#
# Nao requer internet: usa fixtures salvos em inst/testdata/climate/.

devtools::load_all(quiet = TRUE)

# ── Carregar fixtures ─────────────────────────────────────────────────────────
CLIMATE_DIR <- "inst/testdata/climate"
f_clim   <- file.path(CLIMATE_DIR, "climate_ro_2022.parquet")
f_health <- file.path(CLIMATE_DIR, "health_ro_2022_spatial.rds")

if (!file.exists(f_clim) || !file.exists(f_health)) {
  cli::cli_abort(c(
    "Fixtures nao encontrados em '{CLIMATE_DIR}'.",
    "i" = "Execute: source('inst/testdata/create_climate_testdata.R')"
  ))
}

climate_df <- climasus4r:::from_arrow_climasus(f_clim)
health_df  <- readRDS(f_health)

cli::cli_alert_info("health_df:  {nrow(health_df)} linhas, stage = '{sus_meta(health_df,  'stage')}'")
cli::cli_alert_info("climate_df: {nrow(climate_df)} linhas, stage = '{sus_meta(climate_df, 'stage')}'")

# ── Helpers de teste ──────────────────────────────────────────────────────────
ok   <- 0L
fail <- 0L

run_test <- function(name, expr) {
  tryCatch({
    r    <- force(expr)
    rows <- if (inherits(r, "data.frame")) nrow(r) else "?"
    cli::cli_alert_success("{name}: OK ({rows} linhas)")
    ok <<- ok + 1L
    invisible(r)
  }, error = function(e) {
    cli::cli_alert_danger("{name}: FALHOU — {conditionMessage(e)}")
    fail <<- fail + 1L
    invisible(NULL)
  })
}

# Retorna TRUE se o erro foi lancado conforme esperado
expect_error <- function(name, expr) {
  tryCatch({
    force(expr)
    cli::cli_alert_danger("{name}: FALHOU — nenhum erro foi lancado (esperado)")
    fail <<- fail + 1L
    FALSE
  }, error = function(e) {
    cli::cli_alert_success("{name}: OK (erro esperado: {conditionMessage(e)})")
    ok <<- ok + 1L
    TRUE
  })
}

CLIM_VAR <- "tair_dry_bulb_c"

# =============================================================================
# Bloco A: sus_climate_aggregate — 10 estrategias
# =============================================================================
cli::cli_h1("Bloco A: sus_climate_aggregate — 10 estrategias")

# ── A1: exact ─────────────────────────────────────────────────────────────────
r_exact <- run_test("A1 | exact", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = CLIM_VAR,
    temporal_strategy = "exact",
    verbose           = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "exact",
    nrow(r) == nrow(health_df),
    CLIM_VAR %in% names(r),
    !"station_code" %in% names(r)
  )
  r
})

# ── A2: discrete_lag ──────────────────────────────────────────────────────────
r_discrete <- run_test("A2 | discrete_lag (lags 0, 7, 14)", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = CLIM_VAR,
    temporal_strategy = "discrete_lag",
    lag_days          = c(0L, 7L, 14L),
    verbose           = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "discrete_lag",
    nrow(r) == nrow(health_df),
    paste0("lag0_",  CLIM_VAR) %in% names(r),
    paste0("lag7_",  CLIM_VAR) %in% names(r),
    paste0("lag14_", CLIM_VAR) %in% names(r)
  )
  r
})

# ── A3: moving_window ─────────────────────────────────────────────────────────
r_moving <- run_test("A3 | moving_window (window_days=14)", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = CLIM_VAR,
    temporal_strategy = "moving_window",
    window_days       = 14L,
    verbose           = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "moving_window",
    nrow(r) == nrow(health_df),
    paste0(CLIM_VAR, "_mean_w14") %in% names(r)
  )
  r
})

# ── A4: offset_window ─────────────────────────────────────────────────────────
run_test("A4 | offset_window (offset_days=c(7,14))", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = CLIM_VAR,
    temporal_strategy = "offset_window",
    offset_days       = c(7L, 14L),
    verbose           = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "offset_window",
    nrow(r) == nrow(health_df),
    paste0("off7to14_", CLIM_VAR) %in% names(r)
  )
  r
})

# ── A5: distributed_lag ───────────────────────────────────────────────────────
run_test("A5 | distributed_lag (lag_days=7)", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = CLIM_VAR,
    temporal_strategy = "distributed_lag",
    lag_days          = 7L,
    verbose           = FALSE
  )
  expected_lags <- paste0(CLIM_VAR, "_lag", 0:7)
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "distributed_lag",
    nrow(r) == nrow(health_df),
    all(expected_lags %in% names(r))
  )
  r
})

# ── A6: degree_days ───────────────────────────────────────────────────────────
run_test("A6 | degree_days (window=30, temp_base=20)", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    temporal_strategy = "degree_days",
    window_days       = 30L,
    temp_base         = 20,
    verbose           = FALSE
  )
  gdd_col <- "gdd_w30tbase20"
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "degree_days",
    nrow(r) == nrow(health_df),
    gdd_col %in% names(r),
    all(r[[gdd_col]] >= 0, na.rm = TRUE)
  )
  r
})

# ── A7: seasonal ──────────────────────────────────────────────────────────────
# Note: the loaded .join_seasonal (from utils-clim.R) names columns as
# {var}_mean, {var}_max, {var}_min, {var}_range, {var}_n_days, etc.
run_test("A7 | seasonal", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = CLIM_VAR,
    temporal_strategy = "seasonal",
    verbose           = FALSE
  )
  mean_col <- paste0(CLIM_VAR, "_mean")
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "seasonal",
    nrow(r) == nrow(health_df),
    mean_col %in% names(r)
  )
  r
})

# ── A8: threshold_exceedance ──────────────────────────────────────────────────
run_test("A8 | threshold_exceedance (window=7, thr=35, above)", {
  r <- sus_climate_aggregate(
    health_data         = health_df,
    climate_data        = climate_df,
    climate_var         = CLIM_VAR,
    temporal_strategy   = "threshold_exceedance",
    window_days         = 7L,
    threshold_value     = 35,
    threshold_direction = "above",
    verbose             = FALSE
  )
  col_n <- paste0("nexc7_gt35_", CLIM_VAR)
  col_p <- paste0("pexc7_gt35_", CLIM_VAR)
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "threshold_exceedance",
    nrow(r) == nrow(health_df),
    col_n %in% names(r),
    col_p %in% names(r),
    all(r[[col_p]] >= 0 & r[[col_p]] <= 1, na.rm = TRUE)
  )
  r
})

# ── A9: cold_wave_exceedance ──────────────────────────────────────────────────
# Note: the loaded implementation names columns ncold_lt{thr}_{var} (no window).
run_test("A9 | cold_wave_exceedance (window=7, thr=18)", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = CLIM_VAR,
    temporal_strategy = "cold_wave_exceedance",
    window_days       = 7L,
    threshold_value   = 18,
    verbose           = FALSE
  )
  col_n <- paste0("ncold_lt18_", CLIM_VAR)
  col_p <- paste0("pcold_lt18_", CLIM_VAR)
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "cold_wave_exceedance",
    nrow(r) == nrow(health_df),
    col_n %in% names(r),
    col_p %in% names(r),
    all(r[[col_n]] >= 0, na.rm = TRUE)
  )
  r
})

# ── A10: weighted_window ──────────────────────────────────────────────────────
run_test("A10 | weighted_window (window=7, pesos lineares)", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = CLIM_VAR,
    temporal_strategy = "weighted_window",
    window_days       = 7L,
    weights           = NULL,
    verbose           = FALSE
  )
  wwin_col <- paste0("wwin7_", CLIM_VAR)
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "weighted_window",
    nrow(r) == nrow(health_df),
    wwin_col %in% names(r)
  )
  r
})

cli::cli_alert_info("Bloco A: {ok} OK, {fail} FALHOU")
ok_a <- ok; fail_a <- fail

# =============================================================================
# Bloco B: sus_climate_aggregate — validacoes de entrada
# =============================================================================
cli::cli_h1("Bloco B: sus_climate_aggregate — validacoes")
ok <- 0L; fail <- 0L

# B1: health_data no stage errado (nao spatial)
expect_error("B1 | health_data no stage errado (aggregate)", {
  bad_health <- health_df
  bad_health <- sus_meta(bad_health, stage = "aggregate")
  sus_climate_aggregate(
    health_data       = bad_health,
    climate_data      = climate_df,
    temporal_strategy = "exact",
    verbose           = FALSE
  )
})

# B2: discrete_lag sem lag_days
expect_error("B2 | discrete_lag sem lag_days", {
  sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    temporal_strategy = "discrete_lag",
    verbose           = FALSE
  )
})

# B3: moving_window sem window_days
expect_error("B3 | moving_window sem window_days", {
  sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    temporal_strategy = "moving_window",
    verbose           = FALSE
  )
})

# B4: offset_window com offset_days de comprimento errado (length 1)
expect_error("B4 | offset_window com offset_days length 1", {
  sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    temporal_strategy = "offset_window",
    offset_days       = c(7L),
    verbose           = FALSE
  )
})

# B5: health_data nao e climasus_df
expect_error("B5 | health_data nao e climasus_df", {
  sus_climate_aggregate(
    health_data       = as.data.frame(health_df),
    climate_data      = climate_df,
    temporal_strategy = "exact",
    verbose           = FALSE
  )
})

# B6: climate_var inexistente no climate_df
expect_error("B6 | climate_var inexistente ('var_inexistente')", {
  sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = "var_inexistente",
    temporal_strategy = "exact",
    verbose           = FALSE
  )
})

# B7: climate_var filter — so tair_dry_bulb_c no resultado
run_test("B7 | climate_var='tair_dry_bulb_c' — sem outras vars de clima", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    climate_var       = CLIM_VAR,
    temporal_strategy = "exact",
    verbose           = FALSE
  )
  all_clim_vars <- c(
    "patm_mb", "patm_max_mb", "patm_min_mb",
    "tair_max_c", "tair_min_c",
    "dew_tmean_c", "dew_tmax_c", "dew_tmin_c",
    "rh_mean_porc", "rh_max_porc", "rh_min_porc",
    "rainfall_mm", "ws_2_m_s", "ws_gust_m_s", "wd_degrees", "sr_kj_m2"
  )
  extra_cols <- intersect(names(r), all_clim_vars)
  stopifnot(
    CLIM_VAR %in% names(r),
    length(extra_cols) == 0L
  )
  r
})

# B8: temp_base ausente em degree_days usa default regional (nao aborta)
run_test("B8 | degree_days sem temp_base usa default regional", {
  r <- sus_climate_aggregate(
    health_data       = health_df,
    climate_data      = climate_df,
    temporal_strategy = "degree_days",
    window_days       = 14L,
    temp_base         = NULL,
    verbose           = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    any(grepl("^gdd_w14", names(r)))
  )
  r
})

cli::cli_alert_info("Bloco B: {ok} OK, {fail} FALHOU")
ok_b <- ok; fail_b <- fail

# =============================================================================
# Bloco C: sus_climate_plot_aggregate — 6 plot_types + interativo + discrete_lag
# =============================================================================
cli::cli_h1("Bloco C: sus_climate_plot_aggregate — tipos de grafico")
ok <- 0L; fail <- 0L

# Input principal: resultado de moving_window (melhor cobertura de colunas)
df_plot <- r_moving

if (is.null(df_plot)) {
  cli::cli_alert_warning("Resultado moving_window nao disponivel; pulando Bloco C.")
} else {

  run_test("C1 | plot_type='timeseries'", {
    p <- sus_climate_plot_aggregate(
      df_plot,
      climate_cols = paste0(CLIM_VAR, "_mean_w14"),
      plot_type    = "timeseries",
      verbose      = FALSE
    )
    stopifnot(inherits(p, "gg") || inherits(p, "plotly") || is.list(p))
    p
  })

  run_test("C2 | plot_type='scatter'", {
    p <- sus_climate_plot_aggregate(
      df_plot,
      climate_cols = paste0(CLIM_VAR, "_mean_w14"),
      plot_type    = "scatter",
      verbose      = FALSE
    )
    stopifnot(inherits(p, "gg") || inherits(p, "plotly") || is.list(p))
    p
  })

  run_test("C3 | plot_type='ccf' (max_lag=14)", {
    p <- sus_climate_plot_aggregate(
      df_plot,
      climate_cols = paste0(CLIM_VAR, "_mean_w14"),
      plot_type    = "ccf",
      max_lag      = 14L,
      verbose      = FALSE
    )
    stopifnot(inherits(p, "gg") || inherits(p, "plotly") || is.list(p))
    p
  })

  run_test("C4 | plot_type='distribution'", {
    p <- sus_climate_plot_aggregate(
      df_plot,
      climate_cols = paste0(CLIM_VAR, "_mean_w14"),
      plot_type    = "distribution",
      verbose      = FALSE
    )
    stopifnot(inherits(p, "gg") || inherits(p, "plotly") || is.list(p))
    p
  })

  run_test("C5 | plot_type='corr_matrix'", {
    p <- sus_climate_plot_aggregate(
      df_plot,
      climate_cols = paste0(CLIM_VAR, "_mean_w14"),
      plot_type    = "corr_matrix",
      verbose      = FALSE
    )
    stopifnot(inherits(p, "gg") || inherits(p, "plotly") || is.list(p))
    p
  })

  run_test("C6 | plot_type='seasonal'", {
    p <- sus_climate_plot_aggregate(
      df_plot,
      climate_cols = paste0(CLIM_VAR, "_mean_w14"),
      plot_type    = "seasonal",
      verbose      = FALSE
    )
    stopifnot(inherits(p, "gg") || inherits(p, "plotly") || is.list(p))
    p
  })

  run_test("C7 | interactive=TRUE retorna plotly", {
    rlang::check_installed("plotly", reason = "para teste interativo")
    p <- sus_climate_plot_aggregate(
      df_plot,
      climate_cols = paste0(CLIM_VAR, "_mean_w14"),
      plot_type    = "timeseries",
      interactive  = TRUE,
      verbose      = FALSE
    )
    stopifnot(inherits(p, "plotly"))
    p
  })

  # C8: discrete_lag + corr_matrix
  if (!is.null(r_discrete)) {
    run_test("C8 | discrete_lag resultado + corr_matrix", {
      lag_cols <- paste0("lag", c(0, 7, 14), "_", CLIM_VAR)
      p <- sus_climate_plot_aggregate(
        r_discrete,
        climate_cols = lag_cols,
        plot_type    = "corr_matrix",
        verbose      = FALSE
      )
      stopifnot(inherits(p, "gg") || inherits(p, "plotly") || is.list(p))
      p
    })
  } else {
    cli::cli_alert_warning("C8 pulado: resultado discrete_lag nao disponivel.")
  }

}

cli::cli_alert_info("Bloco C: {ok} OK, {fail} FALHOU")
ok_c <- ok; fail_c <- fail

# =============================================================================
# Sumario global
# =============================================================================
cli::cli_h1("Sumario: test_climate_aggregate.R")
total_ok   <- ok_a + ok_b + ok_c
total_fail <- fail_a + fail_b + fail_c

cli::cli_inform(c(
  "Bloco A (estrategias):  {ok_a} OK, {fail_a} FALHOU",
  "Bloco B (validacoes):   {ok_b} OK, {fail_b} FALHOU",
  "Bloco C (plots):        {ok_c} OK, {fail_c} FALHOU",
  " ",
  "Total: {total_ok} OK, {total_fail} FALHOU"
))

if (total_fail == 0L) {
  cli::cli_alert_success("Todos os testes passaram.")
} else {
  cli::cli_alert_danger("{total_fail} teste(s) falharam.")
}
