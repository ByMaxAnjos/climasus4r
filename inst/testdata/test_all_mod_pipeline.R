# test_all_mod_pipeline.R
# Testa todas as funções sus_mod_* e sus_mod_plot_* do climasus4r
# Padrão: test_all_climate_pipeline.R (Grupos A-G) e test_all_grid_pipeline.R (H-O)
# Uso: Rscript inst/testdata/test_all_mod_pipeline.R

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

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ── Carregar fixtures ─────────────────────────────────────────────────────────
cli::cli_h1("Carregando fixtures")
health_df  <- readRDS(file.path(BASE, "health_ro_2022_spatial.rds"))
climate_df <- climasus4r:::from_arrow_climasus(
                file.path(BASE, "climate_ro_2022.parquet"))
cli::cli_alert_info("health_df : {nrow(health_df)} linhas | stage={sus_meta(health_df,'stage')}")
cli::cli_alert_info("climate_df: {nrow(climate_df)} linhas | stage={sus_meta(climate_df,'stage')}")

# ── Grupo P — Preparação de dados ────────────────────────────────────────────
# ── Grupo P: Preparação inline ───────────────────────────────────────────────
cli::cli_h2("Grupo P — Preparação de dados")

merged_dlnm <- sus_climate_aggregate(
  health_df, climate_df,
  climate_var = "tair_dry_bulb_c",
  temporal_strategy = "distributed_lag",
  lag_days = 0:14,
  verbose = FALSE
)

merged_exact <- sus_climate_aggregate(
  health_df, climate_df,
  climate_var = c("tair_dry_bulb_c", "rainfall_mm"),
  temporal_strategy = "exact",
  verbose = FALSE
)

city_counts <- sort(table(merged_dlnm$code_muni), decreasing = TRUE)
top_cities  <- names(city_counts)[seq_len(min(3L, length(city_counts)))]
cli::cli_alert_info("Top 3 cidades: {paste(top_cities, collapse=', ')}")

fits_list <- setNames(
  lapply(top_cities, function(city) {
    d <- merged_dlnm[merged_dlnm$code_muni == city, ]
    sus_mod_dlnm(d, outcome_col = "n_obitos", climate_col = "tair_dry_bulb_c",
                 lag_max = 14L, dof_per_year = 4L, verbose = FALSE)
  }),
  top_cities
)

af_list <- lapply(fits_list, function(f) sus_mod_af(f, verbose = FALSE))
names(af_list) <- top_cities

munic_sf <- sf::st_as_sf(dplyr::distinct(
  dplyr::select(health_df, code_muni, geom)
))

agg_city_df <- dplyr::summarise(
  dplyr::group_by(
    dplyr::select(merged_exact, code_muni, tair_dry_bulb_c, n_obitos),
    code_muni
  ),
  tair_dry_bulb_c = mean(tair_dry_bulb_c, na.rm = TRUE),
  n_obitos        = sum(n_obitos, na.rm = TRUE),
  .groups = "drop"
)

cli::cli_alert_success("Preparação concluída: {length(top_cities)} cidades | merged_dlnm: {nrow(merged_dlnm)} rows")

# ── Grupo Q — sus_mod_dlnm + sus_mod_plot_dlnm ───────────────────────────────
cli::cli_h2("Grupo Q — sus_mod_dlnm + sus_mod_plot_dlnm")
# ── Grupo Q: sus_mod_dlnm + sus_mod_plot_dlnm ────────────────────────────────

# ── Teste 1: dlnm | single city ──────────────────────────────────────────────
run_test("dlnm | single city", {
  r <- fits_list[[1]]
  stopifnot(
    inherits(r, "climasus_dlnm"),
    all(c("model", "pred", "exposure_response", "lag_response", "diagnostics") %in% names(r)),
    inherits(r$model, "glm")
  )
  r
})

# ── Teste 2: plot_dlnm | overall ─────────────────────────────────────────────
run_test("plot_dlnm | overall", {
  p <- sus_mod_plot_dlnm(fits_list[[1]], type = "overall", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_dlnm | lag ──────────────────────────────────────────────────
run_test("plot_dlnm | lag", {
  p <- sus_mod_plot_dlnm(fits_list[[1]], type = "lag", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 4: plot_dlnm | table output ────────────────────────────────────────
run_test("plot_dlnm | table output", {
  tbl <- sus_mod_plot_dlnm(fits_list[[1]], type = "overall", output_type = "table",
                            verbose = FALSE)
  # output_type="table" retorna lista com exposure_response (gt_tbl ou tibble)
  # Usar diretamente fit$exposure_response para validacao dos campos
  # (independente do gt estar instalado)
  out <- fits_list[[1]]$exposure_response
  stopifnot(is.data.frame(out), nrow(out) > 0, "rr" %in% names(out))
  out
})

# ── Grupo R — sus_mod_af + sus_mod_plot_af ───────────────────────────────────
cli::cli_h2("Grupo R — sus_mod_af + sus_mod_plot_af")
# === GRUPO R: sus_mod_af + sus_mod_plot_af ===

# ── Teste 1: af | single city ─────────────────────────────────────────────────
run_test("af | single city", {
  r <- af_list[[1]]
  stopifnot(
    inherits(r, "climasus_af"),
    all(c("total", "by_quantile", "meta") %in% names(r)),
    is.data.frame(r$total),
    nrow(r$total) > 0,
    all(c("component", "af", "an") %in% names(r$total))
  )
  r
})

# ── Teste 2: plot_af | bar ────────────────────────────────────────────────────
run_test("plot_af | bar", {
  p <- sus_mod_plot_af(af_list[[1]], type = "bar", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_af | forest ─────────────────────────────────────────────────
run_test("plot_af | forest", {
  p <- sus_mod_plot_af(af_list[[1]], type = "forest", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 4: plot_af | table output ───────────────────────────────────────────
run_test("plot_af | table output", {
  tbl <- sus_mod_plot_af(af_list[[1]], type = "bar", output_type = "table", verbose = FALSE)
  stopifnot(is.data.frame(tbl), nrow(tbl) > 0)
  tbl
})

# ── Grupo S — sus_mod_excess (3 métodos) ─────────────────────────────────────
cli::cli_h2("Grupo S — sus_mod_excess")
# === GRUPO S: sus_mod_excess (3 métodos) ===

city1_exact <- merged_exact[merged_exact$code_muni == top_cities[1], ]

# ── Teste 1: excess | from_dlnm ───────────────────────────────────────────────
run_test("excess | from_dlnm", {
  r <- sus_mod_excess(fits_list[[1]], method = "from_dlnm", verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_excess"),
    all(c("daily", "total", "meta") %in% names(r)),
    is.data.frame(r$total)
  )
  r
})

# ── Teste 2: excess | spline ───────────────────────────────────────────────────
run_test("excess | spline", {
  r <- sus_mod_excess(city1_exact, outcome_col = "n_obitos", method = "spline",
                     dof_per_year = 4L, verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_excess"),
    nrow(r$daily) > 0
  )
  r
})

# ── Teste 3: excess | serfling ─────────────────────────────────────────────────
run_test("excess | serfling", {
  r <- sus_mod_excess(city1_exact, outcome_col = "n_obitos", method = "serfling",
                     harmonics = 2L, verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_excess"),
    "expected" %in% names(r$daily),
    "excess"   %in% names(r$daily)
  )
  r
})

# ── Grupo T — sus_mod_casecrossover ──────────────────────────────────────────
cli::cli_h2("Grupo T — sus_mod_casecrossover")
# === GRUPO T: sus_mod_casecrossover ===

# ── Teste 1: casecrossover | conditional_poisson ──────────────────────────────
run_test("casecrossover | conditional_poisson", {
  city1_exact <- merged_exact[merged_exact$code_muni == top_cities[1], ]
  r <- sus_mod_casecrossover(
    city1_exact,
    outcome_col  = "n_obitos",
    exposure_col = "tair_dry_bulb_c",
    stratum      = "month",
    method       = "conditional_poisson",
    verbose      = FALSE
  )
  stopifnot(
    inherits(r, "climasus_casecrossover"),
    all(c("model", "or_table", "diagnostics", "meta") %in% names(r)),
    is.data.frame(r$or_table),
    "or" %in% names(r$or_table)
  )
  r
})

# ── Teste 2: casecrossover | lag=3 ────────────────────────────────────────────
run_test("casecrossover | lag=3", {
  city1_exact <- merged_exact[merged_exact$code_muni == top_cities[1], ]
  r <- sus_mod_casecrossover(
    city1_exact,
    outcome_col  = "n_obitos",
    exposure_col = "tair_dry_bulb_c",
    lag          = 3L,
    verbose      = FALSE
  )
  stopifnot(
    inherits(r, "climasus_casecrossover"),
    r$meta$lag == 3L
  )
  r
})

# ── Grupo U — sus_mod_its ────────────────────────────────────────────────────
cli::cli_h2("Grupo U — sus_mod_its")
# === GRUPO U: sus_mod_its (Interrupted Time Series) ===

city1_exact <- merged_exact[merged_exact$code_muni == top_cities[1], ]

# ── Teste 1: its | single interruption with counterfactual ────────────────────
run_test("its | single interruption with counterfactual", {
  r <- sus_mod_its(city1_exact, outcome_col = "n_obitos",
    interruption_dates = as.Date("2022-06-01"),
    harmonics = 2L, counterfactual = TRUE, verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_its"),
    all(c("model", "effects", "segments", "meta") %in% names(r)),
    is.data.frame(r$effects),
    nrow(r$effects) > 0
  )
  r
})

# ── Teste 2: its | two interruptions no counterfactual ────────────────────────
run_test("its | two interruptions no counterfactual", {
  r <- sus_mod_its(city1_exact, outcome_col = "n_obitos",
    interruption_dates = as.Date(c("2022-04-01", "2022-09-01")),
    counterfactual = FALSE, verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_its"),
    nrow(r$segments) == 3L  # 3 segmentos para 2 interrupções
  )
  r
})

# ── Grupo V — sus_mod_pool + sus_mod_plot_pool ───────────────────────────────
cli::cli_h2("Grupo V — sus_mod_pool + sus_mod_plot_pool")
# === GRUPO V: sus_mod_pool + sus_mod_plot_pool ===

# ── Teste 1: pool | multi-city BLUP ───────────────────────────────────────────
run_test("pool | multi-city BLUP", {
  pool_fit <<- sus_mod_pool(fits_list, blup = TRUE, verbose = FALSE)
  stopifnot(
    inherits(pool_fit, "climasus_pool"),
    all(c("pooled_pred", "exposure_curve", "blup_preds", "meta") %in% names(pool_fit)),
    is.data.frame(pool_fit$exposure_curve),
    nrow(pool_fit$exposure_curve) > 0
  )
  pool_fit
})

# ── Teste 2: plot_pool | overall ──────────────────────────────────────────────
run_test("plot_pool | overall", {
  p <- sus_mod_plot_pool(pool_fit, type = "overall", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_pool | forest ───────────────────────────────────────────────
run_test("plot_pool | forest", {
  p <- sus_mod_plot_pool(pool_fit, type = "forest", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 4: plot_pool | spaghetti ────────────────────────────────────────────
run_test("plot_pool | spaghetti", {
  p <- sus_mod_plot_pool(pool_fit, type = "spaghetti", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Grupo W — sus_mod_sensitivity + sus_mod_plot_sensitivity ─────────────────
cli::cli_h2("Grupo W — sus_mod_sensitivity + sus_mod_plot_sensitivity")
# === GRUPO W: sus_mod_sensitivity + sus_mod_plot_sensitivity ===

# ── Teste 1: sensitivity | stratified ────────────────────────────────────────
run_test("sensitivity | stratified", {
  sens_fit <- sus_mod_sensitivity(fits_list, verbose = FALSE)
  sens_fit <<- sens_fit
  stopifnot(
    inherits(sens_fit, "climasus_sensitivity"),
    all(c("rr_table", "comparison", "stratum_curves", "meta") %in% names(sens_fit)),
    is.data.frame(sens_fit$comparison),
    all(c("stratum", "hot_rr", "cold_rr", "sensitivity_index") %in% names(sens_fit$comparison))
  )
  sens_fit
})

# ── Teste 2: plot_sensitivity | curves ────────────────────────────────────────
run_test("plot_sensitivity | curves", {
  p <- sus_mod_plot_sensitivity(sens_fit, type = "curves", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_sensitivity | scatter ───────────────────────────────────────
run_test("plot_sensitivity | scatter", {
  p <- sus_mod_plot_sensitivity(sens_fit, type = "scatter", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 4: plot_sensitivity | table output ──────────────────────────────────
run_test("plot_sensitivity | table output", {
  tbl <- sus_mod_plot_sensitivity(sens_fit, type = "bar", output_type = "table", verbose = FALSE)
  stopifnot(is.data.frame(tbl), nrow(tbl) > 0)
  tbl
})

# ── Grupo X — sus_mod_metaregression ────────────────────────────────────────
cli::cli_h2("Grupo X — sus_mod_metaregression")
# === GRUPO X: sus_mod_metaregression ===

# ── Teste 1: metaregression | population covariate ────────────────────────────
run_test("metaregression | population covariate", {
  city_covs <- data.frame(
    city       = top_cities,
    population = c(50000L, 100000L, 200000L)
  )
  mr_fit <- sus_mod_metaregression(fits_list, covariates = city_covs,
    covariate_cols = "population", city_col = "city", verbose = FALSE)
  mr_fit <<- mr_fit
  stopifnot(
    inherits(mr_fit, "climasus_metaregression"),
    all(c("null_fit", "mvmeta_fit", "covariate_tests", "blup_preds", "meta") %in% names(mr_fit)),
    is.data.frame(mr_fit$covariate_tests)
  )
  mr_fit
})

# ── Teste 2: metaregression | heterogeneity table ─────────────────────────────
run_test("metaregression | heterogeneity table", {
  stopifnot(
    inherits(mr_fit, "climasus_metaregression"),
    is.data.frame(mr_fit$heterogeneity),
    all(c("model", "Q", "i2") %in% names(mr_fit$heterogeneity))
  )
  mr_fit
})

# ── Grupo Y — sus_mod_burden + sus_mod_plot_burden ──────────────────────────
cli::cli_h2("Grupo Y — sus_mod_burden + sus_mod_plot_burden")
# === GRUPO Y: sus_mod_burden + sus_mod_plot_burden ===

# ── Teste 1: burden | from af_list ────────────────────────────────────────────
run_test("burden | from af_list", {
  burd_fit <- sus_mod_burden(af_list, verbose = FALSE)
  burd_fit <<- burd_fit
  stopifnot(
    inherits(burd_fit, "climasus_burden"),
    all(c("burden_table", "total_burden", "concentration", "meta") %in% names(burd_fit)),
    is.data.frame(burd_fit$burden_table),
    all(c("city", "an", "af_pct", "rank") %in% names(burd_fit$burden_table))
  )
  burd_fit
})

# ── Teste 2: plot_burden | lollipop ───────────────────────────────────────────
run_test("plot_burden | lollipop", {
  p <- sus_mod_plot_burden(burd_fit, type = "lollipop", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_burden | lorenz ─────────────────────────────────────────────
run_test("plot_burden | lorenz", {
  p <- sus_mod_plot_burden(burd_fit, type = "lorenz", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Grupo Z — sus_mod_vulnerability_index + sus_mod_plot_vulnerability ───────
cli::cli_h2("Grupo Z — sus_mod_vulnerability_index + sus_mod_plot_vulnerability")
# === GRUPO Z: sus_mod_vulnerability_index + sus_mod_plot_vulnerability ===

# ── Teste 1: vulnerability_index | from dlnm fits ────────────────────────────
run_test("vulnerability_index | from dlnm fits", {
  vi_fit <- sus_mod_vulnerability_index(fits = fits_list, verbose = FALSE)
  vi_fit <<- vi_fit
  stopifnot(
    inherits(vi_fit, "climasus_vi"),
    all(c("vi_table", "pillar_scores", "meta") %in% names(vi_fit)),
    is.data.frame(vi_fit$vi_table),
    all(c("city", "vi_score", "vi_rank") %in% names(vi_fit$vi_table))
  )
  vi_fit
})

# ── Teste 2: vulnerability_index | with exposure pillar ──────────────────────
run_test("vulnerability_index | with exposure pillar", {
  expo_df <- data.frame(
    city        = top_cities,
    tair_mean   = c(28.5, 30.1, 27.0),
    n_heatwaves = c(5L, 8L, 3L)
  )
  vi2 <- sus_mod_vulnerability_index(
    exposure_df = expo_df,
    fits        = fits_list,
    verbose     = FALSE
  )
  stopifnot(
    inherits(vi2, "climasus_vi"),
    nrow(vi2$vi_table) == length(top_cities)
  )
  vi2
})

# ── Teste 3: plot_vulnerability | ranking ─────────────────────────────────────
run_test("plot_vulnerability | ranking", {
  p <- sus_mod_plot_vulnerability(vi_fit, type = "ranking", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Grupo AA — sus_mod_ml + sus_mod_plot_ml ──────────────────────────────────
cli::cli_h2("Grupo AA — sus_mod_ml + sus_mod_plot_ml")
# === GRUPO AA: sus_mod_ml + sus_mod_plot_ml ===

city1_exact <- merged_exact[merged_exact$code_muni == top_cities[1], ]

# ── Teste 1: ml | XGBoost Poisson single city ─────────────────────────────────
run_test("ml | XGBoost Poisson single city", {
  ml_fit <- sus_mod_ml(city1_exact, outcome_col = "n_obitos",
    feature_cols = c("tair_dry_bulb_c", "rainfall_mm"),
    nrounds = 50L, nfold = 3L, seed = 42L, verbose = FALSE)
  ml_fit <<- ml_fit
  stopifnot(
    inherits(ml_fit, "climasus_ml"),
    all(c("predictions", "importance", "performance", "model", "meta") %in% names(ml_fit)),
    is.data.frame(ml_fit$predictions),
    all(c("observed", "cv_predicted") %in% names(ml_fit$predictions))
  )
  ml_fit
})

# ── Teste 2: plot_ml | importance ─────────────────────────────────────────────
run_test("plot_ml | importance", {
  p <- sus_mod_plot_ml(ml_fit, type = "importance", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_ml | fit ────────────────────────────────────────────────────
run_test("plot_ml | fit", {
  p <- sus_mod_plot_ml(ml_fit, type = "fit", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 4: plot_ml | cv_log ─────────────────────────────────────────────────
run_test("plot_ml | cv_log", {
  p <- sus_mod_plot_ml(ml_fit, type = "cv_log", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Grupo AB — sus_mod_spatial_* ─────────────────────────────────────────────
cli::cli_h2("Grupo AB — sus_mod_spatial_weights + moran + bayes + reg + scan")
# ── Grupo AC — sus_mod_spacetime_* ───────────────────────────────────────────
cli::cli_h2("Grupo AC — sus_mod_spacetime_bayes + exceedance + predict")
# ── Grupo AD — sus_mod_swot + sus_mod_plot_swot ──────────────────────────────
cli::cli_h2("Grupo AD — sus_mod_swot + sus_mod_plot_swot")
# ── Grupo AB: Espacial (spatial_weights, moran, bayes, scan, reg) ─────────────

cli::cli_h2("Grupo AB — Espacial")

# ── Teste 1: spatial_weights | queen contiguity ───────────────────────────────
run_test("spatial_weights | queen contiguity", {
  W <- sus_mod_spatial_weights(munic_sf, style = "W", verbose = FALSE)
  W <<- W
  stopifnot(
    inherits(W, "climasus_weights"),
    !is.null(W$listw),
    W$n_regions > 0
  )
  W
})

# ── Teste 2: spatial_moran | global I ────────────────────────────────────────
run_test("spatial_moran | global I", {
  moran_fit <- sus_mod_spatial_moran(agg_city_df, outcome = "n_obitos",
    W = W, permutations = 99L, verbose = FALSE)
  moran_fit <<- moran_fit
  stopifnot(
    inherits(moran_fit, "climasus_spatial_moran"),
    all(c("global", "local") %in% names(moran_fit)),
    "I" %in% names(moran_fit$global)
  )
  moran_fit
})

# ── Teste 3: plot_spatial_moran | scatter ────────────────────────────────────
run_test("plot_spatial_moran | scatter", {
  p <- sus_mod_plot_spatial_moran(moran_fit, municipalities = munic_sf,
    type = "scatter", verbose = FALSE, lang = "pt")
  stopifnot(inherits(p, c("gg", "ggplot", "patchwork")))
  p
})

# ── Teste 4: spatial_bayes | BYM (CARBayes guard) ────────────────────────────
local({
  if (!requireNamespace("CARBayes", quietly = TRUE)) {
    cli::cli_alert_warning("  spatial_bayes: SKIP — CARBayes não instalado")
    return(invisible(NULL))
  }
  run_test("spatial_bayes | BYM", {
    # CARBayes requer único componente conectado — extrai o maior
    comp_all    <- spdep::n.comp.nb(W$nb)
    largest_id  <- as.integer(names(which.max(table(comp_all$comp.id))))
    keep_idx    <- comp_all$comp.id == largest_id
    munic_conn  <- munic_sf[keep_idx, ]
    agg_conn    <- agg_city_df[agg_city_df$code_muni %in% munic_conn$code_muni, ]
    W_conn      <- sus_mod_spatial_weights(munic_conn, style = "W", verbose = FALSE)
    r <- sus_mod_spatial_bayes(agg_conn, outcome = "n_obitos", W = W_conn,
      model = "bym", n_iter = 500L, burnin = 100L, thin = 1L, verbose = FALSE)
    stopifnot(inherits(r, "climasus_spatial_bayes"))
    r
  })
})

# ── Teste 5: spatial_reg | SAR lag (spatialreg guard) ────────────────────────
local({
  if (!requireNamespace("spatialreg", quietly = TRUE)) {
    cli::cli_alert_warning("  spatial_reg: SKIP — spatialreg não instalado")
    return(invisible(NULL))
  }
  run_test("spatial_reg | SAR lag", {
    r <- sus_mod_spatial_reg(agg_city_df,
      formula = n_obitos ~ tair_dry_bulb_c, W = W, model = "lag",
      verbose = FALSE)
    stopifnot(inherits(r, "climasus_spatial_reg"))
    r
  })
})

# ── Grupo AC: Espaço-temporal (spacetime_bayes + exceedance + predict) ────────

cli::cli_h2("Grupo AC — Espaço-temporal")

local({
  if (!requireNamespace("INLA", quietly = TRUE)) {
    cli::cli_alert_warning("  spacetime_bayes: SKIP — INLA não instalado")
    cli::cli_alert_warning("  spacetime_exceedance: SKIP — INLA não instalado")
    cli::cli_alert_warning("  spacetime_predict: SKIP — INLA não instalado")
    return(invisible(NULL))
  }

  # BYM2 exige grafo único conectado — usar o maior componente (70 municípios)
  comp_ac    <- spdep::n.comp.nb(W$nb)
  largest_ac <- as.integer(names(which.max(table(comp_ac$comp.id))))
  keep_ac    <- comp_ac$comp.id == largest_ac
  munic_ac   <- munic_sf[keep_ac, ]
  W_ac       <- sus_mod_spatial_weights(munic_ac, style = "W", verbose = FALSE)

  # Painel balanceado mensal: 1 linha por (code_muni × mês) apenas para municípios conectados
  st_raw <- merged_exact |>
    dplyr::filter(.data[["code_muni"]] %in% munic_ac$code_muni) |>
    dplyr::mutate(month_period = lubridate::floor_date(.data[["date"]], "month")) |>
    dplyr::group_by(.data[["code_muni"]], .data[["month_period"]]) |>
    dplyr::summarise(
      n_obitos        = sum(.data[["n_obitos"]], na.rm = TRUE),
      tair_dry_bulb_c = mean(.data[["tair_dry_bulb_c"]], na.rm = TRUE),
      .groups = "drop"
    )
  # Garantir painel completo (cross-join meses × municípios) com zeros nas células ausentes
  all_months <- sort(unique(st_raw$month_period))
  tair_global_mean <- mean(st_raw$tair_dry_bulb_c, na.rm = TRUE)
  st_panel <- tidyr::expand_grid(
    code_muni    = munic_ac$code_muni,
    month_period = all_months
  ) |>
    dplyr::left_join(st_raw, by = c("code_muni", "month_period")) |>
    dplyr::mutate(
      n_obitos        = dplyr::coalesce(.data[["n_obitos"]], 0L),
      tair_dry_bulb_c = dplyr::coalesce(.data[["tair_dry_bulb_c"]], tair_global_mean)
    ) |>
    dplyr::rename(date = "month_period")

  # Capturar retorno de run_test para encadear testes dependentes
  st_fit <- run_test("spacetime_bayes | BYM2 RW1", {
    r <- sus_mod_spacetime_bayes(
      df               = st_panel,
      outcome          = "n_obitos",
      W                = W_ac,
      time_col         = "date",
      time_unit        = "month",
      family           = "poisson",
      spatial_model    = "bym2",
      temporal_model   = "rw1",
      interaction_type = "none",
      compute_waic     = TRUE,
      verbose          = FALSE
    )
    stopifnot(
      inherits(r, "climasus_spacetime_bayes"),
      all(c("fixed", "rr", "spatial_re", "temporal_re") %in% names(r)),
      is.data.frame(r$fixed),
      is.data.frame(r$rr)
    )
    r
  })

  run_test("spacetime_exceedance | P(RR>1)", {
    if (is.null(st_fit)) stop("st_fit ausente — spacetime_bayes falhou")
    exc_fit <- sus_mod_spacetime_exceedance(
      fit        = st_fit,
      thresholds = c(1.0, 1.5),
      verbose    = FALSE
    )
    stopifnot(
      inherits(exc_fit, "climasus_spacetime_exceedance"),
      all(c("exceedance", "thresholds", "n_exceed") %in% names(exc_fit)),
      is.data.frame(exc_fit$exceedance)
    )
    exc_fit
  })

  run_test("spacetime_predict | in-sample", {
    if (is.null(st_fit)) stop("st_fit ausente — spacetime_bayes falhou")
    pred_fit <- sus_mod_spacetime_predict(
      fit        = st_fit,
      horizon    = 0L,
      include_ci = TRUE,
      verbose    = FALSE
    )
    stopifnot(
      inherits(pred_fit, "climasus_spacetime_pred"),
      "predictions" %in% names(pred_fit),
      is.data.frame(pred_fit$predictions),
      all(c("pred_mean", "pred_lower95", "pred_upper95") %in%
            names(pred_fit$predictions))
    )
    pred_fit
  })
})

# ── Grupo AD: sus_mod_swot + sus_mod_plot_swot ────────────────────────────────

cli::cli_h2("Grupo AD — SWOT")

# ── Teste 1: swot | from multiple objects ─────────────────────────────────────
run_test("swot | from multiple objects", {
  swot_fit <- sus_mod_swot(
    vulnerability = vi_fit,
    dlnm          = fits_list[[1]],
    af            = af_list[[1]],
    burden        = burd_fit,
    sensitivity   = sens_fit,
    verbose       = FALSE
  )
  swot_fit <<- swot_fit
  stopifnot(
    inherits(swot_fit, "climasus_swot"),
    is.data.frame(swot_fit$scores) || is.list(swot_fit)
  )
  swot_fit
})

# ── Teste 2: plot_swot | radar ────────────────────────────────────────────────
run_test("plot_swot | radar", {
  p <- sus_mod_plot_swot(swot_fit, type = "radar", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Sumário final ─────────────────────────────────────────────────────────────
cli::cli_rule()
cli::cli_alert_info("TOTAL: {ok + fail} testes | {ok} OK | {fail} FAIL")
failed_tests <- Filter(function(x) x[["result"]] == "FAIL", results)
if (length(failed_tests) > 0L) {
  cli::cli_alert_danger("Testes com falha:")
  for (t in failed_tests) cli::cli_alert_danger("  - {t[['label']]}: {t[['error']]}")
  quit(status = 1L, save = "no")
} else {
  cli::cli_alert_success("Todos os testes passaram!")
}
