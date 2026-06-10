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
