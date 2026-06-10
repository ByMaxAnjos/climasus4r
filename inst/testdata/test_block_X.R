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
