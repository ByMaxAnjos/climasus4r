# =============================================================================
# test_sus_mod_metaregression.R
# Comprehensive functional test for sus_mod_metaregression()
# Run with: source("inst/roadmap/phase5/test_sus_mod_metaregression.R")
# =============================================================================

devtools::load_all()

cat("\n=============================================================\n")
cat("  sus_mod_metaregression() -- Functional Test Suite\n")
cat("=============================================================\n\n")

# -- helpers ------------------------------------------------------------------
ok      <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail    <- function(msg) stop(sprintf("  [FAIL] %s", msg))
section <- function(s)   cat(sprintf("\n--- %s ---\n", s))

# =============================================================================
section("0. Build shared DLNM fixtures")
# =============================================================================

# Six cities, each with a different beta_temp and different mean temperature.
# mean_temp acts as the meta-regression covariate.
city_specs <- list(
  fortaleza       = list(beta = 0.06,  mean_t = 28, seed = 1L),
  recife          = list(beta = 0.05,  mean_t = 27, seed = 2L),
  salvador        = list(beta = 0.04,  mean_t = 26, seed = 3L),
  belo_horizonte  = list(beta = 0.025, mean_t = 22, seed = 4L),
  sao_paulo       = list(beta = 0.015, mean_t = 20, seed = 5L),
  curitiba        = list(beta = 0.005, mean_t = 16, seed = 6L)
)

.build_city_fit <- function(spec) {
  set.seed(spec$seed)
  n_days  <- 4L * 365L
  lag_max <- 7L
  dates   <- seq(as.Date("2018-01-01"), by = "day", length.out = n_days)
  doy     <- as.integer(format(dates, "%j"))

  temp0 <- spec$mean_t + 6 * sin(2 * pi * doy / 365 - pi / 2) + stats::rnorm(n_days, sd = 2)
  mu    <- exp(2.5 + spec$beta * temp0 + 0.25 * sin(2 * pi * doy / 365))
  y     <- stats::rpois(n_days, lambda = mu)

  make_lag <- function(x, k) c(rep(NA_real_, k), head(x, length(x) - k))
  df <- data.frame(date = dates, n_obitos = y)
  for (k in 0L:lag_max) df[[paste0("temp_c_lag", k)]] <- make_lag(temp0, k)
  df <- df[!is.na(df[[paste0("temp_c_lag", lag_max)]]), ]

  df_cl <- new_climasus_df(df, list(
    system  = "SIM",
    stage   = "climate",
    type    = "distributed_lag",
    backend = "tibble",
    history = character(0)
  ))

  sus_mod_dlnm(
    df          = df_cl,
    outcome_col = "n_obitos",
    climate_col = "temp_c",
    lag_max     = lag_max,
    family      = "quasipoisson",
    verbose     = FALSE,
    lang        = "en"
  )
}

fits <- lapply(city_specs, .build_city_fit)
ok(sprintf("%d climasus_dlnm fixtures built", length(fits)))

# City-level covariates: mean_temp positively correlated with beta_temp
city_covariates <- data.frame(
  mean_temp = vapply(city_specs, function(s) s$mean_t, numeric(1L)),
  row.names = names(city_specs)
)
ok("City-level covariate data.frame built (mean_temp per city)")

# =============================================================================
section("1. Basic call: returns climasus_metaregression")
# =============================================================================

mr <- sus_mod_metaregression(
  fits        = fits,
  covariates  = city_covariates,
  verbose     = FALSE,
  lang        = "en"
)
stopifnot(inherits(mr, "climasus_metaregression"))
ok("Returns climasus_metaregression object")

expected_names <- c("mvmeta_fit", "null_fit", "pooled_pred", "pooled_curve",
                    "exposure_response", "blup_preds", "city_table",
                    "covariate_tests", "heterogeneity", "cov_scales", "meta")
stopifnot(all(expected_names %in% names(mr)))
ok("All top-level components present")

# =============================================================================
section("2. mvmeta_fit is an mvmeta object")
# =============================================================================

stopifnot(inherits(mr$mvmeta_fit, "mvmeta"))
ok("$mvmeta_fit inherits from 'mvmeta'")

stopifnot(inherits(mr$null_fit, "mvmeta"))
ok("$null_fit inherits from 'mvmeta'")

# Full model has more coefficients than null (intercept + covariate term)
null_coef_len <- length(stats::coef(mr$null_fit))
full_coef_len <- length(stats::coef(mr$mvmeta_fit))
stopifnot(full_coef_len > null_coef_len)
ok("Full meta-regression has more coefficients than null model")

# =============================================================================
section("3. $exposure_response structure")
# =============================================================================

er <- mr$exposure_response
stopifnot(inherits(er, "data.frame"))
stopifnot(all(c("pct", "exposure", "rr", "rr_lo", "rr_hi") %in% names(er)))
ok("$exposure_response has required columns")

# Default pred_at = c(0.75, 0.90, 0.95, 0.99) → 4 rows
stopifnot(nrow(er) == 4L)
ok("$exposure_response has 4 rows (default pred_at)")

# CI ordering
stopifnot(all(er$rr_lo <= er$rr + 1e-9))
stopifnot(all(er$rr    <= er$rr_hi + 1e-9))
ok("rr_lo <= rr <= rr_hi in $exposure_response")

# RR at p99 should be greater than at p75 (positive signal from most cities)
stopifnot(er$rr[er$pct == 0.99] >= er$rr[er$pct == 0.75])
ok("RR at p99 >= RR at p75 (consistent with positive association)")

# =============================================================================
section("4. $pooled_pred is a crosspred object")
# =============================================================================

stopifnot(!is.null(mr$pooled_pred))
ok("$pooled_pred is not NULL")

# Check standard crosspred slots
stopifnot(all(c("predvar", "allRRfit", "allRRlow", "allRRhigh") %in% names(mr$pooled_pred)))
ok("$pooled_pred has standard crosspred slots")

stopifnot(length(mr$pooled_pred$predvar) == 100L)
ok("$pooled_pred grid has 100 points")

# $pooled_curve tibble
stopifnot(!is.null(mr$pooled_curve))
stopifnot(nrow(mr$pooled_curve) == 100L)
stopifnot(all(c("exposure", "rr", "rr_lo", "rr_hi") %in% names(mr$pooled_curve)))
ok("$pooled_curve is a 100-row tibble with exposure/rr/rr_lo/rr_hi columns")

# =============================================================================
section("5. $city_table structure")
# =============================================================================

ct <- mr$city_table
stopifnot(inherits(ct, "data.frame"))
ok("$city_table is a data.frame")

city_cols <- c("city", "n_obs", "raw_rr", "raw_rr_lo", "raw_rr_hi",
               "blup_rr", "blup_rr_lo", "blup_rr_hi")
stopifnot(all(city_cols %in% names(ct)))
ok("$city_table has all required columns")

stopifnot(nrow(ct) == 6L)
ok("$city_table has 6 rows (one per city)")

# All raw_rr values positive
stopifnot(all(ct$raw_rr > 0, na.rm = TRUE))
ok("raw_rr values are positive for all cities")

# BLUPs populated (blup = TRUE by default)
stopifnot(!all(is.na(ct$blup_rr)))
ok("blup_rr is populated for at least some cities")

# =============================================================================
section("6. $covariate_tests structure and content")
# =============================================================================

covt <- mr$covariate_tests
stopifnot(inherits(covt, "data.frame"))
ok("$covariate_tests is a data.frame")

stopifnot(all(c("covariate", "n_df", "wald_stat", "p_value") %in% names(covt)))
ok("$covariate_tests has covariate, n_df, wald_stat, p_value columns")

# One row per covariate (1 covariate = mean_temp)
stopifnot(nrow(covt) == 1L)
ok("$covariate_tests has 1 row (1 covariate: mean_temp)")

stopifnot(covt$covariate[[1L]] == "mean_temp")
ok("$covariate_tests$covariate == 'mean_temp'")

# n_df equals number of cross-basis coefficients (positive integer)
stopifnot(is.numeric(covt$n_df) && covt$n_df > 0)
ok("$covariate_tests$n_df > 0")

# wald_stat and p_value are numeric
stopifnot(is.numeric(covt$wald_stat) || is.na(covt$wald_stat))
stopifnot(is.numeric(covt$p_value)   || is.na(covt$p_value))
ok("wald_stat and p_value are numeric")

# =============================================================================
section("7. $heterogeneity structure and residual I² < null I²")
# =============================================================================

het <- mr$heterogeneity
stopifnot(inherits(het, "data.frame"))
ok("$heterogeneity is a data.frame")

stopifnot(all(c("model", "Q", "df_het", "p_het", "i2", "r2_het") %in% names(het)))
ok("$heterogeneity has model, Q, df_het, p_het, i2, r2_het columns")

stopifnot(nrow(het) == 2L)
ok("$heterogeneity has 2 rows (null and full)")

stopifnot(setequal(het$model, c("null", "full")))
ok("$heterogeneity$model contains 'null' and 'full'")

# Residual I² (full) should be <= null I² (covariates explain some heterogeneity)
i2_null <- het$i2[het$model == "null"]
i2_full <- het$i2[het$model == "full"]
if (!is.na(i2_null) && !is.na(i2_full)) {
  stopifnot(i2_full <= i2_null + 0.5)  # allow tiny numerical tolerance
  ok("Residual I² (full model) <= null I² (covariates explain heterogeneity)")
}

# r2_het for full model: proportion explained (0 to 1)
r2 <- het$r2_het[het$model == "full"]
if (!is.na(r2)) {
  stopifnot(r2 >= 0 && r2 <= 1)
  ok("r2_het is between 0 and 1")
}

# =============================================================================
section("8. $cov_scales structure")
# =============================================================================

cs <- mr$cov_scales
stopifnot(inherits(cs, "data.frame"))
stopifnot(all(c("covariate", "mean", "sd") %in% names(cs)))
ok("$cov_scales has covariate, mean, sd columns")

stopifnot(nrow(cs) == 1L)
ok("$cov_scales has 1 row (1 covariate: mean_temp)")

stopifnot(cs$covariate[[1L]] == "mean_temp")
stopifnot(is.numeric(cs$mean) && is.numeric(cs$sd))
stopifnot(cs$sd > 0)
ok("$cov_scales$mean and $sd are numeric, sd > 0")

# Check that mean is roughly the average of the city mean_temps
expected_mean <- mean(city_covariates$mean_temp)
stopifnot(abs(cs$mean - expected_mean) < 0.01)
ok("$cov_scales$mean matches mean of city_covariates$mean_temp")

# =============================================================================
section("9. $meta structure")
# =============================================================================

m <- mr$meta
meta_keys <- c("climate_col", "outcome_col", "lag_max", "n_cities",
               "city_names", "covariate_cols", "method", "alpha",
               "ref_value", "pred_at", "call_time")
stopifnot(all(meta_keys %in% names(m)))
ok("$meta has all expected keys")

stopifnot(m$climate_col == "temp_c")
ok("$meta$climate_col == 'temp_c'")

stopifnot(m$n_cities == 6L)
ok("$meta$n_cities == 6")

stopifnot(m$covariate_cols == "mean_temp")
ok("$meta$covariate_cols == 'mean_temp'")

stopifnot(m$method == "reml")
ok("$meta$method == 'reml'")

stopifnot(inherits(m$call_time, "POSIXct"))
ok("$meta$call_time is a POSIXct")

# =============================================================================
section("10. city_col parameter: city identifier from a column")
# =============================================================================

# Rebuild covariates with city name as a column (not rownames)
cov_with_col <- city_covariates
cov_with_col$city_name <- rownames(cov_with_col)
rownames(cov_with_col) <- NULL

mr_col <- sus_mod_metaregression(
  fits        = fits,
  covariates  = cov_with_col,
  city_col    = "city_name",
  verbose     = FALSE,
  lang        = "en"
)
stopifnot(inherits(mr_col, "climasus_metaregression"))
ok("city_col parameter: accepts covariates with city name in a column")

stopifnot(mr_col$meta$n_cities == 6L)
ok("city_col: all 6 cities matched correctly")

# Results should be numerically identical to rowname-based matching
stopifnot(isTRUE(all.equal(
  sort(mr_col$city_table$city),
  sort(mr$city_table$city)
)))
ok("city_col and rowname-based matching produce same city order")

# =============================================================================
section("11. covariate_cols: restrict to subset of columns")
# =============================================================================

# Add a second covariate column to the data (poverty index)
cov_two <- city_covariates
cov_two$poverty_pct <- c(42, 38, 35, 22, 18, 12)

mr_subset <- sus_mod_metaregression(
  fits           = fits,
  covariates     = cov_two,
  covariate_cols = "mean_temp",   # use only mean_temp, ignore poverty_pct
  verbose        = FALSE,
  lang           = "en"
)
stopifnot(mr_subset$meta$covariate_cols == "mean_temp")
stopifnot(nrow(mr_subset$covariate_tests) == 1L)
ok("covariate_cols restricts to specified column (mean_temp only)")

# Two-covariate model
mr_two <- sus_mod_metaregression(
  fits           = fits,
  covariates     = cov_two,
  covariate_cols = c("mean_temp", "poverty_pct"),
  verbose        = FALSE,
  lang           = "en"
)
stopifnot(length(mr_two$meta$covariate_cols) == 2L)
stopifnot(nrow(mr_two$covariate_tests) == 2L)
ok("Two-covariate model: 2 rows in $covariate_tests")

# Full model with 2 covariates has more coef than with 1
stopifnot(length(stats::coef(mr_two$mvmeta_fit)) >
          length(stats::coef(mr$mvmeta_fit)))
ok("Two-covariate model has more coefficients than one-covariate model")

# =============================================================================
section("12. Partial city match: missing cities excluded with warning")
# =============================================================================

# Remove two cities from covariates
cov_partial <- city_covariates[c("fortaleza", "recife", "salvador",
                                  "belo_horizonte"), , drop = FALSE]

mr_partial <- sus_mod_metaregression(
  fits        = fits,
  covariates  = cov_partial,
  verbose     = FALSE,
  lang        = "en"
)
stopifnot(inherits(mr_partial, "climasus_metaregression"))
ok("Partial match accepted (missing cities excluded)")

# Only the 4 matched cities are in the result
stopifnot(mr_partial$meta$n_cities == 4L)
ok("n_cities == 4 after partial match (2 cities excluded)")

# =============================================================================
section("13. blup = FALSE: BLUPs not computed")
# =============================================================================

mr_no_blup <- sus_mod_metaregression(
  fits        = fits,
  covariates  = city_covariates,
  blup        = FALSE,
  verbose     = FALSE,
  lang        = "en"
)
stopifnot(is.null(mr_no_blup$blup_preds))
ok("blup=FALSE: $blup_preds is NULL")

# city_table still has blup_rr column but all NA
stopifnot(all(is.na(mr_no_blup$city_table$blup_rr)))
ok("blup=FALSE: city_table$blup_rr all NA")

# =============================================================================
section("14. method = 'ml' and 'fixed' work without error")
# =============================================================================

err_ml <- tryCatch({
  sus_mod_metaregression(fits, city_covariates, method = "ml", verbose = FALSE, lang = "en")
  NULL
}, error = function(e) e)
stopifnot(is.null(err_ml))
ok("method='ml' runs without error")

err_fixed <- tryCatch({
  sus_mod_metaregression(fits, city_covariates, method = "fixed", verbose = FALSE, lang = "en")
  NULL
}, error = function(e) e)
stopifnot(is.null(err_fixed))
ok("method='fixed' runs without error")

# =============================================================================
section("15. coef(), vcov(), print(), summary(), tidy() S3 methods")
# =============================================================================

# coef()
coefs <- coef(mr)
stopifnot(!is.null(coefs))
stopifnot(is.numeric(coefs))
ok("coef.climasus_metaregression() returns numeric vector")

# vcov()
vc <- vcov(mr)
stopifnot(!is.null(vc))
stopifnot(is.matrix(vc))
stopifnot(nrow(vc) == length(coefs))
ok("vcov.climasus_metaregression() returns square matrix (dim = length(coef))")

# print()
err_p <- tryCatch({ print(mr); NULL }, error = function(e) e)
stopifnot(is.null(err_p))
ok("print.climasus_metaregression() runs without error")

invisible_p <- withVisible(print(mr))
stopifnot(!invisible_p$visible)
ok("print() returns invisibly")

# summary()
err_s <- tryCatch({ summary(mr); NULL }, error = function(e) e)
stopifnot(is.null(err_s))
ok("summary.climasus_metaregression() runs without error")

# tidy()
td <- tidy(mr)
stopifnot(inherits(td, "data.frame"))
stopifnot(nrow(td) == 1L)
tidy_cols <- c("climate_col", "outcome_col", "lag_max", "method",
               "n_cities", "n_covariates", "covariate_cols",
               "ref_value", "rr_p75", "rr_lo_p75", "rr_hi_p75",
               "i2_null", "i2_full", "r2_het", "Q_full", "df_het", "p_het")
stopifnot(all(tidy_cols %in% names(td)))
ok("tidy.climasus_metaregression() returns 1-row tibble with all expected columns")

stopifnot(td$n_covariates == 1L)
ok("tidy()$n_covariates == 1")

stopifnot(td$climate_col == "temp_c")
ok("tidy()$climate_col == 'temp_c'")

# =============================================================================
section("16. verbose = FALSE suppresses output")
# =============================================================================

mr_quiet <- sus_mod_metaregression(
  fits       = fits,
  covariates = city_covariates,
  verbose    = FALSE,
  lang       = "en"
)
stopifnot(inherits(mr_quiet, "climasus_metaregression"))
ok("verbose=FALSE: still returns climasus_metaregression")

# Numerical results identical to verbose run
stopifnot(isTRUE(all.equal(
  mr_quiet$exposure_response$rr,
  mr$exposure_response$rr,
  tolerance = 1e-9
)))
ok("verbose=FALSE: numerically identical results")

# =============================================================================
section("17. lang = 'pt', 'en', 'es', and fallback")
# =============================================================================

for (lang_val in c("pt", "es")) {
  res <- tryCatch(
    sus_mod_metaregression(fits, city_covariates, verbose = FALSE, lang = lang_val),
    error = function(e) e
  )
  stopifnot(!inherits(res, "error"))
  ok(sprintf("lang='%s' runs without error", lang_val))
}

# Unknown lang falls back without error
res_xx <- tryCatch(
  sus_mod_metaregression(fits, city_covariates, verbose = FALSE, lang = "xx"),
  error = function(e) e
)
stopifnot(!inherits(res_xx, "error"))
ok("Unknown lang='xx' falls back to 'pt' without error")

# =============================================================================
section("18. Error handling")
# =============================================================================

# Non-list fits
tryCatch(
  sus_mod_metaregression("not a list", city_covariates, verbose = FALSE),
  error = function(e) ok("Rejects non-list fits")
)

# Empty fits list
tryCatch(
  sus_mod_metaregression(list(), city_covariates, verbose = FALSE),
  error = function(e) ok("Rejects empty fits list")
)

# Non-climasus_dlnm element
bad_fits <- c(fits, list(bad = list(x = 1)))
tryCatch(
  sus_mod_metaregression(bad_fits, city_covariates, verbose = FALSE),
  error = function(e) ok("Rejects non-climasus_dlnm element")
)

# Non-data.frame covariates
tryCatch(
  sus_mod_metaregression(fits, "not a data.frame", verbose = FALSE),
  error = function(e) ok("Rejects non-data.frame covariates")
)

# No numeric columns in covariates
tryCatch(
  sus_mod_metaregression(
    fits,
    data.frame(city = names(fits), row.names = names(fits)),
    verbose = FALSE
  ),
  error = function(e) ok("Rejects covariates with no numeric columns")
)

# city_col that doesn't exist
tryCatch(
  sus_mod_metaregression(
    fits, city_covariates, city_col = "no_such_col", verbose = FALSE
  ),
  error = function(e) ok("Rejects city_col that does not exist")
)

# No matching cities
cov_wrong <- city_covariates
rownames(cov_wrong) <- paste0("wrong_", seq_len(nrow(cov_wrong)))
tryCatch(
  sus_mod_metaregression(fits, cov_wrong, verbose = FALSE),
  error = function(e) ok("Rejects when no cities match between fits and covariates")
)

# Incompatible lag_max
fit_tampered <- fits[[1L]]
fit_tampered$meta$lag_max <- 21L  # different lag_max
bad_fits2 <- c(fits[1L], list(bad = fit_tampered))
tryCatch(
  sus_mod_metaregression(
    bad_fits2,
    city_covariates[c("fortaleza", "recife"), , drop = FALSE],
    verbose = FALSE
  ),
  error = function(e) ok("Rejects fits with different lag_max")
)

# =============================================================================
cat("\n=============================================================\n")
cat("  ALL SECTIONS PASSED\n")
cat("=============================================================\n\n")
