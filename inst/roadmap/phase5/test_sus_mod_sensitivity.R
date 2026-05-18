# =============================================================================
# test_sus_mod_sensitivity.R
# Comprehensive functional test for sus_mod_sensitivity()
# Run with: source("inst/roadmap/phase5/test_sus_mod_sensitivity.R")
# =============================================================================

devtools::load_all()

cat("\n=============================================================\n")
cat("  sus_mod_sensitivity() -- Functional Test Suite\n")
cat("=============================================================\n\n")

# -- helpers ------------------------------------------------------------------
ok      <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail    <- function(msg) stop(sprintf("  [FAIL] %s", msg))
section <- function(s)   cat(sprintf("\n--- %s ---\n", s))

# =============================================================================
section("0. Build shared DLNM fixtures (3 strata, different sensitivities)")
# =============================================================================

# Same seed + same underlying noise, three different beta_temp values.
# 5 years and large beta differences make the hot_rr ordering deterministic.
#   high:   beta = 0.10  -> p99 hot RR ~ 2.95
#   medium: beta = 0.05  -> p99 hot RR ~ 1.59
#   low:    beta = 0.001 -> p99 hot RR ~ 1.15
.build_fit <- function(beta_temp, seed = 42L) {
  set.seed(seed)
  n_days  <- 5L * 365L
  lag_max <- 7L
  dates   <- seq(as.Date("2019-01-01"), by = "day", length.out = n_days)
  doy     <- as.integer(format(dates, "%j"))

  temp0 <- 25 + 8 * sin(2 * pi * doy / 365 - pi / 2) + stats::rnorm(n_days, sd = 2)
  mu    <- exp(2.5 + beta_temp * temp0 + 0.3 * sin(2 * pi * doy / 365))
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

fit_high   <- .build_fit(beta_temp = 0.10,  seed = 42L)
fit_medium <- .build_fit(beta_temp = 0.05,  seed = 42L)
fit_low    <- .build_fit(beta_temp = 0.001, seed = 42L)

stopifnot(inherits(fit_high,   "climasus_dlnm"))
stopifnot(inherits(fit_medium, "climasus_dlnm"))
stopifnot(inherits(fit_low,    "climasus_dlnm"))
ok("3 climasus_dlnm fixtures built (beta 0.10 / 0.05 / 0.001, same seed)")

fits_named <- list(
  high   = fit_high,
  medium = fit_medium,
  low    = fit_low
)
ok("Named fits list ready")

# =============================================================================
section("1. Basic call: returns climasus_sensitivity")
# =============================================================================

sens <- sus_mod_sensitivity(
  fits_named,
  hot_percentile  = 0.99,
  cold_percentile = 0.01,
  verbose         = FALSE,
  lang            = "en"
)
stopifnot(inherits(sens, "climasus_sensitivity"))
ok("Returns climasus_sensitivity object")

stopifnot(all(c("rr_table", "comparison", "stratum_curves", "meta") %in% names(sens)))
ok("Top-level names: rr_table, comparison, stratum_curves, meta")

# =============================================================================
section("2. Unnamed fits: auto-labels as 'Stratum 1', 'Stratum 2', etc.")
# =============================================================================

fits_unnamed <- list(fit_high, fit_medium)
sens_un <- sus_mod_sensitivity(fits_unnamed, verbose = FALSE, lang = "en")
stopifnot(inherits(sens_un, "climasus_sensitivity"))
ok("Accepts unnamed list")

stopifnot(all(c("Stratum 1", "Stratum 2") %in% unique(sens_un$comparison$stratum)))
ok("Auto-labels 'Stratum 1', 'Stratum 2' for unnamed list")

# =============================================================================
section("3. $rr_table structure")
# =============================================================================

rt <- sens$rr_table
stopifnot(inherits(rt, "data.frame"))
ok("$rr_table is a data.frame")

required_cols <- c("stratum", "label", "component", "quantile_prob",
                   "exposure", "rr", "rr_lo", "rr_hi", "ref_exposure")
stopifnot(all(required_cols %in% names(rt)))
ok("$rr_table has all required columns")

# 3 strata × 2 components (hot + cold) = 6 rows
stopifnot(nrow(rt) == 6L)
ok("$rr_table has 6 rows (3 strata x 2 components)")

stopifnot(setequal(unique(rt$component), c("hot", "cold")))
ok("$rr_table$component values are 'hot' and 'cold'")

hot_rows  <- rt[rt$component == "hot",  ]
cold_rows <- rt[rt$component == "cold", ]
stopifnot(all(abs(hot_rows$quantile_prob  - 0.99) < 1e-9))
stopifnot(all(abs(cold_rows$quantile_prob - 0.01) < 1e-9))
ok("quantile_prob values match hot_percentile=0.99 and cold_percentile=0.01")

# =============================================================================
section("4. $comparison structure")
# =============================================================================

cm <- sens$comparison
stopifnot(inherits(cm, "data.frame"))
ok("$comparison is a data.frame")

comp_cols <- c("stratum", "label", "hot_rr", "hot_rr_lo", "hot_rr_hi",
               "cold_rr", "cold_rr_lo", "cold_rr_hi",
               "sensitivity_index", "hot_rank", "cold_rank")
stopifnot(all(comp_cols %in% names(cm)))
ok("$comparison has all required columns")

stopifnot(nrow(cm) == 3L)
ok("$comparison has 3 rows (one per stratum)")

stopifnot(setequal(cm$stratum, c("high", "medium", "low")))
ok("$comparison contains all 3 stratum names")

# =============================================================================
section("5. $stratum_curves structure")
# =============================================================================

sc <- sens$stratum_curves
stopifnot(inherits(sc, "data.frame"))
ok("$stratum_curves is a data.frame")

curve_cols <- c("stratum", "label", "exposure", "rr", "rr_lo", "rr_hi")
stopifnot(all(curve_cols %in% names(sc)))
ok("$stratum_curves has all required columns")

# 3 strata × 100 crosspred grid points
stopifnot(nrow(sc) == 3L * 100L)
ok("$stratum_curves has 300 rows (3 strata x 100 grid points)")

stopifnot(setequal(unique(sc$stratum), c("high", "medium", "low")))
ok("$stratum_curves covers all 3 strata")

# =============================================================================
section("6. Hot RR direction: high-beta > medium-beta > low-beta")
# =============================================================================

rr_hot <- function(s) cm$hot_rr[cm$stratum == s]

stopifnot(rr_hot("high") > rr_hot("medium"))
ok("Hot RR: high > medium (betas 0.10 vs 0.05, same seed)")

stopifnot(rr_hot("medium") > rr_hot("low"))
ok("Hot RR: medium > low (betas 0.05 vs 0.001, same seed)")

stopifnot(rr_hot("high") > 1)
ok("Hot RR for high-beta stratum > 1 (positive temperature-mortality association)")

# =============================================================================
section("7. $comparison sorted by sensitivity_index descending")
# =============================================================================

si_vals <- cm$sensitivity_index
stopifnot(all(diff(si_vals) <= 0))
ok("$comparison rows are sorted by sensitivity_index descending")

# hot_rank reflects the hot_rr ordering: high=1, medium=2, low=3
stopifnot(cm$hot_rank[cm$stratum == "high"]   == 1L)
stopifnot(cm$hot_rank[cm$stratum == "medium"] == 2L)
stopifnot(cm$hot_rank[cm$stratum == "low"]    == 3L)
ok("hot_rank: high=1, medium=2, low=3")

# sensitivity_index values are all finite
stopifnot(all(is.finite(cm$sensitivity_index)))
ok("sensitivity_index is finite for all strata")

# =============================================================================
section("8. CI ordering: rr_lo <= rr <= rr_hi throughout")
# =============================================================================

stopifnot(all(rt$rr_lo <= rt$rr + 1e-9))
stopifnot(all(rt$rr    <= rt$rr_hi + 1e-9))
ok("rr_lo <= rr <= rr_hi in $rr_table (all 6 rows)")

stopifnot(all(cm$hot_rr_lo  <= cm$hot_rr   + 1e-9))
stopifnot(all(cm$hot_rr     <= cm$hot_rr_hi + 1e-9))
ok("hot_rr_lo <= hot_rr <= hot_rr_hi in $comparison")

stopifnot(all(cm$cold_rr_lo <= cm$cold_rr   + 1e-9))
stopifnot(all(cm$cold_rr    <= cm$cold_rr_hi + 1e-9))
ok("cold_rr_lo <= cold_rr <= cold_rr_hi in $comparison")

stopifnot(all(sc$rr_lo <= sc$rr + 1e-9))
stopifnot(all(sc$rr    <= sc$rr_hi + 1e-9))
ok("rr_lo <= rr <= rr_hi in $stratum_curves (all 300 rows)")

# =============================================================================
section("9. stratum_labels override")
# =============================================================================

labels_custom <- c(
  high   = "Alta Sensibilidade",
  medium = "Sensibilidade Media",
  low    = "Baixa Sensibilidade"
)
sens_lab <- sus_mod_sensitivity(
  fits_named,
  stratum_labels  = labels_custom,
  verbose         = FALSE,
  lang            = "en"
)

stopifnot(sens_lab$comparison$label[sens_lab$comparison$stratum == "high"]   == "Alta Sensibilidade")
stopifnot(sens_lab$comparison$label[sens_lab$comparison$stratum == "medium"] == "Sensibilidade Media")
stopifnot(sens_lab$comparison$label[sens_lab$comparison$stratum == "low"]    == "Baixa Sensibilidade")
ok("$comparison$label uses custom stratum_labels")

stopifnot(any(sens_lab$stratum_curves$label == "Alta Sensibilidade"))
ok("$stratum_curves$label also reflects custom labels")

# Partial override: only one stratum relabelled; others keep stratum name
sens_partial <- sus_mod_sensitivity(
  fits_named,
  stratum_labels = c(high = "High Group"),
  verbose        = FALSE,
  lang           = "en"
)
stopifnot(sens_partial$comparison$label[sens_partial$comparison$stratum == "high"]   == "High Group")
stopifnot(sens_partial$comparison$label[sens_partial$comparison$stratum == "medium"] == "medium")
ok("Partial override: relabelled entry uses custom label, others keep stratum name")

# =============================================================================
section("10. Custom percentiles (p95 hot / p05 cold)")
# =============================================================================

sens_p95 <- sus_mod_sensitivity(
  fits_named,
  hot_percentile  = 0.95,
  cold_percentile = 0.05,
  verbose         = FALSE,
  lang            = "en"
)

rt95 <- sens_p95$rr_table
stopifnot(all(abs(rt95$quantile_prob[rt95$component == "hot"]  - 0.95) < 1e-9))
stopifnot(all(abs(rt95$quantile_prob[rt95$component == "cold"] - 0.05) < 1e-9))
ok("Custom percentile p95/p05: quantile_prob stored correctly")

stopifnot(abs(sens_p95$meta$hot_percentile  - 0.95) < 1e-9)
stopifnot(abs(sens_p95$meta$cold_percentile - 0.05) < 1e-9)
ok("$meta$hot_percentile and cold_percentile match custom values")

# Less extreme hot percentile should give lower hot RR for high-beta stratum
rr95_high <- sens_p95$comparison$hot_rr[sens_p95$comparison$stratum == "high"]
rr99_high <- sens$comparison$hot_rr[sens$comparison$stratum == "high"]
stopifnot(rr95_high < rr99_high)
ok("p95 hot RR < p99 hot RR for high-beta stratum (less extreme percentile)")

# =============================================================================
section("11. tidy() output")
# =============================================================================

td <- tidy(sens)
stopifnot(inherits(td, "data.frame"))
ok("tidy() returns a data.frame")

stopifnot(nrow(td) == 3L)
ok("tidy() has 3 rows (one per stratum)")

tidy_cols <- c("climate_col", "hot_percentile", "cold_percentile", "n_strata",
               "stratum", "label", "hot_rr", "cold_rr",
               "sensitivity_index", "hot_rank", "cold_rank")
stopifnot(all(tidy_cols %in% names(td)))
ok("tidy() has all required metadata + comparison columns")

stopifnot(all(td$climate_col    == "temp_c"))
stopifnot(all(td$n_strata       == 3L))
stopifnot(all(abs(td$hot_percentile - 0.99) < 1e-9))
ok("tidy() metadata fields (climate_col, n_strata, hot_percentile) correct")

# tidy() should preserve the SI sort order
stopifnot(all(diff(td$sensitivity_index) <= 0))
ok("tidy() preserves sensitivity_index sort order")

# =============================================================================
section("12. print() runs without error and returns invisibly")
# =============================================================================

err_p <- tryCatch({ print(sens); NULL }, error = function(e) e)
stopifnot(is.null(err_p))
ok("print.climasus_sensitivity() runs without error")

invisible_result <- withVisible(print(sens))
stopifnot(!invisible_result$visible)
ok("print() returns x invisibly")

# =============================================================================
section("13. summary() runs without error and returns invisibly")
# =============================================================================

err_s <- tryCatch({ summary(sens); NULL }, error = function(e) e)
stopifnot(is.null(err_s))
ok("summary.climasus_sensitivity() runs without error")

invisible_s <- withVisible(summary(sens))
stopifnot(!invisible_s$visible)
ok("summary() returns object invisibly")

# =============================================================================
section("14. verbose = FALSE: no crash, identical results")
# =============================================================================

sens_quiet <- sus_mod_sensitivity(
  fits_named,
  verbose = FALSE,
  lang    = "en"
)
stopifnot(inherits(sens_quiet, "climasus_sensitivity"))
ok("verbose=FALSE: function still returns climasus_sensitivity")

stopifnot(isTRUE(all.equal(sens_quiet$comparison$hot_rr,
                            sens$comparison$hot_rr,
                            tolerance = 1e-9)))
ok("verbose=FALSE: results numerically identical to verbose=TRUE")

# =============================================================================
section("15. lang = 'en', 'es', and fallback for unknown lang")
# =============================================================================

err_en <- tryCatch({ sus_mod_sensitivity(fits_named, verbose = FALSE, lang = "en"); NULL },
                   error = function(e) e)
stopifnot(is.null(err_en))
ok("lang='en' runs without error")

err_es <- tryCatch({ sus_mod_sensitivity(fits_named, verbose = FALSE, lang = "es"); NULL },
                   error = function(e) e)
stopifnot(is.null(err_es))
ok("lang='es' runs without error")

err_xx <- tryCatch({ sus_mod_sensitivity(fits_named, verbose = FALSE, lang = "xx"); NULL },
                   error = function(e) e)
stopifnot(is.null(err_xx))
ok("lang='xx' falls back to 'pt' without error")

# =============================================================================
section("16. $meta fields complete and correct")
# =============================================================================

m <- sens$meta
stopifnot(is.list(m))
meta_keys <- c("climate_col", "n_strata", "stratum_names",
               "stratum_labels", "hot_percentile", "cold_percentile",
               "alpha", "call_time")
stopifnot(all(meta_keys %in% names(m)))
ok("$meta has all expected keys")

stopifnot(m$climate_col == "temp_c")
ok("$meta$climate_col == 'temp_c'")

stopifnot(m$n_strata == 3L)
ok("$meta$n_strata == 3")

stopifnot(setequal(m$stratum_names, c("high", "medium", "low")))
ok("$meta$stratum_names matches names(fits)")

stopifnot(abs(m$hot_percentile  - 0.99) < 1e-9)
stopifnot(abs(m$cold_percentile - 0.01) < 1e-9)
ok("$meta percentile values correct")

stopifnot(m$alpha == 0.05)
ok("$meta$alpha == 0.05")

stopifnot(inherits(m$call_time, "POSIXct"))
ok("$meta$call_time is a POSIXct timestamp")

# =============================================================================
section("17. RR near reference exposure is approximately 1.0 in stratum_curves")
# =============================================================================

for (nm in names(fits_named)) {
  fit    <- fits_named[[nm]]
  ref_x  <- as.numeric(fit$meta$ref_value)
  sc_sub <- sc[sc$stratum == nm, ]

  idx_ref   <- which.min(abs(sc_sub$exposure - ref_x))
  rr_at_ref <- sc_sub$rr[[idx_ref]]

  # crosspred centers at ref_value; nearest grid point should have RR ~1.0
  stopifnot(abs(rr_at_ref - 1.0) < 0.01)
}
ok("RR at nearest-to-reference grid point is within 0.01 of 1.0 for all strata")

# =============================================================================
section("18. Error handling")
# =============================================================================

# Single fit (< 2)
tryCatch(
  sus_mod_sensitivity(list(fit_high), verbose = FALSE),
  error = function(e) ok("Rejects list with fewer than 2 fits")
)

# Non-list input
tryCatch(
  sus_mod_sensitivity("not a list", verbose = FALSE),
  error = function(e) ok("Rejects non-list fits")
)

# Non-climasus_dlnm element in list
bad_list <- list(high = fit_high, broken = list(x = 1))
tryCatch(
  sus_mod_sensitivity(bad_list, verbose = FALSE),
  error = function(e) ok("Rejects list with non-climasus_dlnm element")
)

# Mismatched climate_col across fits
fit_tampered <- fit_medium
fit_tampered$meta$climate_col <- "rh_pct"
tryCatch(
  sus_mod_sensitivity(list(a = fit_high, b = fit_tampered), verbose = FALSE),
  error = function(e) ok("Rejects fits with different climate_col values")
)

# hot_percentile < cold_percentile
tryCatch(
  sus_mod_sensitivity(fits_named, hot_percentile = 0.05, cold_percentile = 0.95, verbose = FALSE),
  error = function(e) ok("Rejects hot_percentile < cold_percentile")
)

# hot_percentile out of (0, 1)
tryCatch(
  sus_mod_sensitivity(fits_named, hot_percentile = 1.5, verbose = FALSE),
  error = function(e) ok("Rejects hot_percentile = 1.5 (out of range)")
)

# hot_percentile == cold_percentile
tryCatch(
  sus_mod_sensitivity(fits_named, hot_percentile = 0.50, cold_percentile = 0.50, verbose = FALSE),
  error = function(e) ok("Rejects hot_percentile == cold_percentile")
)

# =============================================================================
cat("\n=============================================================\n")
cat("  ALL SECTIONS PASSED\n")
cat("=============================================================\n\n")
