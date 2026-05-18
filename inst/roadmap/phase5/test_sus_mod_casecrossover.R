# =============================================================================
# test_sus_mod_casecrossover.R
# Comprehensive functional test for sus_mod_casecrossover()
# Run with: source("inst/roadmap/phase5/test_sus_mod_casecrossover.R")
# =============================================================================

devtools::load_all()
library(survival)

cat("\n=============================================================\n")
cat("  sus_mod_casecrossover() -- Functional Test Suite\n")
cat("=============================================================\n\n")

# -- helpers ------------------------------------------------------------------
ok      <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail    <- function(msg) stop(sprintf("  [FAIL] %s", msg))
section <- function(s)   cat(sprintf("\n--- %s ---\n", s))

# =============================================================================
section("0. Build shared fixtures")
# =============================================================================

set.seed(42)
n_days <- 4L * 365L
dates  <- seq(as.Date("2019-01-01"), by = "day", length.out = n_days)
doy    <- as.integer(format(dates, "%j"))

# Seasonal exposure with known positive association
temp   <- 22 + 8 * sin(2 * pi * doy / 365 - pi / 2) + stats::rnorm(n_days, sd = 2)
deaths <- stats::rpois(n_days, exp(2.2 + 0.025 * temp + 0.3 * sin(2 * pi * doy / 365)))
rh     <- 70 + 10 * cos(2 * pi * doy / 365) + stats::rnorm(n_days, sd = 3)

df_plain <- data.frame(
  date      = dates,
  n_obitos  = deaths,
  temp_c    = temp,
  rh_pct    = rh
)

ok(sprintf("Fixture: %d days, temp %.1f-%.1f deg, mean deaths %.1f/day",
           n_days, min(temp), max(temp), mean(deaths)))

# Also build a climasus_df fixture
df_cl <- ensure_climasus_df(
  df_plain, system = "SIM", stage = "climate", type = "exact"
)
ok("climasus_df fixture built")

# =============================================================================
section("1. Basic call: conditional_poisson, lag=0, plain data.frame")
# =============================================================================

cc0 <- sus_mod_casecrossover(
  df_plain,
  outcome_col  = "n_obitos",
  exposure_col = "temp_c",
  stratum      = "month",
  lag          = 0L,
  verbose      = FALSE,
  lang         = "en"
)
stopifnot(inherits(cc0, "climasus_casecrossover"))
ok("Returns climasus_casecrossover object")

stopifnot(cc0$meta$method == "conditional_poisson")
ok("meta$method == 'conditional_poisson'")

stopifnot(cc0$meta$lag == 0L)
ok("meta$lag == 0")

# =============================================================================
section("2. Input: climasus_df accepted")
# =============================================================================

cc_cl <- sus_mod_casecrossover(
  df_cl,
  outcome_col  = "n_obitos",
  exposure_col = "temp_c",
  stratum      = "month",
  lag          = 0L,
  verbose      = FALSE,
  lang         = "en"
)
stopifnot(inherits(cc_cl, "climasus_casecrossover"))
ok("climasus_df input accepted")

# OR should be very close to plain data.frame result (same data)
stopifnot(abs(cc_cl$or_table$or - cc0$or_table$or) < 1e-6)
ok("Same OR from climasus_df and data.frame inputs")

# =============================================================================
section("3. OR table structure and values")
# =============================================================================

tt <- cc0$or_table
stopifnot(inherits(tt, "data.frame"))
stopifnot(nrow(tt) == 1L)
stopifnot(all(c("term", "lag_spec", "estimate", "or", "or_lo", "or_hi", "p_value")
              %in% names(tt)))
ok("or_table has exactly 1 row and all required columns")

# OR > 1 because beta_temp = 0.025 (positive association)
stopifnot(tt$or > 1)
ok("OR > 1 (expected: positive temperature-mortality association)")

# CI ordering
stopifnot(tt$or_lo < tt$or)
stopifnot(tt$or < tt$or_hi)
ok("or_lo < or < or_hi")

# log-OR consistency
stopifnot(abs(log(tt$or) - tt$estimate) < 1e-9)
ok("log(or) == estimate (log-scale consistency)")

# =============================================================================
section("4. $model is a glm object for conditional_poisson")
# =============================================================================

stopifnot(inherits(cc0$model, "glm"))
ok("$model is a glm object")

stopifnot("exposure_val" %in% names(coef(cc0$model)))
ok("exposure_val term present in model coefficients")

# =============================================================================
section("5. $data: analysis dataset structure")
# =============================================================================

d <- cc0$data
stopifnot(inherits(d, "data.frame"))
stopifnot(all(c("date", "outcome_val", "exposure_val", "stratum_id") %in% names(d)))
ok("$data has required columns: date, outcome_val, exposure_val, stratum_id")

stopifnot(nrow(d) <= nrow(df_plain))
ok("$data has <= nrow(df_plain) rows (NAs removed)")

stopifnot(all(!is.na(d$exposure_val)))
ok("No NAs in $data$exposure_val after lag creation")

# =============================================================================
section("6. Stratum = 'month': correct year-month labels")
# =============================================================================

strata <- unique(cc0$data$stratum_id)
stopifnot(all(grepl("^[0-9]{4}-[0-9]{2}$", strata)))
ok("Monthly strata follow 'YYYY-MM' format")

# 4 years -> 48 months
stopifnot(cc0$diagnostics$n_strata == 48L)
ok("n_strata == 48 for 4-year monthly dataset")

# =============================================================================
section("7. Stratum = 'week': ISO year-week format")
# =============================================================================

cc_week <- sus_mod_casecrossover(
  df_plain,
  outcome_col  = "n_obitos",
  exposure_col = "temp_c",
  stratum      = "week",
  lag          = 0L,
  verbose      = FALSE,
  lang         = "en"
)
wk_strata <- unique(cc_week$data$stratum_id)
stopifnot(all(grepl("^[0-9]{4}-W[0-9]{2}$", wk_strata)))
ok("Weekly strata follow 'YYYY-Www' ISO format")

stopifnot(cc_week$diagnostics$n_strata >= 200L)
ok("n_strata >= 200 for 4-year weekly dataset")

# =============================================================================
section("8. Custom stratum column")
# =============================================================================

df_custom <- df_plain
df_custom$quarter <- paste0(
  format(df_custom$date, "%Y"), "-Q",
  ceiling(as.integer(format(df_custom$date, "%m")) / 3)
)
cc_q <- sus_mod_casecrossover(
  df_custom,
  outcome_col  = "n_obitos",
  exposure_col = "temp_c",
  stratum      = "quarter",
  lag          = 0L,
  verbose      = FALSE,
  lang         = "en"
)
stopifnot(all(grepl("^[0-9]{4}-Q[1-4]$", unique(cc_q$data$stratum_id))))
ok("Custom stratum column 'quarter' used correctly")

stopifnot(cc_q$diagnostics$n_strata == 16L)   # 4 years x 4 quarters
ok("n_strata == 16 for quarterly stratum over 4 years")

# =============================================================================
section("9. Lag = single non-zero value (lag 1)")
# =============================================================================

cc_lag1 <- sus_mod_casecrossover(
  df_plain,
  outcome_col  = "n_obitos",
  exposure_col = "temp_c",
  stratum      = "month",
  lag          = 1L,
  verbose      = FALSE,
  lang         = "en"
)
stopifnot(cc_lag1$or_table$lag_spec == "1")
ok("lag_spec == '1' for lag = 1")

# With lag=1, first row has NA -> slightly fewer obs
stopifnot(cc_lag1$diagnostics$n_obs < nrow(df_plain))
ok("n_obs < nrow(df_plain) due to lag-1 NA removal")

# =============================================================================
section("10. Lag = vector (moving average lag 0-6)")
# =============================================================================

cc_lag06 <- sus_mod_casecrossover(
  df_plain,
  outcome_col  = "n_obitos",
  exposure_col = "temp_c",
  stratum      = "month",
  lag          = 0L:6L,
  verbose      = FALSE,
  lang         = "en"
)
stopifnot(cc_lag06$or_table$lag_spec == "0-6")
ok("lag_spec == '0-6' for lag = 0:6 vector")

# Moving average should produce a positive association (same direction as lag 0)
stopifnot(cc_lag06$or_table$or > 1)
ok("Moving-average lag 0-6 OR > 1 (correct direction)")

# =============================================================================
section("11. Covariate inclusion")
# =============================================================================

cc_cov <- sus_mod_casecrossover(
  df_plain,
  outcome_col  = "n_obitos",
  exposure_col = "temp_c",
  covariates   = "rh_pct",
  stratum      = "month",
  lag          = 0L,
  verbose      = FALSE,
  lang         = "en"
)
stopifnot(inherits(cc_cov, "climasus_casecrossover"))
ok("Accepts covariates without error")

stopifnot("rh_pct" %in% names(coef(cc_cov$model)))
ok("Covariate 'rh_pct' present in model coefficients")

# exposure_val still the primary term
stopifnot("exposure_val" %in% names(coef(cc_cov$model)))
ok("exposure_val still present alongside covariate")

# =============================================================================
section("12. Method = 'clogit' (sparse events)")
# =============================================================================

# clogit needs binary variation: build a sparse dataset where ~50% of days
# have at least one event (Poisson lambda ~0.5), with a clear positive signal
set.seed(77)
n_sparse  <- 3L * 365L
dates_sp  <- seq(as.Date("2020-01-01"), by = "day", length.out = n_sparse)
doy_sp    <- as.integer(format(dates_sp, "%j"))
temp_sp   <- 22 + 6 * sin(2 * pi * doy_sp / 365 - pi / 2) + stats::rnorm(n_sparse, sd = 2)
# rare events: lambda ~0.5, strong association (beta_temp = 0.08)
events_sp <- stats::rpois(n_sparse, exp(-1.5 + 0.08 * (temp_sp - 22)))
df_sparse <- data.frame(date = dates_sp, n_obitos = events_sp, temp_c = temp_sp)

cc_clogit <- sus_mod_casecrossover(
  df_sparse,
  outcome_col  = "n_obitos",
  exposure_col = "temp_c",
  stratum      = "month",
  lag          = 0L,
  method       = "clogit",
  verbose      = FALSE,
  lang         = "en"
)
stopifnot(inherits(cc_clogit, "climasus_casecrossover"))
ok("clogit method returns climasus_casecrossover")

stopifnot(cc_clogit$meta$method == "clogit")
ok("meta$method == 'clogit'")

stopifnot(inherits(cc_clogit$model, "clogit"))
ok("$model is a clogit object")

stopifnot(nrow(cc_clogit$or_table) == 1L)
ok("or_table has 1 row for clogit")

# OR > 0 (always true), CI properly ordered
stopifnot(cc_clogit$or_table$or > 0)
stopifnot(cc_clogit$or_table$or_lo < cc_clogit$or_table$or_hi)
ok("clogit OR > 0 and CI properly ordered (lo < hi)")

# With strong positive signal (beta=0.08), OR should be > 1 for sparse events
stopifnot(cc_clogit$or_table$or > 1)
ok("clogit OR > 1 for sparse sparse events with known positive signal")

# =============================================================================
section("13. Diagnostics structure")
# =============================================================================

dg <- cc0$diagnostics
stopifnot(is.list(dg))
diag_keys <- c("n_obs", "n_cases", "n_strata", "disp_ratio", "method", "family")
stopifnot(all(diag_keys %in% names(dg)))
ok("$diagnostics contains all expected keys")

stopifnot(dg$n_cases > 0L)
ok("n_cases > 0")

stopifnot(dg$n_strata == 48L)
ok("n_strata == 48 in diagnostics")

# dispersion ratio should be positive for quasipoisson
stopifnot(!is.na(dg$disp_ratio) && dg$disp_ratio > 0)
ok("disp_ratio is a positive number for conditional_poisson")

# =============================================================================
section("14. tidy() output")
# =============================================================================

td <- tidy(cc0)
stopifnot(inherits(td, "data.frame"))
stopifnot(nrow(td) == 1L)
tidy_cols <- c("outcome_col", "exposure_col", "method", "stratum",
               "n_obs", "n_cases", "n_strata",
               "term", "lag_spec", "estimate", "or", "or_lo", "or_hi", "p_value")
stopifnot(all(tidy_cols %in% names(td)))
ok("tidy() returns 1-row tibble with all required columns")

stopifnot(td$outcome_col == "n_obitos")
stopifnot(td$exposure_col == "temp_c")
ok("tidy() metadata fields are correct")

# =============================================================================
section("15. print() and summary() run without error")
# =============================================================================

err_p <- tryCatch({ print(cc0); NULL }, error = function(e) e)
stopifnot(is.null(err_p))
ok("print.climasus_casecrossover() runs without error")

err_s <- tryCatch({ summary(cc0); NULL }, error = function(e) e)
stopifnot(is.null(err_s))
ok("summary.climasus_casecrossover() runs without error")

# =============================================================================
section("16. family = 'poisson' works")
# =============================================================================

cc_pois <- sus_mod_casecrossover(
  df_plain,
  outcome_col  = "n_obitos",
  exposure_col = "temp_c",
  stratum      = "month",
  lag          = 0L,
  family       = "poisson",
  verbose      = FALSE,
  lang         = "en"
)
stopifnot(inherits(cc_pois, "climasus_casecrossover"))
stopifnot(cc_pois$meta$family == "poisson")
ok("family='poisson' accepted and stored in meta")

# Poisson and quasi-Poisson give identical point estimates
stopifnot(abs(cc_pois$or_table$estimate - cc0$or_table$estimate) < 1e-6)
ok("Poisson and quasi-Poisson yield identical point estimates")

# =============================================================================
section("17. Association strength: OR per degree consistent with beta_temp = 0.025")
# =============================================================================

# True log-RR per degree = 0.025 -> OR per deg ~ exp(0.025) = 1.0253
# We expect estimated OR to be in the ballpark [1.005, 1.06]
est_or <- cc0$or_table$or
stopifnot(est_or > 1.005 && est_or < 1.06)
ok(sprintf("OR per degree C = %.4f (plausible range [1.005, 1.06])", est_or))

# p-value for exposure should be very small
stopifnot(cc0$or_table$p_value < 0.001)
ok("p_value < 0.001 (strong signal from simulated data)")

# =============================================================================
section("18. Error handling")
# =============================================================================

# Missing date column
tryCatch(
  sus_mod_casecrossover(
    data.frame(outcome = 1:10, temp = 1:10),
    outcome_col  = "outcome",
    exposure_col = "temp",
    verbose      = FALSE
  ),
  error = function(e) ok("Rejects data without 'date' column")
)

# Missing outcome column
tryCatch(
  sus_mod_casecrossover(
    df_plain, outcome_col = "no_col", exposure_col = "temp_c", verbose = FALSE
  ),
  error = function(e) ok("Rejects missing outcome column")
)

# Missing exposure column
tryCatch(
  sus_mod_casecrossover(
    df_plain, outcome_col = "n_obitos", exposure_col = "no_col", verbose = FALSE
  ),
  error = function(e) ok("Rejects missing exposure column")
)

# Bad method
tryCatch(
  sus_mod_casecrossover(
    df_plain, outcome_col = "n_obitos", exposure_col = "temp_c",
    method = "something_else", verbose = FALSE
  ),
  error = function(e) ok("Rejects unsupported method")
)

# Bad stratum
tryCatch(
  sus_mod_casecrossover(
    df_plain, outcome_col = "n_obitos", exposure_col = "temp_c",
    stratum = "no_such_col", verbose = FALSE
  ),
  error = function(e) ok("Rejects bad stratum value")
)

# =============================================================================
cat("\n=============================================================\n")
cat("  ALL SECTIONS PASSED\n")
cat("=============================================================\n\n")
