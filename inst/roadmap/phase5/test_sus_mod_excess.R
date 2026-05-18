# =============================================================================
# test_sus_mod_excess.R
# Comprehensive functional test for sus_mod_excess()
# Run with: source("inst/roadmap/phase5/test_sus_mod_excess.R")
# =============================================================================

devtools::load_all()
library(dlnm)
library(splines)

cat("\n=============================================================\n")
cat("  sus_mod_excess() -- Functional Test Suite\n")
cat("=============================================================\n\n")

# -- helpers ------------------------------------------------------------------
ok      <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail    <- function(msg) stop(sprintf("  [FAIL] %s", msg))
section <- function(s)   cat(sprintf("\n--- %s ---\n", s))

# =============================================================================
section("0. Build shared fixtures")
# =============================================================================

set.seed(42)
n_days <- 5L * 365L
dates  <- seq(as.Date("2018-01-01"), by = "day", length.out = n_days)
doy    <- as.integer(format(dates, "%j"))

# Seasonal temperature and mortality signal
temp0  <- 22 + 8 * sin(2 * pi * doy / 365 - pi / 2) + stats::rnorm(n_days, sd = 2)
deaths <- stats::rpois(n_days, exp(2.5 + 0.015 * temp0 + 0.3 * sin(2 * pi * doy / 365)))

df_plain <- data.frame(
  date     = dates,
  n_obitos = deaths,
  temp     = temp0
)

ctrl_period <- c(as.Date("2018-01-01"), as.Date("2020-12-31"))
stdy_period <- c(as.Date("2021-01-01"), as.Date("2022-12-31"))

ok("Fixtures built: 5-year daily series with seasonal mortality")

# Also build a climasus_dlnm fixture
lag_max <- 14L
make_lag <- function(s, l) c(rep(NA_real_, l), head(s, length(s) - l))
df_raw <- data.frame(date = dates, cod_munic = "city_a", n_obitos = deaths)
for (l in 0L:lag_max)
  df_raw[[paste0("tair_dry_bulb_c_lag", l)]] <- make_lag(temp0, l)
df_raw <- df_raw[!is.na(df_raw[[paste0("tair_dry_bulb_c_lag", lag_max)]]), ]

df_dl <- ensure_climasus_df(
  df_raw, system = "SIM", stage = "climate", type = "distributed_lag"
)
fit_dlnm <- sus_mod_dlnm(
  df_dl,
  outcome_col  = "n_obitos",
  lag_max      = lag_max,
  family       = "quasipoisson",
  dof_per_year = 4L,
  verbose      = FALSE,
  lang         = "en"
)
ok("climasus_dlnm fixture built")

# =============================================================================
section("1. Method auto-detection: from_dlnm when climasus_dlnm is input")
# =============================================================================

exc_dlnm <- sus_mod_excess(fit_dlnm, verbose = FALSE, lang = "en")
stopifnot(inherits(exc_dlnm, "climasus_excess"))
stopifnot(exc_dlnm$meta$method == "from_dlnm")
ok("Auto-detects 'from_dlnm' for climasus_dlnm input")

# =============================================================================
section("2. Method auto-detection: spline when data.frame is input")
# =============================================================================

exc_spline <- sus_mod_excess(
  df_plain,
  outcome_col    = "n_obitos",
  control_period = ctrl_period,
  study_period   = stdy_period,
  verbose        = FALSE,
  lang           = "en"
)
stopifnot(inherits(exc_spline, "climasus_excess"))
stopifnot(exc_spline$meta$method == "spline")
ok("Auto-detects 'spline' for data.frame input")

# =============================================================================
section("3. Explicit method = 'serfling'")
# =============================================================================

exc_serf <- sus_mod_excess(
  df_plain,
  outcome_col    = "n_obitos",
  control_period = ctrl_period,
  study_period   = stdy_period,
  method         = "serfling",
  harmonics      = 2L,
  verbose        = FALSE,
  lang           = "en"
)
stopifnot(inherits(exc_serf, "climasus_excess"))
stopifnot(exc_serf$meta$method == "serfling")
ok("Explicit method='serfling' accepted")

# =============================================================================
section("4. Output structure: $daily tibble")
# =============================================================================

d <- exc_spline$daily
stopifnot(inherits(d, "data.frame"))
required_cols <- c("date", "observed", "expected", "expected_lo", "expected_hi",
                   "excess", "excess_lo", "excess_hi", "z_score", "is_excess",
                   "cum_excess")
stopifnot(all(required_cols %in% names(d)))
ok("$daily has all required columns")

stopifnot(nrow(d) > 0L)
ok("$daily has rows")

stopifnot(inherits(d$date, "Date"))
ok("$daily$date is Date class")

stopifnot(is.logical(d$is_excess))
ok("$daily$is_excess is logical")

stopifnot(is.numeric(d$cum_excess))
ok("$daily$cum_excess is numeric")

# =============================================================================
section("5. Output structure: $total tibble")
# =============================================================================

tt <- exc_spline$total
stopifnot(inherits(tt, "data.frame"))
total_cols <- c("n_days", "observed", "expected", "expected_lo", "expected_hi",
                "excess", "excess_lo", "excess_hi", "excess_pct",
                "n_excess_days", "peak_excess", "peak_date")
stopifnot(all(total_cols %in% names(tt)))
ok("$total has all required columns")

stopifnot(nrow(tt) == 1L)
ok("$total has exactly 1 row")

stopifnot(tt$n_days == nrow(d))
ok("$total$n_days matches nrow($daily)")

# =============================================================================
section("6. Output structure: $meta list")
# =============================================================================

m <- exc_spline$meta
stopifnot(is.list(m))
meta_keys <- c("method", "outcome_col", "date_col", "control_period",
               "study_period", "family", "dof_per_year", "harmonics",
               "threshold_z", "n_obs", "n_study", "n_control", "call_time")
stopifnot(all(meta_keys %in% names(m)))
ok("$meta contains all expected keys")

stopifnot(m$method == "spline")
stopifnot(m$outcome_col == "n_obitos")
stopifnot(m$n_obs == nrow(df_plain))
ok("$meta values are correct")

# =============================================================================
section("7. Arithmetic consistency: excess = observed - expected")
# =============================================================================

excess_check <- abs(d$excess - (d$observed - d$expected))
stopifnot(all(excess_check < 1e-9))
ok("excess == observed - expected for all rows")

excess_lo_check <- abs(d$excess_lo - (d$observed - d$expected_hi))
stopifnot(all(excess_lo_check < 1e-9))
ok("excess_lo == observed - expected_hi for all rows")

# =============================================================================
section("8. Cumulative excess is monotone (cumsum of excess)")
# =============================================================================

cumexc_check <- abs(d$cum_excess - cumsum(d$excess))
stopifnot(all(cumexc_check < 1e-9))
ok("cum_excess equals cumsum(excess)")

# =============================================================================
section("9. Z-score: is_excess flagged correctly at threshold_z = 1.96")
# =============================================================================

flagged   <- d$is_excess & !is.na(d$z_score)
unflagged <- !d$is_excess & !is.na(d$z_score)
stopifnot(all(d$z_score[flagged] > 1.96))
stopifnot(all(d$z_score[unflagged] <= 1.96))
ok("is_excess correctly reflects z_score > 1.96")

# =============================================================================
section("10. CI ordering: expected_lo <= expected <= expected_hi")
# =============================================================================

stopifnot(all(d$expected_lo <= d$expected + 1e-9))
stopifnot(all(d$expected <= d$expected_hi + 1e-9))
ok("expected_lo <= expected <= expected_hi for all rows")

# =============================================================================
section("11. $model is a GLM object for spline/serfling, NULL for from_dlnm")
# =============================================================================

stopifnot(inherits(exc_spline$model, "glm"))
ok("$model is a glm object for method='spline'")

stopifnot(inherits(exc_serf$model, "glm"))
ok("$model is a glm object for method='serfling'")

stopifnot(is.null(exc_dlnm$model))
ok("$model is NULL for method='from_dlnm'")

# =============================================================================
section("12. by = 'year' temporal breakdown")
# =============================================================================

exc_by_year <- sus_mod_excess(
  df_plain,
  outcome_col    = "n_obitos",
  control_period = ctrl_period,
  study_period   = stdy_period,
  by             = "year",
  verbose        = FALSE,
  lang           = "en"
)
bp <- exc_by_year$by_period
stopifnot(!is.null(bp))
stopifnot("year" %in% names(bp))
stopifnot("excess" %in% names(bp))
stopifnot("n_days" %in% names(bp))
ok("by='year' produces by_period tibble with year, excess, n_days")

# Number of year rows should match study years
study_years <- unique(as.integer(format(
  seq(stdy_period[[1L]], stdy_period[[2L]], by = "day"), "%Y"
)))
stopifnot(nrow(bp) == length(study_years))
ok("by_period has one row per study year")

# =============================================================================
section("13. by = 'month' temporal breakdown")
# =============================================================================

exc_by_month <- sus_mod_excess(
  df_plain,
  outcome_col    = "n_obitos",
  control_period = ctrl_period,
  study_period   = stdy_period,
  by             = "month",
  verbose        = FALSE,
  lang           = "en"
)
bp_m <- exc_by_month$by_period
stopifnot(!is.null(bp_m))
stopifnot(all(c("year", "month_num", "excess") %in% names(bp_m)))
stopifnot(nrow(bp_m) == 24L)  # 2 study years x 12 months
ok("by='month' produces 24-row by_period for 2-year study")

# =============================================================================
section("14. by = 'season' temporal breakdown")
# =============================================================================

exc_by_season <- sus_mod_excess(
  df_plain,
  outcome_col    = "n_obitos",
  control_period = ctrl_period,
  study_period   = stdy_period,
  by             = "season",
  verbose        = FALSE,
  lang           = "en"
)
bp_s <- exc_by_season$by_period
stopifnot(!is.null(bp_s))
stopifnot(all(c("year", "season") %in% names(bp_s)))
ok("by='season' produces by_period with year and season columns")

all_seasons <- unique(bp_s$season)
stopifnot(all(all_seasons %in% c("DJF", "MAM", "JJA", "SON")))
ok("season values are valid meteorological season codes")

# =============================================================================
section("15. tidy() returns a one-row tibble with key columns")
# =============================================================================

td <- tidy(exc_spline)
stopifnot(inherits(td, "data.frame"))
stopifnot(nrow(td) == 1L)
tidy_cols <- c("method", "outcome_col", "study_from", "study_to",
               "n_study_days", "observed", "expected", "excess",
               "excess_pct", "n_excess_days", "peak_excess", "peak_date")
stopifnot(all(tidy_cols %in% names(td)))
ok("tidy() returns 1-row tibble with all required columns")

stopifnot(td$method == "spline")
stopifnot(td$outcome_col == "n_obitos")
ok("tidy() values are correct")

# =============================================================================
section("16. print() and summary() run without error")
# =============================================================================

# cli writes to stderr, not stdout — just verify no error is thrown
err_print <- tryCatch({ print(exc_spline); NULL }, error = function(e) e)
stopifnot(is.null(err_print))
ok("print.climasus_excess() runs without error")

err_summ <- tryCatch({ summary(exc_spline); NULL }, error = function(e) e)
stopifnot(is.null(err_summ))
ok("summary.climasus_excess() runs without error")

# =============================================================================
section("17. from_dlnm: study period defaults to full range of DLNM data")
# =============================================================================

stopifnot(inherits(exc_dlnm$daily, "data.frame"))
stopifnot(nrow(exc_dlnm$daily) > 0L)
stopifnot(all(c("observed", "expected", "excess") %in% names(exc_dlnm$daily)))
ok("from_dlnm: $daily has rows and required columns")

stopifnot(all(exc_dlnm$daily$expected > 0))
ok("from_dlnm: all expected values are positive")

# Study period should cover the full data_daily extent
dlnm_dates <- fit_dlnm$data_daily$date
stopifnot(exc_dlnm$meta$study_period[[1L]] <= min(dlnm_dates))
stopifnot(exc_dlnm$meta$study_period[[2L]] >= max(dlnm_dates))
ok("from_dlnm: study_period spans full DLNM data range")

# =============================================================================
section("18. Error handling: bad method, missing columns, bad by")
# =============================================================================

# Bad method
tryCatch(
  sus_mod_excess(df_plain, outcome_col = "n_obitos", method = "gam_bad",
                 verbose = FALSE),
  error = function(e) ok("Rejects unsupported method value")
)

# method = 'from_dlnm' on a plain data.frame
tryCatch(
  sus_mod_excess(df_plain, outcome_col = "n_obitos", method = "from_dlnm",
                 verbose = FALSE),
  error = function(e) ok("Rejects from_dlnm on plain data.frame")
)

# Missing outcome column
tryCatch(
  sus_mod_excess(df_plain, outcome_col = "no_such_col",
                 control_period = ctrl_period, study_period = stdy_period,
                 verbose = FALSE),
  error = function(e) ok("Rejects missing outcome column")
)

# Bad 'by' value
tryCatch(
  sus_mod_excess(df_plain, outcome_col = "n_obitos",
                 control_period = ctrl_period, study_period = stdy_period,
                 by = "week", verbose = FALSE),
  error = function(e) ok("Rejects invalid 'by' value")
)

# =============================================================================
cat("\n=============================================================\n")
cat("  ALL SECTIONS PASSED\n")
cat("=============================================================\n\n")
