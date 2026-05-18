# =============================================================================
# test_sus_mod_its.R
# Comprehensive functional test for sus_mod_its()
# Run with: source("inst/roadmap/phase5/test_sus_mod_its.R")
# =============================================================================

devtools::load_all()

cat("\n=============================================================\n")
cat("  sus_mod_its() -- Functional Test Suite\n")
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
t_num  <- seq_along(dates) - 1L

# Pre-intervention: stable baseline with seasonality
# Intervention at day 730 (2020-01-01): strong level drop (RR ~0.7) + slope up
int_date <- as.Date("2020-01-01")
int_t    <- as.numeric(int_date - dates[1L])

step_true  <- as.numeric(dates >= int_date)
slope_true <- pmax(0, t_num - int_t)

lambda <- exp(
  3.0                                          # baseline
  + 0.0001 * t_num                             # slight upward pre-trend
  - 0.357  * step_true                         # level drop: exp(-0.357) ~0.70
  + 0.0002 * slope_true                        # slope recovery
  + 0.3    * sin(2 * pi * doy / 365)           # seasonality
  - 0.2    * cos(2 * pi * doy / 365)
)
deaths <- stats::rpois(n_days, lambda)

df_plain <- data.frame(date = dates, n_obitos = deaths)

# Second interruption for multi-break tests
int_date2 <- as.Date("2021-07-01")

ok(sprintf("Fixture: %d days, mean deaths %.1f/day, 1 known interruption at %s",
           n_days, mean(deaths), format(int_date, "%Y-%m-%d")))

# climasus_df fixture
df_cl <- ensure_climasus_df(
  df_plain, system = "SIM", stage = "climate", type = "exact"
)
ok("climasus_df fixture built")

# =============================================================================
section("1. Basic single-interruption call")
# =============================================================================

its1 <- sus_mod_its(
  df_plain,
  outcome_col        = "n_obitos",
  interruption_dates = int_date,
  harmonics          = 2L,
  verbose            = FALSE,
  lang               = "en"
)
stopifnot(inherits(its1, "climasus_its"))
ok("Returns climasus_its object")

stopifnot(its1$meta$n_interruptions == 1L)
ok("meta$n_interruptions == 1")

stopifnot(its1$meta$outcome_col == "n_obitos")
ok("meta$outcome_col correct")

# =============================================================================
section("2. Input: climasus_df accepted")
# =============================================================================

its_cl <- sus_mod_its(
  df_cl,
  outcome_col        = "n_obitos",
  interruption_dates = int_date,
  harmonics          = 2L,
  verbose            = FALSE,
  lang               = "en"
)
stopifnot(inherits(its_cl, "climasus_its"))
ok("climasus_df input accepted")

# Same model — coefficients should be identical
stopifnot(abs(coef(its_cl$model) - coef(its1$model)) < 1e-8)
ok("climasus_df and data.frame inputs yield identical model coefficients")

# =============================================================================
section("3. $effects: one row per interruption, correct structure")
# =============================================================================

ef <- its1$effects
stopifnot(inherits(ef, "data.frame"))
stopifnot(nrow(ef) == 1L)

eff_cols <- c("label", "interruption_date", "level_ratio",
              "level_ci_lo", "level_ci_hi", "level_p",
              "slope_daily_log", "slope_ratio_annual", "slope_p")
stopifnot(all(eff_cols %in% names(ef)))
ok("$effects has 1 row and all required columns")

# Level ratio CI ordering
stopifnot(ef$level_ci_lo < ef$level_ratio)
stopifnot(ef$level_ratio < ef$level_ci_hi)
ok("level_ci_lo < level_ratio < level_ci_hi")

# True level drop is ~0.70; estimate should be in plausible range [0.55, 0.85]
stopifnot(ef$level_ratio > 0.55 && ef$level_ratio < 0.85)
ok(sprintf("Estimated level_ratio = %.3f (expected ~0.70)", ef$level_ratio))

# log-ratio consistency: exp(log-OR) == level_ratio
est_log <- coef(its1$model)["step_1"]
stopifnot(abs(exp(est_log) - ef$level_ratio) < 1e-9)
ok("level_ratio == exp(step_1 coef)")

# =============================================================================
section("4. $model is a glm with expected terms")
# =============================================================================

stopifnot(inherits(its1$model, "glm"))
ok("$model is a glm object")

coef_names <- names(coef(its1$model))
stopifnot("t_num"   %in% coef_names)
stopifnot("step_1"  %in% coef_names)
stopifnot("slope_1" %in% coef_names)
stopifnot("sin1"    %in% coef_names)
stopifnot("cos1"    %in% coef_names)
ok("Model has t_num, step_1, slope_1, sin1, cos1 terms")

# =============================================================================
section("5. $counterfactual tibble structure")
# =============================================================================

cf <- its1$counterfactual
stopifnot(!is.null(cf))
stopifnot(inherits(cf, "data.frame"))
stopifnot(nrow(cf) == nrow(df_plain))
cf_cols <- c("date", "observed", "predicted", "counterfactual",
             "cf_lo", "cf_hi", "ratio_to_cf", "prevented")
stopifnot(all(cf_cols %in% names(cf)))
ok("$counterfactual is a data.frame with all required columns and correct nrow")

stopifnot(all(!is.na(cf$counterfactual)))
ok("counterfactual has no NAs")

stopifnot(all(cf$counterfactual > 0))
ok("All counterfactual values are positive")

# Pre-interruption: counterfactual should closely match predicted (step/slope ~0)
pre_mask <- cf$date < int_date
cf_pre_diff <- abs(cf$counterfactual[pre_mask] - cf$predicted[pre_mask])
stopifnot(max(cf_pre_diff) < 1e-6)
ok("Counterfactual == predicted during pre-interruption period")

# =============================================================================
section("6. counterfactual = FALSE suppresses $counterfactual")
# =============================================================================

its_nocf <- sus_mod_its(
  df_plain,
  outcome_col        = "n_obitos",
  interruption_dates = int_date,
  counterfactual     = FALSE,
  verbose            = FALSE,
  lang               = "en"
)
stopifnot(is.null(its_nocf$counterfactual))
ok("counterfactual = FALSE returns NULL for $counterfactual")

# =============================================================================
section("7. $segments: one per period with correct structure")
# =============================================================================

sg <- its1$segments
stopifnot(inherits(sg, "data.frame"))
stopifnot(nrow(sg) == 2L)  # pre + post for 1 interruption
stopifnot(sg$segment[1L] == "Pre-interruption")
stopifnot(sg$segment[2L] == "Post-interruption")
ok("$segments has 2 rows (pre + post) with correct labels")

seg_cols <- c("segment", "start_date", "end_date", "n_days",
              "mean_observed", "mean_predicted", "mean_counterfactual")
stopifnot(all(seg_cols %in% names(sg)))
ok("$segments has all required columns")

stopifnot(sg$n_days[1L] + sg$n_days[2L] == nrow(df_plain))
ok("Segment n_days sums to total n_obs")

# Post-interruption mean_observed < mean_counterfactual (intervention reduced counts)
stopifnot(sg$mean_observed[2L] < sg$mean_counterfactual[2L])
ok("Post-interruption: mean_observed < mean_counterfactual (intervention reduced counts)")

# =============================================================================
section("8. Multiple interruptions (2 break points)")
# =============================================================================

its2 <- sus_mod_its(
  df_plain,
  outcome_col        = "n_obitos",
  interruption_dates = c(int_date, int_date2),
  harmonics          = 2L,
  verbose            = FALSE,
  lang               = "en"
)
stopifnot(its2$meta$n_interruptions == 2L)
ok("n_interruptions == 2 for two-break model")

ef2 <- its2$effects
stopifnot(nrow(ef2) == 2L)
stopifnot(ef2$label[1L] == "Interruption 1")
stopifnot(ef2$label[2L] == "Interruption 2")
ok("$effects has 2 rows for two-break model")

# Model has step_1, slope_1, step_2, slope_2
coef2 <- names(coef(its2$model))
stopifnot(all(c("step_1", "slope_1", "step_2", "slope_2") %in% coef2))
ok("Two-break model has step_1, slope_1, step_2, slope_2 terms")

# Segments: 3 (pre + post-1 + post-2)
sg2 <- its2$segments
stopifnot(nrow(sg2) == 3L)
ok("3 segments for two-break model")

stopifnot(sum(sg2$n_days) == nrow(df_plain))
ok("All segment n_days sum to total n_obs")

# =============================================================================
section("9. harmonics = 0 suppresses seasonal terms")
# =============================================================================

its_noh <- sus_mod_its(
  df_plain,
  outcome_col        = "n_obitos",
  interruption_dates = int_date,
  harmonics          = 0L,
  verbose            = FALSE,
  lang               = "en"
)
stopifnot(!any(grepl("^sin|^cos", names(coef(its_noh$model)))))
ok("harmonics=0: no sin/cos terms in model")

stopifnot(its_noh$meta$harmonics == 0L)
ok("meta$harmonics == 0")

# =============================================================================
section("10. harmonics = 1 and 4 accepted")
# =============================================================================

its_h1 <- sus_mod_its(
  df_plain,
  outcome_col        = "n_obitos",
  interruption_dates = int_date,
  harmonics          = 1L,
  verbose            = FALSE
)
stopifnot(all(c("sin1", "cos1") %in% names(coef(its_h1$model))))
stopifnot(!any(c("sin2", "cos2") %in% names(coef(its_h1$model))))
ok("harmonics=1: only sin1, cos1 in model")

its_h4 <- sus_mod_its(
  df_plain,
  outcome_col        = "n_obitos",
  interruption_dates = int_date,
  harmonics          = 4L,
  verbose            = FALSE
)
stopifnot(all(paste0("sin", 1:4) %in% names(coef(its_h4$model))))
ok("harmonics=4: sin1-sin4 all in model")

# =============================================================================
section("11. family = 'poisson' accepted; same point estimates as quasipoisson")
# =============================================================================

its_pois <- sus_mod_its(
  df_plain,
  outcome_col        = "n_obitos",
  interruption_dates = int_date,
  family             = "poisson",
  verbose            = FALSE
)
stopifnot(inherits(its_pois, "climasus_its"))
stopifnot(its_pois$meta$family == "poisson")
ok("family='poisson' accepted")

# Poisson and quasi-Poisson give identical point estimates
diff_coef <- abs(coef(its_pois$model) - coef(its1$model))
stopifnot(max(diff_coef) < 1e-8)
ok("Poisson and quasi-Poisson yield identical coefficient estimates")

# =============================================================================
section("12. Covariate inclusion")
# =============================================================================

df_cov <- df_plain
df_cov$rh_pct <- 70 + 10 * sin(2 * pi * doy / 365) + stats::rnorm(n_days, sd = 3)

its_cov <- sus_mod_its(
  df_cov,
  outcome_col        = "n_obitos",
  interruption_dates = int_date,
  covariates         = "rh_pct",
  verbose            = FALSE
)
stopifnot("rh_pct" %in% names(coef(its_cov$model)))
ok("Covariate 'rh_pct' appears in model coefficients")

stopifnot(inherits(its_cov, "climasus_its"))
ok("Model with covariate returns valid climasus_its")

# =============================================================================
section("13. $meta completeness")
# =============================================================================

m <- its1$meta
meta_keys <- c("outcome_col", "date_col", "interruption_dates", "n_interruptions",
               "harmonics", "family", "covariates", "alpha",
               "n_obs", "n_pre", "disp_ratio", "call_time")
stopifnot(all(meta_keys %in% names(m)))
ok("$meta contains all expected keys")

stopifnot(m$n_obs == nrow(df_plain))
ok("meta$n_obs == nrow(df_plain)")

stopifnot(m$n_pre == sum(df_plain$date < int_date))
ok("meta$n_pre == number of pre-interruption rows")

stopifnot(!is.na(m$disp_ratio) && m$disp_ratio > 0)
ok("meta$disp_ratio is positive (quasipoisson)")

# =============================================================================
section("14. tidy() output")
# =============================================================================

td <- tidy(its1)
stopifnot(inherits(td, "data.frame"))
stopifnot(nrow(td) == 1L)
tidy_cols <- c("outcome_col", "family", "harmonics", "n_obs", "n_pre",
               "n_interruptions", "label", "interruption_date",
               "level_ratio", "level_ci_lo", "level_ci_hi", "level_p")
stopifnot(all(tidy_cols %in% names(td)))
ok("tidy() returns 1-row tibble with all required columns")

td2 <- tidy(its2)
stopifnot(nrow(td2) == 2L)
ok("tidy() returns 2-row tibble for two-interruption model")

# =============================================================================
section("15. print() and summary() run without error")
# =============================================================================

err_p <- tryCatch({ print(its1); NULL }, error = function(e) e)
stopifnot(is.null(err_p))
ok("print.climasus_its() runs without error")

err_s <- tryCatch({ summary(its1); NULL }, error = function(e) e)
stopifnot(is.null(err_s))
ok("summary.climasus_its() runs without error")

# =============================================================================
section("16. Date column flexibility (non-default date_col)")
# =============================================================================

df_altdate <- df_plain
names(df_altdate)[1L] <- "data"   # rename date column

its_altdate <- sus_mod_its(
  df_altdate,
  outcome_col        = "n_obitos",
  date_col           = "data",
  interruption_dates = int_date,
  harmonics          = 2L,           # same spec as its1 for comparison
  verbose            = FALSE
)
stopifnot(inherits(its_altdate, "climasus_its"))
ok("Accepts non-default date_col = 'data'")

# Same spec -> identical coefficients
stopifnot(abs(coef(its_altdate$model) - coef(its1$model)) < 1e-8)
ok("Alternative date_col yields same model as default")

# =============================================================================
section("17. Ratio_to_cf and prevented are economically sensible post-intervention")
# =============================================================================

cf_post <- its1$counterfactual |>
  dplyr::filter(date >= int_date)

# ratio_to_cf: observed / counterfactual; should be < 1 because intervention reduced counts
mean_ratio <- mean(cf_post$ratio_to_cf, na.rm = TRUE)
stopifnot(mean_ratio < 1)
ok(sprintf("Post-intervention mean ratio_to_cf = %.3f (< 1, intervention reduced counts)",
           mean_ratio))

# prevented: counterfactual - observed; should be positive on average post-intervention
mean_prev <- mean(cf_post$prevented, na.rm = TRUE)
stopifnot(mean_prev > 0)
ok(sprintf("Post-intervention mean prevented = %.1f (> 0)", mean_prev))

# =============================================================================
section("18. Error handling")
# =============================================================================

# Missing date column
tryCatch(
  sus_mod_its(
    data.frame(outcome = 1:100),
    outcome_col = "outcome", interruption_dates = "2019-06-01", verbose = FALSE
  ),
  error = function(e) ok("Rejects data without a date column")
)

# Missing outcome column
tryCatch(
  sus_mod_its(
    df_plain, outcome_col = "no_col", interruption_dates = int_date, verbose = FALSE
  ),
  error = function(e) ok("Rejects missing outcome column")
)

# No interruption_dates provided
tryCatch(
  sus_mod_its(df_plain, outcome_col = "n_obitos", verbose = FALSE),
  error = function(e) ok("Rejects missing interruption_dates argument")
)

# Interruption date outside data range
tryCatch(
  sus_mod_its(
    df_plain, outcome_col = "n_obitos",
    interruption_dates = "2017-01-01", verbose = FALSE
  ),
  error = function(e) ok("Rejects interruption_date outside data range")
)

# Bad family
tryCatch(
  sus_mod_its(
    df_plain, outcome_col = "n_obitos",
    interruption_dates = int_date, family = "gamma", verbose = FALSE
  ),
  error = function(e) ok("Rejects unsupported family")
)

# =============================================================================
cat("\n=============================================================\n")
cat("  ALL SECTIONS PASSED\n")
cat("=============================================================\n\n")
