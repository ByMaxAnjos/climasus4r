# =============================================================================
# test_sus_mod_af.R
# Comprehensive functional test for sus_mod_af()
# Run with: source("inst/roadmap/phase5/test_sus_mod_af.R")
# =============================================================================

devtools::load_all()
library(dlnm)
library(splines)
library(MASS)

cat("\n=============================================================\n")
cat("  sus_mod_af() -- Functional Test Suite\n")
cat("=============================================================\n\n")

# -- helpers ------------------------------------------------------------------
ok      <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail    <- function(msg) stop(sprintf("  [FAIL] %s", msg))
section <- function(s)   cat(sprintf("\n--- %s ---\n", s))

# =============================================================================
section("1. Build shared climasus_dlnm fixture (3 years, 1 municipality)")
# =============================================================================
set.seed(99)
n_days      <- 3L * 365L
lag_max_fix <- 14L                       # keep lag_max modest for speed
dates       <- seq(as.Date("2020-01-01"), by = "day", length.out = n_days)
doy         <- as.integer(format(dates, "%j"))
temp0       <- 28 + 6 * sin(2 * pi * doy / 365 - pi / 2) + stats::rnorm(n_days, sd = 1.5)
deaths      <- stats::rpois(n_days,
                 exp(2.0 + 0.015 * temp0 + 0.3 * sin(2 * pi * doy / 365)))
rh          <- 70 + 10 * cos(2 * pi * doy / 365) + stats::rnorm(n_days, sd = 3)

make_lag <- function(s, l) c(rep(NA_real_, l), head(s, length(s) - l))

df_raw <- data.frame(date = dates, cod_munic = "3500001",
                     n_obitos = deaths, rh_mean_porc = rh)
for (l in 0L:lag_max_fix)
  df_raw[[paste0("tair_dry_bulb_c_lag", l)]] <- make_lag(temp0, l)
df_raw <- df_raw[!is.na(df_raw[[paste0("tair_dry_bulb_c_lag", lag_max_fix)]]), ]

df_dl <- new_climasus_df(df_raw, list(
  system = "SIM", stage = "climate",
  type   = "distributed_lag", backend = "tibble",
  history = "[2020-01-01] synthetic test data for sus_mod_af"
))

# Fit DLNM (verbose=FALSE to keep test output clean)
fit_base <- sus_mod_dlnm(
  df          = df_dl,
  outcome_col = "n_obitos",
  climate_col = "tair_dry_bulb_c",
  lag_max     = lag_max_fix,
  family      = "quasipoisson",
  dof_per_year = 4L,
  verbose     = FALSE,
  lang        = "pt"
)
stopifnot(inherits(fit_base, "climasus_dlnm"))
ok(sprintf("DLNM fixture fitted: %d obs, lag_max=%d, family=%s",
           fit_base$meta$n, fit_base$meta$lag_max, fit_base$meta$family))


# =============================================================================
section("2. Basic run -- defaults (MC CI, no temporal breakdown)")
# =============================================================================
af1 <- sus_mod_af(
  fit     = fit_base,
  nsim    = 200L,        # small for speed; accuracy is irrelevant in tests
  lang    = "pt",
  verbose = TRUE
)

stopifnot(inherits(af1, "climasus_af"))              ; ok("class = climasus_af")
stopifnot(!is.null(af1$total))                       ; ok("$total present")
stopifnot(!is.null(af1$by_quantile))                 ; ok("$by_quantile present")
stopifnot(is.null(af1$by_period))                    ; ok("$by_period = NULL when by=NULL")
stopifnot(!is.null(af1$daily))                       ; ok("$daily present")
stopifnot(is.null(af1$custom))                       ; ok("$custom = NULL when range=NULL")
stopifnot(!is.null(af1$meta))                        ; ok("$meta present")


# =============================================================================
section("3. $total table structure")
# =============================================================================
t <- af1$total

stopifnot(nrow(t) == 3L)                             ; ok("$total has 3 rows (total/heat/cold)")
stopifnot(all(c("total", "heat", "cold") %in% t$component))
                                                      ok("components: total, heat, cold present")
expected_cols <- c("component", "threshold",
                   "af", "af_lo", "af_hi",
                   "af_pct", "af_pct_lo", "af_pct_hi",
                   "an", "an_lo", "an_hi", "n_cases")
stopifnot(all(expected_cols %in% names(t)))          ; ok("$total has all 12 required columns")

# af_lo <= af <= af_hi for all components
stopifnot(all(t$af_lo <= t$af))                      ; ok("af_lo <= af for all components")
stopifnot(all(t$af    <= t$af_hi))                   ; ok("af <= af_hi for all components")

# heat + cold components should approximately sum to total AN
an_sum <- t$an[t$component == "heat"] + t$an[t$component == "cold"]
an_tot <- t$an[t$component == "total"]
stopifnot(abs(an_sum - an_tot) < abs(an_tot) * 0.01 + 1)
                                                      ok("AN_heat + AN_cold ~= AN_total")
ok(sprintf("Total AF = %.2f%% [%.2f%%, %.2f%%] | AN = %.0f",
           t$af_pct[1], t$af_pct_lo[1], t$af_pct_hi[1], t$an[1]))


# =============================================================================
section("4. $by_quantile table structure")
# =============================================================================
bq <- af1$by_quantile

# Default pred_at = c(0.75, 0.90, 0.95, 0.99) --> 4 x 2 (hot+cold) = 8 rows
stopifnot(nrow(bq) == 8L)                            ; ok("$by_quantile has 8 rows (4 quantiles x 2 sides)")
bq_cols <- c("component", "quantile_prob", "quantile_label",
             "threshold_val", "af", "af_lo", "af_hi",
             "af_pct", "an", "an_lo", "an_hi")
stopifnot(all(bq_cols %in% names(bq)))               ; ok("$by_quantile has all 11 required columns")
stopifnot(all(bq$component %in% c("hot", "cold")))   ; ok("$by_quantile components = hot | cold")
stopifnot(all(bq$af_lo <= bq$af))                    ; ok("af_lo <= af in $by_quantile")
stopifnot(all(bq$af    <= bq$af_hi))                 ; ok("af <= af_hi in $by_quantile")

# Hot-side: higher quantile threshold should yield smaller (less negative or
# smaller positive) absolute AN -- i.e. fewer days above P99 than P75
hot_rows <- bq[bq$component == "hot", ]
hot_rows  <- hot_rows[order(hot_rows$quantile_prob), ]
stopifnot(all(diff(abs(hot_rows$an)) <= 0))
                                                      ok("Higher quantile -> smaller |AN| (hot side)")


# =============================================================================
section("5. $daily table structure")
# =============================================================================
dl <- af1$daily

stopifnot(nrow(dl) == fit_base$meta$n)               ; ok("$daily rows = fit$meta$n (n_obs)")
stopifnot(all(c("date", "exposure", "cases", "an", "af") %in% names(dl)))
                                                      ok("$daily has 5 required columns")
stopifnot(inherits(dl$date, "Date"))                  ; ok("$daily$date is Date class")
# Daily AN should sum to approximately total AN (within floating point)
stopifnot(abs(sum(dl$an, na.rm = TRUE) - af1$total$an[af1$total$component == "total"]) < 0.01)
                                                      ok("sum($daily$an) == $total$an[total]")


# =============================================================================
section("6. $meta completeness")
# =============================================================================
m <- af1$meta
expected_meta <- c("climate_col", "outcome_col", "family", "lag_max",
                   "ref_value", "threshold", "n_cases", "n_obs",
                   "pred_at", "nsim", "alpha", "by", "ci_method", "call_time")
stopifnot(all(expected_meta %in% names(m)))          ; ok("$meta has all 14 required fields")
stopifnot(m$ci_method == "monte_carlo")               ; ok("ci_method = monte_carlo (MASS available)")
stopifnot(m$nsim == 200L)                             ; ok("nsim stored correctly in $meta")
stopifnot(m$n_cases == sum(fit_base$data_daily$y))    ; ok("n_cases matches sum of daily outcomes")


# =============================================================================
section("7. Temporal breakdown -- by = 'year'")
# =============================================================================
af_yr <- sus_mod_af(fit_base, nsim = 200L, by = "year", verbose = FALSE, lang = "en")

stopifnot(!is.null(af_yr$by_period))                  ; ok("$by_period present when by='year'")
bp_yr <- af_yr$by_period
stopifnot(all(c("year", "cases", "an", "af", "af_pct") %in% names(bp_yr)))
                                                       ok("$by_period has year/cases/an/af/af_pct")
stopifnot(nrow(bp_yr) == 3L)                           ; ok("3 years of data -> 3 rows in $by_period")
stopifnot(all(!is.na(bp_yr$af)))                       ; ok("No NA in $by_period$af")
# Yearly AN should approximately sum to total AN
stopifnot(abs(sum(bp_yr$an) - af_yr$total$an[af_yr$total$component == "total"]) < 1)
                                                        ok("sum(by_period$an) == total AN")
ok(sprintf("Years: %s | annual ANs: %s",
           paste(bp_yr$year, collapse = ", "),
           paste(round(bp_yr$an), collapse = ", ")))


# =============================================================================
section("8. Temporal breakdown -- by = 'month'")
# =============================================================================
af_mo <- sus_mod_af(fit_base, nsim = 200L, by = "month", verbose = FALSE, lang = "pt")

bp_mo <- af_mo$by_period
stopifnot(!is.null(bp_mo))                             ; ok("$by_period present when by='month'")
stopifnot(all(c("year", "month_num", "cases", "an", "af") %in% names(bp_mo)))
                                                        ok("$by_period has year/month_num columns")
# 3 years x 12 months = 36 rows (all months have data in 3-year synthetic series)
stopifnot(nrow(bp_mo) >= 30L)                          ; ok("by='month' yields >= 30 rows")
stopifnot(all(bp_mo$month_num %in% 1L:12L))            ; ok("month_num in 1..12")


# =============================================================================
section("9. Temporal breakdown -- by = 'season'")
# =============================================================================
af_se <- sus_mod_af(fit_base, nsim = 200L, by = "season", verbose = FALSE, lang = "es")

bp_se <- af_se$by_period
stopifnot(!is.null(bp_se))                             ; ok("$by_period present when by='season'")
stopifnot(all(c("year", "season", "cases", "an", "af") %in% names(bp_se)))
                                                        ok("$by_period has year/season columns")
stopifnot(all(bp_se$season %in% c("DJF", "MAM", "JJA", "SON")))
                                                        ok("seasons are DJF/MAM/JJA/SON")


# =============================================================================
section("10. Custom range")
# =============================================================================
p90 <- as.numeric(stats::quantile(fit_base$data_daily$tair_dry_bulb_c_lag0, 0.90))
af_rng <- sus_mod_af(fit_base, range = c(p90, Inf), nsim = 200L,
                     verbose = FALSE, lang = "en")

stopifnot(!is.null(af_rng$custom))                     ; ok("$custom present when range is provided")
custom_cols <- c("range_lo", "range_hi", "af", "af_lo", "af_hi", "af_pct", "an")
stopifnot(all(custom_cols %in% names(af_rng$custom)))  ; ok("$custom has all 7 required columns")
stopifnot(abs(af_rng$custom$range_lo - p90) < 1e-6)    ; ok("$custom$range_lo matches supplied range")
stopifnot(is.infinite(af_rng$custom$range_hi))          ; ok("$custom$range_hi = Inf")
# Custom range (above P90) should have smaller |AN| than total
stopifnot(abs(af_rng$custom$an) <= abs(af_rng$total$an[1]) + 1)
                                                         ok("Custom range AN <= total AN")


# =============================================================================
section("11. Explicit threshold parameter")
# =============================================================================
p50 <- as.numeric(stats::median(fit_base$data_daily$tair_dry_bulb_c_lag0, na.rm = TRUE))
af_thr <- sus_mod_af(fit_base, threshold = p50, nsim = 200L, verbose = FALSE, lang = "pt")

stopifnot(abs(af_thr$meta$threshold - p50) < 1e-6)     ; ok("meta$threshold honours explicit value")
# With symmetric threshold at median: heat AN + cold AN ~= total AN
an_h <- af_thr$total$an[af_thr$total$component == "heat"]
an_c <- af_thr$total$an[af_thr$total$component == "cold"]
an_t <- af_thr$total$an[af_thr$total$component == "total"]
stopifnot(abs((an_h + an_c) - an_t) < abs(an_t) * 0.01 + 1)
                                                         ok("heat + cold ~= total with explicit threshold")


# =============================================================================
section("12. Custom pred_at")
# =============================================================================
af_pq <- sus_mod_af(fit_base, pred_at = c(0.80, 0.95), nsim = 200L,
                    verbose = FALSE, lang = "en")

stopifnot(nrow(af_pq$by_quantile) == 4L)               ; ok("2 quantiles x 2 sides = 4 rows in $by_quantile")
# Use near-equality because 1 - 0.80 != 0.20 exactly in IEEE 754
hot_probs  <- sort(af_pq$by_quantile$quantile_prob[af_pq$by_quantile$component == "hot"])
cold_probs <- sort(af_pq$by_quantile$quantile_prob[af_pq$by_quantile$component == "cold"], decreasing = TRUE)
stopifnot(all(abs(hot_probs  - sort(c(0.80, 0.95))) < 1e-9))
stopifnot(all(abs(cold_probs - sort(c(0.20, 0.05), decreasing = TRUE)) < 1e-9))
                                                         ok("quantile_prob values match pred_at (both sides)")


# =============================================================================
section("13. S3 methods")
# =============================================================================
cat("\n  -- print.climasus_af --\n")
print(af1)
ok("print() completed without error")

cat("\n  -- summary.climasus_af --\n")
summary(af1)
ok("summary() completed without error")

cat("\n  -- tidy.climasus_af --\n")
td <- tidy(af1)
stopifnot(inherits(td, "data.frame"))                   ; ok("tidy() returns data.frame")
stopifnot(nrow(td) == 1L)                               ; ok("tidy() returns 1-row tibble")
tidy_cols <- c("climate_col", "outcome_col", "family", "lag_max",
               "threshold", "n_cases", "ci_method", "nsim",
               "af_total", "af_total_lo", "af_total_hi", "af_pct",
               "af_pct_lo", "af_pct_hi", "an_total", "an_total_lo", "an_total_hi",
               "af_heat", "af_cold", "an_heat", "an_cold")
stopifnot(all(tidy_cols %in% names(td)))                ; ok(sprintf("tidy() has all %d required columns", length(tidy_cols)))
print(td[, c("climate_col", "family", "af_pct", "af_pct_lo", "af_pct_hi",
             "an_total", "an_heat", "an_cold")])


# =============================================================================
section("14. tidy() multi-run binding (broom / multi-city pattern)")
# =============================================================================
# Fit a second DLNM with different lag_max to simulate another city
fit_b <- sus_mod_dlnm(df_dl, outcome_col = "n_obitos",
                      lag_max = 7L, family = "quasipoisson",
                      dof_per_year = 4L, verbose = FALSE, lang = "pt")
af_b  <- sus_mod_af(fit_b, nsim = 200L, verbose = FALSE, lang = "pt")

bound <- dplyr::bind_rows(tidy(af1), tidy(af_b))
stopifnot(nrow(bound) == 2L)                            ; ok("bind_rows(tidy(af1), tidy(af2)) = 2 rows")
stopifnot(all(c(14L, 7L) %in% bound$lag_max))           ; ok("lag_max distinguishes the two cities")
print(bound[, c("lag_max", "af_pct", "af_pct_lo", "af_pct_hi", "an_total")])


# =============================================================================
section("15. Language variants")
# =============================================================================
af_pt <- sus_mod_af(fit_base, nsim = 200L, verbose = TRUE, lang = "pt")
ok("lang='pt' run succeeded")
af_en <- sus_mod_af(fit_base, nsim = 200L, verbose = TRUE, lang = "en")
ok("lang='en' run succeeded")
af_es <- sus_mod_af(fit_base, nsim = 200L, verbose = TRUE, lang = "es")
ok("lang='es' run succeeded")
# All three should give same point estimates
stopifnot(abs(af_pt$total$af[1] - af_en$total$af[1]) < 1e-9)
stopifnot(abs(af_pt$total$af[1] - af_es$total$af[1]) < 1e-9)
ok("Point estimates identical across all three languages")


# =============================================================================
section("16. Delta-method fallback (simulate MASS unavailable)")
# =============================================================================
# Temporarily mask MASS::mvrnorm to trigger the fallback path
local({
  # Patch: replace mc_draws-producing code by passing nsim=0 (no MASS path)
  af_delta <- sus_mod_af(fit_base, nsim = 0L, verbose = FALSE, lang = "en")
  stopifnot(af_delta$meta$ci_method == "delta")         ; ok("nsim=0 -> ci_method = 'delta'")
  # Point estimates should be identical to MC version (same formula)
  stopifnot(abs(af_delta$total$af[1] - af1$total$af[1]) < 1e-9)
                                                          ok("delta AF point estimate == MC AF point estimate")
  # CI should still bracket the point estimate
  stopifnot(all(af_delta$total$af_lo <= af_delta$total$af))
  stopifnot(all(af_delta$total$af    <= af_delta$total$af_hi))
                                                          ok("delta CI satisfies lo <= af <= hi")
})


# =============================================================================
section("17. Error cases")
# =============================================================================

# 17a -- not a climasus_dlnm
tryCatch({
  sus_mod_af(fit = list(x = 1), lang = "en")
  fail("should error on plain list")
}, error = function(e) {
  ok(sprintf("Error caught (not climasus_dlnm): %s", conditionMessage(e)))
})

# 17b -- bad `by` value
tryCatch({
  sus_mod_af(fit_base, by = "quarter", lang = "en")
  fail("should error on bad by='quarter'")
}, error = function(e) {
  ok(sprintf("Error caught (bad by=): %s", conditionMessage(e)))
})

# 17c -- climasus_dlnm with missing $pred
fit_nopred <- fit_base
fit_nopred$pred <- NULL
tryCatch({
  sus_mod_af(fit_nopred, lang = "en")
  fail("should error when $pred is NULL")
}, error = function(e) {
  ok(sprintf("Error caught (NULL $pred -- crosspred fails): %s",
             conditionMessage(e)))
})


# =============================================================================
section("18. nsim sensitivity check")
# =============================================================================
# Very small nsim should still give a finite result (not crash)
af_tiny <- sus_mod_af(fit_base, nsim = 10L, verbose = FALSE, lang = "en")
stopifnot(is.finite(af_tiny$total$af[1]))               ; ok("nsim=10: finite AF estimate")
stopifnot(is.finite(af_tiny$total$af_lo[1]))            ; ok("nsim=10: finite AF_lo")
stopifnot(is.finite(af_tiny$total$af_hi[1]))            ; ok("nsim=10: finite AF_hi")

# Larger nsim should produce a tighter CI (probabilistic, not guaranteed with
# tiny data, so we just check that the run completes successfully)
af_large <- sus_mod_af(fit_base, nsim = 500L, verbose = FALSE, lang = "en")
stopifnot(inherits(af_large, "climasus_af"))             ; ok("nsim=500: run completed")
ok(sprintf("CI width nsim=10: %.4f | nsim=500: %.4f",
           af_tiny$total$af_hi[1]  - af_tiny$total$af_lo[1],
           af_large$total$af_hi[1] - af_large$total$af_lo[1]))


# =============================================================================
cat("\n=============================================================\n")
cat("  ALL TESTS PASSED\n")
cat("=============================================================\n\n")
