# =============================================================================
# test_sus_mod_dlnm.R
# Comprehensive functional test for sus_mod_dlnm()
# Run with: source("inst/roadmap/phase5/test_sus_mod_dlnm.R")
# =============================================================================

devtools::load_all()
library(dlnm)
library(splines)

cat("\n=============================================================\n")
cat("  sus_mod_dlnm() – Functional Test Suite\n")
cat("=============================================================\n\n")

# ── helpers ────────────────────────────────────────────────────────────────────
ok  <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail <- function(msg) stop(sprintf("  [FAIL] %s", msg))
section <- function(s) cat(sprintf("\n--- %s ---\n", s))

# ── 1. Build synthetic climasus_df at stage "climate" / type "distributed_lag"
section("1. Build synthetic distributed_lag data (3 years, 3 municipalities)")

set.seed(42)
n_days  <- 3 * 365   # ~3 years
n_munic <- 3
lag_max_true <- 21L

dates <- seq(as.Date("2020-01-01"), by = "day", length.out = n_days)

# Simulate smooth temperature (seasonal + noise)
day_of_year <- as.integer(format(dates, "%j"))
temp_base   <- 28 + 6 * sin(2 * pi * day_of_year / 365 - pi / 2)

# Build lag matrix (lag0 = same-day, lag21 = 21 days ago)
make_lag_series <- function(series, lag_n) {
  c(rep(NA_real_, lag_n), head(series, length(series) - lag_n))
}

# Create per-municipality data
df_list <- lapply(seq_len(n_munic), function(m) {
  temp0  <- temp_base + stats::rnorm(n_days, sd = 1.5) + stats::rnorm(1, sd = 2)
  # Deaths: Poisson GLM of temp with overdispersion
  mu     <- exp(2.0 + 0.01 * temp0 + 0.3 * sin(2 * pi * day_of_year / 365))
  deaths <- stats::rpois(n_days, lambda = mu)

  row_df <- data.frame(
    date     = dates,
    cod_munic = sprintf("35000%d", m),
    n_obitos = deaths,
    rh_mean_porc = 70 + 10 * cos(2 * pi * day_of_year / 365) + stats::rnorm(n_days, sd = 3)
  )

  for (lag_i in 0L:lag_max_true) {
    col_nm <- paste0("tair_dry_bulb_c_lag", lag_i)
    row_df[[col_nm]] <- make_lag_series(temp0, lag_i)
  }
  row_df
})

df_raw <- do.call(rbind, df_list)
df_raw <- df_raw[!is.na(df_raw$tair_dry_bulb_c_lag21), ]  # remove NA-lag rows

# Promote to climasus_df
df_dl <- new_climasus_df(
  df_raw,
  list(
    system  = "SIM",
    stage   = "climate",
    type    = "distributed_lag",
    backend = "tibble",
    history = c("[2020-01-01] synthetic test data created")
  )
)
ok(sprintf("climasus_df created: %d rows x %d cols; stage=%s; type=%s",
           nrow(df_dl), ncol(df_dl),
           sus_meta(df_dl, "stage"), sus_meta(df_dl, "type")))
stopifnot(inherits(df_dl, "climasus_df"))
stopifnot(sus_meta(df_dl, "stage") == "climate")
stopifnot(sus_meta(df_dl, "type")  == "distributed_lag")


# =============================================================================
section("2. Basic run – auto-detect climate_col and lag_max")
# =============================================================================
fit1 <- sus_mod_dlnm(
  df          = df_dl,
  outcome_col = "n_obitos",
  climate_col = NULL,     # auto-detect
  lag_max     = NULL,     # auto-detect
  family      = "quasipoisson",
  dof_per_year = 4L,
  lang        = "pt",
  verbose     = TRUE
)

ok("sus_mod_dlnm() returned without error")
stopifnot(inherits(fit1, "climasus_dlnm"))                ; ok("class = climasus_dlnm")
stopifnot(!is.null(fit1$model))                           ; ok("$model present")
stopifnot(!is.null(fit1$crossbasis))                      ; ok("$crossbasis present")
stopifnot(!is.null(fit1$pred))                            ; ok("$pred present")
stopifnot(!is.null(fit1$exposure_response))               ; ok("$exposure_response present")
stopifnot(!is.null(fit1$lag_response))                    ; ok("$lag_response present")
stopifnot(!is.null(fit1$models))                          ; ok("$models present")
stopifnot(!is.null(fit1$data_daily))                      ; ok("$data_daily present")
stopifnot(!is.null(fit1$diagnostics))                     ; ok("$diagnostics present")
stopifnot(!is.null(fit1$meta))                            ; ok("$meta present")

# Check sus_meta attribute
meta_attr <- attr(fit1, "sus_meta")
stopifnot(!is.null(meta_attr))                            ; ok("attr(result, 'sus_meta') present")
stopifnot(length(meta_attr$history) >= 2L)                ; ok("history updated (>= 2 entries)")

# Check auto-detection
stopifnot(fit1$meta$climate_col == "tair_dry_bulb_c")     ; ok("climate_col auto-detected correctly")
stopifnot(fit1$meta$lag_max == lag_max_true)              ; ok("lag_max auto-detected correctly")

# Check data_daily content
stopifnot("date"   %in% names(fit1$data_daily))           ; ok("$data_daily has 'date' column")
stopifnot("y"      %in% names(fit1$data_daily))           ; ok("$data_daily has 'y' column")
stopifnot(nrow(fit1$data_daily) <= n_days)                ; ok("$data_daily has <= n_days rows (aggregated)")


# =============================================================================
section("3. Exposure-response table structure")
# =============================================================================
er <- fit1$exposure_response
stopifnot(all(c("pct","exposure","rr","lo","hi") %in% names(er))) ; ok("exposure_response has all 5 columns")
stopifnot(nrow(er) == 6L)                                          ; ok("exposure_response has 6 rows (pred_at defaults)")
stopifnot(all(er$lo <= er$rr & er$rr <= er$hi))                   ; ok("lo <= rr <= hi in exposure_response")
ok(sprintf("RR at p75=%.2f: %.4f [%.4f, %.4f]",
           er$exposure[er$pct == 0.75],
           er$rr[er$pct == 0.75],
           er$lo[er$pct == 0.75],
           er$hi[er$pct == 0.75]))


# =============================================================================
section("4. Lag-response table structure")
# =============================================================================
lr <- fit1$lag_response
stopifnot(all(c("lag","rr","lo","hi","rr_cum") %in% names(lr)))   ; ok("lag_response has all 5 columns")
stopifnot(nrow(lr) == lag_max_true + 1L)                           ; ok(sprintf("lag_response has %d rows (lag 0-%d)", lag_max_true+1, lag_max_true))
stopifnot(all(lr$lo <= lr$rr & lr$rr <= lr$hi))                   ; ok("lo <= rr <= hi in lag_response")
ok(sprintf("Lag-peak: day %d", fit1$models$lag_peak))


# =============================================================================
section("5. models summary tibble")
# =============================================================================
m <- fit1$models
stopifnot(nrow(m) == 1L)                                           ; ok("$models has 1 row")
stopifnot(all(c("variable","n","family","lag_max","ref_value",
                "exposure_p75","rr","lo","hi","lag_peak",
                "disp_ratio","aic_poisson") %in% names(m)))        ; ok("$models has all 12 columns")
stopifnot(m$family == "quasipoisson")                              ; ok("family = quasipoisson in $models")


# =============================================================================
section("6. Diagnostics")
# =============================================================================
d <- fit1$diagnostics
stopifnot(is.numeric(d$disp_ratio))                                ; ok("disp_ratio numeric")
stopifnot(d$disp_category %in% c("adequate","moderate","high"))    ; ok("disp_category valid")
stopifnot(is.numeric(d$autocorr_pval))                             ; ok("autocorr_pval numeric")
stopifnot(is.logical(d$has_autocorr))                              ; ok("has_autocorr logical")
stopifnot(is.numeric(d$aic_poisson))                               ; ok("aic_poisson numeric")
ok(sprintf("disp_ratio=%.2f (%s); Ljung-Box p=%.4f; AIC=%.1f",
           d$disp_ratio, d$disp_category, d$autocorr_pval, d$aic_poisson))


# =============================================================================
section("7. S3 methods")
# =============================================================================
cat("\n  -- print.climasus_dlnm --\n")
print(fit1)
ok("print() completed without error")

cat("\n  -- coef.climasus_dlnm --\n")
coefs <- coef(fit1)
stopifnot(is.numeric(coefs) && length(coefs) > 0L)                ; ok(sprintf("coef() returned %d coefficients", length(coefs)))

cat("\n  -- tidy.climasus_dlnm --\n")
td <- tidy(fit1)
stopifnot(inherits(td, "data.frame") && nrow(td) == 1L)           ; ok("tidy() returned 1-row tibble")
stopifnot("rr" %in% names(td))                                    ; ok("tidy() tibble has 'rr' column")
print(td)


# =============================================================================
section("8. Explicit parameters – poisson family, explicit lag_max, ns_df, pred_at")
# =============================================================================
fit2 <- sus_mod_dlnm(
  df           = df_dl,
  outcome_col  = "n_obitos",
  climate_col  = "tair_dry_bulb_c",
  lag_max      = 14L,
  argvar       = list(fun = "ns", df = 3),
  arglag       = list(fun = "ns", df = 2),
  family       = "poisson",
  ns_df        = 12L,
  ref_value    = 25.0,
  pred_at      = c(0.10, 0.50, 0.90),
  alpha        = 0.05,
  lang         = "en",
  verbose      = FALSE
)
stopifnot(inherits(fit2, "climasus_dlnm"))                         ; ok("poisson family run succeeded")
stopifnot(fit2$meta$lag_max == 14L)                               ; ok("explicit lag_max = 14 honoured")
stopifnot(fit2$meta$ns_df   == 12L)                               ; ok("explicit ns_df = 12 honoured")
stopifnot(abs(fit2$meta$ref_value - 25.0) < 1e-9)                ; ok("explicit ref_value = 25.0 honoured")
stopifnot(nrow(fit2$exposure_response) == 3L)                     ; ok("pred_at = 3 quantiles → 3-row exposure_response")
stopifnot(fit2$meta$family == "poisson")                          ; ok("family = poisson stored in meta")


# =============================================================================
section("9. negbin → quasipoisson redirect with warning")
# =============================================================================
fit3 <- withCallingHandlers(
  sus_mod_dlnm(
    df          = df_dl,
    outcome_col = "n_obitos",
    climate_col = "tair_dry_bulb_c",
    lag_max     = 7L,
    family      = "negbin",
    verbose     = FALSE,
    lang        = "en"
  ),
  warning = function(w) {
    if (grepl("quasipoisson|negbin|negative", conditionMessage(w), ignore.case = TRUE)) {
      cat(sprintf("  [WARNING caught] %s\n", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  }
)
stopifnot(inherits(fit3, "climasus_dlnm"))                         ; ok("negbin run completed (redirected to quasipoisson)")
stopifnot(fit3$meta$family == "quasipoisson")                     ; ok("meta$family = quasipoisson after redirect")


# =============================================================================
section("10. Covariates parameter")
# =============================================================================
fit4 <- sus_mod_dlnm(
  df          = df_dl,
  outcome_col = "n_obitos",
  climate_col = "tair_dry_bulb_c",
  lag_max     = 7L,
  covariates  = "rh_mean_porc",
  family      = "quasipoisson",
  dof_per_year = 4L,
  verbose     = FALSE,
  lang        = "pt"
)
stopifnot(inherits(fit4, "climasus_dlnm"))                         ; ok("covariates run succeeded")
# rh_mean_porc should appear in model coefficients
coef_names <- names(coef(fit4))
stopifnot(any(grepl("rh_mean_porc", coef_names)))                 ; ok("rh_mean_porc appears in GLM coefficients")


# =============================================================================
section("11. Language = 'es' (Spanish messages)")
# =============================================================================
fit5 <- sus_mod_dlnm(
  df          = df_dl,
  outcome_col = "n_obitos",
  climate_col = "tair_dry_bulb_c",
  lag_max     = 7L,
  family      = "quasipoisson",
  verbose     = TRUE,
  lang        = "es"
)
stopifnot(inherits(fit5, "climasus_dlnm"))                         ; ok("Spanish language run succeeded")


# =============================================================================
section("12. Error cases")
# =============================================================================

# 12a – not a climasus_df
tryCatch({
  sus_mod_dlnm(df = data.frame(x = 1), outcome_col = "x")
  fail("should have errored on plain data.frame")
}, error = function(e) {
  ok(sprintf("Error caught (not climasus_df): %s", conditionMessage(e)))
})

# 12b – wrong stage
df_wrong_stage <- new_climasus_df(
  data.frame(x = 1, date = Sys.Date()),
  list(stage = "aggregate", type = "agg", system = "SIM")
)
tryCatch({
  sus_mod_dlnm(df = df_wrong_stage, outcome_col = "x")
  fail("should have errored on wrong stage")
}, error = function(e) {
  ok(sprintf("Error caught (wrong stage): %s", conditionMessage(e)))
})

# 12c – missing outcome column
tryCatch({
  sus_mod_dlnm(df = df_dl, outcome_col = "nonexistent_col")
  fail("should have errored on missing outcome column")
}, error = function(e) {
  ok(sprintf("Error caught (bad outcome col): %s", conditionMessage(e)))
})

# 12d – no lag columns found for specified climate_col
tryCatch({
  sus_mod_dlnm(df = df_dl, climate_col = "totally_fake_var_xyz")
  fail("should have errored on missing lag cols")
}, error = function(e) {
  ok(sprintf("Error caught (no lag cols): %s", conditionMessage(e)))
})

# 12e – bad covariate
tryCatch({
  sus_mod_dlnm(df = df_dl, climate_col = "tair_dry_bulb_c",
               lag_max = 7L, covariates = "ghost_col_999")
  fail("should have errored on bad covariate")
}, error = function(e) {
  ok(sprintf("Error caught (bad covariate): %s", conditionMessage(e)))
})


# =============================================================================
section("13. summary() method (verbose)")
# =============================================================================
cat("\n")
summary(fit1)
ok("summary() completed without error")


# =============================================================================
section("14. tidy() multi-run binding (broom pattern)")
# =============================================================================
bound <- dplyr::bind_rows(
  tidy(fit1),
  tidy(fit2),
  tidy(fit3),
  tidy(fit4)
)
stopifnot(nrow(bound) == 4L)    ; ok("bind_rows(tidy(...), ...) produced 4-row tibble")
print(bound[, c("climate_col","family","lag_max","rr","lo","hi","disp_ratio")])


# =============================================================================
cat("\n=============================================================\n")
cat("  ALL TESTS PASSED\n")
cat("=============================================================\n\n")
