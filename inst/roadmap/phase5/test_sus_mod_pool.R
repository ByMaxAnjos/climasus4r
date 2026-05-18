# =============================================================================
# test_sus_mod_pool.R
# Comprehensive functional test for sus_mod_pool()
# Run with: source("inst/roadmap/phase5/test_sus_mod_pool.R")
# =============================================================================

devtools::load_all()
library(dlnm)
library(splines)
library(mvmeta)

cat("\n=============================================================\n")
cat("  sus_mod_pool() -- Functional Test Suite\n")
cat("=============================================================\n\n")

# -- helpers ------------------------------------------------------------------
ok      <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail    <- function(msg) stop(sprintf("  [FAIL] %s", msg))
section <- function(s)   cat(sprintf("\n--- %s ---\n", s))

# =============================================================================
section("1. Build shared climasus_dlnm fixtures (3 cities, 3 years)")
# =============================================================================
set.seed(42)
n_days  <- 3L * 365L
lag_max_fix <- 14L
dates   <- seq(as.Date("2020-01-01"), by = "day", length.out = n_days)
doy     <- as.integer(format(dates, "%j"))

make_city_fit <- function(seed, base_temp = 22, amp = 8, beta_temp = 0.012,
                           lag_max = lag_max_fix) {
  set.seed(seed)
  temp0  <- base_temp + amp * sin(2 * pi * doy / 365 - pi / 2) +
              stats::rnorm(n_days, sd = 2)
  deaths <- stats::rpois(n_days,
               exp(2.0 + beta_temp * temp0 + 0.3 * sin(2 * pi * doy / 365)))
  rh     <- 70 + 10 * cos(2 * pi * doy / 365) + stats::rnorm(n_days, sd = 3)

  make_lag <- function(s, l) c(rep(NA_real_, l), head(s, length(s) - l))

  df_raw <- data.frame(
    date          = dates,
    cod_munic     = paste0("city_", seed),
    n_obitos      = deaths,
    rh_mean_porc  = rh
  )
  for (l in 0L:lag_max)
    df_raw[[paste0("tair_dry_bulb_c_lag", l)]] <- make_lag(temp0, l)
  df_raw <- df_raw[!is.na(df_raw[[paste0("tair_dry_bulb_c_lag", lag_max)]]), ]

  df_dl <- ensure_climasus_df(
    df_raw, system = "SIM", stage = "climate", type = "distributed_lag"
  )
  sus_mod_dlnm(df_dl,
    outcome_col  = "n_obitos",
    lag_max      = lag_max,
    family       = "quasipoisson",
    dof_per_year = 4L,
    verbose      = FALSE,
    lang         = "pt"
  )
}

fit_a <- make_city_fit(seed = 1L)
fit_b <- make_city_fit(seed = 2L, base_temp = 18, beta_temp = 0.015)
fit_c <- make_city_fit(seed = 3L, base_temp = 26, amp = 6, beta_temp = 0.010)

ok(sprintf("City A: %d obs | City B: %d obs | City C: %d obs",
           fit_a$meta$n, fit_b$meta$n, fit_c$meta$n))

fits_all <- list(city_a = fit_a, city_b = fit_b, city_c = fit_c)


# =============================================================================
section("2. Basic pool -- 3 named cities, defaults")
# =============================================================================
pool1 <- sus_mod_pool(fits_all, lang = "en", verbose = TRUE)

stopifnot(inherits(pool1, "climasus_pool"))             ; ok("class = climasus_pool")
stopifnot(!is.null(pool1$mvmeta_fit))                  ; ok("$mvmeta_fit present")
stopifnot(!is.null(pool1$pooled_pred))                 ; ok("$pooled_pred present")
stopifnot(!is.null(pool1$exposure_response))            ; ok("$exposure_response present")
stopifnot(!is.null(pool1$lag_response))                 ; ok("$lag_response present")
stopifnot(!is.null(pool1$blup_preds))                   ; ok("$blup_preds present (blup=TRUE default)")
stopifnot(!is.null(pool1$city_table))                   ; ok("$city_table present")
stopifnot(!is.null(pool1$heterogeneity))                ; ok("$heterogeneity present")
stopifnot(!is.null(pool1$meta))                         ; ok("$meta present")


# =============================================================================
section("3. $meta completeness")
# =============================================================================
required_meta <- c("climate_col", "outcome_col", "lag_max", "n_cities",
                   "city_names", "method", "argvar", "arglag", "ref_value",
                   "expo_grid", "pred_at", "call_time")
stopifnot(all(required_meta %in% names(pool1$meta)))    ; ok("$meta has all 12 required fields")
stopifnot(pool1$meta$n_cities == 3L)                    ; ok("$meta$n_cities = 3")
stopifnot(all(c("city_a", "city_b", "city_c") %in% pool1$meta$city_names))
                                                         ok("$meta$city_names correct")
stopifnot(pool1$meta$climate_col == "tair_dry_bulb_c")  ; ok("$meta$climate_col correct")
stopifnot(pool1$meta$lag_max == lag_max_fix)             ; ok("$meta$lag_max matches input")
stopifnot(pool1$meta$method == "reml")                  ; ok("$meta$method = reml (default)")


# =============================================================================
section("4. $exposure_response structure and validity")
# =============================================================================
er <- pool1$exposure_response
er_cols <- c("pct", "exposure", "rr", "lo", "hi")
stopifnot(all(er_cols %in% names(er)))                  ; ok("$exposure_response has all 5 columns")
stopifnot(nrow(er) == 4L)                               ; ok("4 rows (default pred_at has 4 quantiles)")
stopifnot(all(is.finite(er$rr)))                        ; ok("All pooled RR are finite")
stopifnot(all(er$lo <= er$rr))                          ; ok("lo <= rr for all rows")
stopifnot(all(er$rr <= er$hi))                          ; ok("rr <= hi for all rows")

# Log the p75 result
p75 <- er[which.min(abs(er$pct - 0.75)), ]
ok(sprintf("Pooled RR (p75): %.4f [%.4f, %.4f]", p75$rr, p75$lo, p75$hi))


# =============================================================================
section("5. $lag_response structure and validity")
# =============================================================================
lr <- pool1$lag_response
stopifnot(!is.null(lr))                                 ; ok("$lag_response is not NULL")
lr_cols <- c("lag", "rr_lag", "lo", "hi")
stopifnot(all(lr_cols %in% names(lr)))                  ; ok("$lag_response has 4 required columns")
stopifnot(nrow(lr) == lag_max_fix + 1L)                 ; ok("$lag_response has lag_max+1 rows")
stopifnot(all(lr$lag == 0L:lag_max_fix))                ; ok("$lag_response$lag = 0:lag_max")
stopifnot(all(is.finite(lr$rr_lag)))                    ; ok("All lag-RR values are finite")


# =============================================================================
section("6. $heterogeneity structure and validity")
# =============================================================================
het <- pool1$heterogeneity
het_cols <- c("Q", "df", "p_value", "i2")
stopifnot(all(het_cols %in% names(het)))                ; ok("$heterogeneity has Q/df/p_value/i2")
stopifnot(!is.na(het$Q) && is.finite(het$Q))            ; ok("Q is finite")
stopifnot(!is.na(het$i2) && het$i2 >= 0 && het$i2 <= 100)
                                                         ok(sprintf("I2 = %.1f%% (in [0, 100])", het$i2))
# df = (n_cities - 1) * n_coef (multivariate Q-test degrees of freedom)
stopifnot(het$df > 0L && het$df %% (3L - 1L) == 0L)    ; ok("df is positive multiple of (n_cities-1)")
ok(sprintf("Heterogeneity: Q=%.2f (df=%d, p=%.4f), I²=%.1f%%",
           het$Q, het$df, het$p_value, het$i2))


# =============================================================================
section("7. $city_table structure and validity")
# =============================================================================
ct <- pool1$city_table
ct_cols <- c("city", "n", "rr", "lo", "hi", "blup_rr", "blup_lo", "blup_hi")
stopifnot(all(ct_cols %in% names(ct)))                  ; ok("$city_table has all 8 required columns")
stopifnot(nrow(ct) == 3L)                               ; ok("$city_table has 3 rows")
stopifnot(all(c("city_a", "city_b", "city_c") %in% ct$city))
                                                         ok("City names in $city_table")
stopifnot(all(is.finite(ct$rr)))                        ; ok("All city-specific RR finite")
stopifnot(all(is.finite(ct$blup_rr)))                   ; ok("All BLUP RR finite (BLUPs computed)")


# =============================================================================
section("8. $blup_preds structure")
# =============================================================================
stopifnot(length(pool1$blup_preds) == 3L)               ; ok("blup_preds has 3 entries")
stopifnot(all(c("city_a", "city_b", "city_c") %in% names(pool1$blup_preds)))
                                                         ok("blup_preds named by city")
stopifnot(all(vapply(pool1$blup_preds, function(p) !is.null(p), logical(1L))))
                                                         ok("All BLUP crosspred objects non-NULL")

# BLUP RRs should be finite at p75
for (nm in names(pool1$blup_preds)) {
  p <- pool1$blup_preds[[nm]]
  idx <- which.min(abs(as.numeric(p$predvar) - pool1$meta$ref_value))
  stopifnot(is.finite(as.numeric(p$allRRfit[[idx]])))
}
ok("All BLUP crosspred RR at ref_value are finite")


# =============================================================================
section("9. blup = FALSE disables BLUP computation")
# =============================================================================
pool_no_blup <- sus_mod_pool(fits_all, blup = FALSE, lang = "en", verbose = FALSE)
stopifnot(is.null(pool_no_blup$blup_preds))             ; ok("blup_preds = NULL when blup=FALSE")
# city_table should have NA blup columns
stopifnot(all(is.na(pool_no_blup$city_table$blup_rr)))  ; ok("city_table$blup_rr = NA when blup=FALSE")


# =============================================================================
section("10. method = 'fixed' (fixed-effects model)")
# =============================================================================
pool_fe <- sus_mod_pool(fits_all, method = "fixed", blup = FALSE,
                        lang = "en", verbose = FALSE)
stopifnot(inherits(pool_fe, "climasus_pool"))            ; ok("Fixed-effects pool succeeds")
stopifnot(pool_fe$meta$method == "fixed")               ; ok("$meta$method = fixed")
stopifnot(!is.null(pool_fe$pooled_pred))                ; ok("$pooled_pred present in fixed model")


# =============================================================================
section("11. Custom exposure_range")
# =============================================================================
pool_range <- sus_mod_pool(fits_all, exposure_range = c(20, 35),
                            n_grid = 50L, blup = FALSE,
                            lang = "en", verbose = FALSE)
stopifnot(length(pool_range$meta$expo_grid) == 50L)     ; ok("n_grid = 50 honoured")
stopifnot(abs(min(pool_range$meta$expo_grid) - 20) < 1e-6)
                                                         ok("expo_grid min = 20")
stopifnot(abs(max(pool_range$meta$expo_grid) - 35) < 1e-6)
                                                         ok("expo_grid max = 35")


# =============================================================================
section("12. Custom pred_at")
# =============================================================================
pool_pq <- sus_mod_pool(fits_all, pred_at = c(0.80, 0.99),
                        blup = FALSE, lang = "en", verbose = FALSE)
stopifnot(nrow(pool_pq$exposure_response) == 2L)        ; ok("2 rows in $exposure_response (pred_at = 2 quantiles)")
stopifnot(all(abs(pool_pq$exposure_response$pct - c(0.80, 0.99)) < 1e-9))
                                                         ok("exposure_response$pct matches pred_at")


# =============================================================================
section("13. S3 methods")
# =============================================================================
cat("\n  -- print.climasus_pool --\n")
print(pool1)
ok("print() completed without error")

cat("\n  -- summary.climasus_pool --\n")
summary(pool1)
ok("summary() completed without error")

cat("\n  -- coef.climasus_pool --\n")
cfs <- coef(pool1)
stopifnot(is.numeric(cfs) && length(cfs) > 0)           ; ok("coef() returns non-empty numeric vector")
ok(sprintf("  %d pooled coefficients", length(cfs)))

cat("\n  -- vcov.climasus_pool --\n")
vc <- vcov(pool1)
stopifnot(is.matrix(vc) && nrow(vc) == length(cfs))     ; ok("vcov() returns square matrix matching coef")

cat("\n  -- tidy.climasus_pool --\n")
td <- tidy(pool1)
stopifnot(inherits(td, "data.frame"))                   ; ok("tidy() returns data.frame")
stopifnot(nrow(td) == 1L)                               ; ok("tidy() returns 1-row tibble")
tidy_cols <- c("climate_col", "outcome_col", "lag_max", "method", "n_cities",
               "ref_value", "rr_p75", "lo_p75", "hi_p75", "Q", "df_het", "p_het", "i2")
stopifnot(all(tidy_cols %in% names(td)))                ; ok(sprintf("tidy() has all %d required columns", length(tidy_cols)))
print(td[, c("climate_col", "method", "n_cities", "rr_p75", "lo_p75", "hi_p75", "i2")])


# =============================================================================
section("14. tidy() multi-pool binding")
# =============================================================================
# Fit a second pool with different lag_max
fits_short <- list(
  city_a_s = make_city_fit(seed = 1L, lag_max = 7L),
  city_b_s = make_city_fit(seed = 2L, lag_max = 7L, base_temp = 18)
)
pool_short <- sus_mod_pool(fits_short, blup = FALSE, lang = "pt", verbose = FALSE)

bound <- dplyr::bind_rows(tidy(pool1), tidy(pool_short))
stopifnot(nrow(bound) == 2L)                            ; ok("bind_rows(tidy(pool1), tidy(pool2)) = 2 rows")
stopifnot(all(c(14L, 7L) %in% bound$lag_max))           ; ok("lag_max distinguishes the two pools")
stopifnot(c(3L, 2L) %in% bound$n_cities)               ; ok("n_cities differs between pools")
print(bound[, c("lag_max", "n_cities", "rr_p75", "lo_p75", "hi_p75", "i2")])


# =============================================================================
section("15. Language variants")
# =============================================================================
pool_pt <- sus_mod_pool(fits_all, blup = FALSE, verbose = TRUE, lang = "pt")
ok("lang='pt' run succeeded")
pool_en <- sus_mod_pool(fits_all, blup = FALSE, verbose = TRUE, lang = "en")
ok("lang='en' run succeeded")
pool_es <- sus_mod_pool(fits_all, blup = FALSE, verbose = TRUE, lang = "es")
ok("lang='es' run succeeded")
# Point estimates should be identical across languages
p75_pt <- pool_pt$exposure_response$rr[which.min(abs(pool_pt$exposure_response$pct - 0.75))]
p75_en <- pool_en$exposure_response$rr[which.min(abs(pool_en$exposure_response$pct - 0.75))]
p75_es <- pool_es$exposure_response$rr[which.min(abs(pool_es$exposure_response$pct - 0.75))]
stopifnot(abs(p75_pt - p75_en) < 1e-9)                 ; ok("pt and en give same pooled RR")
stopifnot(abs(p75_pt - p75_es) < 1e-9)                 ; ok("pt and es give same pooled RR")


# =============================================================================
section("16. Two-city edge case")
# =============================================================================
fits_two <- list(city_a = fit_a, city_b = fit_b)
pool_two <- sus_mod_pool(fits_two, blup = TRUE, lang = "en", verbose = FALSE)
stopifnot(inherits(pool_two, "climasus_pool"))           ; ok("Two-city pool succeeds")
stopifnot(pool_two$meta$n_cities == 2L)                 ; ok("$meta$n_cities = 2")
stopifnot(pool_two$heterogeneity$df > 0L && pool_two$heterogeneity$df %% 1L == 0L)
                                                         ok("df is a positive integer for two-city pool")
stopifnot(!is.null(pool_two$blup_preds))                 ; ok("BLUP computed for two cities")


# =============================================================================
section("17. Unnamed fits list gets auto-named")
# =============================================================================
fits_unnamed <- list(fit_a, fit_b, fit_c)   # no names
pool_unm <- sus_mod_pool(fits_unnamed, blup = FALSE, lang = "en", verbose = FALSE)
stopifnot(all(startsWith(pool_unm$meta$city_names, "city_")))
                                                         ok("Unnamed list auto-assigned city_1/2/3 names")
stopifnot(nrow(pool_unm$city_table) == 3L)              ; ok("$city_table has 3 rows for unnamed input")


# =============================================================================
section("18. Error cases")
# =============================================================================
# Non-list input
caught <- tryCatch(sus_mod_pool("not_a_list", lang = "en"), error = function(e) e)
stopifnot(inherits(caught, "error"))                    ; ok("Non-list input raises error")

# Empty list
caught <- tryCatch(sus_mod_pool(list(), lang = "en"), error = function(e) e)
stopifnot(inherits(caught, "error"))                    ; ok("Empty list raises error")

# List with non-climasus_dlnm element
bad_fits <- list(city_a = fit_a, city_x = list(foo = "bar"))
caught <- tryCatch(sus_mod_pool(bad_fits, lang = "en"), error = function(e) e)
stopifnot(inherits(caught, "error"))                    ; ok("Non-climasus_dlnm element raises error")

# Incompatible lag_max
fit_bad <- make_city_fit(seed = 99L, lag_max = 10L)
fits_bad_lag <- list(city_a = fit_a, city_bad = fit_bad)
caught <- tryCatch(sus_mod_pool(fits_bad_lag, lang = "en"), error = function(e) e)
stopifnot(inherits(caught, "error"))                    ; ok("Incompatible lag_max raises error")

cat("\n=============================================================\n")
cat("  ALL TESTS PASSED\n")
cat("=============================================================\n\n")
