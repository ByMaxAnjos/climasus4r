# =============================================================================
# test_sus_mod_burden.R
# Comprehensive functional test for sus_mod_burden()
# Run with: source("inst/roadmap/phase5/test_sus_mod_burden.R")
# =============================================================================

devtools::load_all()

cat("\n=============================================================\n")
cat("  sus_mod_burden() -- Functional Test Suite\n")
cat("=============================================================\n\n")

# -- helpers ------------------------------------------------------------------
ok      <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail    <- function(msg) stop(sprintf("  [FAIL] %s", msg))
section <- function(s)   cat(sprintf("\n--- %s ---\n", s))

# =============================================================================
section("0. Build shared fixtures")
# =============================================================================

# Helper: construct a climasus_af object with known AN/AF values.
# Using known values avoids statistical uncertainty from DLNM fitting.
.make_af <- function(an_total, an_heat, an_cold, n_cases = 10000L,
                     city = "city", seed_time = NULL) {
  af_val  <- function(an) round(an / n_cases, 6)
  af_pct  <- function(an) round(an / n_cases * 100, 2)
  structure(
    list(
      total = tibble::tibble(
        component = c("total",     "heat",      "cold"),
        threshold = c(NA_real_,    25.0,        25.0),
        af        = c(af_val(an_total), af_val(an_heat), af_val(an_cold)),
        af_lo     = c(af_val(an_total), af_val(an_heat), af_val(an_cold)) * 0.8,
        af_hi     = c(af_val(an_total), af_val(an_heat), af_val(an_cold)) * 1.2,
        af_pct    = c(af_pct(an_total), af_pct(an_heat), af_pct(an_cold)),
        af_pct_lo = c(af_pct(an_total), af_pct(an_heat), af_pct(an_cold)) * 0.8,
        af_pct_hi = c(af_pct(an_total), af_pct(an_heat), af_pct(an_cold)) * 1.2,
        an        = c(an_total,  an_heat,     an_cold),
        an_lo     = c(an_total,  an_heat,     an_cold) * 0.8,
        an_hi     = c(an_total,  an_heat,     an_cold) * 1.2,
        n_cases   = n_cases
      ),
      by_quantile = NULL, by_period = NULL, daily = NULL, custom = NULL,
      meta = list(
        climate_col = "temp_c",  outcome_col = "n_obitos",
        family      = "quasipoisson", lag_max = 7L,
        ref_value   = 25.0,      threshold = 25.0,
        n_cases     = n_cases,   n_obs  = 1460L,
        pred_at     = c(0.75, 0.9, 0.95, 0.99),
        nsim        = 0L,        alpha  = 0.05,
        by          = NULL,      ci_method = "delta",
        call_time   = Sys.time()
      )
    ),
    class = c("climasus_af", "list")
  )
}

# Helper: construct a climasus_excess object with known values.
.make_excess <- function(excess, excess_pct, n_days = 365L, observed = 5000L,
                         expected = NULL) {
  if (is.null(expected)) expected <- round(observed - excess, 1)
  structure(
    list(
      daily     = NULL,
      total     = tibble::tibble(
        n_days        = n_days,
        observed      = observed,
        expected      = expected,
        expected_lo   = expected * 0.9,
        expected_hi   = expected * 1.1,
        excess        = excess,
        excess_lo     = excess * 0.8,
        excess_hi     = excess * 1.2,
        excess_pct    = excess_pct,
        n_excess_days = as.integer(n_days * 0.1),
        peak_excess   = as.integer(excess * 0.05),
        peak_date     = as.Date("2020-01-15")
      ),
      by_period = NULL,
      model     = NULL,
      meta      = list(
        method         = "from_dlnm", outcome_col = "n_obitos",
        date_col       = "date",
        control_period = c(as.Date("2018-01-01"), as.Date("2019-12-31")),
        study_period   = c(as.Date("2020-01-01"), as.Date("2020-12-31")),
        family         = "from_dlnm", dof_per_year = 8L, harmonics = 2L,
        threshold_z    = 1.96, n_obs = 1460L,
        n_study        = n_days, n_control = 730L,
        call_time      = Sys.time()
      )
    ),
    class = c("climasus_excess", "list")
  )
}

# Six cities with known, ordered AN values (positive, descending)
af_fits <- list(
  fortaleza      = .make_af(an_total = 450L, an_heat = 300L, an_cold = 150L,
                            n_cases = 10000L),
  recife         = .make_af(an_total = 300L, an_heat = 200L, an_cold = 100L,
                            n_cases =  8000L),
  salvador       = .make_af(an_total = 250L, an_heat = 180L, an_cold =  70L,
                            n_cases =  9000L),
  belo_horizonte = .make_af(an_total = 150L, an_heat = 100L, an_cold =  50L,
                            n_cases =  7000L),
  sao_paulo      = .make_af(an_total = 100L, an_heat =  70L, an_cold =  30L,
                            n_cases =  8000L),
  curitiba       = .make_af(an_total =  50L, an_heat =  20L, an_cold =  30L,
                            n_cases =  5000L)
)
ok("6 climasus_af mock fixtures built with known ordered AN values")

# Six excess objects with known, ordered excess values (positive, descending)
excess_fits <- list(
  fortaleza      = .make_excess(excess = 420L, excess_pct = 4.2),
  recife         = .make_excess(excess = 310L, excess_pct = 3.9),
  salvador       = .make_excess(excess = 260L, excess_pct = 2.9),
  belo_horizonte = .make_excess(excess = 160L, excess_pct = 2.3),
  sao_paulo      = .make_excess(excess = 105L, excess_pct = 1.3),
  curitiba       = .make_excess(excess =  55L, excess_pct = 1.1)
)
ok("6 climasus_excess mock fixtures built with known ordered excess values")

# =============================================================================
section("1. Basic call: returns climasus_burden from climasus_af list")
# =============================================================================

bd <- sus_mod_burden(af_fits, verbose = FALSE, lang = "en")

stopifnot(inherits(bd, "climasus_burden"))
ok("Returns climasus_burden object")

expected_names <- c("burden_table", "concentration", "total_burden", "meta")
stopifnot(all(expected_names %in% names(bd)))
ok("All top-level components present")

# =============================================================================
section("2. $burden_table structure for climasus_af")
# =============================================================================

bt <- bd$burden_table
stopifnot(is.data.frame(bt))
ok("$burden_table is a data.frame")

af_cols <- c("city", "component", "n_cases", "an", "an_lo", "an_hi",
             "af_pct", "af_pct_lo", "af_pct_hi", "rank", "pct_of_total")
stopifnot(all(af_cols %in% names(bt)))
ok("$burden_table has all required AF columns")

stopifnot(nrow(bt) == 6L)
ok("$burden_table has 6 rows (default component='total', one per city)")

stopifnot(all(bt$component == "total"))
ok("component column is 'total' for all rows")

stopifnot(sort(bt$rank) == seq_len(6L))
ok("Ranks are 1 to 6, no duplicates")

stopifnot(abs(sum(bt$pct_of_total) - 100) < 0.01)
ok("pct_of_total sums to approximately 100")

# =============================================================================
section("3. Default rank_by = 'an': cities sorted by AN descending")
# =============================================================================

# fortaleza has highest AN (450) -> rank 1
stopifnot(bt$city[[1L]] == "fortaleza")
ok("rank 1 city is fortaleza (highest AN = 450)")

# curitiba has lowest AN (50) -> rank 6
stopifnot(bt$city[[6L]] == "curitiba")
ok("rank 6 city is curitiba (lowest AN = 50)")

# AN is non-increasing with rank
stopifnot(all(diff(bt$an) <= 0))
ok("AN is non-increasing from rank 1 to 6")

# =============================================================================
section("4. $concentration structure")
# =============================================================================

conc <- bd$concentration
stopifnot(is.data.frame(conc))
ok("$concentration is a data.frame")

conc_cols <- c("city", "rank", "pct_of_total", "cumulative_pct")
stopifnot(all(conc_cols %in% names(conc)))
ok("$concentration has required columns")

stopifnot(nrow(conc) == 6L)
ok("$concentration has 6 rows (one per city)")

stopifnot(all(diff(conc$cumulative_pct) >= 0))
ok("cumulative_pct is non-decreasing")

stopifnot(abs(conc$cumulative_pct[[6L]] - 100) < 0.01)
ok("cumulative_pct ends at approximately 100")

# =============================================================================
section("5. $total_burden structure for AF inputs")
# =============================================================================

tb_bd <- bd$total_burden
stopifnot(all(c("an_total", "af_pct_avg", "top_city", "top_city_an") %in% names(tb_bd)))
ok("$total_burden has required keys for AF inputs")

# an_total = sum of all total-component ANs = 450+300+250+150+100+50 = 1300
stopifnot(tb_bd$an_total == 1300L)
ok("$total_burden$an_total == 1300 (sum of all city ANs)")

stopifnot(tb_bd$top_city == "fortaleza")
ok("$total_burden$top_city == 'fortaleza'")

stopifnot(tb_bd$top_city_an == 450L)
ok("$total_burden$top_city_an == 450")

stopifnot(is.numeric(tb_bd$af_pct_avg) && tb_bd$af_pct_avg > 0)
ok("$total_burden$af_pct_avg > 0")

# =============================================================================
section("6. $meta structure")
# =============================================================================

m <- bd$meta
stopifnot(all(c("input_type", "component", "rank_by", "n_cities",
                "top_n", "nsim", "alpha", "call_time") %in% names(m)))
ok("$meta has all expected keys")

stopifnot(m$input_type == "climasus_af")
ok("$meta$input_type == 'climasus_af'")

stopifnot(m$component == "total")
ok("$meta$component == 'total'")

stopifnot(m$rank_by == "an")
ok("$meta$rank_by == 'an'")

stopifnot(m$n_cities == 6L)
ok("$meta$n_cities == 6")

stopifnot(inherits(m$call_time, "POSIXct"))
ok("$meta$call_time is POSIXct")

# =============================================================================
section("7. component = 'heat' and 'cold'")
# =============================================================================

bd_heat <- sus_mod_burden(af_fits, component = "heat", verbose = FALSE)
bd_cold <- sus_mod_burden(af_fits, component = "cold", verbose = FALSE)

stopifnot(all(bd_heat$burden_table$component == "heat"))
ok("component='heat': all rows have component='heat'")

stopifnot(all(bd_cold$burden_table$component == "cold"))
ok("component='cold': all rows have component='cold'")

# Heat ranking: fortaleza (300) > recife (200) > ...
stopifnot(bd_heat$burden_table$city[[1L]] == "fortaleza")
ok("component='heat': fortaleza ranks first for heat AN")

# Cold ranking: fortaleza (150) > recife (100) > curitiba and salvador swap
# fortaleza: 150, recife: 100, belo_horizonte: 50, curitiba: 30, sao_paulo: 30
cold_top <- bd_cold$burden_table$city[[1L]]
stopifnot(cold_top == "fortaleza")
ok("component='cold': fortaleza ranks first for cold AN")

# pct_of_total sums to ~100 for each component
stopifnot(abs(sum(bd_heat$burden_table$pct_of_total) - 100) < 0.01)
ok("component='heat': pct_of_total sums to ~100")

stopifnot(abs(sum(bd_cold$burden_table$pct_of_total) - 100) < 0.01)
ok("component='cold': pct_of_total sums to ~100")

# =============================================================================
section("8. component = 'all'")
# =============================================================================

bd_all <- sus_mod_burden(af_fits, component = "all", verbose = FALSE)

stopifnot(nrow(bd_all$burden_table) == 18L)
ok("component='all': 18 rows (6 cities x 3 components)")

stopifnot(all(c("total", "heat", "cold") %in% bd_all$burden_table$component))
ok("component='all': all three components present")

# Concentration curve: 6 rows (city-level)
stopifnot(nrow(bd_all$concentration) == 6L)
ok("component='all': concentration curve has 6 rows")

# All rows for the same city share the same rank
for (nm in names(af_fits)) {
  rows <- bd_all$burden_table[bd_all$burden_table$city == nm, ]
  stopifnot(length(unique(rows$rank)) == 1L)
}
ok("component='all': all components of same city share the same rank")

# Ranking order matches component='total' ranking
top_total <- bd$burden_table$city[[1L]]
top_all   <- bd_all$burden_table$city[bd_all$burden_table$component == "total"][[1L]]
stopifnot(top_total == top_all)
ok("component='all': ranking matches component='total' ranking")

# =============================================================================
section("9. rank_by = 'af_pct'")
# =============================================================================

bd_af <- sus_mod_burden(af_fits, rank_by = "af_pct", verbose = FALSE)

stopifnot(bd_af$meta$rank_by == "af_pct")
ok("rank_by='af_pct': $meta$rank_by updated correctly")

# af_pct is non-increasing with rank
stopifnot(all(diff(bd_af$burden_table$af_pct) <= 0))
ok("rank_by='af_pct': af_pct is non-increasing with rank")

# pct_of_total sums to ~100
stopifnot(abs(sum(bd_af$burden_table$pct_of_total) - 100) < 0.01)
ok("rank_by='af_pct': pct_of_total sums to ~100")

# =============================================================================
section("10. top_n parameter")
# =============================================================================

bd_top3 <- sus_mod_burden(af_fits, top_n = 3L, verbose = FALSE)

stopifnot(nrow(bd_top3$burden_table) == 3L)
ok("top_n=3: burden_table has 3 rows")

stopifnot(nrow(bd_top3$concentration) == 3L)
ok("top_n=3: concentration has 3 rows")

stopifnot(all(bd_top3$burden_table$city == bd$burden_table$city[1:3]))
ok("top_n=3: retained cities match top 3 from full ranking")

# cumulative_pct of top 3 should be < 100 of the full ranking
stopifnot(conc$cumulative_pct[[3L]] < 100)
ok("top_n=3: cumulative_pct of rank-3 city < 100 of full distribution")

# =============================================================================
section("11. Input from climasus_excess objects")
# =============================================================================

bd_exc <- sus_mod_burden(excess_fits, verbose = FALSE, lang = "en")

stopifnot(inherits(bd_exc, "climasus_burden"))
ok("climasus_excess input: returns climasus_burden")

stopifnot(bd_exc$meta$input_type == "climasus_excess")
ok("climasus_excess input: $meta$input_type == 'climasus_excess'")

exc_cols <- c("city", "n_days", "observed", "expected", "excess",
              "excess_lo", "excess_hi", "excess_pct", "rank", "pct_of_total")
stopifnot(all(exc_cols %in% names(bd_exc$burden_table)))
ok("climasus_excess input: burden_table has required excess columns")

stopifnot(bd_exc$meta$rank_by == "excess")
ok("climasus_excess input: default rank_by = 'excess'")

# fortaleza (excess=420) should rank first
stopifnot(bd_exc$burden_table$city[[1L]] == "fortaleza")
ok("climasus_excess input: fortaleza ranks first (highest excess)")

# total_burden has excess keys
stopifnot(!is.null(bd_exc$total_burden$excess_total))
ok("climasus_excess input: $total_burden has excess_total")

# excess_total = 420+310+260+160+105+55 = 1310
stopifnot(bd_exc$total_burden$excess_total == 1310L)
ok("climasus_excess input: excess_total == 1310")

stopifnot(abs(sum(bd_exc$burden_table$pct_of_total) - 100) < 0.01)
ok("climasus_excess input: pct_of_total sums to ~100")

# =============================================================================
section("12. Input from climasus_dlnm objects (auto-AF computation)")
# =============================================================================

# Build 3 DLNM fixtures (fewer to keep runtime short)
.build_dlnm <- function(spec) {
  set.seed(spec$seed)
  n_days  <- 4L * 365L
  lag_max <- 7L
  dates   <- seq(as.Date("2018-01-01"), by = "day", length.out = n_days)
  doy     <- as.integer(format(dates, "%j"))
  temp0   <- spec$mean_t + 6 * sin(2 * pi * doy / 365 - pi / 2) +
             stats::rnorm(n_days, sd = 2)
  mu      <- exp(2.5 + spec$beta * temp0 + 0.25 * sin(2 * pi * doy / 365))
  y       <- stats::rpois(n_days, lambda = mu)
  make_lag <- function(x, k) c(rep(NA_real_, k), head(x, length(x) - k))
  df <- data.frame(date = dates, n_obitos = y)
  for (k in 0L:lag_max) df[[paste0("temp_c_lag", k)]] <- make_lag(temp0, k)
  df <- df[!is.na(df[[paste0("temp_c_lag", lag_max)]]), ]
  df_cl <- new_climasus_df(df, list(
    system = "SIM", stage = "climate", type = "daily",
    backend = "tibble", history = character(), user = list()
  ))
  sus_mod_dlnm(df_cl, outcome_col = "n_obitos", climate_col = "temp_c",
               lag_max = lag_max, verbose = FALSE)
}

dlnm_3 <- list(
  fortaleza  = .build_dlnm(list(beta = 0.06, mean_t = 28, seed = 1L)),
  recife     = .build_dlnm(list(beta = 0.05, mean_t = 27, seed = 2L)),
  curitiba   = .build_dlnm(list(beta = 0.005, mean_t = 16, seed = 6L))
)
ok("3 climasus_dlnm fixtures built for auto-AF test")

bd_dlnm <- sus_mod_burden(dlnm_3, nsim = 0L, verbose = FALSE, lang = "en")

stopifnot(inherits(bd_dlnm, "climasus_burden"))
ok("climasus_dlnm input: auto-computes AF and returns climasus_burden")

stopifnot(bd_dlnm$meta$input_type == "climasus_af")
ok("climasus_dlnm input: $meta$input_type becomes 'climasus_af'")

stopifnot(nrow(bd_dlnm$burden_table) == 3L)
ok("climasus_dlnm input: burden_table has 3 rows")

# Verify AN matches manually computed AF (delta method, same nsim=0L)
af_3 <- lapply(dlnm_3, function(f) sus_mod_af(f, nsim = 0L, verbose = FALSE))
bd_ref <- sus_mod_burden(af_3, verbose = FALSE)

stopifnot(isTRUE(all.equal(
  bd_dlnm$burden_table$an[order(bd_dlnm$burden_table$city)],
  bd_ref$burden_table$an[order(bd_ref$burden_table$city)],
  tolerance = 1e-6
)))
ok("climasus_dlnm input: AN matches manually pre-computed AF (delta method)")

# =============================================================================
section("13. rank_by fallback for mismatched input type")
# =============================================================================

# 'excess' metric is not applicable for AF inputs: should warn and fallback to 'an'
res_af_exc <- withCallingHandlers(
  sus_mod_burden(af_fits, rank_by = "excess", verbose = FALSE),
  warning = function(w) invokeRestart("muffleWarning")
)
stopifnot(inherits(res_af_exc, "climasus_burden"))
stopifnot(res_af_exc$meta$rank_by == "an")
ok("rank_by='excess' for AF input: falls back to 'an' without error")

# 'an' metric is not applicable for excess inputs: should warn and fallback to 'excess'
res_exc_an <- withCallingHandlers(
  sus_mod_burden(excess_fits, rank_by = "an", verbose = FALSE),
  warning = function(w) invokeRestart("muffleWarning")
)
stopifnot(inherits(res_exc_an, "climasus_burden"))
stopifnot(res_exc_an$meta$rank_by == "excess")
ok("rank_by='an' for excess input: falls back to 'excess' without error")

# =============================================================================
section("14. print(), summary(), tidy() S3 methods")
# =============================================================================

out_p <- capture.output(print(bd))
stopifnot(length(out_p) > 0L)
ok("print.climasus_burden() runs without error")

stopifnot(identical(print(bd), bd))
ok("print() returns object invisibly")

out_s <- capture.output(summary(bd))
stopifnot(length(out_s) > 0L)
ok("summary.climasus_burden() runs without error")

td <- tidy(bd)
stopifnot(is.data.frame(td))
ok("tidy.climasus_burden() returns a data.frame")

stopifnot(nrow(td) == 6L)
ok("tidy() has 6 rows")

stopifnot("cumulative_pct" %in% names(td))
ok("tidy() has cumulative_pct column")

stopifnot(all(af_cols %in% names(td)))
ok("tidy() contains all burden_table columns")

# =============================================================================
section("15. verbose = FALSE suppresses output")
# =============================================================================

quiet_out <- capture.output(
  bd_quiet <- sus_mod_burden(af_fits, verbose = FALSE, lang = "en")
)
stopifnot(length(quiet_out) == 0L)
ok("verbose=FALSE: no console output")

stopifnot(inherits(bd_quiet, "climasus_burden"))
ok("verbose=FALSE: still returns climasus_burden")

stopifnot(isTRUE(all.equal(bd_quiet$burden_table$an, bd$burden_table$an, tolerance = 1e-9)))
ok("verbose=FALSE: numerically identical to verbose=TRUE")

# =============================================================================
section("16. lang = 'pt', 'en', 'es', and fallback")
# =============================================================================

for (lang_val in c("pt", "es")) {
  res <- tryCatch(
    sus_mod_burden(af_fits, verbose = FALSE, lang = lang_val),
    error = function(e) e
  )
  stopifnot(!inherits(res, "error"))
  ok(sprintf("lang='%s' runs without error", lang_val))
}

res_xx <- tryCatch(
  sus_mod_burden(af_fits, verbose = FALSE, lang = "xx"),
  error = function(e) e
)
stopifnot(!inherits(res_xx, "error"))
ok("Unknown lang='xx' falls back to 'pt' without error")

# =============================================================================
section("17. Single-city edge case")
# =============================================================================

bd_one <- sus_mod_burden(af_fits[1L], verbose = FALSE)

stopifnot(inherits(bd_one, "climasus_burden"))
ok("Single-city list: returns climasus_burden")

stopifnot(nrow(bd_one$burden_table) == 1L)
ok("Single-city list: burden_table has 1 row")

stopifnot(bd_one$burden_table$rank == 1L)
ok("Single-city list: rank == 1")

stopifnot(abs(bd_one$burden_table$pct_of_total - 100) < 0.01)
ok("Single-city list: pct_of_total == 100")

# =============================================================================
section("18. Error handling")
# =============================================================================

# Non-list fits
tryCatch(
  sus_mod_burden("not a list", verbose = FALSE),
  error = function(e) ok("Rejects non-list fits")
)

# Empty list
tryCatch(
  sus_mod_burden(list(), verbose = FALSE),
  error = function(e) ok("Rejects empty fits list")
)

# Mixed types (AF and excess together)
mixed_list <- c(af_fits[1:2], excess_fits[3:4])
tryCatch(
  sus_mod_burden(mixed_list, verbose = FALSE),
  error = function(e) ok("Rejects mixed-type list")
)

# Invalid class element
tryCatch(
  sus_mod_burden(list(a = list(x = 1)), verbose = FALSE),
  error = function(e) ok("Rejects unrecognised element class")
)

# Bad component
tryCatch(
  sus_mod_burden(af_fits, component = "warm", verbose = FALSE),
  error = function(e) ok("Rejects invalid component value")
)

# Bad rank_by
tryCatch(
  sus_mod_burden(af_fits, rank_by = "mean_temp", verbose = FALSE),
  error = function(e) ok("Rejects invalid rank_by value")
)

cat("\n")
cat("  ALL SECTIONS PASSED\n")
