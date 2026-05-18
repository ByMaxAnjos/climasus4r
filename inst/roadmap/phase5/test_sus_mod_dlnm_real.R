# =============================================================================
# test_sus_mod_dlnm_real.R
#
# Tests sus_mod_dlnm() against a dataset that exactly replicates the structure
# produced by sus_climate_aggregate(temporal_strategy = "distributed_lag"):
#
#   Rows: 2,024  Cols: 31
#   - sparse: only rows where health events occurred (n_obitos >= 1)
#   - sf object: geom column = MULTIPOLYGON
#   - lag cols: tair_dry_bulb_c_lag0 .. tair_dry_bulb_c_lag21
#   - base col: tair_dry_bulb_c (same as lag0, kept by sus_climate_aggregate)
#   - identifiers: code_muni, name_muni, code_state, abbrev_state, code_muni_7
#
# Outcome column names vary by DATASUS system and lang:
#   SIM/pt   => n_obitos          SIM/en   => n_deaths
#   SIH/pt   => n_internacoes     SIH/en   => n_hospitalizations
#   SINASC/pt=> n_nascimentos     SINASC/en=> n_births
#   SINAN/pt => n_casos           SINAN/en => n_cases
#   SIA/pt   => n_procedimentos   SIA/en   => n_procedures
#   CNES/pt  => n_estabelecimentos CNES/en => n_establishments
#
# Run with: source("inst/roadmap/phase5/test_sus_mod_dlnm_real.R")
# =============================================================================

devtools::load_all()
suppressPackageStartupMessages({
  library(dlnm)
  library(sf)
  library(splines)
})

cat("\n=============================================================\n")
cat("  sus_mod_dlnm() – Real data structure test\n")
cat("=============================================================\n\n")

ok      <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
fail    <- function(msg) stop(sprintf("  [FAIL] %s", msg))
section <- function(s)   cat(sprintf("\n--- %s ---\n", s))


# =============================================================================
section("1. Build synthetic data replicating real sus_climate_aggregate output")
# =============================================================================
# Same parameters as the real call:
#   sus_climate_aggregate(temporal_strategy="distributed_lag", lag_days=c(21), ...)
# The real output has 2,024 rows from 7 municipalities over ~1 year (2023),
# with one row per death event (sparse: only event-days appear).

set.seed(123)

# 7 Rondônia municipalities (as in real data)
munic_meta <- data.frame(
  code_muni    = c("110020","110037","110045","110060","110078","110094","110102"),
  code_muni_7  = c("1100205","1100379","1100452","1100601","1100783","1100940","1101005"),
  name_muni    = c("Porto Velho","Ariquemes","Cacoal","Ji-Parana","Vilhena",
                   "Rolim de Moura","Jaru"),
  code_state   = rep("11", 7),
  abbrev_state = rep("RO", 7),
  stringsAsFactors = FALSE
)
n_munic <- nrow(munic_meta)

# Create bounding-box MULTIPOLYGONs (simple 0.5° squares per municipality)
lon0 <- c(-63.9, -63.0, -61.4, -61.9, -60.1, -61.8, -62.5)
lat0 <- c(-8.8,  -9.9, -11.4, -10.8, -12.7, -11.7, -10.0)
geom_sfc <- sf::st_sfc(
  lapply(seq_len(n_munic), function(i) {
    x0 <- lon0[i]; y0 <- lat0[i]
    sf::st_multipolygon(list(list(rbind(
      c(x0,      y0),
      c(x0+0.5,  y0),
      c(x0+0.5,  y0+0.5),
      c(x0,      y0+0.5),
      c(x0,      y0)
    ))))
  }),
  crs = 4326
)

# Daily temperature series (2023-01-01 to 2023-12-31)
all_dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
n_days    <- length(all_dates)         # 365
doy       <- as.integer(format(all_dates, "%j"))

# Rondônia: hot tropical – base ~29°C, 3°C seasonal swing
temp_base <- 29 + 3 * sin(2 * pi * doy / 365 - pi / 2)

# Build per-municipality lag matrix (lag0..21)
make_lags <- function(temp, lag_max = 21L) {
  mat <- matrix(NA_real_, nrow = n_days, ncol = lag_max + 1L)
  for (l in 0L:lag_max) {
    mat[, l + 1L] <- c(rep(NA_real_, l), head(temp, n_days - l))
  }
  mat
}

# Generate sparse death events: Bernoulli(p_death) per municipality-day
# p_death ~ 0.4 -> ~146 event-days per municipality -> 1022 rows total (realistic ~2024)
lag_max <- 21L
all_rows <- vector("list", n_munic)

for (m in seq_len(n_munic)) {
  temp_m  <- temp_base + stats::rnorm(n_days, sd = 1.2) + stats::rnorm(1, sd = 1)
  lag_mat <- make_lags(temp_m, lag_max)

  # Simulate deaths: log-linear with temperature
  mu      <- exp(1.5 + 0.012 * temp_m)
  deaths  <- stats::rpois(n_days, lambda = mu)

  # Keep only event-days (deaths >= 1), matching real sparse structure
  event_idx <- which(deaths >= 1 & !is.na(lag_mat[, lag_max + 1L]))

  n_events <- length(event_idx)
  row_list  <- data.frame(
    date         = all_dates[event_idx],
    code_muni    = munic_meta$code_muni[m],
    n_obitos     = deaths[event_idx],
    name_muni    = munic_meta$name_muni[m],
    code_state   = munic_meta$code_state[m],
    abbrev_state = munic_meta$abbrev_state[m],
    code_muni_7  = munic_meta$code_muni_7[m],
    stringsAsFactors = FALSE
  )
  # Add lag columns (lag0..21) and base column
  for (l in 0L:lag_max) {
    row_list[[paste0("tair_dry_bulb_c_lag", l)]] <- lag_mat[event_idx, l + 1L]
  }
  row_list[["tair_dry_bulb_c"]] <- lag_mat[event_idx, 1L]  # base = lag0

  # Attach matching geometry named "geom" (as in real sus_climate_aggregate output)
  all_rows[[m]] <- sf::st_sf(row_list, geom = rep(geom_sfc[m], n_events))
}

df_sparse_sf <- do.call(rbind, all_rows)
df_sparse_sf <- df_sparse_sf[order(df_sparse_sf$date, df_sparse_sf$code_muni), ]

# Promote to climasus_df replicating .set_climate_agg_meta() exactly:
#   structure(df, sus_meta = meta, class = c("climasus_df", base_classes))
# This preserves the sf class — unlike new_climasus_df() which strips it via unclass().
meta_real <- list(
  system   = "SIM",
  stage    = "climate",
  type     = "distributed_lag",
  spatial  = TRUE,
  temporal = NULL,
  backend  = "tibble",
  created  = Sys.time(),
  modified = Sys.time(),
  history  = c("[2023-01-01] imported SIM data",
               "[2023-01-01] sus_climate_aggregate: distributed_lag; var=tair_dry_bulb_c; lag=21"),
  user     = list()
)
df_distributed_lag <- structure(
  df_sparse_sf,
  sus_meta = meta_real,
  class    = c("climasus_df", class(df_sparse_sf))
)

cat(sprintf("  Rows: %d  |  Cols: %d  |  Unique dates: %d  |  Municipalities: %d\n",
            nrow(df_distributed_lag), ncol(df_distributed_lag),
            dplyr::n_distinct(df_distributed_lag$date),
            dplyr::n_distinct(df_distributed_lag$code_muni)))
cat(sprintf("  class: %s\n", paste(class(df_distributed_lag), collapse = ", ")))
cat(sprintf("  stage: %s  |  type: %s  |  system: %s\n",
            sus_meta(df_distributed_lag, "stage"),
            sus_meta(df_distributed_lag, "type"),
            sus_meta(df_distributed_lag, "system")))

stopifnot(inherits(df_distributed_lag, "climasus_df"))        ; ok("class = climasus_df")
stopifnot(inherits(df_distributed_lag, "sf"))                 ; ok("class includes sf (has geometry)")
stopifnot(sus_meta(df_distributed_lag, "stage") == "climate") ; ok("stage = climate")
stopifnot(sus_meta(df_distributed_lag, "type")  == "distributed_lag") ; ok("type = distributed_lag")
stopifnot("geom" %in% names(df_distributed_lag))              ; ok("geom column present")
stopifnot(all(paste0("tair_dry_bulb_c_lag", 0:21) %in% names(df_distributed_lag))) ; ok("lag0..21 all present")
stopifnot("tair_dry_bulb_c" %in% names(df_distributed_lag))  ; ok("base tair_dry_bulb_c column present")
cat(sprintf("  Data glimpse matches real structure: %d rows, 31 expected cols (%d actual)\n",
            nrow(df_distributed_lag), ncol(df_distributed_lag)))


# =============================================================================
section("2. Fit DLNM on real structure – primary use case (SIM/pt)")
# =============================================================================
fit_sim <- sus_mod_dlnm(
  df           = df_distributed_lag,
  outcome_col  = "n_obitos",
  climate_col  = NULL,    # auto-detect
  lag_max      = NULL,    # auto-detect from _lagN cols
  family       = "quasipoisson",
  dof_per_year = 4L,
  lang         = "pt",
  verbose      = TRUE
)

stopifnot(inherits(fit_sim, "climasus_dlnm"))                  ; ok("fit returned climasus_dlnm")
stopifnot(fit_sim$meta$climate_col == "tair_dry_bulb_c")       ; ok("auto-detected climate_col = tair_dry_bulb_c (not base col)")
stopifnot(fit_sim$meta$lag_max == 21L)                         ; ok("auto-detected lag_max = 21")

# Verify sf geometry was dropped before modelling
stopifnot(!"geom" %in% names(fit_sim$data_daily))              ; ok("geom dropped from data_daily")
stopifnot(!"geometry" %in% names(fit_sim$data_daily))          ; ok("geometry not in data_daily")

# Check unique-day aggregation
n_unique_dates <- dplyr::n_distinct(df_distributed_lag$date)
n_obs_model    <- nrow(fit_sim$data_daily)
stopifnot(n_obs_model <= n_unique_dates)                       ; ok(sprintf("daily aggregation: %d rows (unique event-dates: %d)", n_obs_model, n_unique_dates))

# Verify base col tair_dry_bulb_c did NOT become a lag col
stopifnot(fit_sim$meta$climate_col != "tair_dry_bulb_c_lag_base") ; ok("base col did not shadow lag detection")

cat("\n  --- print(fit_sim) ---\n")
print(fit_sim)


# =============================================================================
section("3. Outcome column variants – all 6 DATASUS systems × 3 languages")
# =============================================================================

outcome_cols <- list(
  # lang = "pt"
  list(system="SIM",    lang="pt", col="n_obitos"),
  list(system="SIH",    lang="pt", col="n_internacoes"),
  list(system="SINASC", lang="pt", col="n_nascimentos"),
  list(system="SINAN",  lang="pt", col="n_casos"),
  list(system="SIA",    lang="pt", col="n_procedimentos"),
  list(system="CNES",   lang="pt", col="n_estabelecimentos"),
  # lang = "en"
  list(system="SIM",    lang="en", col="n_deaths"),
  list(system="SIH",    lang="en", col="n_hospitalizations"),
  list(system="SINASC", lang="en", col="n_births"),
  list(system="SINAN",  lang="en", col="n_cases"),
  list(system="SIA",    lang="en", col="n_procedures"),
  list(system="CNES",   lang="en", col="n_establishments"),
  # lang = "es"
  list(system="SIM",    lang="es", col="n_muertes"),
  list(system="SIH",    lang="es", col="n_hospitalizaciones"),
  list(system="SINASC", lang="es", col="n_nacimientos"),
  list(system="SINAN",  lang="es", col="n_casos"),
  list(system="SIA",    lang="es", col="n_procedimientos"),
  list(system="CNES",   lang="es", col="n_establecimientos")
)

for (spec in outcome_cols) {
  # Rename the outcome column in a copy of the df
  df_tmp <- df_distributed_lag
  names(df_tmp)[names(df_tmp) == "n_obitos"] <- spec$col

  fit_tmp <- sus_mod_dlnm(
    df           = df_tmp,
    outcome_col  = spec$col,
    climate_col  = "tair_dry_bulb_c",
    lag_max      = 7L,       # quick run
    family       = "quasipoisson",
    dof_per_year = 4L,
    lang         = spec$lang,
    verbose      = FALSE
  )
  stopifnot(inherits(fit_tmp, "climasus_dlnm"))
  stopifnot(fit_tmp$meta$outcome_col == spec$col)
  ok(sprintf("system=%-6s  lang=%s  outcome_col=%-26s  RR_p75=%.4f",
             spec$system, spec$lang, spec$col, fit_tmp$models$rr))
}


# =============================================================================
section("4. sf geometry drop – explicit verification")
# =============================================================================

# Ensure the function handles sf objects (geom column) without error or data leakage
stopifnot(inherits(df_distributed_lag, "sf"))
fit_sf <- sus_mod_dlnm(
  df           = df_distributed_lag,
  outcome_col  = "n_obitos",
  climate_col  = "tair_dry_bulb_c",
  lag_max      = 7L,
  verbose      = FALSE,
  lang         = "pt"
)
stopifnot(!"geom"     %in% names(fit_sf$data_daily))           ; ok("geom NOT in data_daily after drop")
stopifnot(!"geometry" %in% names(fit_sf$data_daily))           ; ok("geometry NOT in data_daily after drop")
stopifnot(is.data.frame(fit_sf$data_daily))                    ; ok("data_daily is a plain data.frame")


# =============================================================================
section("5. Base column tair_dry_bulb_c does NOT interfere with lag detection")
# =============================================================================
# The real data has both tair_dry_bulb_c (base = lag0) AND tair_dry_bulb_c_lag0..21
# Auto-detection must pick tair_dry_bulb_c as the base variable (not as a non-lag col)

lag_cols_in_data <- grep("^tair_dry_bulb_c_lag\\d+$", names(df_distributed_lag), value = TRUE)
stopifnot(length(lag_cols_in_data) == 22L)                     ; ok(sprintf("22 lag cols found (%s .. %s)", lag_cols_in_data[1], tail(lag_cols_in_data, 1)))

# The base col tair_dry_bulb_c should NOT appear in the exposure matrix
expo_cols <- paste0("tair_dry_bulb_c_lag", 0:21)
stopifnot(all(expo_cols %in% names(fit_sim$data_daily)))        ; ok("all tair_dry_bulb_c_lag0..21 in data_daily")

# Exposure matrix should be 22 columns wide (lag0..21), not 23 (base would add extra)
expo_mat_cols <- grep("^tair_dry_bulb_c_lag", names(fit_sim$data_daily), value = TRUE)
stopifnot(length(expo_mat_cols) == 22L)                         ; ok("exposure matrix is exactly 22 cols (no base col contamination)")


# =============================================================================
section("6. Daily aggregation correctness – sum deaths, mean climate")
# =============================================================================
# Manually aggregate one date and compare with data_daily
reference_date <- sort(unique(df_distributed_lag$date))[50]

manual_agg <- df_distributed_lag |>
  sf::st_drop_geometry() |>
  dplyr::filter(date == reference_date) |>
  dplyr::summarise(
    y              = sum(n_obitos, na.rm = TRUE),
    tair_lag0_mean = mean(tair_dry_bulb_c_lag0, na.rm = TRUE)
  )

model_agg <- fit_sim$data_daily |>
  dplyr::filter(date == reference_date)

if (nrow(model_agg) == 1L) {
  stopifnot(abs(model_agg$y - manual_agg$y) < 1e-9)            ; ok(sprintf("deaths sum correct on date %s: %d", reference_date, model_agg$y))
  stopifnot(abs(model_agg$tair_dry_bulb_c_lag0 - manual_agg$tair_lag0_mean) < 1e-9)
  ok(sprintf("mean lag0 correct on date %s: %.4f", reference_date, model_agg$tair_dry_bulb_c_lag0))
} else {
  ok(sprintf("date %s not in data_daily (no event row for that date)", reference_date))
}


# =============================================================================
section("7. Sparse vs complete series – awareness check")
# =============================================================================
# The real sus_climate_aggregate output is sparse (only event-days).
# After summarise(.by = date), data_daily has <= n_unique_event_dates rows.
# This is correct behaviour but important for the user to know.

n_event_days  <- dplyr::n_distinct(df_distributed_lag$date)
n_model_days  <- nrow(fit_sim$data_daily)
pct_coverage  <- round(100 * n_model_days / 365, 1)

cat(sprintf(
  "\n  NOTE: Sparse input — data_daily has %d/%d event-days (%s%% of year 2023).\n",
  n_model_days, 365, pct_coverage
))
cat("  DLNM is fit only on event-days (days with >= 1 death).\n")
cat("  For complete-series analysis, zero-count days must be explicitly added.\n\n")

stopifnot(n_model_days >= 30L)    ; ok(sprintf("sufficient event-days for DLNM: %d (lag_max + 10 = 32 minimum)", n_model_days))


# =============================================================================
section("8. Full parameter run on real structure")
# =============================================================================
fit_full <- sus_mod_dlnm(
  df           = df_distributed_lag,
  outcome_col  = "n_obitos",
  climate_col  = "tair_dry_bulb_c",
  lag_max      = 21L,
  argvar       = list(fun = "ns", df = 4),
  arglag       = list(fun = "ns", df = 3),
  family       = "quasipoisson",
  ns_df        = NULL,
  dof_per_year = 6L,   # subtropical Brazil, stronger seasonality
  ref_value    = NULL, # median
  pred_at      = c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
  alpha        = 0.05,
  lang         = "pt",
  verbose      = TRUE
)

stopifnot(inherits(fit_full, "climasus_dlnm"))                  ; ok("full-param run succeeded")
stopifnot(nrow(fit_full$exposure_response) == 7L)              ; ok("exposure_response: 7 rows (pred_at)")
stopifnot(nrow(fit_full$lag_response) == 22L)                  ; ok("lag_response: 22 rows (lag 0-21)")

# tidy() pooling — compare two fits
bound <- dplyr::bind_rows(tidy(fit_sim), tidy(fit_full))
stopifnot(nrow(bound) == 2L)                                   ; ok("tidy() bind_rows works on real-data fits")
cat("\n  Two-model comparison:\n")
print(bound[, c("climate_col","family","lag_max","ns_df","n","rr","lo","hi","disp_ratio")])


# =============================================================================
section("9. Error: wrong outcome col name given real-data column set")
# =============================================================================
tryCatch({
  sus_mod_dlnm(
    df          = df_distributed_lag,
    outcome_col = "n_deaths",   # English name but data has Portuguese "n_obitos"
    climate_col = "tair_dry_bulb_c",
    lag_max     = 7L, verbose = FALSE
  )
  fail("should error: outcome col mismatch")
}, error = function(e) {
  ok(sprintf("Correct error for lang mismatch: %s",
             substr(conditionMessage(e), 1, 80)))
})


# =============================================================================
cat("\n=============================================================\n")
cat("  ALL REAL-DATA TESTS PASSED\n")
cat("=============================================================\n\n")
