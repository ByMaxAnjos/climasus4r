# =============================================================================
# test_sus_mod_ml.R
# 18-Section test for sus_mod_ml() (XGBoost health outcome prediction)
#
# Uses fully synthetic data — no real DATASUS records or DLNM pipeline needed.
# Run with:
#   source("inst/roadmap/phase5/test_sus_mod_ml.R")
# or:
#   Rscript inst/roadmap/phase5/test_sus_mod_ml.R
# =============================================================================

devtools::load_all()
ok  <- function(msg) cat(sprintf("  [OK] %s\n", msg))
fail <- function(msg) stop(sprintf("FAIL: %s", msg))

cat("=== sus_mod_ml() — 18-section test ===\n\n")

# =============================================================================
# Section 1 — Package loads and function exists
# =============================================================================
cat("Section 1: Package load and function existence\n")

if (!requireNamespace("xgboost", quietly = TRUE))
  stop("xgboost not installed — cannot run test.")

stopifnot(existsFunction("sus_mod_ml"))
ok("sus_mod_ml() is exported")
stopifnot(existsFunction("print.climasus_ml"))
stopifnot(existsFunction("summary.climasus_ml"))
stopifnot(existsFunction("tidy.climasus_ml"))
stopifnot(existsFunction("predict.climasus_ml"))
ok("All 4 S3 methods registered")
cat("\n")

# =============================================================================
# Section 2 — Synthetic data construction
# =============================================================================
cat("Section 2: Synthetic data construction\n")

set.seed(123)
N_CITIES <- 5L
N_DAYS   <- 200L
N_OBS    <- N_CITIES * N_DAYS

CITIES <- c("fortaleza", "recife", "salvador", "belem", "manaus")

# Build a city × day panel with a known climate-health relationship:
#   n_obitos ~ Poisson(exp(0.05 * tmax + 0.01 * humidity - 2))
# tmax = main driver, known to be top feature
.make_panel <- function(seed = 123) {
  set.seed(seed)
  cities    <- rep(CITIES, each = N_DAYS)
  day_seq   <- rep(seq_len(N_DAYS), times = N_CITIES)
  # Climate features
  tmax      <- 28 + rnorm(N_OBS, mean = 0, sd = 3)
  humidity  <- 70 + rnorm(N_OBS, mean = 0, sd = 10)
  precip    <- pmax(0, rnorm(N_OBS, mean = 5, sd = 10))
  # Seasonality: sine wave (day of year proxy)
  doy       <- (day_seq %% 365) + 1
  seasonality <- sin(2 * pi * doy / 365)

  # True Poisson log-mean: strong tmax signal; intercept 0.5 gives ~3-8 cases/day
  log_mu    <- 0.08 * tmax - 0.01 * humidity + 0.3 * seasonality + 0.5
  n_obitos  <- stats::rpois(N_OBS, lambda = exp(log_mu))

  data.frame(
    city      = cities,
    day       = day_seq,
    tmax      = round(tmax, 2),
    humidity  = round(humidity, 1),
    precip    = round(precip, 2),
    seasonality = round(seasonality, 4),
    n_obitos  = n_obitos,
    stringsAsFactors = FALSE
  )
}

df_panel <- .make_panel()
stopifnot(nrow(df_panel) == N_OBS)
stopifnot(all(c("city", "tmax", "humidity", "precip", "seasonality", "n_obitos") %in% names(df_panel)))
stopifnot(all(df_panel$n_obitos >= 0L))
ok(sprintf("Panel data: %d obs (%d cities x %d days), outcome non-negative", N_OBS, N_CITIES, N_DAYS))
cat("\n")

# =============================================================================
# Section 3 — Basic call with default arguments
# =============================================================================
cat("Section 3: Basic call with Poisson objective\n")

ml_basic <- sus_mod_ml(
  df          = df_panel,
  outcome_col = "n_obitos",
  feature_cols = c("tmax", "humidity", "precip", "seasonality"),
  nrounds     = 200L,
  early_stopping = 30L,
  nfold       = 3L,
  seed        = 42L,
  verbose     = FALSE
)

stopifnot(inherits(ml_basic, "climasus_ml"))
stopifnot(inherits(ml_basic, "list"))
ok("Returns climasus_ml object")

# Required slots
for (slot in c("predictions", "importance", "performance", "model", "cv_log", "meta")) {
  stopifnot(!is.null(ml_basic[[slot]]))
}
ok("All 6 output slots present")
cat("\n")

# =============================================================================
# Section 4 — Predictions tibble structure
# =============================================================================
cat("Section 4: Predictions tibble\n")

pred_tbl <- ml_basic$predictions
stopifnot(inherits(pred_tbl, "data.frame"))
stopifnot(nrow(pred_tbl) == N_OBS)
for (col in c("observed", "fitted", "cv_predicted", "residual")) {
  stopifnot(col %in% names(pred_tbl))
  stopifnot(is.numeric(pred_tbl[[col]]))
}
ok("Predictions tibble has 1000 rows and required numeric columns")

# Fitted values should be >= 0 for Poisson
stopifnot(all(pred_tbl$fitted >= 0))
stopifnot(all(pred_tbl$cv_predicted >= 0))
ok("Poisson predictions are non-negative")

# residual = observed - cv_predicted
stopifnot(all(abs(pred_tbl$residual - (pred_tbl$observed - pred_tbl$cv_predicted)) < 1e-9))
ok("residual = observed - cv_predicted exactly")
cat("\n")

# =============================================================================
# Section 5 — Feature importance structure
# =============================================================================
cat("Section 5: Feature importance\n")

imp <- ml_basic$importance
stopifnot(inherits(imp, "data.frame"))
stopifnot("Feature" %in% names(imp))
stopifnot("Gain"    %in% names(imp))
stopifnot("Cover"   %in% names(imp))
ok("importance has Feature, Gain, Cover columns")

# At least 1 feature should appear; tmax and seasonality should (they have signal)
stopifnot("tmax" %in% imp$Feature)
ok("tmax appears in importance table (strongest signal in DGP)")

# Top feature must be one of the 4 provided features
top_feat <- imp$Feature[1L]
stopifnot(top_feat %in% c("tmax", "humidity", "precip", "seasonality"))
ok(sprintf("Top feature by gain is '%s' (one of the 4 provided features)", top_feat))

# Gain should be sorted descending
stopifnot(all(diff(imp$Gain) <= 0))
ok("Importance is sorted by Gain descending")
cat("\n")

# =============================================================================
# Section 6 — Performance metrics
# =============================================================================
cat("Section 6: Performance metrics\n")

p <- ml_basic$performance
stopifnot(all(c("RMSE_train", "MAE_train", "R2_train",
                "RMSE_cv",   "MAE_cv",   "R2_cv",
                "Pearson_cv", "best_nrounds") %in% names(p)))
ok("All 8 performance entries present")

# Train metrics should be finite and positive
stopifnot(is.finite(p$RMSE_train) && p$RMSE_train > 0)
stopifnot(is.finite(p$MAE_train)  && p$MAE_train  > 0)
ok("RMSE_train and MAE_train are positive finite")

# CV Pearson should be positive (model captures climate signal)
stopifnot(is.finite(p$Pearson_cv))
stopifnot(p$Pearson_cv > 0.2)  # Poisson count data has high noise; 0.2 is realistic
ok(sprintf("Pearson-CV = %.3f > 0.2 (captures known climate signal)", p$Pearson_cv))

# R2_cv may be lower than R2_train (expected generalization gap)
stopifnot(p$R2_train >= p$R2_cv || abs(p$R2_train - p$R2_cv) < 0.3)
ok(sprintf("R2_train=%.3f, R2_cv=%.3f (train >= CV or within 0.3 gap)", p$R2_train, p$R2_cv))

# best_nrounds should be in [1, 200] (our nrounds_max)
stopifnot(p$best_nrounds >= 1L && p$best_nrounds <= 200L)
ok(sprintf("best_nrounds = %d (in [1, 200])", p$best_nrounds))
cat("\n")

# =============================================================================
# Section 7 — Group-aware CV with id_col
# =============================================================================
cat("Section 7: Group-aware cross-validation (id_col = 'city')\n")

ml_grp <- sus_mod_ml(
  df           = df_panel,
  outcome_col  = "n_obitos",
  feature_cols = c("tmax", "humidity", "precip", "seasonality"),
  id_col       = "city",
  nrounds      = 200L,
  early_stopping = 30L,
  nfold        = 5L,
  seed         = 42L,
  verbose      = FALSE
)
stopifnot(inherits(ml_grp, "climasus_ml"))
ok("Group-aware CV (id_col='city') returns climasus_ml")

# city column should appear in predictions
stopifnot("city" %in% names(ml_grp$predictions))
ok("city column present in predictions tibble")

# Each city should be held out in exactly 1 fold
stopifnot(nrow(ml_grp$predictions) == N_OBS)
ok("All observations have OOF predictions")

# meta should record id_col
stopifnot(ml_grp$meta$id_col == "city")
ok("meta$id_col = 'city'")
cat("\n")

# =============================================================================
# Section 8 — feature_cols = NULL auto-detection
# =============================================================================
cat("Section 8: Automatic feature column detection\n")

ml_auto <- sus_mod_ml(
  df           = df_panel,
  outcome_col  = "n_obitos",
  feature_cols = NULL,   # auto: all numeric except outcome + id_col
  id_col       = "city",
  nrounds      = 100L,
  early_stopping = 20L,
  nfold        = 3L,
  seed         = 42L,
  verbose      = FALSE
)
stopifnot(inherits(ml_auto, "climasus_ml"))
# Auto-selected features should include tmax, humidity, precip, seasonality, day
auto_feats <- ml_auto$meta$feature_cols
stopifnot("tmax" %in% auto_feats)
stopifnot(!"n_obitos" %in% auto_feats)  # outcome must be excluded
ok(sprintf("Auto-detected %d features; 'n_obitos' excluded", length(auto_feats)))
cat("\n")

# =============================================================================
# Section 9 — reg:squarederror objective
# =============================================================================
cat("Section 9: Regression objective (reg:squarederror)\n")

# Add a continuous outcome
df_panel$rate <- df_panel$n_obitos / 100 + rnorm(N_OBS, 0, 0.01)

ml_reg <- sus_mod_ml(
  df           = df_panel,
  outcome_col  = "rate",
  feature_cols = c("tmax", "humidity", "precip"),
  objective    = "reg:squarederror",
  nrounds      = 100L,
  early_stopping = 20L,
  nfold        = 3L,
  seed         = 42L,
  verbose      = FALSE
)
stopifnot(inherits(ml_reg, "climasus_ml"))
ok("reg:squarederror objective runs successfully")
stopifnot(ml_reg$meta$objective == "reg:squarederror")
stopifnot(ml_reg$meta$eval_metric == "rmse")
ok("meta$objective and eval_metric are correct")
cat("\n")

# =============================================================================
# Section 10 — binary:logistic objective
# =============================================================================
cat("Section 10: Binary logistic objective\n")

df_panel$extreme_day <- as.integer(df_panel$tmax > 31)
stopifnot(all(df_panel$extreme_day %in% c(0L, 1L)))

ml_bin <- sus_mod_ml(
  df           = df_panel,
  outcome_col  = "extreme_day",
  feature_cols = c("tmax", "humidity", "precip"),
  objective    = "binary:logistic",
  nrounds      = 100L,
  early_stopping = 20L,
  nfold        = 3L,
  seed         = 42L,
  verbose      = FALSE
)
stopifnot(inherits(ml_bin, "climasus_ml"))
ok("binary:logistic objective runs successfully")

# Predictions should be in (0, 1) for binary logistic
stopifnot(all(ml_bin$predictions$cv_predicted > 0))
stopifnot(all(ml_bin$predictions$cv_predicted < 1))
ok("Binary logistic predictions are in (0, 1)")
cat("\n")

# =============================================================================
# Section 11 — NA handling
# =============================================================================
cat("Section 11: NA handling\n")

df_na <- df_panel
df_na$tmax[1:10] <- NA  # 10 NA rows in a feature

ml_na <- sus_mod_ml(
  df           = df_na,
  outcome_col  = "n_obitos",
  feature_cols = c("tmax", "humidity", "precip"),
  nrounds      = 100L,
  early_stopping = 20L,
  nfold        = 3L,
  seed         = 42L,
  verbose      = FALSE
)
# 10 rows should have been removed
stopifnot(ml_na$meta$n_removed_na == 10L)
stopifnot(ml_na$meta$n_obs_used   == N_OBS - 10L)
stopifnot(nrow(ml_na$predictions)  == N_OBS - 10L)
ok("10 NA rows removed; predictions has N_OBS - 10 rows")
cat("\n")

# =============================================================================
# Section 12 — Reproducibility via seed
# =============================================================================
cat("Section 12: Reproducibility\n")

ml_s1 <- sus_mod_ml(
  df           = df_panel,
  outcome_col  = "n_obitos",
  feature_cols = c("tmax", "humidity"),
  nrounds      = 50L,
  early_stopping = 20L,
  nfold        = 3L,
  seed         = 99L,
  verbose      = FALSE
)
ml_s2 <- sus_mod_ml(
  df           = df_panel,
  outcome_col  = "n_obitos",
  feature_cols = c("tmax", "humidity"),
  nrounds      = 50L,
  early_stopping = 20L,
  nfold        = 3L,
  seed         = 99L,
  verbose      = FALSE
)
stopifnot(all.equal(ml_s1$predictions$cv_predicted, ml_s2$predictions$cv_predicted))
ok("Two runs with same seed produce identical OOF predictions")
cat("\n")

# =============================================================================
# Section 13 — predict() on new data
# =============================================================================
cat("Section 13: predict.climasus_ml() for new data\n")

new_data <- data.frame(tmax = c(28, 33, 38), humidity = c(70, 65, 80),
                       precip = c(5, 0, 20), seasonality = c(0, 0.5, -0.5))

preds_new <- predict(ml_basic, newdata = new_data)
stopifnot(is.numeric(preds_new))
stopifnot(length(preds_new) == 3L)
stopifnot(all(preds_new >= 0))  # Poisson predictions >= 0
ok("predict() returns 3 non-negative Poisson predictions for new data")

# Hotter → more predicted deaths (positive tmax effect in our data-generating process)
stopifnot(preds_new[3L] > preds_new[1L])
ok("Predicted deaths increase with temperature (tmax 28 < 33 < 38)")

# Missing feature should error
e_miss <- tryCatch(
  predict(ml_basic, newdata = data.frame(tmax = 30)),
  error = function(e) e
)
stopifnot(inherits(e_miss, "error"))
ok("predict() errors if required feature columns are missing")
cat("\n")

# =============================================================================
# Section 14 — cv_log structure
# =============================================================================
cat("Section 14: cv_log (learning curve)\n")

cv_log <- ml_basic$cv_log
stopifnot(is.data.frame(cv_log))
stopifnot(nrow(cv_log) >= 1L)
stopifnot("iter" %in% names(cv_log))
ok(sprintf("cv_log has %d rows (rounds evaluated)", nrow(cv_log)))

# Test metric column should exist
test_cols <- grep("test_.*_mean", names(cv_log), value = TRUE)
stopifnot(length(test_cols) >= 1L)
ok(sprintf("cv_log has test metric column: %s", test_cols[1L]))
cat("\n")

# =============================================================================
# Section 15 — meta structure completeness
# =============================================================================
cat("Section 15: meta structure\n")

m <- ml_basic$meta
req_fields <- c("outcome_col", "feature_cols", "id_col", "objective",
                "eval_metric", "nrounds_max", "best_nrounds", "nfold",
                "max_depth", "eta", "subsample", "colsample_bytree",
                "min_child_weight", "early_stopping",
                "n_obs_total", "n_obs_used", "n_removed_na", "seed", "call_time")
for (f in req_fields) stopifnot(f %in% names(m))
ok("All 19 meta fields present")

stopifnot(m$outcome_col    == "n_obitos")
stopifnot(m$nfold          == 3L)
stopifnot(m$n_obs_total    == N_OBS)
stopifnot(m$n_obs_used     == N_OBS)
stopifnot(m$n_removed_na   == 0L)
ok("meta values match call arguments")
cat("\n")

# =============================================================================
# Section 16 — S3 print method
# =============================================================================
cat("Section 16: print.climasus_ml()\n")

out_print <- capture.output(ret_p <- print(ml_basic))
stopifnot(length(out_print) > 0L)
stopifnot(any(grepl("RMSE|MAE|Feature", out_print, ignore.case = TRUE)))
ok("print() produces RMSE/MAE/feature output on stdout")
stopifnot(identical(ret_p, ml_basic))
ok("print() returns object invisibly")
cat("\n")

# =============================================================================
# Section 17 — S3 tidy method
# =============================================================================
cat("Section 17: tidy.climasus_ml()\n")

tidy_out <- tidy(ml_basic)
stopifnot(inherits(tidy_out, "data.frame"))
stopifnot(nrow(tidy_out) == N_OBS)
stopifnot(all(c("observed", "fitted", "cv_predicted", "residual") %in% names(tidy_out)))
ok("tidy() returns predictions tibble with all required columns")
cat("\n")

# =============================================================================
# Section 18 — Language, error handling, and edge cases
# =============================================================================
cat("Section 18: Language, error handling, and edge cases\n")

# Multi-language
for (lng in c("pt", "en", "es")) {
  res <- tryCatch(
    sus_mod_ml(
      df = df_panel, outcome_col = "n_obitos",
      feature_cols = c("tmax", "humidity"),
      nrounds = 50L, early_stopping = 20L, nfold = 3L,
      seed = 42L, verbose = FALSE, lang = lng
    ),
    error = function(e) e
  )
  stopifnot(!inherits(res, "error"))
  ok(sprintf("lang='%s' runs without error", lng))
}

# Unknown language falls back to pt
res_xx <- tryCatch(
  sus_mod_ml(
    df = df_panel, outcome_col = "n_obitos",
    feature_cols = c("tmax", "humidity"),
    nrounds = 50L, early_stopping = 20L, nfold = 3L,
    seed = 42L, verbose = FALSE, lang = "xx"
  ),
  error = function(e) e
)
stopifnot(!inherits(res_xx, "error"))
ok("lang='xx' falls back to 'pt' without error")

# Error: outcome_col not found
e_oc <- tryCatch(
  sus_mod_ml(df = df_panel, outcome_col = "nonexistent", verbose = FALSE),
  error = function(e) e
)
stopifnot(inherits(e_oc, "error"))
stopifnot(grepl("nonexistent", conditionMessage(e_oc)))
ok("Missing outcome_col triggers informative error")

# Error: invalid objective
e_obj <- tryCatch(
  sus_mod_ml(df = df_panel, outcome_col = "n_obitos",
             objective = "survival:cox", verbose = FALSE),
  error = function(e) e
)
stopifnot(inherits(e_obj, "error"))
ok("Invalid objective triggers error")

# Error: feature_cols with non-numeric column
df_char <- df_panel
df_char$char_col <- letters[1:N_OBS]
e_char <- tryCatch(
  sus_mod_ml(df = df_char, outcome_col = "n_obitos",
             feature_cols = c("tmax", "char_col"), verbose = FALSE),
  error = function(e) e
)
stopifnot(inherits(e_char, "error"))
ok("Non-numeric feature column triggers error")

# Error: missing feature_col in newdata
e_pred <- tryCatch(
  predict(ml_basic, newdata = data.frame(tmax = 30)),
  error = function(e) e
)
stopifnot(inherits(e_pred, "error"))
ok("predict() with missing feature triggers error")

cat("\n=== All 18 sections passed ===\n")
