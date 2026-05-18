# =============================================================================
# test_sus_mod_plot_ml.R
# 18-section test for sus_mod_plot_ml()
# Uses a hand-crafted climasus_ml mock object -- no XGBoost pipeline needed.
# Run with: devtools::load_all(); source("inst/roadmap/phase5/test_sus_mod_plot_ml.R")
# =============================================================================

devtools::load_all(quiet = TRUE)
library(tibble)

cat("=== test_sus_mod_plot_ml.R ===\n")
PASS <- 0L; FAIL <- 0L

ok <- function(cond, msg) {
  if (isTRUE(cond)) { cat("  PASS:", msg, "\n"); PASS <<- PASS + 1L }
  else               { cat("  FAIL:", msg, "\n"); FAIL <<- FAIL + 1L }
}

# -----------------------------------------------------------------------------
# Section 1 -- Build mock climasus_ml
# -----------------------------------------------------------------------------
cat("\n--- Section 1: mock climasus_ml ---\n")

set.seed(42L)
n_obs <- 80L

mock_predictions <- tibble::tibble(
  observed     = round(rpois(n_obs, lambda = 5), 0),
  fitted       = round(rpois(n_obs, lambda = 5) + rnorm(n_obs, 0, 0.3), 2),
  cv_predicted = round(rpois(n_obs, lambda = 5) + rnorm(n_obs, 0, 0.5), 2),
  residual     = round(observed - cv_predicted, 2)
)

mock_importance <- tibble::tibble(
  Feature   = c("tmax", "pop_density", "rh", "prec", "tmin", "wind"),
  Gain      = c(0.52,   0.21,          0.11,  0.07,   0.05,   0.04),
  Cover     = c(0.40,   0.25,          0.15,  0.10,   0.06,   0.04),
  Frequency = c(0.38,   0.28,          0.14,  0.10,   0.06,   0.04)
)

n_rounds <- 50L
mock_cv_log <- data.frame(
  iter                    = seq_len(n_rounds),
  train_rmse_mean         = seq(5.0, 1.5, length.out = n_rounds),
  train_rmse_std          = runif(n_rounds, 0.05, 0.20),
  test_rmse_mean          = c(seq(5.5, 2.2, length.out = 40L), seq(2.2, 2.5, length.out = 10L)),
  test_rmse_std           = runif(n_rounds, 0.10, 0.30)
)

mock_ml <- structure(
  list(
    predictions = mock_predictions,
    importance  = mock_importance,
    performance = list(
      RMSE_train   = 0.85,
      MAE_train    = 0.62,
      R2_train     = 0.91,
      RMSE_cv      = 1.42,
      MAE_cv       = 1.05,
      R2_cv        = 0.74,
      Pearson_cv   = 0.86,
      best_nrounds = 40L
    ),
    model  = NULL,
    cv_log = mock_cv_log,
    meta   = list(
      outcome_col      = "n_obitos",
      feature_cols     = c("tmax", "pop_density", "rh", "prec", "tmin", "wind"),
      id_col           = NULL,
      objective        = "count:poisson",
      eval_metric      = "rmse",
      nrounds_max      = 500L,
      best_nrounds     = 40L,
      nfold            = 5L,
      max_depth        = 4L,
      eta              = 0.05,
      subsample        = 0.8,
      colsample_bytree = 0.8,
      min_child_weight = 10L,
      early_stopping   = 30L,
      n_obs_total      = n_obs,
      n_obs_used       = n_obs,
      n_removed_na     = 0L,
      seed             = 42L,
      call_time        = Sys.time()
    )
  ),
  class = c("climasus_ml", "list")
)

ok(inherits(mock_ml, "climasus_ml"),          "mock is climasus_ml")
ok(nrow(mock_ml$predictions) == n_obs,        "predictions has correct nrow")
ok(nrow(mock_ml$importance)  == 6L,           "importance has 6 features")
ok(nrow(mock_ml$cv_log)      == n_rounds,     "cv_log has correct nrow")


# -----------------------------------------------------------------------------
# Section 2 -- importance plot (default)
# -----------------------------------------------------------------------------
cat("\n--- Section 2: importance plot ---\n")

p_imp <- sus_mod_plot_ml(mock_ml, type = "importance", lang = "pt")
ok(inherits(p_imp, "ggplot"), "importance returns ggplot object")


# -----------------------------------------------------------------------------
# Section 3 -- importance layers
# -----------------------------------------------------------------------------
cat("\n--- Section 3: importance layers ---\n")

layer_cls_imp <- vapply(p_imp$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomCol"  %in% layer_cls_imp, "importance has bar (col) layer")
ok("GeomText" %in% layer_cls_imp, "importance has text (value label) layer")


# -----------------------------------------------------------------------------
# Section 4 -- importance feature ordering (highest Gain at top)
# -----------------------------------------------------------------------------
cat("\n--- Section 4: feature ordering ---\n")

imp_data <- p_imp$data
ok("Feature" %in% names(imp_data), "importance data has 'Feature' column")
if ("Feature" %in% names(imp_data) && is.factor(imp_data$Feature)) {
  lvls <- levels(imp_data$Feature)
  ok(lvls[length(lvls)] == "tmax", "highest-Gain feature is last factor level (topmost)")
} else {
  ok(FALSE, "Feature is an ordered factor")
}


# -----------------------------------------------------------------------------
# Section 5 -- n_top parameter
# -----------------------------------------------------------------------------
cat("\n--- Section 5: n_top parameter ---\n")

p_top3 <- sus_mod_plot_ml(mock_ml, type = "importance", n_top = 3L, lang = "pt")
ok(inherits(p_top3, "ggplot"), "n_top=3 returns ggplot")
ok(nrow(p_top3$data) == 3L, "n_top=3 limits plot data to 3 rows")

tbl_top3 <- sus_mod_plot_ml(mock_ml, type = "importance", n_top = 3L, output_type = "table")
ok(nrow(tbl_top3) == 3L, "n_top=3 table has 3 rows")


# -----------------------------------------------------------------------------
# Section 6 -- fit plot
# -----------------------------------------------------------------------------
cat("\n--- Section 6: fit plot ---\n")

p_fit <- sus_mod_plot_ml(mock_ml, type = "fit", lang = "en")
ok(inherits(p_fit, "ggplot"), "fit returns ggplot object")


# -----------------------------------------------------------------------------
# Section 7 -- fit layers
# -----------------------------------------------------------------------------
cat("\n--- Section 7: fit layers ---\n")

layer_cls_fit <- vapply(p_fit$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomAbline" %in% layer_cls_fit, "fit has reference line (abline) layer")
ok("GeomPoint"  %in% layer_cls_fit, "fit has point layer")
ok("GeomText"   %in% layer_cls_fit, "fit has R2/RMSE annotation text layer")


# -----------------------------------------------------------------------------
# Section 8 -- cv_log plot
# -----------------------------------------------------------------------------
cat("\n--- Section 8: cv_log plot ---\n")

p_cv <- sus_mod_plot_ml(mock_ml, type = "cv_log", lang = "es")
ok(inherits(p_cv, "ggplot"), "cv_log returns ggplot object")


# -----------------------------------------------------------------------------
# Section 9 -- cv_log layers
# -----------------------------------------------------------------------------
cat("\n--- Section 9: cv_log layers ---\n")

layer_cls_cv <- vapply(p_cv$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomLine"  %in% layer_cls_cv, "cv_log has line layer")
ok("GeomVline" %in% layer_cls_cv, "cv_log has best-round vline")
ok("GeomText"  %in% layer_cls_cv, "cv_log has best-round label text")


# -----------------------------------------------------------------------------
# Section 10 -- cv_log has two split levels (train and test)
# -----------------------------------------------------------------------------
cat("\n--- Section 10: cv_log split types ---\n")

cv_data <- p_cv$data
ok("split_type" %in% names(cv_data), "cv_log data has 'split_type' column")
if ("split_type" %in% names(cv_data)) {
  ok(nlevels(cv_data$split_type) == 2L, "cv_log has 2 split_type levels (train + test)")
}


# -----------------------------------------------------------------------------
# Section 11 -- output_type = "table"
# -----------------------------------------------------------------------------
cat("\n--- Section 11: output_type = 'table' ---\n")

tbl_imp <- sus_mod_plot_ml(mock_ml, type = "importance", output_type = "table")
ok(is.data.frame(tbl_imp),        "importance table is data.frame")
ok("Gain" %in% names(tbl_imp),    "importance table has 'Gain' column")

tbl_fit <- sus_mod_plot_ml(mock_ml, type = "fit",        output_type = "table")
ok("observed" %in% names(tbl_fit), "fit table has 'observed' column")
ok("cv_predicted" %in% names(tbl_fit), "fit table has 'cv_predicted' column")

tbl_cv <- sus_mod_plot_ml(mock_ml,  type = "cv_log",     output_type = "table")
ok("iter" %in% names(tbl_cv),     "cv_log table has 'iter' column")


# -----------------------------------------------------------------------------
# Section 12 -- output_type = "all"
# -----------------------------------------------------------------------------
cat("\n--- Section 12: output_type = 'all' ---\n")

all_out <- sus_mod_plot_ml(mock_ml, type = "importance", output_type = "all")
ok(is.list(all_out),                  "'all' returns list")
ok(inherits(all_out$plot, "ggplot"),  "'all' $plot is ggplot")
ok(is.data.frame(all_out$table),      "'all' $table is data.frame")


# -----------------------------------------------------------------------------
# Section 13 -- interactive = TRUE
# -----------------------------------------------------------------------------
cat("\n--- Section 13: interactive = TRUE ---\n")

skip_plotly <- !requireNamespace("plotly", quietly = TRUE)
if (skip_plotly) {
  cat("  SKIP: plotly not installed\n")
} else {
  p_int <- sus_mod_plot_ml(mock_ml, type = "importance", interactive = TRUE)
  ok(inherits(p_int, "plotly"), "interactive importance is plotly object")
}


# -----------------------------------------------------------------------------
# Section 14 -- lang = "en"
# -----------------------------------------------------------------------------
cat("\n--- Section 14: lang = 'en' ---\n")

p_en <- sus_mod_plot_ml(mock_ml, type = "importance", lang = "en")
ok(inherits(p_en, "ggplot"), "lang='en' produces ggplot")
ok(grepl("Importance|Gain|Feature", p_en$labels$title %||% ""), "title contains English text")


# -----------------------------------------------------------------------------
# Section 15 -- lang = "es"
# -----------------------------------------------------------------------------
cat("\n--- Section 15: lang = 'es' ---\n")

p_es <- sus_mod_plot_ml(mock_ml, type = "fit", lang = "es")
ok(inherits(p_es, "ggplot"), "lang='es' produces ggplot")


# -----------------------------------------------------------------------------
# Section 16 -- base_size and verbose
# -----------------------------------------------------------------------------
cat("\n--- Section 16: base_size + verbose ---\n")

p_bs <- sus_mod_plot_ml(mock_ml, type = "importance", base_size = 14L)
ok(p_bs$theme$text$size == 14L, "theme base_size is 14")

p_v <- sus_mod_plot_ml(mock_ml, type = "fit", verbose = TRUE)
ok(inherits(p_v, "ggplot"), "verbose=TRUE returns ggplot")


# -----------------------------------------------------------------------------
# Section 17 -- save_plot to tempfile
# -----------------------------------------------------------------------------
cat("\n--- Section 17: save_plot ---\n")

tmp <- tempfile(fileext = ".png")
sus_mod_plot_ml(mock_ml, type = "importance", save_plot = tmp)
ok(file.exists(tmp), "PNG saved to tempfile")
unlink(tmp)


# -----------------------------------------------------------------------------
# Section 18 -- error on wrong class + unsupported lang fallback
# -----------------------------------------------------------------------------
cat("\n--- Section 18: error + lang fallback ---\n")

err_cls <- tryCatch(sus_mod_plot_ml(list(a = 1)), error = function(e) e)
ok(inherits(err_cls, "error"), "error on non-climasus_ml input")

p_bad <- suppressWarnings(
  sus_mod_plot_ml(mock_ml, type = "importance", lang = "fr")
)
ok(inherits(p_bad, "ggplot"), "unsupported lang falls back to 'pt'")


# -----------------------------------------------------------------------------
cat(sprintf("\n=== RESULT: %d PASS / %d FAIL ===\n", PASS, FAIL))
if (FAIL > 0L) stop(sprintf("%d test(s) failed in test_sus_mod_plot_ml.R", FAIL))
