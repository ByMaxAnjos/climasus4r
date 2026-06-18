# =============================================================================
# sus_mod_ml.R
# XGBoost Machine Learning for Climate-Health Outcome Prediction
#
# Theory:
#   Chen & Guestrin (2016, KDD) - XGBoost: A Scalable Tree Boosting System
#   Hastie et al. (2009) - Elements of Statistical Learning, ch. 10 (boosting)
# Input : Aggregated city x time data.frame (from climasus4r pipeline or plain df)
# Output: climasus_ml with predictions, feature importance, CV metrics, model
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "Feature", "Gain", "Cover", "Frequency",
  "observed", "fitted", "cv_predicted", "residual", "fold"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.ml_labels <- list(
  title = list(
    pt = "climasus4r \u2014 Previsao de Desfechos em Saude (XGBoost)",
    en = "climasus4r \u2014 Health Outcome Prediction (XGBoost)",
    es = "climasus4r \u2014 Prediccion de Resultados en Salud (XGBoost)"
  ),
  step_prepare = list(
    pt = "Preparando dados: {n_obs} obs, {n_feat} variavel(is), objetivo = {objective}...",
    en = "Preparing data: {n_obs} obs, {n_feat} variable(s), objective = {objective}...",
    es = "Preparando datos: {n_obs} obs, {n_feat} variable(s), objetivo = {objective}..."
  ),
  step_cv = list(
    pt = "Validacao cruzada ({nfold} folds) para otimizar nrounds...",
    en = "Cross-validation ({nfold} folds) to select optimal nrounds...",
    es = "Validacion cruzada ({nfold} pliegues) para optimizar nrounds..."
  ),
  step_train = list(
    pt = "Treinando modelo final (nrounds = {best_nrounds}, eta = {eta})...",
    en = "Training final model (nrounds = {best_nrounds}, eta = {eta})...",
    es = "Entrenando modelo final (nrounds = {best_nrounds}, eta = {eta})..."
  ),
  step_importance = list(
    pt = "Calculando importancia das variaveis...",
    en = "Computing feature importance...",
    es = "Calculando importancia de variables..."
  ),
  done = list(
    pt = "Concluido. RMSE-CV = {rmse_cv} | MAE-CV = {mae_cv} | R2-CV = {r2_cv} | Top feature: {top_feat}",
    en = "Done. RMSE-CV = {rmse_cv} | MAE-CV = {mae_cv} | R\u00b2-CV = {r2_cv} | Top feature: {top_feat}",
    es = "Listo. RMSE-CV = {rmse_cv} | MAE-CV = {mae_cv} | R2-CV = {r2_cv} | Principal variable: {top_feat}"
  ),
  warn_na = list(
    pt = "{n_na} observacao(oes) com NA removida(s) ({pct}% do total).",
    en = "{n_na} observation(s) with NA removed ({pct}% of total).",
    es = "{n_na} observacion(es) con NA eliminadas ({pct}% del total)."
  ),
  warn_few_obs = list(
    pt = "Apenas {n_obs} observacoes. Resultados de CV podem ser inst\u00e1veis.",
    en = "Only {n_obs} observations. CV results may be unstable.",
    es = "Solo {n_obs} observaciones. Resultados de CV pueden ser inestables."
  )
)

#' @keywords internal
#' @noRd
.mrl_ml <- function(key, lang, ...) {
  entry <- .ml_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' XGBoost Machine Learning for Climate-Health Outcome Prediction
#'
#' Trains an XGBoost gradient-boosted tree model to predict health outcomes
#' (disease counts, mortality, hospitalizations) from climate and socioeconomic
#' features. Accepts any aggregated data frame produced by the climasus4r
#' pipeline (or a plain `data.frame`). Uses k-fold cross-validation to select
#' the optimal number of trees (`nrounds`) and returns out-of-fold (OOF)
#' predictions alongside the final model trained on full data.
#'
#' This function complements the DLNM epidemiological pipeline
#' ([sus_mod_dlnm()]): DLNM models the exposure-response relationship and
#' provides causal inference; `sus_mod_ml()` focuses on predictive accuracy
#' across a wider feature space and can incorporate lagged climate variables,
#' socioeconomic covariates, and spatial predictors simultaneously.
#'
#' @section XGBoost objective:
#'
#' The `objective` controls the loss function and the scale of predictions:
#' \describe{
#'   \item{`"count:poisson"`}{Poisson log-linear model for non-negative integer
#'     counts (deaths, hospitalizations). Predictions are in count scale
#'     (predicted lambda). **Default** — appropriate for most DATASUS outcomes.}
#'   \item{`"reg:squarederror"`}{Squared-error regression for continuous
#'     outcomes (rates, indices). Predictions are in the same scale as the
#'     outcome.}
#'   \item{`"binary:logistic"`}{Binary outcome (0/1). Predictions are
#'     probabilities ∈ (0, 1).}
#' }
#'
#' @param df A data frame (plain or `climasus_df`) with one row per
#'   observation (e.g., city × day or city × month). Must contain
#'   `outcome_col` and at least one numeric feature column.
#' @param outcome_col Character. Name of the column to predict (the target
#'   variable). Must be numeric.
#' @param feature_cols Character vector or `NULL`. Names of columns to use as
#'   predictors. When `NULL` (default), all numeric columns except
#'   `outcome_col` and `id_col` are used automatically.
#' @param id_col Character or `NULL`. City or group identifier column used for
#'   **group-aware cross-validation**: each group is kept entirely in one fold,
#'   preventing data leakage across cities. When `NULL`, standard random
#'   k-fold is used. Strongly recommended when data has a city dimension.
#' @param objective Character. XGBoost objective function. One of
#'   `"count:poisson"` (default), `"reg:squarederror"`, or
#'   `"binary:logistic"`. See Details.
#' @param nrounds Integer. Maximum number of boosting rounds. The optimal
#'   value is selected via early stopping during cross-validation. Default
#'   `500L`.
#' @param max_depth Integer. Maximum tree depth. Default `6L`. Increase for
#'   more complex patterns; decrease to reduce overfitting.
#' @param eta Numeric. Learning rate (step size shrinkage). Default `0.05`.
#'   Smaller values require more rounds but often generalize better.
#' @param subsample Numeric ∈ (0, 1\]. Row subsampling ratio per tree.
#'   Default `0.8`.
#' @param colsample_bytree Numeric ∈ (0, 1\]. Column subsampling ratio per
#'   tree. Default `0.8`.
#' @param min_child_weight Numeric. Minimum sum of instance weight in a leaf.
#'   Default `1`. Increase to prevent learning rare patterns.
#' @param nfold Integer. Number of cross-validation folds. Default `5L`.
#' @param early_stopping Integer. Stop training if the CV metric does not
#'   improve for this many consecutive rounds. Default `50L`.
#' @param seed Integer. Random seed for reproducibility. Default `42L`.
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_ml` object (named list) with:
#'   \describe{
#'     \item{`$predictions`}{Tibble with one row per (non-NA) observation:
#'       `observed`, `fitted` (train-set prediction), `cv_predicted`
#'       (out-of-fold CV prediction), `residual` (observed - cv_predicted),
#'       and the `id_col` and `outcome_col` columns if present.}
#'     \item{`$importance`}{Tibble sorted by `Gain` (descending): `Feature`,
#'       `Gain` (average loss reduction per split), `Cover` (average number
#'       of samples per split), `Frequency` (proportion of splits using this
#'       feature). These are the standard XGBoost importance metrics.}
#'     \item{`$performance`}{Named list: `RMSE_train`, `MAE_train`,
#'       `R2_train`, `RMSE_cv`, `MAE_cv`, `R2_cv`, `Pearson_cv`
#'       (correlation between observed and OOF predictions), and
#'       `best_nrounds`.}
#'     \item{`$model`}{The final XGBoost model (class `xgb.Booster`) trained
#'       on the full dataset using `best_nrounds`. Pass to `predict()` to
#'       generate forecasts for new data (see [predict.climasus_ml()]).}
#'     \item{`$cv_log`}{Data frame from `xgb.cv()` with per-round train and
#'       test metrics. Useful for plotting the learning curve.}
#'     \item{`$meta`}{Named list of all parameters used in this call.}
#'   }
#'
#' @examples
#' \dontrun{
#' # From a climasus4r pipeline output (aggregated city x day data with
#' # climate columns from sus_climate_aggregate())
#' ml <- sus_mod_ml(
#'   df          = df_aggregated,
#'   outcome_col = "n_obitos",
#'   id_col      = "name_muni",        # group-aware CV by city
#'   objective   = "count:poisson",
#'   nfold       = 5L,
#'   lang        = "pt"
#' )
#' print(ml)
#' tidy(ml)    # predictions tibble
#'
#' # Forecast for a new climate scenario
#' new_climate <- data.frame(tair_max_c = 36, rh_pct_mean = 60, ...)
#' predict(ml, newdata = new_climate)
#' }
#'
#' @seealso [sus_mod_dlnm()], [sus_mod_vulnerability_index()],
#'   [sus_climate_aggregate()], [predict.climasus_ml()]
#'
#' @export
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success
#' @importFrom cli cli_alert_warning cli_abort cli_text cli_rule
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr arrange desc bind_cols select
#' @importFrom glue glue
#' @importFrom stats cor sd predict complete.cases
sus_mod_ml <- function(
    df,
    outcome_col,
    feature_cols       = NULL,
    id_col             = NULL,
    objective          = "count:poisson",
    nrounds            = 500L,
    max_depth          = 6L,
    eta                = 0.05,
    subsample          = 0.8,
    colsample_bytree   = 0.8,
    min_child_weight   = 1,
    nfold              = 5L,
    early_stopping     = 50L,
    seed               = 42L,
    lang               = "pt",
    verbose            = TRUE
) {

  # ── 1. Check xgboost is available ────────────────────────────────────────────
  if (!requireNamespace("xgboost", quietly = TRUE))
    cli::cli_abort(
      c("The {.pkg xgboost} package is required for {.fn sus_mod_ml}.",
        "i" = "Install with: {.code install.packages('xgboost')}")
    )

  # ── 2. Language ───────────────────────────────────────────────────────────────
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning("Unsupported language {.val {lang}}. Using {.val pt}.")
    lang <- "pt"
  }
  if (verbose) cli::cli_h1(.mrl_ml("title", lang))

  # ── 3. Validate objective ─────────────────────────────────────────────────────
  ok_obj <- c("count:poisson", "reg:squarederror", "binary:logistic")
  if (!objective %in% ok_obj)
    cli::cli_abort(
      "{.arg objective} must be one of {.val {ok_obj}}. Got: {.val {objective}}."
    )

  eval_metric <- switch(objective,
    "count:poisson"    = "poisson-nloglik",
    "reg:squarederror" = "rmse",
    "binary:logistic"  = "logloss"
  )

  # ── 4. Validate and materialize df ───────────────────────────────────────────
  if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table")))
    df <- dplyr::collect(df)
  if (!is.data.frame(df))
    cli::cli_abort("{.arg df} must be a data frame. Got: {.cls {class(df)[1]}}.")
  if (!outcome_col %in% names(df))
    cli::cli_abort("Outcome column {.val {outcome_col}} not found in {.arg df}.")
  if (!is.numeric(df[[outcome_col]]))
    cli::cli_abort("{.val {outcome_col}} must be numeric. Got: {.cls {class(df[[outcome_col]])[1]}}.")
  if (!is.null(id_col) && !id_col %in% names(df))
    cli::cli_abort("Group column {.val {id_col}} not found in {.arg df}.")

  # ── 5. Resolve feature columns ────────────────────────────────────────────────
  exclude_always <- unique(c(outcome_col, id_col))

  if (is.null(feature_cols)) {
    feature_cols <- names(df)[
      vapply(df, is.numeric, logical(1L)) &
      !names(df) %in% exclude_always
    ]
    if (length(feature_cols) == 0L)
      cli::cli_abort(
        "No numeric feature columns found after excluding {.val {exclude_always}}. Provide {.arg feature_cols} explicitly."
      )
  } else {
    bad_fc <- setdiff(feature_cols, names(df))
    if (length(bad_fc) > 0L)
      cli::cli_abort("feature_cols not found in {.arg df}: {.val {bad_fc}}.")
    not_num <- feature_cols[!vapply(df[feature_cols], is.numeric, logical(1L))]
    if (length(not_num) > 0L)
      cli::cli_abort("Non-numeric feature columns: {.val {not_num}}.")
  }

  # ── 6. Remove rows with NA in outcome or any feature ─────────────────────────
  all_cols  <- c(outcome_col, feature_cols)
  na_rows   <- !complete.cases(df[, all_cols, drop = FALSE])
  n_na      <- sum(na_rows)
  n_total   <- nrow(df)

  if (n_na > 0L) {
    pct <- round(n_na / n_total * 100, 1)
    if (verbose) cli::cli_alert_warning(.mrl_ml("warn_na", lang, n_na = n_na, pct = pct))
    df <- df[!na_rows, , drop = FALSE]
  }

  n_obs  <- nrow(df)
  n_feat <- length(feature_cols)

  if (n_obs < 2L * nfold)
    cli::cli_abort(
      "Only {n_obs} rows after NA removal. Need at least {2L * nfold} for {nfold}-fold CV."
    )
  if (n_obs < 100L && verbose)
    cli::cli_alert_warning(.mrl_ml("warn_few_obs", lang, n_obs = n_obs))

  if (verbose) cli::cli_alert_info(.mrl_ml("step_prepare", lang,
    n_obs = n_obs, n_feat = n_feat, objective = objective))

  # ── 7. Build matrices ─────────────────────────────────────────────────────────
  X     <- as.matrix(df[, feature_cols, drop = FALSE])
  y     <- as.numeric(df[[outcome_col]])
  set.seed(seed)
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)

  xgb_params <- list(
    objective          = objective,
    eval_metric        = eval_metric,
    max_depth          = as.integer(max_depth),
    eta                = eta,
    subsample          = subsample,
    colsample_bytree   = colsample_bytree,
    min_child_weight   = min_child_weight,
    seed               = as.integer(seed)
  )

  # ── 8. Cross-validation to find best nrounds ──────────────────────────────────
  if (verbose) cli::cli_alert_info(.mrl_ml("step_cv", lang, nfold = nfold))

  # Build fold IDs: group-aware if id_col provided, random otherwise
  fold_ids <- .ml_make_folds(df, id_col, nfold, seed)

  cv_log <- withCallingHandlers(
    xgboost::xgb.cv(
      params                = xgb_params,
      data                  = dtrain,
      nrounds               = as.integer(nrounds),
      folds                 = fold_ids,
      early_stopping_rounds = as.integer(early_stopping),
      verbose               = 0L,
      prediction            = FALSE
    ),
    warning = function(w) {
      if (grepl("watchlist.*renamed|evals", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )

  cv_df       <- as.data.frame(cv_log$evaluation_log)
  test_col    <- grep("test_.*_mean$", names(cv_df), value = TRUE)[1L]
  best_nrounds <- which.min(cv_df[[test_col]])
  if (verbose) cli::cli_alert_info(.mrl_ml("step_train", lang,
    best_nrounds = best_nrounds, eta = eta))

  # ── 9. Out-of-fold (OOF) predictions via manual k-fold with best_nrounds ─────
  oof_pred <- numeric(n_obs)
  for (k in seq_len(nfold)) {
    train_k <- fold_ids[[k]]   # indices held OUT in fold k
    # xgb.cv fold convention: fold[[k]] = held-out indices
    test_k  <- fold_ids[[k]]
    train_k <- setdiff(seq_len(n_obs), test_k)

    X_tr <- X[train_k, , drop = FALSE]
    y_tr <- y[train_k]
    X_te <- X[test_k,  , drop = FALSE]

    d_tr <- xgboost::xgb.DMatrix(data = X_tr, label = y_tr)
    d_te <- xgboost::xgb.DMatrix(data = X_te)

    m_k <- withCallingHandlers(
      xgboost::xgb.train(
        params   = xgb_params,
        data     = d_tr,
        nrounds  = as.integer(best_nrounds),
        verbose  = 0L,
        watchlist = list(train = d_tr)
      ),
      warning = function(w) {
        if (grepl("watchlist.*renamed|evals", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    )
    oof_pred[test_k] <- stats::predict(m_k, d_te)
  }

  # ── 10. Train final model on full data ────────────────────────────────────────
  final_model <- withCallingHandlers(
    xgboost::xgb.train(
      params    = xgb_params,
      data      = dtrain,
      nrounds   = as.integer(best_nrounds),
      verbose   = 0L,
      watchlist = list(train = dtrain)
    ),
    warning = function(w) {
      if (grepl("watchlist.*renamed|evals", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )

  fitted_vals <- stats::predict(final_model, dtrain)

  # ── 11. Feature importance ────────────────────────────────────────────────────
  if (verbose) cli::cli_alert_info(.mrl_ml("step_importance", lang))

  imp_raw  <- xgboost::xgb.importance(model = final_model)
  imp_tbl  <- tibble::as_tibble(imp_raw) |>
    dplyr::arrange(dplyr::desc(Gain))

  # ── 12. Performance metrics ───────────────────────────────────────────────────
  .rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
  .mae  <- function(obs, pred) mean(abs(obs - pred), na.rm = TRUE)
  .r2   <- function(obs, pred) {
    ss_res <- sum((obs - pred)^2, na.rm = TRUE)
    ss_tot <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
    if (ss_tot < .Machine$double.eps) return(NA_real_)
    1 - ss_res / ss_tot
  }

  perf <- list(
    RMSE_train   = .rmse(y, fitted_vals),
    MAE_train    = .mae(y,  fitted_vals),
    R2_train     = .r2(y,   fitted_vals),
    RMSE_cv      = .rmse(y, oof_pred),
    MAE_cv       = .mae(y,  oof_pred),
    R2_cv        = .r2(y,   oof_pred),
    Pearson_cv   = as.numeric(stats::cor(y, oof_pred, use = "complete.obs")),
    best_nrounds = best_nrounds
  )

  # ── 13. Predictions tibble ────────────────────────────────────────────────────
  pred_tbl <- tibble::tibble(
    observed     = y,
    fitted       = round(fitted_vals, 4),
    cv_predicted = round(oof_pred,    4),
    residual     = round(y - oof_pred, 4)
  )
  if (!is.null(id_col))
    pred_tbl <- dplyr::bind_cols(
      tibble::tibble(!!rlang::sym(id_col) := df[[id_col]]),
      pred_tbl
    )

  # ── 14. Report ────────────────────────────────────────────────────────────────
  top_feat <- if (nrow(imp_tbl) > 0L) imp_tbl$Feature[1L] else "N/A"
  if (verbose) cli::cli_alert_success(.mrl_ml("done", lang,
    rmse_cv = round(perf$RMSE_cv, 3),
    mae_cv  = round(perf$MAE_cv,  3),
    r2_cv   = round(perf$R2_cv,   3),
    top_feat = top_feat
  ))

  # ── 15. Return ────────────────────────────────────────────────────────────────
  structure(
    list(
      predictions  = pred_tbl,
      importance   = imp_tbl,
      performance  = perf,
      model        = final_model,
      cv_log       = cv_df,
      meta         = list(
        outcome_col      = outcome_col,
        feature_cols     = feature_cols,
        id_col           = id_col,
        objective        = objective,
        eval_metric      = eval_metric,
        nrounds_max      = as.integer(nrounds),
        best_nrounds     = best_nrounds,
        nfold            = as.integer(nfold),
        max_depth        = as.integer(max_depth),
        eta              = eta,
        subsample        = subsample,
        colsample_bytree = colsample_bytree,
        min_child_weight = min_child_weight,
        early_stopping   = as.integer(early_stopping),
        n_obs_total      = n_total,
        n_obs_used       = n_obs,
        n_removed_na     = n_na,
        seed             = as.integer(seed),
        call_time        = Sys.time()
      )
    ),
    class = c("climasus_ml", "list")
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Build cross-validation fold assignment list
#'
#' When id_col is provided, groups are kept whole in each fold (group k-fold).
#' When id_col is NULL, standard random k-fold is used.
#' Returns a list of integer vectors: fold_ids[[k[] = row indices held out in fold k.
#' @keywords internal
#' @noRd
.ml_make_folds <- function(df, id_col, nfold, seed) {
  n <- nrow(df)
  set.seed(seed)

  if (!is.null(id_col)) {
    groups   <- as.character(df[[id_col]])
    uniq_grp <- unique(groups)
    n_grp    <- length(uniq_grp)
    if (n_grp < nfold)
      cli::cli_alert_warning(
        "Number of unique groups ({n_grp}) is less than {.arg nfold} ({nfold}). Reducing nfold."
      )
    nfold     <- min(nfold, n_grp)
    grp_fold  <- sample(rep_len(seq_len(nfold), n_grp))
    names(grp_fold) <- uniq_grp
    fold_vec  <- grp_fold[groups]
  } else {
    fold_vec  <- sample(rep_len(seq_len(nfold), n))
  }

  lapply(seq_len(nfold), function(k) which(fold_vec == k))
}

#' Compute RMSE, MAE, R2 from numeric vectors
#' @keywords internal
#' @noRd
.ml_metrics <- function(obs, pred) {
  ss_res <- sum((obs - pred)^2, na.rm = TRUE)
  ss_tot <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  list(
    RMSE    = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
    MAE     = mean(abs(obs - pred), na.rm = TRUE),
    R2      = if (ss_tot < .Machine$double.eps) NA_real_ else 1 - ss_res / ss_tot,
    Pearson = as.numeric(stats::cor(obs, pred, use = "complete.obs"))
  )
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_ml object
#'
#' @param x A `climasus_ml` object from [sus_mod_ml()].
#' @param n_imp Integer. Number of top features to show. Default `10L`.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_ml <- function(x, n_imp = 10L, ...) {
  m <- x$meta
  p <- x$performance

  cli::cli_h2("climasus_ml \u2014 XGBoost")
  cli::cli_text("{.strong Outcome}      : {m$outcome_col}")
  cli::cli_text("{.strong Features}     : {length(m$feature_cols)} ({paste(utils::head(m$feature_cols, 5L), collapse=', ')}{if(length(m$feature_cols)>5) ', ...' else ''})")
  cli::cli_text("{.strong Objective}    : {m$objective}")
  cli::cli_text("{.strong Best nrounds} : {m$best_nrounds} / {m$nrounds_max} (eta = {m$eta})")
  cli::cli_text("{.strong CV folds}     : {m$nfold} | Obs: {m$n_obs_used} (removed: {m$n_removed_na})")
  cli::cli_rule()
  cli::cli_text("{.strong Performance (CV / train):}")
  cli::cli_text(
    "  RMSE: {round(p$RMSE_cv,3)} / {round(p$RMSE_train,3)} | MAE: {round(p$MAE_cv,3)} / {round(p$MAE_train,3)} | R\u00b2: {round(p$R2_cv,3)} / {round(p$R2_train,3)} | Pearson-CV: {round(p$Pearson_cv,3)}"
  )
  cli::cli_rule()

  n_show <- min(as.integer(n_imp), nrow(x$importance))
  cli::cli_text("{.strong Feature importance (top {n_show} by gain):}")
  print(utils::head(x$importance, n_show))
  invisible(x)
}

#' Summarise a climasus_ml object
#'
#' @param object A `climasus_ml` object from [sus_mod_ml()].
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_ml <- function(object, ...) {
  print(object, n_imp = nrow(object$importance))
  cat("\n-- CV log (first / last 3 rounds) --\n")
  n_log <- nrow(object$cv_log)
  print(rbind(utils::head(object$cv_log, 3L), utils::tail(object$cv_log, 3L)))
  invisible(object)
}

#' Tidy a climasus_ml object into a flat predictions tibble
#'
#' Returns the predictions table with `observed`, `fitted`, `cv_predicted`,
#' and `residual` columns. Suitable for plotting, residual diagnostics,
#' or combining across analyses.
#'
#' @param x A `climasus_ml` object from [sus_mod_ml()].
#' @param ... Unused.
#' @return A tibble with one row per observation.
#' @export
#' @importFrom generics tidy
tidy.climasus_ml <- function(x, ...) {
  x$predictions
}

#' Generate predictions from a climasus_ml model on new data
#'
#' Applies the fitted XGBoost model to a new data frame. The new data must
#' contain all feature columns used during training (see
#' `object$meta$feature_cols`).
#'
#' @param object A `climasus_ml` object from [sus_mod_ml()].
#' @param newdata A data frame with the same feature columns as the training
#'   data. Missing features trigger an informative error.
#' @param ... Unused.
#' @return A numeric vector of predictions (in the natural scale of the
#'   objective: counts for Poisson, values for squarederror, probabilities
#'   for binary logistic).
#' @export
predict.climasus_ml <- function(object, newdata, ...) {
  feat <- object$meta$feature_cols
  miss <- setdiff(feat, names(newdata))
  if (length(miss) > 0L)
    cli::cli_abort(
      "New data is missing feature columns used during training: {.val {miss}}."
    )
  X_new <- as.matrix(newdata[, feat, drop = FALSE])
  stats::predict(object$model, xgboost::xgb.DMatrix(data = X_new))
}
