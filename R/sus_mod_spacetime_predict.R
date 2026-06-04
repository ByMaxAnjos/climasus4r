# =============================================================================
# sus_mod_spacetime_predict.R
# Approximate Space-Time Predictions from a Fitted climasus_spacetime_bayes
#
# Theory:
#   Rue, H., Martino, S., & Chopin, N. (2009). Approximate Bayesian inference
#     for latent Gaussian models by using integrated nested Laplace
#     approximations. Journal of the Royal Statistical Society: Series B,
#     71(2), 319-392. (INLA)
#   Blangiardo, M., & Cameletti, M. (2015). Spatial and Spatio-temporal
#     Bayesian Models with R-INLA. Wiley.
#   Held, L., & Schrdle, B. (2010). Posterior and cross-validatory predictive
#     checks: a comparison of MCMC and INLA. In Statistical Modelling and
#     Regression Structures (pp. 91-110). Physica-Verlag.
#
# Input : climasus_spacetime_bayes object from sus_mod_spacetime_bayes()
# Output: climasus_spacetime_pred (list) with predictions, CIs, metadata
#
# Note  : This implements an APPROXIMATE linear-predictor approach.
#         For exact Bayesian posterior predictive distributions, re-run
#         sus_mod_spacetime_bayes() with the full dataset including future
#         periods (the recommended workflow for production surveillance).
# =============================================================================

# -- NSE variable declarations -------------------------------------------------
utils::globalVariables(c(
  "code_muni", "time_idx", "pred_mean", "pred_lower95", "pred_upper95",
  "eta_mean", "eta_var", "covariate", "beta_mean", "beta_sd"
))

# -- Local i18n ----------------------------------------------------------------
.pred_msgs <- list(

  step_validate = list(
    pt = "Validando objeto {.cls climasus_spacetime_bayes} e par\u00e2metros...",
    en = "Validating {.cls climasus_spacetime_bayes} object and parameters...",
    es = "Validando objeto {.cls climasus_spacetime_bayes} y par\u00e1metros..."
  ),

  step_extract = list(
    pt = "Extraindo efeitos fixos e aleat\u00f3rios do modelo ajustado...",
    en = "Extracting fixed and random effects from the fitted model...",
    es = "Extrayendo efectos fijos y aleatorios del modelo ajustado..."
  ),

  step_newdata = list(
    pt = "Preparando {n} linhas de dados para predi\u00e7\u00e3o...",
    en = "Preparing {n} rows of new data for prediction...",
    es = "Preparando {n} filas de datos nuevos para predicci\u00f3n..."
  ),

  step_horizon = list(
    pt = "Extrapolando {horizon} per\u00edodo(s) \u00e0 frente via RW{rw_order}...",
    en = "Extrapolating {horizon} period(s) forward via RW{rw_order}...",
    es = "Extrapolando {horizon} per\u00edodo(s) hacia adelante v\u00eda RW{rw_order}..."
  ),

  step_lp = list(
    pt = "Calculando preditor linear (X_new \u00d7 \u03b2 + RE espacial + RE temporal)...",
    en = "Computing linear predictor (X_new x beta + spatial RE + temporal RE)...",
    es = "Calculando predictor lineal (X_new x \u03b2 + RE espacial + RE temporal)..."
  ),

  step_ci = list(
    pt = "Construindo intervalos de credibilidade de 95%...",
    en = "Building 95% credibility intervals...",
    es = "Construyendo intervalos de credibilidad al 95%..."
  ),

  done = list(
    pt = "Predi\u00e7\u00e3o conclu\u00edda. {n_pred} observa\u00e7\u00f5es previstas | horizon = {horizon}",
    en = "Prediction complete. {n_pred} observations predicted | horizon = {horizon}",
    es = "Predicci\u00f3n finalizada. {n_pred} observaciones predichas | horizon = {horizon}"
  ),

  warn_new_muni = list(
    pt = "{n_new} munic\u00edpio(s) em {.arg newdata} n\u00e3o encontrado(s) no modelo. Efeito espacial definido como NA para: {munis}.",
    en = "{n_new} municipality(ies) in {.arg newdata} not found in fitted model. Spatial effect set to NA for: {munis}.",
    es = "{n_new} municipio(s) en {.arg newdata} no encontrado(s) en el modelo. Efecto espacial definido como NA para: {munis}."
  ),

  warn_covariate_override = list(
    pt = "Sobrescrevendo covari\u00e1vel {.val {cov}} com valor de {.arg covariates_new}.",
    en = "Overriding covariate {.val {cov}} with value from {.arg covariates_new}.",
    es = "Sobreescribiendo covariable {.val {cov}} con valor de {.arg covariates_new}."
  ),

  warn_horizon_only = list(
    pt = "{.arg newdata} \u00e9 NULL. Extrapolando {horizon} per\u00edodo(s) ap\u00f3s o \u00faltimo \u00edndice de tempo observado ({last_t}).",
    en = "{.arg newdata} is NULL. Extrapolating {horizon} period(s) after the last observed time index ({last_t}).",
    es = "{.arg newdata} es NULL. Extrapolando {horizon} per\u00edodo(s) tras el \u00faltimo \u00edndice de tiempo observado ({last_t})."
  ),

  warn_approx = list(
    pt = "AVISO: preditor linear aproximado. Para predi\u00e7\u00f5es Bayesianas completas, reajuste {.fn sus_mod_spacetime_bayes} com os dados completos.",
    en = "NOTE: approximate linear predictor used. For full Bayesian predictions, refit {.fn sus_mod_spacetime_bayes} with the complete dataset.",
    es = "NOTA: predictor lineal aproximado. Para predicciones bayesianas completas, reajuste {.fn sus_mod_spacetime_bayes} con los datos completos."
  ),

  err_not_spacetime = list(
    pt = "{.arg fit} deve ser um objeto {.cls climasus_spacetime_bayes} produzido por {.fn sus_mod_spacetime_bayes}.",
    en = "{.arg fit} must be a {.cls climasus_spacetime_bayes} object from {.fn sus_mod_spacetime_bayes}.",
    es = "{.arg fit} debe ser un objeto {.cls climasus_spacetime_bayes} de {.fn sus_mod_spacetime_bayes}."
  ),

  err_no_fixed = list(
    pt = "O objeto {.cls climasus_spacetime_bayes} n\u00e3o cont\u00e9m {.field $fixed}. O modelo foi ajustado corretamente?",
    en = "The {.cls climasus_spacetime_bayes} object has no {.field $fixed} slot. Was the model fitted correctly?",
    es = "El objeto {.cls climasus_spacetime_bayes} no contiene {.field $fixed}. \u00bfEl modelo fue ajustado correctamente?"
  ),

  err_horizon_newdata = list(
    pt = "{.arg horizon} deve ser 0 quando {.arg newdata} \u00e9 fornecido. Defina {.code horizon = 0L} ou omita {.arg newdata}.",
    en = "{.arg horizon} must be 0 when {.arg newdata} is supplied. Set {.code horizon = 0L} or omit {.arg newdata}.",
    es = "{.arg horizon} debe ser 0 cuando se provee {.arg newdata}. Defina {.code horizon = 0L} u omita {.arg newdata}."
  ),

  err_horizon_pos = list(
    pt = "{.arg horizon} deve ser um inteiro \u22650. Recebido: {.val {h}}.",
    en = "{.arg horizon} must be a non-negative integer. Got: {.val {h}}.",
    es = "{.arg horizon} debe ser un entero no negativo. Recibido: {.val {h}}."
  ),

  err_no_code_muni = list(
    pt = "{.arg newdata} deve conter a coluna {.val code_muni}.",
    en = "{.arg newdata} must contain the {.val code_muni} column.",
    es = "{.arg newdata} debe contener la columna {.val code_muni}."
  ),

  err_no_time_idx = list(
    pt = "{.arg newdata} deve conter a coluna {.val time_idx} (inteiro indicando o per\u00edodo).",
    en = "{.arg newdata} must contain the {.val time_idx} column (integer period index).",
    es = "{.arg newdata} debe contener la columna {.val time_idx} (entero que indica el per\u00edodo)."
  ),

  err_missing_covariates = list(
    pt = "Covari\u00e1vel(is) {.val {missing_str}} presente(s) no modelo mas ausente(s) em {.arg newdata}. Forne\u00e7a as colunas ou use {.arg covariates_new}.",
    en = "Covariate(s) {.val {missing_str}} present in model but missing from {.arg newdata}. Provide the columns or use {.arg covariates_new}.",
    es = "Covariable(s) {.val {missing_str}} presente(s) en el modelo pero ausente(s) en {.arg newdata}. Provea las columnas o use {.arg covariates_new}."
  ),

  err_samples_unavailable = list(
    pt = "{.arg return_samples = TRUE} requer que o objeto {.cls climasus_spacetime_bayes} contenha amostras INLA completas em {.field $inla_fit}. Reajuste com {.code return_inla_fit = TRUE}.",
    en = "{.arg return_samples = TRUE} requires the {.cls climasus_spacetime_bayes} object to contain full INLA samples in {.field $inla_fit}. Refit with {.code return_inla_fit = TRUE}.",
    es = "{.arg return_samples = TRUE} requiere que el objeto {.cls climasus_spacetime_bayes} contenga muestras INLA completas en {.field $inla_fit}. Reajuste con {.code return_inla_fit = TRUE}."
  )
)

# -- Internal message helper ---------------------------------------------------
#' @keywords internal
#' @noRd
.prl <- function(key, lang, ...) {
  entry <- .pred_msgs[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}

# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Generate Predictions from a Fitted Space-Time Bayesian Model
#'
#' @description
#' Produces approximate predictions at new or future space-time points from a
#' `climasus_spacetime_bayes` object fitted by [sus_mod_spacetime_bayes()].
#' This function is designed for **disease surveillance projections** and
#' **counterfactual scenario analysis** (e.g. "what if temperature was 2 °C
#' higher?").
#'
#' @section Algorithm:
#'
#' Because re-running INLA for new data is computationally expensive, this
#' function uses an **approximate linear-predictor approach**:
#'
#' 1. Extract posterior mean fixed effects \eqn{\hat{\beta}} and their standard
#'    deviations \eqn{\sigma_\beta} from `fit$fixed`.
#' 2. For new covariate matrix \eqn{X_{\text{new}}}, compute the fixed-effect
#'    contribution: \eqn{\eta^{(f)} = X_{\text{new}} \hat{\beta}}.
#' 3. Spatial random effect: reuse `fit$spatial_re` for municipalities present
#'    in the fitted model. For municipalities not in the training data, the
#'    spatial random effect is set to `NA` with a warning.
#' 4. Temporal random effect: for in-sample time points, reuse `fit$temporal_re`
#'    directly. For future periods (`horizon > 0`), extrapolate using the last
#'    observed increment (RW1: repeat last increment; RW2: project second
#'    differences) with uncertainty propagation.
#' 5. Full linear predictor: \eqn{\hat{\eta} = \eta^{(f)} + \phi_i + \gamma_t}.
#' 6. Variance propagation:
#'    \deqn{\text{Var}(\hat{\eta}) \approx x^T V_\beta x + \sigma^2_\phi + \sigma^2_\gamma}
#'    where \eqn{V_\beta = \text{diag}(\sigma_\beta^2)} (diagonal approximation).
#' 7. Point prediction: \eqn{\hat{\mu} = \exp(\hat{\eta})} (Poisson / NB) or
#'    \eqn{\hat{\mu} = \hat{\eta}} (Gaussian).
#' 8. 95\% credible interval:
#'    \eqn{\exp(\hat{\eta} \pm 1.96 \sqrt{\text{Var}(\hat{\eta})})}.
#'
#' **Important**: this is an approximation. For exact posterior predictive
#' distributions, re-run [sus_mod_spacetime_bayes()] with the full dataset
#' including future periods.
#'
#' @section Covariate override (scenario analysis):
#'
#' `covariates_new` accepts a named list of scalar or vector overrides, e.g.:
#' ```r
#' covariates_new = list(temp_mean = new_grid$temp_mean + 2)
#' ```
#' Overrides are applied to the resolved covariate matrix **after** `newdata`
#' columns are extracted, so they can reference vectors of length
#' `nrow(newdata)`.
#'
#' @param fit A `climasus_spacetime_bayes` object from
#'   [sus_mod_spacetime_bayes()].
#' @param newdata A `data.frame` with at least columns `code_muni` (7-digit
#'   IBGE code) and `time_idx` (integer period index, 1-based, matching the
#'   encoding used in the original fit) plus any covariate columns required
#'   by the model. If `NULL` and `horizon > 0`, a synthetic grid is constructed
#'   by crossing all fitted municipalities with the next `horizon` time steps,
#'   using covariate values from the last observed period (stored in
#'   `fit$data_last_obs`, if available).
#' @param horizon Non-negative integer. Number of periods ahead to predict when
#'   `newdata` is `NULL`. If `newdata` is provided, `horizon` must be `0L`
#'   (default).
#' @param covariates_new Named list of covariate overrides for scenario
#'   analysis. Each element must be a scalar (applied to all rows) or a vector
#'   of length `nrow(newdata)`. Applied after `newdata` columns are extracted.
#'   Default `NULL` (no overrides).
#' @param include_ci Logical. If `TRUE` (default), compute 95\% approximate
#'   credible intervals via variance propagation from posterior standard
#'   deviations in `fit$fixed`, `fit$spatial_re`, and `fit$temporal_re`.
#' @param return_samples Logical. If `TRUE`, attempt to draw posterior
#'   predictive samples from the stored INLA fit object (`fit$inla_fit`).
#'   Requires the model to have been fitted with `return_inla_fit = TRUE` and
#'   the **INLA** package to be installed. Default `FALSE`.
#' @param lang Character. Language for CLI messages: `"pt"` (default),
#'   `"en"`, or `"es"`.
#' @param verbose Logical. If `TRUE` (default), print progress messages via
#'   **cli**.
#'
#' @return A named list of class `c("climasus_spacetime_pred", "list")` with:
#'   \describe{
#'     \item{`$predictions`}{`data.frame` with columns `code_muni`, `time_idx`,
#'       `pred_mean`, and (if `include_ci = TRUE`) `pred_lower95`,
#'       `pred_upper95`.}
#'     \item{`$n_predicted`}{Integer. Total number of predicted observations.}
#'     \item{`$horizon`}{Integer. Horizon used (0 when `newdata` is supplied).}
#'     \item{`$call`}{The matched call.}
#'   }
#'   If `return_samples = TRUE` and INLA is available, an additional
#'   `$samples` slot contains raw INLA posterior samples (list from
#'   `INLA::inla.posterior.sample()`).
#'
#' @references
#' Rue, H., Martino, S., & Chopin, N. (2009). Approximate Bayesian inference
#' for latent Gaussian models by using integrated nested Laplace approximations.
#' \emph{Journal of the Royal Statistical Society: Series B}, 71(2), 319-392.
#'
#' Blangiardo, M., & Cameletti, M. (2015).
#' \emph{Spatial and Spatio-temporal Bayesian Models with R-INLA}. Wiley.
#'
#' Held, L., & Schrödle, B. (2010). Posterior and cross-validatory predictive
#' checks: a comparison of MCMC and INLA. In \emph{Statistical Modelling and
#' Regression Structures} (pp. 91-110). Physica-Verlag HD.
#'
#' @seealso
#' [sus_mod_spatial_bayes()] for cross-sectional Bayesian disease mapping,
#' [sus_mod_spatial_weights()] to build the spatial adjacency object,
#' [sus_mod_spatial_moran()] for exploratory spatial autocorrelation analysis.
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Assume `st_fit` is a climasus_spacetime_bayes object
#' # fitted on monthly data for 2015-2020
#'
#' # 1. In-sample predictions for validation
#' pred_insample <- sus_mod_spacetime_predict(
#'   fit     = st_fit,
#'   newdata = training_data,
#'   lang    = "pt"
#' )
#' head(pred_insample$predictions)
#'
#' # 2. Out-of-sample: 6 months ahead
#' pred_future <- sus_mod_spacetime_predict(
#'   fit     = st_fit,
#'   horizon = 6L,
#'   lang    = "en"
#' )
#' pred_future$predictions
#'
#' # 3. Counterfactual: +2 degrees C scenario
#' pred_counter <- sus_mod_spacetime_predict(
#'   fit            = st_fit,
#'   newdata        = new_grid,
#'   covariates_new = list(temp_mean = new_grid$temp_mean + 2),
#'   lang           = "pt"
#' )
#' }
#'
#' @export
sus_mod_spacetime_predict <- function(
    fit,
    newdata        = NULL,
    horizon        = 0L,
    covariates_new = NULL,
    include_ci     = TRUE,
    return_samples = FALSE,
    lang           = "pt",
    verbose        = TRUE
) {
  .call <- match.call()
  lang  <- match.arg(lang, c("pt", "en", "es"))

  # -- 0. validate fit object ---------------------------------------------------
  if (verbose) cli::cli_progress_step(.prl("step_validate", lang))

  if (!inherits(fit, "climasus_spacetime_bayes")) {
    cli::cli_abort(.prl("err_not_spacetime", lang))
  }

  if (is.null(fit[["fixed"]]) || !is.data.frame(fit[["fixed"]])) {
    cli::cli_abort(.prl("err_no_fixed", lang))
  }

  # horizon validation
  horizon <- as.integer(horizon)
  if (is.na(horizon) || horizon < 0L) {
    cli::cli_abort(.prl("err_horizon_pos", lang, h = horizon))
  }

  if (!is.null(newdata) && horizon > 0L) {
    cli::cli_abort(.prl("err_horizon_newdata", lang))
  }

  # return_samples: requires inla_fit slot
  if (isTRUE(return_samples)) {
    if (is.null(fit[["inla_fit"]])) {
      cli::cli_abort(.prl("err_samples_unavailable", lang))
    }
  }

  # -- 1. extract model components ----------------------------------------------
  if (verbose) cli::cli_progress_step(.prl("step_extract", lang))

  fixed_df <- fit[["fixed"]]   # data.frame: term, mean, sd (minimum columns)

  # Identify fixed-effect terms (exclude intercept for design matrix building)
  all_terms    <- fixed_df[["term"]]
  intercept_ix <- grepl("^(Intercept|\\(Intercept\\))$", all_terms,
                        ignore.case = TRUE)
  covariate_terms <- all_terms[!intercept_ix]

  # beta vector (posterior means) and sd vector
  beta_mean_vec <- stats::setNames(fixed_df[["mean"]], all_terms)
  beta_sd_vec   <- if ("sd" %in% names(fixed_df)) {
    stats::setNames(fixed_df[["sd"]], all_terms)
  } else {
    stats::setNames(rep(0, length(all_terms)), all_terms)
  }

  # Intercept contribution (sum handles unlikely multi-intercept edge case)
  intercept_val <- if (any(intercept_ix)) sum(beta_mean_vec[intercept_ix]) else 0
  intercept_var <- if (any(intercept_ix)) sum(beta_sd_vec[intercept_ix]^2)  else 0

  # Spatial random effects
  # Slot precedence: $spatial_re > $random (both may exist depending on fit impl.)
  spatial_re <- NULL
  if (!is.null(fit[["spatial_re"]]) && is.data.frame(fit[["spatial_re"]])) {
    spatial_re <- fit[["spatial_re"]]
  } else if (!is.null(fit[["random"]]) && is.data.frame(fit[["random"]]) &&
             "phi_mean" %in% names(fit[["random"]])) {
    spatial_re <- fit[["random"]]
  }

  # Temporal random effects: data.frame(time_idx, gamma_mean, [gamma_sd])
  temporal_re <- if (!is.null(fit[["temporal_re"]]) &&
                     is.data.frame(fit[["temporal_re"]])) {
    fit[["temporal_re"]]
  } else {
    NULL
  }

  # Family / link
  model_family <- if (!is.null(fit[["family"]])) fit[["family"]] else "poisson"

  # -- 2. build prediction grid -------------------------------------------------
  if (!is.null(newdata)) {
    # 2a. use supplied newdata
    if (verbose) {
      cli::cli_progress_step(.prl("step_newdata", lang, n = nrow(newdata)))
    }

    nd <- as.data.frame(newdata)

    if (!("code_muni" %in% names(nd))) {
      cli::cli_abort(.prl("err_no_code_muni", lang))
    }
    if (!("time_idx" %in% names(nd))) {
      cli::cli_abort(.prl("err_no_time_idx", lang))
    }

    # check covariate presence (overrides may satisfy missing columns)
    provided_overrides <- if (!is.null(covariates_new)) names(covariates_new) else character(0L)
    needed_from_data   <- setdiff(covariate_terms, provided_overrides)
    missing_from_nd    <- setdiff(needed_from_data, names(nd))

    if (length(missing_from_nd) > 0L) {
      cli::cli_abort(
        .prl("err_missing_covariates", lang,
             missing_str = paste(missing_from_nd, collapse = ", "))
      )
    }

    pred_grid <- nd

  } else {
    # 2b. build synthetic forward grid (horizon periods)
    if (horizon == 0L) {
      # nothing to predict; return empty result
      empty_pred <- data.frame(
        code_muni    = character(0L),
        time_idx     = integer(0L),
        pred_mean    = numeric(0L),
        stringsAsFactors = FALSE
      )
      if (include_ci) {
        empty_pred[["pred_lower95"]] <- numeric(0L)
        empty_pred[["pred_upper95"]] <- numeric(0L)
      }
      return(
        structure(
          list(
            predictions = empty_pred,
            n_predicted = 0L,
            horizon     = 0L,
            call        = .call
          ),
          class = c("climasus_spacetime_pred", "list")
        )
      )
    }

    # Last observed time index
    last_t <- if (!is.null(temporal_re) && "time_idx" %in% names(temporal_re)) {
      max(temporal_re[["time_idx"]], na.rm = TRUE)
    } else if (!is.null(fit[["fitted"]]) && is.data.frame(fit[["fitted"]]) &&
               "time_idx" %in% names(fit[["fitted"]])) {
      max(fit[["fitted"]][["time_idx"]], na.rm = TRUE)
    } else {
      1L
    }

    if (verbose) {
      cli::cli_alert_info(
        .prl("warn_horizon_only", lang, horizon = horizon, last_t = last_t)
      )
    }

    # Municipalities in the fitted model
    fitted_munis <- if (!is.null(spatial_re) && "code_muni" %in% names(spatial_re)) {
      unique(spatial_re[["code_muni"]])
    } else if (!is.null(fit[["fitted"]]) && is.data.frame(fit[["fitted"]]) &&
               "code_muni" %in% names(fit[["fitted"]])) {
      unique(fit[["fitted"]][["code_muni"]])
    } else {
      character(0L)
    }

    future_t <- seq_len(horizon) + last_t

    if (length(fitted_munis) > 0L) {
      pred_grid <- expand.grid(
        code_muni = fitted_munis,
        time_idx  = future_t,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    } else {
      pred_grid <- data.frame(
        code_muni = character(0L),
        time_idx  = integer(0L),
        stringsAsFactors = FALSE
      )
    }

    # Carry forward last-observed covariate values from fit$data_last_obs
    if (length(covariate_terms) > 0L && !is.null(fit[["data_last_obs"]]) &&
        is.data.frame(fit[["data_last_obs"]])) {
      last_obs <- fit[["data_last_obs"]]
      keep_cols <- intersect(c("code_muni", covariate_terms), names(last_obs))
      pred_grid <- merge(
        pred_grid,
        last_obs[, keep_cols, drop = FALSE],
        by    = "code_muni",
        all.x = TRUE
      )
    } else {
      for (cv in covariate_terms) pred_grid[[cv]] <- NA_real_
    }
  }

  # -- 3. apply covariate overrides ---------------------------------------------
  if (!is.null(covariates_new)) {
    for (cov_nm in names(covariates_new)) {
      if (verbose) {
        cli::cli_alert_info(.prl("warn_covariate_override", lang, cov = cov_nm))
      }
      pred_grid[[cov_nm]] <- covariates_new[[cov_nm]]
    }
  }

  n_pred <- nrow(pred_grid)

  # -- 4. compute approximate linear predictor ----------------------------------
  if (verbose) cli::cli_progress_step(.prl("step_lp", lang))

  # 4a. fixed-effect contribution (sum over covariates)
  eta_fixed     <- rep(intercept_val, n_pred)
  eta_var_fixed <- rep(intercept_var, n_pred)

  for (trm in covariate_terms) {
    if (trm %in% names(pred_grid)) {
      x_vec         <- as.numeric(pred_grid[[trm]])
      eta_fixed     <- eta_fixed     + x_vec * beta_mean_vec[[trm]]
      eta_var_fixed <- eta_var_fixed + (x_vec * beta_sd_vec[[trm]])^2
    }
  }

  # 4b. spatial random effect
  eta_spatial     <- rep(0, n_pred)
  eta_var_spatial <- rep(0, n_pred)

  if (!is.null(spatial_re) && "code_muni" %in% names(spatial_re)) {
    sp_match      <- match(pred_grid[["code_muni"]], spatial_re[["code_muni"]])
    new_muni_idx  <- which(is.na(sp_match))

    if (length(new_muni_idx) > 0L && verbose) {
      new_muni_vals <- unique(pred_grid[["code_muni"]][new_muni_idx])
      cli::cli_alert_warning(
        .prl("warn_new_muni", lang,
             n_new = length(new_muni_vals),
             munis = paste(utils::head(new_muni_vals, 5L), collapse = ", "))
      )
    }

    ok <- !is.na(sp_match)
    if (any(ok)) {
      phi_mean_vals          <- spatial_re[["phi_mean"]][sp_match[ok]]
      eta_spatial[ok]        <- phi_mean_vals

      if ("phi_sd" %in% names(spatial_re)) {
        phi_sd_vals            <- spatial_re[["phi_sd"]][sp_match[ok]]
        eta_var_spatial[ok]    <- phi_sd_vals^2
      }
    }
  }

  # 4c. temporal random effect with RW1/RW2 extrapolation
  eta_temporal     <- rep(0, n_pred)
  eta_var_temporal <- rep(0, n_pred)

  if (!is.null(temporal_re) && "time_idx" %in% names(temporal_re) &&
      "gamma_mean" %in% names(temporal_re)) {
    t_vec     <- as.integer(pred_grid[["time_idx"]])
    t_max_obs <- max(temporal_re[["time_idx"]], na.rm = TRUE)

    in_sample  <- t_vec <= t_max_obs
    out_sample <- !in_sample

    # In-sample: direct lookup
    if (any(in_sample)) {
      t_match_in <- match(t_vec[in_sample], temporal_re[["time_idx"]])
      valid_in   <- !is.na(t_match_in)
      if (any(valid_in)) {
        idx_in     <- which(in_sample)[valid_in]
        gm_vals    <- temporal_re[["gamma_mean"]][t_match_in[valid_in]]
        eta_temporal[idx_in] <- gm_vals
        if ("gamma_sd" %in% names(temporal_re)) {
          gs_vals                  <- temporal_re[["gamma_sd"]][t_match_in[valid_in]]
          eta_var_temporal[idx_in] <- gs_vals^2
        }
      }
    }

    # Out-of-sample: RW1/RW2 extrapolation with variance inflation
    if (any(out_sample)) {
      n_obs_t    <- nrow(temporal_re)
      gamma_last <- temporal_re[["gamma_mean"]][n_obs_t]
      gamma_sd_last <- if ("gamma_sd" %in% names(temporal_re)) {
        temporal_re[["gamma_sd"]][n_obs_t]
      } else {
        0
      }

      # Detect RW order from fit$temporal_model (e.g. "rw1", "rw2", "RW2")
      rw_order <- if (!is.null(fit[["temporal_model"]])) {
        as.integer(gsub("[^0-9]", "", as.character(fit[["temporal_model"]])))
      } else {
        1L
      }
      if (is.na(rw_order) || !(rw_order %in% c(1L, 2L))) rw_order <- 1L

      # RW2: linear drift from last second difference
      gamma_drift <- if (rw_order == 2L && n_obs_t >= 2L) {
        gamma_last - temporal_re[["gamma_mean"]][n_obs_t - 1L]
      } else {
        0
      }

      if (verbose) {
        cli::cli_progress_step(
          .prl("step_horizon", lang, horizon = horizon, rw_order = rw_order)
        )
      }

      steps_ah <- t_vec[out_sample] - t_max_obs   # steps ahead (>= 1)

      eta_temporal[out_sample]     <- gamma_last + gamma_drift * steps_ah
      # Variance grows linearly with steps ahead (conservative)
      eta_var_temporal[out_sample] <- gamma_sd_last^2 * steps_ah
    }
  }

  # 4d. total linear predictor and propagated variance
  eta_total     <- eta_fixed + eta_spatial + eta_temporal
  eta_var_total <- eta_var_fixed + eta_var_spatial + eta_var_temporal

  # -- 5. credible intervals ----------------------------------------------------
  if (verbose) cli::cli_progress_step(.prl("step_ci", lang))

  use_log_link <- model_family %in% c(
    "poisson", "nbinomial", "negative.binomial",
    "zeroinflatednbinomial0", "zeroinflatednbinomial1",
    "zeroinflatedpoisson0", "zeroinflatedpoisson1"
  )

  if (use_log_link) {
    pred_mean_vec <- exp(eta_total)
    pred_lo_vec   <- exp(eta_total - 1.96 * sqrt(pmax(eta_var_total, 0)))
    pred_hi_vec   <- exp(eta_total + 1.96 * sqrt(pmax(eta_var_total, 0)))
  } else {
    # identity link (Gaussian, etc.)
    se_vec        <- sqrt(pmax(eta_var_total, 0))
    pred_mean_vec <- eta_total
    pred_lo_vec   <- eta_total - 1.96 * se_vec
    pred_hi_vec   <- eta_total + 1.96 * se_vec
  }

  # -- 6. assemble predictions data.frame ---------------------------------------
  out_df <- data.frame(
    code_muni = pred_grid[["code_muni"]],
    time_idx  = pred_grid[["time_idx"]],
    pred_mean = pred_mean_vec,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (include_ci) {
    out_df[["pred_lower95"]] <- pred_lo_vec
    out_df[["pred_upper95"]] <- pred_hi_vec
  }

  # -- 7. posterior samples (optional) ------------------------------------------
  samples_list <- NULL
  if (isTRUE(return_samples)) {
    ask_install_inla <- NULL
    rlang::check_installed(
      "INLA",
      reason = paste0(
        "for posterior predictive sampling. ",
        "Install via: install.packages('INLA', ",
        "repos='https://inla.r-inla-download.org/R/stable')"
      ),
      action = ask_install_inla
    )
    samples_list <- tryCatch(
      {
        samp <- INLA::inla.posterior.sample(1000L, fit[["inla_fit"]])
        samp
      },
      error = function(e) {
        cli::cli_alert_warning(
          "Posterior sampling failed: {conditionMessage(e)}. Returning NULL."
        )
        NULL
      }
    )
  }

  # -- 8. done message ----------------------------------------------------------
  if (verbose) {
    cli::cli_alert_warning(.prl("warn_approx", lang))
    cli::cli_alert_success(
      .prl("done", lang, n_pred = n_pred, horizon = horizon)
    )
  }

  # -- 9. return ----------------------------------------------------------------
  result <- structure(
    list(
      predictions = out_df,
      n_predicted = n_pred,
      horizon     = horizon,
      call        = .call
    ),
    class = c("climasus_spacetime_pred", "list")
  )

  if (!is.null(samples_list)) {
    result[["samples"]] <- samples_list
  }

  result
}

# =============================================================================
# S3 METHODS
# =============================================================================

#' Print method for climasus_spacetime_pred objects
#'
#' Displays a formatted summary of space-time predictions, including the
#' number of predicted observations, the forward horizon, municipality and
#' time-index range, and a preview of the predictions table.
#'
#' @param x A `climasus_spacetime_pred` object from
#'   [sus_mod_spacetime_predict()].
#' @param n_rows Integer. Maximum number of prediction rows to display in the
#'   preview. Default `6L`.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.climasus_spacetime_pred <- function(x, n_rows = 6L, ...) {
  cli::cli_h1(
    "climasus4r -- Space-Time Predictions ({.cls climasus_spacetime_pred})"
  )

  has_ci   <- all(c("pred_lower95", "pred_upper95") %in% names(x$predictions))
  n_munis  <- length(unique(x$predictions[["code_muni"]]))
  t_range  <- if (x$n_predicted > 0L) {
    paste0(
      min(x$predictions[["time_idx"]], na.rm = TRUE),
      " - ",
      max(x$predictions[["time_idx"]], na.rm = TRUE)
    )
  } else {
    "none"
  }

  cli::cli_dl(c(
    "Predicted observations" = as.character(x$n_predicted),
    "Horizon"                = as.character(x$horizon),
    "Municipalities"         = as.character(n_munis),
    "Time indices"           = t_range,
    "Credible intervals"     = if (has_ci) "95% CrI (approximate)" else "not computed"
  ))

  cli::cli_rule(left = "Predictions preview")

  if (x$n_predicted == 0L) {
    cli::cli_text("{.emph (no predictions)}")
  } else {
    n_show <- min(as.integer(n_rows), x$n_predicted)
    pd     <- x$predictions[seq_len(n_show), , drop = FALSE]
    for (i in seq_len(n_show)) {
      muni    <- pd[["code_muni"]][i]
      tidx    <- pd[["time_idx"]][i]
      pmean_s <- formatC(pd[["pred_mean"]][i], digits = 4, format = "f")
      if (has_ci) {
        plo_s <- formatC(pd[["pred_lower95"]][i], digits = 4, format = "f")
        phi_s <- formatC(pd[["pred_upper95"]][i], digits = 4, format = "f")
        cli::cli_text(
          "{.field {muni}} t={tidx}: {pmean_s}  [{plo_s}, {phi_s}]"
        )
      } else {
        cli::cli_text("{.field {muni}} t={tidx}: {pmean_s}")
      }
    }
    if (x$n_predicted > n_show) {
      cli::cli_text(
        "... {x$n_predicted - n_show} more prediction(s) not shown."
      )
    }
  }

  cli::cli_rule()
  cli::cli_alert_info(
    "Approximate linear predictor. ",
    "For exact posterior predictive distributions, ",
    "refit with {.fn sus_mod_spacetime_bayes} on the full dataset."
  )

  invisible(x)
}

#' Summary method for climasus_spacetime_pred objects
#'
#' Alias for [print.climasus_spacetime_pred()].
#'
#' @param object A `climasus_spacetime_pred` object.
#' @param ... Additional arguments passed to [print.climasus_spacetime_pred()].
#'
#' @return Invisibly returns `object`.
#'
#' @export
summary.climasus_spacetime_pred <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
