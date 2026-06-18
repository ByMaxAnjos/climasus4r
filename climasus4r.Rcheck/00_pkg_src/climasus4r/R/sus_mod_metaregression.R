# =============================================================================
# sus_mod_metaregression.R
# Meta-Regression of Pooled DLNM Estimates with City-Level Covariates
#
# Theory:
#   Gasparrini et al. (2012, Statistics in Medicine) — two-stage DLNM pooling
#   Berkey et al. (1998, Statistics in Medicine) — multivariate meta-regression
#   Baccini et al. (2011, Epidemiology) — explaining heterogeneity in
#     temperature-mortality associations
# Input : Named list of climasus_dlnm objects + city-level covariate data.frame
# Output: climasus_metaregression with meta-regression coefficients, BLUPs,
#         covariate Wald tests, and residual heterogeneity
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "city", "n_obs", "raw_rr", "raw_rr_lo", "raw_rr_hi",
  "blup_rr", "blup_rr_lo", "blup_rr_hi",
  "covariate", "n_df", "wald_stat", "p_value",
  "model", "Q", "df_het", "p_het", "i2"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.mr_labels <- list(
  step_validate = list(
    pt = "Validando {n_cities} ajustes e covariveis...",
    en = "Validating {n_cities} fits and covariates...",
    es = "Validando {n_cities} ajustes y covariables..."
  ),
  step_align = list(
    pt = "Alinhando covariveis com {n_cities} cidade(s)...",
    en = "Aligning covariates with {n_cities} city/cities...",
    es = "Alineando covariables con {n_cities} ciudad(es)..."
  ),
  step_metareg = list(
    pt = "Ajustando meta-regressao multivariada ({n_cov} covarivel(is))...",
    en = "Fitting multivariate meta-regression ({n_cov} covariate(s))...",
    es = "Ajustando meta-regresion multivariada ({n_cov} covariable(s))..."
  ),
  step_pred = list(
    pt = "Calculando predicao para cidade media ({n_grid} pontos)...",
    en = "Computing prediction for average city ({n_grid} points)...",
    es = "Calculando prediccion para ciudad media ({n_grid} puntos)..."
  ),
  step_blup = list(
    pt = "Calculando BLUPs para {n_cities} cidades...",
    en = "Computing BLUPs for {n_cities} cities...",
    es = "Calculando BLUPs para {n_cities} ciudades..."
  ),
  step_tests = list(
    pt = "Testando significancia de {n_cov} covarivel(is) (teste de Wald)...",
    en = "Testing significance of {n_cov} covariate(s) (Wald test)...",
    es = "Probando significancia de {n_cov} covariable(s) (prueba de Wald)..."
  ),
  done = list(
    pt = "Concluido. RR (p75, cidade media): {rr} [{lo}, {hi}] | I\u00b2 residual = {i2}%",
    en = "Done. RR (p75, avg city): {rr} [{lo}, {hi}] | Residual I\u00b2 = {i2}%",
    es = "Listo. RR (p75, ciudad media): {rr} [{lo}, {hi}] | I\u00b2 residual = {i2}%"
  ),
  err_empty = list(
    pt = "{.arg fits} nao pode ser vazio.",
    en = "{.arg fits} cannot be empty.",
    es = "{.arg fits} no puede estar vacio."
  ),
  err_not_list = list(
    pt = "{.arg fits} deve ser uma lista de objetos {.cls climasus_dlnm}.",
    en = "{.arg fits} must be a list of {.cls climasus_dlnm} objects.",
    es = "{.arg fits} debe ser una lista de objetos {.cls climasus_dlnm}."
  ),
  err_not_dlnm = list(
    pt = "Elementos nao sao {.cls climasus_dlnm}: {.val {bad_names}}.",
    en = "Elements are not {.cls climasus_dlnm}: {.val {bad_names}}.",
    es = "Elementos no son {.cls climasus_dlnm}: {.val {bad_names}}."
  ),
  err_covariates = list(
    pt = "{.arg covariates} deve ser um data.frame com pelo menos uma coluna numerica.",
    en = "{.arg covariates} must be a data.frame with at least one numeric column.",
    es = "{.arg covariates} debe ser un data.frame con al menos una columna numerica."
  ),
  err_city_col = list(
    pt = "{.arg city_col} '{city_col}' nao encontrada em {.arg covariates}.",
    en = "{.arg city_col} '{city_col}' not found in {.arg covariates}.",
    es = "{.arg city_col} '{city_col}' no encontrado en {.arg covariates}."
  ),
  err_no_match = list(
    pt = "Nenhuma cidade de {.arg fits} encontrada em {.arg covariates}. Verifique os nomes das linhas ou {.arg city_col}.",
    en = "No cities from {.arg fits} found in {.arg covariates}. Check row names or {.arg city_col}.",
    es = "Ninguna ciudad de {.arg fits} encontrada en {.arg covariates}. Verifique los nombres de filas o {.arg city_col}."
  ),
  err_no_mvmeta = list(
    pt = "Pacote {.pkg mvmeta} necessario. Instale com {.code install.packages('mvmeta')}.",
    en = "Package {.pkg mvmeta} required. Install with {.code install.packages('mvmeta')}.",
    es = "Se requiere el paquete {.pkg mvmeta}. Instale con {.code install.packages('mvmeta')}."
  ),
  err_incompatible = list(
    pt = "Ajustes incompativeis: {.field {field}} difere entre cidades.",
    en = "Incompatible fits: {.field {field}} differs across cities.",
    es = "Ajustes incompatibles: {.field {field}} difiere entre ciudades."
  ),
  warn_partial_match = list(
    pt = "{n_unmatched} cidade(s) de {.arg fits} ausente(s) em {.arg covariates}: {missing_cities}. Sera(o) excluida(s) da meta-regressao.",
    en = "{n_unmatched} city/cities from {.arg fits} absent in {.arg covariates}: {missing_cities}. Will be excluded from meta-regression.",
    es = "{n_unmatched} ciudad(es) de {.arg fits} ausente(s) en {.arg covariates}: {missing_cities}. Sera(n) excluida(s) de la meta-regresion."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} nao suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Using {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  )
)

#' @keywords internal
#' @noRd
.smrl <- function(key, lang) {
  entry <- .mr_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# ── Exported function ─────────────────────────────────────────────────────────

#' Meta-Regression of Pooled DLNM Estimates with City-Level Covariates
#'
#' Extends two-stage multivariate meta-analysis (`sus_mod_pool()`) by including
#' city-level covariates (e.g., mean temperature, poverty index, age structure)
#' to explain between-city heterogeneity in climate-health associations.
#' Covariates are automatically standardized to mean 0, SD 1 so the intercept
#' represents the predicted association for an "average city". Returns
#' covariate Wald tests, residual heterogeneity statistics, and BLUP
#' predictions per city adjusted for covariate information.
#'
#' @section Statistical framework:
#'
#' Let \eqn{\boldsymbol{\theta}_i} be the \eqn{p}-dimensional vector of
#' cross-basis coefficients for city \eqn{i}, with variance-covariance
#' matrix \eqn{S_i}. The meta-regression model is:
#'
#' \deqn{\boldsymbol{\theta}_i \sim
#'   N\!\left(\boldsymbol{B}_0 + \sum_{j=1}^{k} \boldsymbol{B}_j x_{ij},
#'   S_i + \Psi\right)}
#'
#' where \eqn{\boldsymbol{B}_j} is the \eqn{p}-vector of regression
#' coefficients for covariate \eqn{j} and \eqn{\Psi} is the residual
#' between-city covariance estimated by REML. Because covariates are
#' standardized before fitting, \eqn{\boldsymbol{B}_0} is the predicted
#' association for a city with mean covariate values.
#'
#' The **significance** of each covariate is assessed by a multivariate Wald
#' test:
#' \deqn{W_j = \hat{\boldsymbol{B}}_j^{\top} \hat{V}_j^{-1}
#'   \hat{\boldsymbol{B}}_j \;\sim\; \chi^2_p}
#'
#' where \eqn{\hat{V}_j} is the estimated covariance of
#' \eqn{\hat{\boldsymbol{B}}_j}.
#'
#' @param fits A named list of `climasus_dlnm` objects, one per city or region.
#'   All fits must use the same `climate_col`, `lag_max`, `argvar`, and
#'   `arglag`. Produced by `sus_mod_dlnm()`.
#' @param covariates A `data.frame` with one row per city and city-level
#'   predictors. City names must be stored as row names (default) or in the
#'   column specified by `city_col`. Cities present in `fits` but absent in
#'   `covariates` are excluded from the meta-regression with a warning.
#' @param covariate_cols Character vector of column names to use as
#'   meta-regression predictors. Default `NULL` uses all numeric columns in
#'   `covariates` (after removing the city identifier column if any).
#' @param city_col Character. Name of the column in `covariates` that holds
#'   city identifiers. Default `NULL` uses row names of `covariates`.
#' @param pred_at Numeric vector of quantile probabilities (0–1) for the
#'   exposure-response summary table. Default `c(0.75, 0.90, 0.95, 0.99)`.
#' @param blup Logical. Compute BLUP city-specific predictions? Default `TRUE`.
#' @param method Character. `mvmeta` estimation method: `"reml"` (default),
#'   `"ml"`, or `"fixed"`.
#' @param alpha Numeric. Significance level for confidence intervals.
#'   Default `0.05`.
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_metaregression` list with:
#'   \describe{
#'     \item{`$mvmeta_fit`}{The `mvmeta` model with covariates.}
#'     \item{`$null_fit`}{Intercept-only `mvmeta` model (for heterogeneity
#'       comparison).}
#'     \item{`$pooled_pred`}{`dlnm::crosspred` for the average city (all
#'       standardized covariates = 0, i.e., the intercept).}
#'     \item{`$blup_preds`}{Named list of per-city BLUP `crosspred` objects
#'       (when `blup = TRUE`).}
#'     \item{`$city_table`}{Tibble: one row per city with raw and BLUP RRs at
#'       the 75th percentile exposure.}
#'     \item{`$covariate_tests`}{Tibble: one row per covariate with Wald
#'       chi-square statistic (df = `n_crossbasis_coef`) and p-value.}
#'     \item{`$heterogeneity`}{Tibble comparing null and full-model heterogeneity:
#'       Cochran Q, df, p-value, I^2, and proportion of heterogeneity
#'       explained by covariates (R^2_het).}
#'     \item{`$cov_scales`}{Tibble with the mean and SD used to standardize
#'       each covariate (for back-transformation).}
#'     \item{`$meta`}{Metadata: `climate_col`, `outcome_col`, `lag_max`,
#'       `n_cities`, `city_names`, `covariate_cols`, `method`, `alpha`,
#'       `call_time`.}
#'   }
#'
#' @references
#' Gasparrini, A., et al. (2012). Multivariate meta-analysis for non-linear
#' and other multi-parameter associations. *Statistics in Medicine*, 31(29),
#' 3821-3839. \doi{10.1002/sim.5471}
#'
#' Berkey, C.S., et al. (1998). Meta-analysis of multiple outcomes by
#' regression with random effects. *Statistics in Medicine*, 17(22),
#' 2537-2550.
#'
#' @examples
#' \dontrun{
#' # City-level covariates: mean annual temperature and poverty index
#' city_meta <- data.frame(
#'   mean_temp  = c(28.5, 22.1, 19.8),
#'   poverty_pct = c(42, 28, 15),
#'   row.names  = c("fortaleza", "belo_horizonte", "curitiba")
#' )
#'
#' mr <- sus_mod_metaregression(fits, city_meta, lang = "en")
#' print(mr)
#' tidy(mr)
#' }
#'
#' @seealso [sus_mod_pool()], [sus_mod_dlnm()], [sus_mod_sensitivity()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_rows
#' @importFrom purrr map list_rbind imap
#' @importFrom glue glue
#' @importFrom stats coef vcov median quantile pchisq qnorm setNames
sus_mod_metaregression <- function(
    fits,
    covariates,
    covariate_cols = NULL,
    city_col       = NULL,
    pred_at        = c(0.75, 0.90, 0.95, 0.99),
    blup           = TRUE,
    method         = "reml",
    alpha          = 0.05,
    lang           = "pt",
    verbose        = TRUE
) {

  # 1. Language
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(
      "Unsupported language {.val {lang}}. Using {.val pt}."
    )
    lang <- "pt"
  }

  if (verbose)
    cli::cli_h1("climasus4r \u2014 Meta-Regression ({length(fits)} cities)")

  # 2. Validate fits
  if (!is.list(fits))
    cli::cli_abort(.smrl("err_not_list", lang))
  if (length(fits) == 0L)
    cli::cli_abort(.smrl("err_empty", lang))

  bad_names <- names(fits)[!vapply(fits, inherits, logical(1L), "climasus_dlnm")]
  if (length(bad_names) > 0L) {
    cli::cli_abort(
      "All elements must be {.cls climasus_dlnm}. Invalid: {.val {bad_names}}."
    )
  }

  city_names <- names(fits)
  if (is.null(city_names) || any(city_names == "")) {
    city_names <- paste0("city_", seq_along(fits))
    names(fits) <- city_names
  }
  n_cities_in <- length(fits)

  # 3. Validate covariates
  if (!is.data.frame(covariates))
    cli::cli_abort(.smrl("err_covariates", lang))

  # Resolve city identifiers in covariates
  if (!is.null(city_col)) {
    if (!city_col %in% names(covariates))
      cli::cli_abort(
        "{.arg city_col} column {.val {city_col}} not found in {.arg covariates}."
      )
    cov_city_ids <- as.character(covariates[[city_col]])
    cov_data     <- covariates[, setdiff(names(covariates), city_col), drop = FALSE]
  } else {
    cov_city_ids <- rownames(covariates)
    cov_data     <- covariates
  }
  rownames(cov_data) <- cov_city_ids

  # Choose covariate columns
  if (is.null(covariate_cols)) {
    covariate_cols <- names(cov_data)[vapply(cov_data, is.numeric, logical(1L))]
  }
  covariate_cols <- intersect(covariate_cols, names(cov_data))

  if (length(covariate_cols) == 0L)
    cli::cli_abort(.smrl("err_covariates", lang))

  cov_matrix <- as.matrix(cov_data[, covariate_cols, drop = FALSE])

  # Match cities: only keep fits that have covariate data
  matched      <- intersect(city_names, cov_city_ids)
  unmatched    <- setdiff(city_names, cov_city_ids)

  if (length(matched) == 0L)
    cli::cli_abort(.smrl("err_no_match", lang))

  if (length(unmatched) > 0L) {
    n_unmatched    <- length(unmatched)
    missing_cities <- paste(unmatched, collapse = ", ")
    cli::cli_alert_warning(
      "{n_unmatched} city/cities in {.arg fits} absent from {.arg covariates}: {missing_cities}. Excluded from meta-regression."
    )
  }

  # Subset fits and covariates to matched cities only
  fits       <- fits[matched]
  city_names <- matched
  n_cities   <- length(fits)
  cov_matrix <- cov_matrix[city_names, , drop = FALSE]

  if (verbose)
    cli::cli_alert_info(glue::glue(.smrl("step_validate", lang)))

  # 4. Compatibility check across fits
  lag_maxes  <- vapply(fits, function(f) f$meta$lag_max,     integer(1L))
  clim_cols  <- vapply(fits, function(f) f$meta$climate_col, character(1L))

  if (length(unique(lag_maxes)) > 1L) {
    field <- "lag_max"
    cli::cli_abort("Incompatible fits: {.field {field}} differs across cities.")
  }
  if (length(unique(clim_cols)) > 1L) {
    field <- "climate_col"
    cli::cli_abort("Incompatible fits: {.field {field}} differs across cities.")
  }

  rlang::check_installed("mvmeta", reason = .smrl("err_no_mvmeta", lang))
  rlang::check_installed("dlnm",   reason = "required for pooled prediction")

  lag_max     <- lag_maxes[[1L]]
  climate_col <- clim_cols[[1L]]
  argvar      <- fits[[1L]]$meta$argvar
  arglag      <- fits[[1L]]$meta$arglag

  # 5. Standardize covariates (mean 0, SD 1) — intercept = average city
  cov_means <- colMeans(cov_matrix, na.rm = TRUE)
  cov_sds   <- apply(cov_matrix, 2, stats::sd, na.rm = TRUE)
  cov_sds[cov_sds == 0] <- 1  # avoid division by zero for constant covariates
  cov_scaled <- scale(cov_matrix, center = cov_means, scale = cov_sds)
  cov_scales <- tibble::tibble(
    covariate = covariate_cols,
    mean      = as.numeric(cov_means),
    sd        = as.numeric(cov_sds)
  )

  n_cov <- length(covariate_cols)

  if (verbose)
    cli::cli_alert_info(glue::glue(.smrl("step_align", lang)))

  # 6. Extract cross-basis coefficients and vcov from each city
  coef_list <- vector("list", n_cities)
  vcov_list <- vector("list", n_cities)

  for (i in seq_len(n_cities)) {
    model_i <- fits[[i]]$model
    cb_nms  <- grep("^cb", names(stats::coef(model_i)), value = TRUE)
    coef_list[[i]] <- stats::coef(model_i)[cb_nms]
    vcov_list[[i]] <- stats::vcov(model_i)[cb_nms, cb_nms, drop = FALSE]
  }

  n_coef   <- length(coef_list[[1L]])
  coef_mat <- do.call(rbind, coef_list)
  rownames(coef_mat) <- city_names

  # 7. Fit null (intercept-only) and full (with covariates) meta-regression
  if (verbose)
    cli::cli_alert_info(glue::glue(.smrl("step_metareg", lang)))

  null_fit <- tryCatch(
    mvmeta::mvmeta(coef_mat, S = vcov_list, method = method),
    error = function(e) {
      cli::cli_alert_warning("Null meta-analysis failed: {conditionMessage(e)}")
      NULL
    }
  )

  # Build formula: coef_mat ~ cov1 + cov2 + ...
  # Include coef_mat in the data list so mvmeta can find it via the formula
  cov_df    <- as.data.frame(cov_scaled)
  data_list <- c(as.list(cov_df), list(coef_mat = coef_mat))
  mr_form   <- stats::as.formula(
    paste("coef_mat ~", paste(covariate_cols, collapse = " + ")),
    env = environment()
  )

  mr_fit <- tryCatch(
    do.call(mvmeta::mvmeta, list(
      formula = mr_form,
      data    = data_list,
      S       = vcov_list,
      method  = method
    )),
    error = function(e) {
      cli::cli_alert_warning(
        "Meta-regression fit failed: {conditionMessage(e)}. Returning null model."
      )
      NULL
    }
  )

  # Fall back to null fit if meta-regression fails
  active_fit <- if (!is.null(mr_fit)) mr_fit else null_fit

  # 8. Pooled prediction (average city = intercept, scaled covariates = 0)
  n_grid <- 100L
  if (verbose)
    cli::cli_alert_info(glue::glue(.smrl("step_pred", lang)))

  # Collect all exposure observations across fitted cities
  all_expo <- unlist(lapply(fits, function(f) {
    col0 <- paste0(f$meta$climate_col, "_lag0")
    if (col0 %in% names(f$data_daily)) f$data_daily[[col0]]
    else f$data_daily[[f$meta$climate_col]]
  }), use.names = FALSE)
  all_expo <- all_expo[is.finite(all_expo)]

  expo_grid <- seq(min(all_expo), max(all_expo), length.out = 100L)
  ref_value <- as.numeric(stats::median(all_expo, na.rm = TRUE))
  cb_ref    <- fits[[1L]]$crossbasis

  # Extract intercept block from the active fit
  coef_intercept <- as.numeric(stats::coef(active_fit))[seq_len(n_coef)]
  vcov_all       <- stats::vcov(active_fit)
  vcov_intercept <- vcov_all[seq_len(n_coef), seq_len(n_coef), drop = FALSE]

  pooled_pred <- tryCatch(
    dlnm::crosspred(
      cb_ref,
      coef       = coef_intercept,
      vcov       = vcov_intercept,
      model.link = "log",
      at         = expo_grid,
      cen        = ref_value
    ),
    error = function(e) {
      cli::cli_alert_warning(
        "Pooled crosspred failed: {conditionMessage(e)}. Returning NULL."
      )
      NULL
    }
  )

  # Exposure-response summary at requested quantiles
  pred_quantiles <- as.numeric(stats::quantile(all_expo, pred_at, na.rm = TRUE))
  expo_resp <- purrr::map(seq_along(pred_at), function(i) {
    val <- pred_quantiles[[i]]
    idx <- which.min(abs(expo_grid - val))
    tibble::tibble(
      pct      = pred_at[[i]],
      exposure = round(val, 3L),
      rr       = if (!is.null(pooled_pred)) as.numeric(pooled_pred$allRRfit[[idx]]) else NA_real_,
      rr_lo    = if (!is.null(pooled_pred)) as.numeric(pooled_pred$allRRlow[[idx]])  else NA_real_,
      rr_hi    = if (!is.null(pooled_pred)) as.numeric(pooled_pred$allRRhigh[[idx]]) else NA_real_
    )
  }) |> purrr::list_rbind()

  # Full 100-point pooled curve
  pooled_curve <- if (!is.null(pooled_pred)) {
    tibble::tibble(
      exposure = as.numeric(pooled_pred$predvar),
      rr       = as.numeric(pooled_pred$allRRfit),
      rr_lo    = as.numeric(pooled_pred$allRRlow),
      rr_hi    = as.numeric(pooled_pred$allRRhigh)
    )
  } else {
    NULL
  }

  # 9. BLUPs
  blup_preds <- NULL
  p75_val    <- as.numeric(stats::quantile(all_expo, 0.75, na.rm = TRUE))
  p75_idx    <- which.min(abs(expo_grid - p75_val))

  city_tbl <- purrr::imap(fits, function(fit_i, city_nm) {
    m_i <- fit_i$models
    tibble::tibble(
      city        = city_nm,
      n_obs       = fit_i$meta$n,
      raw_rr      = m_i$rr,
      raw_rr_lo   = m_i$lo,
      raw_rr_hi   = m_i$hi,
      blup_rr     = NA_real_,
      blup_rr_lo  = NA_real_,
      blup_rr_hi  = NA_real_
    )
  }) |> purrr::list_rbind()

  if (blup && n_cities >= 2L && !is.null(active_fit)) {
    if (verbose)
      cli::cli_alert_info(glue::glue(.smrl("step_blup", lang)))

    blup_res <- tryCatch(
      mvmeta::blup(active_fit, vcov = TRUE),
      error = function(e) NULL
    )

    if (!is.null(blup_res)) {
      blup_preds <- vector("list", n_cities)
      names(blup_preds) <- city_names

      for (i in seq_len(n_cities)) {
        coef_b <- as.numeric(blup_res[[i]]$blup)
        vcov_b <- blup_res[[i]]$vcov

        pred_b <- tryCatch(
          dlnm::crosspred(
            cb_ref,
            coef       = coef_b,
            vcov       = vcov_b,
            model.link = "log",
            at         = expo_grid,
            cen        = ref_value
          ),
          error = function(e) NULL
        )
        blup_preds[[i]] <- pred_b

        if (!is.null(pred_b)) {
          city_tbl$blup_rr[[i]]    <- as.numeric(pred_b$allRRfit[[p75_idx]])
          city_tbl$blup_rr_lo[[i]] <- as.numeric(pred_b$allRRlow[[p75_idx]])
          city_tbl$blup_rr_hi[[i]] <- as.numeric(pred_b$allRRhigh[[p75_idx]])
        }
      }
    }
  }

  # 10. Covariate Wald tests
  if (verbose)
    cli::cli_alert_info(glue::glue(.smrl("step_tests", lang)))

  cov_tests <- .mr_wald_tests(active_fit, covariate_cols, n_coef)

  # 11. Heterogeneity: null vs. full model
  het_tbl <- .mr_heterogeneity(null_fit, active_fit, n_cities, n_coef, n_cov)

  # 12. Done message
  if (verbose && !is.null(pooled_pred)) {
    p75_row <- expo_resp[which.min(abs(expo_resp$pct - 0.75)), ]
    rr  <- round(p75_row$rr,    3)
    lo  <- round(p75_row$rr_lo, 3)
    hi  <- round(p75_row$rr_hi, 3)
    i2  <- if (!is.na(het_tbl$i2[het_tbl$model == "full"])) {
      het_tbl$i2[het_tbl$model == "full"]
    } else "N/A"
    cli::cli_alert_success(glue::glue(.smrl("done", lang)))
  }

  # 13. Assemble output
  structure(
    list(
      mvmeta_fit       = active_fit,
      null_fit         = null_fit,
      pooled_pred      = pooled_pred,
      pooled_curve     = pooled_curve,
      exposure_response = expo_resp,
      blup_preds       = blup_preds,
      city_table       = city_tbl,
      covariate_tests  = cov_tests,
      heterogeneity    = het_tbl,
      cov_scales       = cov_scales,
      meta = list(
        climate_col    = climate_col,
        outcome_col    = fits[[1L]]$meta$outcome_col,
        lag_max        = lag_max,
        n_cities       = n_cities,
        city_names     = city_names,
        covariate_cols = covariate_cols,
        method         = method,
        alpha          = alpha,
        ref_value      = ref_value,
        expo_grid      = expo_grid,
        pred_at        = pred_at,
        call_time      = Sys.time()
      )
    ),
    class = c("climasus_metaregression", "list")
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Multivariate Wald tests for each covariate in a meta-regression fit
#' @keywords internal
#' @noRd
.mr_wald_tests <- function(mr_fit, covariate_cols, n_coef) {
  if (is.null(mr_fit) || length(covariate_cols) == 0L) {
    return(tibble::tibble(
      covariate  = character(0),
      n_df       = integer(0),
      wald_stat  = numeric(0),
      p_value    = numeric(0)
    ))
  }

  all_coef <- tryCatch(as.numeric(stats::coef(mr_fit)), error = function(e) NULL)
  all_vcov <- tryCatch(stats::vcov(mr_fit), error = function(e) NULL)

  if (is.null(all_coef) || is.null(all_vcov)) {
    return(tibble::tibble(
      covariate = covariate_cols,
      n_df      = rep(n_coef, length(covariate_cols)),
      wald_stat = rep(NA_real_,  length(covariate_cols)),
      p_value   = rep(NA_real_,  length(covariate_cols))
    ))
  }

  rows <- lapply(seq_along(covariate_cols), function(j) {
    # Block indices for covariate j (after intercept block)
    idx <- (j * n_coef + 1L):((j + 1L) * n_coef)
    idx <- idx[idx <= length(all_coef)]

    if (length(idx) < n_coef) {
      return(tibble::tibble(
        covariate = covariate_cols[[j]],
        n_df      = n_coef,
        wald_stat = NA_real_,
        p_value   = NA_real_
      ))
    }

    b_j <- all_coef[idx]
    V_j <- all_vcov[idx, idx, drop = FALSE]

    W <- tryCatch(
      as.numeric(t(b_j) %*% solve(V_j) %*% b_j),
      error = function(e) NA_real_
    )
    p <- if (!is.na(W)) stats::pchisq(W, df = n_coef, lower.tail = FALSE) else NA_real_

    tibble::tibble(
      covariate = covariate_cols[[j]],
      n_df      = n_coef,
      wald_stat = round(W, 3),
      p_value   = round(p, 4)
    )
  })

  do.call(rbind, rows)
}

#' Compute heterogeneity statistics for null and full meta-regression models
#' @keywords internal
#' @noRd
.mr_heterogeneity <- function(null_fit, full_fit, n_cities, n_coef, n_cov) {
  .het_row <- function(fit, label) {
    if (is.null(fit)) {
      return(tibble::tibble(
        model   = label,
        Q       = NA_real_,
        df_het  = NA_integer_,
        p_het   = NA_real_,
        i2      = NA_real_,
        r2_het  = NA_real_
      ))
    }
    q_res <- tryCatch(mvmeta::qtest(fit), error = function(e) NULL)
    if (is.null(q_res)) {
      return(tibble::tibble(
        model  = label,
        Q      = NA_real_,
        df_het = NA_integer_,
        p_het  = NA_real_,
        i2     = NA_real_,
        r2_het = NA_real_
      ))
    }
    q_val  <- as.numeric(q_res$Q[[1L]])
    df_val <- as.integer(q_res$df[[1L]])
    p_val  <- as.numeric(q_res$pvalue[[1L]])
    i2_val <- round(max(0, (q_val - df_val) / q_val * 100), 1)
    tibble::tibble(
      model  = label,
      Q      = round(q_val, 2),
      df_het = df_val,
      p_het  = round(p_val, 4),
      i2     = i2_val,
      r2_het = NA_real_
    )
  }

  null_row <- .het_row(null_fit, "null")
  full_row <- .het_row(full_fit, "full")

  # R²_het = proportion of heterogeneity explained by covariates
  if (!is.na(null_row$i2) && !is.na(full_row$i2) && null_row$i2 > 0) {
    r2 <- round(max(0, (null_row$i2 - full_row$i2) / null_row$i2), 3)
    full_row$r2_het <- r2
  }

  dplyr::bind_rows(null_row, full_row)
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_metaregression object
#'
#' @param x A `climasus_metaregression` object from `sus_mod_metaregression()`.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_metaregression <- function(x, ...) {
  m   <- x$meta
  het <- x$heterogeneity
  er  <- x$exposure_response
  ct  <- x$covariate_tests

  cli::cli_h2("climasus_metaregression")
  cli::cli_text("{.strong Exposure}     : {m$climate_col}  (lag 0\u2013{m$lag_max})")
  cli::cli_text("{.strong Outcome}      : {m$outcome_col}")
  cli::cli_text("{.strong Cities}       : {m$n_cities}  ({paste(m$city_names, collapse = ', ')})")
  cli::cli_text("{.strong Covariates}   : {paste(m$covariate_cols, collapse = ', ')}")
  cli::cli_text("{.strong Method}       : {m$method}")
  cli::cli_text("{.strong Ref value}    : {round(m$ref_value, 3)}")
  cli::cli_rule()

  if (!is.null(er) && nrow(er) > 0L) {
    p75_row <- er[which.min(abs(er$pct - 0.75)), ]
    cli::cli_text(
      "RR avg city (p75): {round(p75_row$rr, 4)} [{round(p75_row$rr_lo, 4)}, {round(p75_row$rr_hi, 4)}]"
    )
  }

  if (!is.null(het) && nrow(het) >= 2L) {
    full_row <- het[het$model == "full", ]
    null_row <- het[het$model == "null", ]
    if (!is.na(full_row$i2)) {
      cli::cli_text(
        "Residual I\u00b2       : {full_row$i2}%  (null: {null_row$i2}%, R\u00b2_het = {full_row$r2_het})"
      )
    }
  }

  if (!is.null(ct) && nrow(ct) > 0L) {
    cli::cli_text("\nCovariate Wald tests (df = n_crossbasis_coef):")
    print(ct)
  }

  invisible(x)
}

#' Summarise a climasus_metaregression object
#'
#' @param object A `climasus_metaregression` object from `sus_mod_metaregression()`.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_metaregression <- function(object, ...) {
  print(object)
  cat("\n-- Exposure-Response (average city) --\n")
  print(object$exposure_response)
  cat("\n-- Heterogeneity (null vs. full) --\n")
  print(object$heterogeneity)
  cat("\n-- City table --\n")
  print(object$city_table)
  if (!is.null(object$mvmeta_fit)) {
    cat("\n-- mvmeta fit summary --\n")
    print(summary(object$mvmeta_fit))
  }
  invisible(object)
}

#' Extract pooled coefficients from a climasus_metaregression object
#'
#' @param object A `climasus_metaregression` object.
#' @param ... Passed to [stats::coef()].
#' @return Named numeric vector of meta-regression coefficients.
#' @export
coef.climasus_metaregression <- function(object, ...) {
  if (is.null(object$mvmeta_fit)) return(NULL)
  stats::coef(object$mvmeta_fit, ...)
}

#' Extract variance-covariance from a climasus_metaregression object
#'
#' @param object A `climasus_metaregression` object.
#' @param ... Passed to [stats::vcov()].
#' @return Variance-covariance matrix of meta-regression coefficients.
#' @export
vcov.climasus_metaregression <- function(object, ...) {
  if (is.null(object$mvmeta_fit)) return(NULL)
  stats::vcov(object$mvmeta_fit, ...)
}

#' Tidy a climasus_metaregression object into a summary tibble
#'
#' Returns a one-row tibble summarising the meta-regression result plus
#' a `covariate_tests` column containing the nested covariate test table.
#' Suitable for combining results across multiple analyses.
#'
#' @param x A `climasus_metaregression` object from `sus_mod_metaregression()`.
#' @param ... Unused.
#' @return A one-row tibble.
#' @export
#' @importFrom generics tidy
tidy.climasus_metaregression <- function(x, ...) {
  m    <- x$meta
  het  <- x$heterogeneity
  er   <- x$exposure_response

  p75_row <- if (!is.null(er) && nrow(er) > 0L)
    er[which.min(abs(er$pct - 0.75)), ]
  else
    tibble::tibble(rr = NA_real_, rr_lo = NA_real_, rr_hi = NA_real_)

  full_het  <- if (!is.null(het)) het[het$model == "full", ] else
    tibble::tibble(Q = NA_real_, df_het = NA_integer_, p_het = NA_real_, i2 = NA_real_, r2_het = NA_real_)
  null_het  <- if (!is.null(het)) het[het$model == "null", ] else
    tibble::tibble(i2 = NA_real_)

  tibble::tibble(
    climate_col    = m$climate_col,
    outcome_col    = m$outcome_col,
    lag_max        = m$lag_max,
    method         = m$method,
    n_cities       = m$n_cities,
    n_covariates   = length(m$covariate_cols),
    covariate_cols = paste(m$covariate_cols, collapse = ", "),
    ref_value      = round(m$ref_value, 3),
    rr_p75         = round(p75_row$rr,    4),
    rr_lo_p75      = round(p75_row$rr_lo, 4),
    rr_hi_p75      = round(p75_row$rr_hi, 4),
    i2_null        = null_het$i2,
    i2_full        = full_het$i2,
    r2_het         = full_het$r2_het,
    Q_full         = full_het$Q,
    df_het         = full_het$df_het,
    p_het          = full_het$p_het
  )
}
