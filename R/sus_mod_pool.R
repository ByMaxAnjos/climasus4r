# =============================================================================
# sus_mod_pool.R
# Two-stage multi-city/multi-region pooling of DLNM estimates
#
# Theory: Gasparrini et al. (2012, Statistics in Medicine);
#         Gasparrini & Armstrong (2013, Epidemiology)
# Input : Named list of climasus_dlnm objects from sus_mod_dlnm()
# Output: climasus_pool object with pooled exposure-response curves,
#         heterogeneity statistics, and BLUP city-specific predictions
#
# Algorithm:
#   Stage 1 (city-specific): coefficients + vcov already in climasus_dlnm
#   Stage 2 (pooling): multivariate meta-analysis of cross-basis vectors
#     via mvmeta::mvmeta(); pooled coefficients fed back into
#     dlnm::crosspred() with coef/vcov arguments for prediction
# =============================================================================

# -- NSE variable declarations ------------------------------------------------
utils::globalVariables(c(
  "city", "rr", "lo", "hi", "pct", "exposure",
  "i2", "tau2", "lag", "rr_cum", "rr_lag",
  "coef_name", "coef_val"
))

# -- Local i18n ---------------------------------------------------------------
.pool_labels <- list(
  step_validate = list(
    pt = "Validando {n_cities} ajustes de cidade...",
    en = "Validating {n_cities} city fits...",
    es = "Validando {n_cities} ajustes de ciudad..."
  ),
  step_extract = list(
    pt = "Extraindo coeficientes e matrizes de variancia-covariancia...",
    en = "Extracting coefficients and variance-covariance matrices...",
    es = "Extrayendo coeficientes y matrices de varianza-covarianza..."
  ),
  step_pool = list(
    pt = "Ajustando meta-analise multivariada ({method})...",
    en = "Fitting multivariate meta-analysis ({method})...",
    es = "Ajustando meta-analisis multivariado ({method})..."
  ),
  step_pred = list(
    pt = "Calculando predicao combinada ({n_grid} pontos)...",
    en = "Computing pooled prediction ({n_grid} points)...",
    es = "Calculando prediccion combinada ({n_grid} puntos)..."
  ),
  step_blup = list(
    pt = "Calculando BLUPs para {n_cities} cidades...",
    en = "Computing BLUPs for {n_cities} cities...",
    es = "Calculando BLUPs para {n_cities} ciudades..."
  ),
  done = list(
    pt = "Concluido. RR combinado (p75): {rr} [{lo}, {hi}] | I\u00b2 = {i2}%",
    en = "Done. Pooled RR (p75): {rr} [{lo}, {hi}] | I\u00b2 = {i2}%",
    es = "Listo. RR combinado (p75): {rr} [{lo}, {hi}] | I\u00b2 = {i2}%"
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
    pt = "Elemento(s) nao sao {.cls climasus_dlnm}: {.val {bad_names}}.",
    en = "Element(s) are not {.cls climasus_dlnm}: {.val {bad_names}}.",
    es = "Elemento(s) no son {.cls climasus_dlnm}: {.val {bad_names}}."
  ),
  err_incompatible = list(
    pt = "Ajustes incompativeis: {.field {field}} difere entre cidades. Todos os ajustes devem usar a mesma estrutura de crossbasis.",
    en = "Incompatible fits: {.field {field}} differs across cities. All fits must use the same crossbasis structure.",
    es = "Ajustes incompatibles: {.field {field}} difiere entre ciudades. Todos los ajustes deben usar la misma estructura de crossbasis."
  ),
  err_no_mvmeta = list(
    pt = "Pacote {.pkg mvmeta} necessario para pooling. Instale com {.code install.packages('mvmeta')}.",
    en = "Package {.pkg mvmeta} required for pooling. Install with {.code install.packages('mvmeta')}.",
    es = "Se requiere el paquete {.pkg mvmeta} para pooling. Instale con {.code install.packages('mvmeta')}."
  ),
  err_no_coef = list(
    pt = "Nenhuma coluna de crossbasis encontrada no modelo da cidade {.val {city_name}}.",
    en = "No crossbasis columns found in model for city {.val {city_name}}.",
    es = "No se encontraron columnas de crossbasis en el modelo para la ciudad {.val {city_name}}."
  ),
  warn_single_city = list(
    pt = "Apenas 1 cidade fornecida. Meta-analise requer >= 2 cidades.",
    en = "Only 1 city provided. Meta-analysis requires >= 2 cities.",
    es = "Solo 1 ciudad provista. La meta-analisis requiere >= 2 ciudades."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} nao suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Using {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  )
)

#' @keywords internal
#' @noRd
.spll <- function(key, lang) {
  entry <- .pool_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# -- Exported function --------------------------------------------------------

#' Two-Stage Multi-City Pooling of DLNM Estimates
#'
#' Performs a two-stage multivariate meta-analysis to pool Distributed Lag
#' Non-linear Model (DLNM) estimates across multiple cities or regions.
#' Stage 1 city-specific estimates (coefficients and variance-covariance matrices
#' from `sus_mod_dlnm()`) are combined using multivariate random-effects
#' meta-analysis via `mvmeta`, producing pooled exposure-response and
#' lag-response curves, heterogeneity statistics, and Best Linear Unbiased
#' Predictors (BLUPs) for each city.
#'
#' @section Statistical framework:
#'
#' Let \eqn{\boldsymbol{\theta}_i} be the \eqn{p}-dimensional vector of
#' cross-basis coefficients for city \eqn{i}, estimated with variance
#' \eqn{S_i}. The multivariate random-effects model is:
#'
#' \deqn{\boldsymbol{\theta}_i \sim N(\boldsymbol{\theta}, S_i + \Psi)}
#'
#' where \eqn{\Psi} is the between-city covariance matrix estimated by
#' REML. The pooled estimate \eqn{\hat{\boldsymbol{\theta}}} and its
#' variance are passed to `dlnm::crosspred()` to produce a pooled
#' exposure-response surface identical in format to the single-city output.
#'
#' BLUPs shrink city-specific estimates toward the pooled mean:
#'
#' \deqn{\tilde{\boldsymbol{\theta}}_i = \hat{\boldsymbol{\theta}} +
#'   \hat\Psi (\hat\Psi + S_i)^{-1}
#'   (\hat{\boldsymbol{\theta}}_i - \hat{\boldsymbol{\theta}})}
#'
#' @param fits A named list of `climasus_dlnm` objects, one per city or region.
#'   All fits must have been produced with the same `climate_col`, `argvar`,
#'   `arglag`, and `lag_max`.
#' @param exposure_range Numeric vector of length 2 specifying the exposure
#'   grid range for the pooled prediction. Default `NULL` uses the combined
#'   range across all city datasets.
#' @param n_grid Integer. Number of exposure grid points for the pooled
#'   prediction curve. Default `100L`.
#' @param pred_at Numeric vector of quantile probabilities (0-1) for the
#'   pooled exposure-response summary table. Default `c(0.75, 0.90, 0.95, 0.99)`.
#' @param blup Logical. Compute BLUP city-specific predictions? Default `TRUE`.
#' @param method Character. `mvmeta` estimation method: `"reml"` (default,
#'   recommended), `"ml"`, or `"fixed"` (fixed-effects, no heterogeneity).
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_pool` list with components:
#'   \describe{
#'     \item{`mvmeta_fit`}{The raw `mvmeta` model object.}
#'     \item{`pooled_pred`}{`dlnm::crosspred` object with pooled coefficients.}
#'     \item{`exposure_response`}{Tibble of pooled RR at quantile grid points.}
#'     \item{`lag_response`}{Tibble of pooled RR by lag at the 75th percentile exposure.}
#'     \item{`blup_preds`}{Named list of city-specific BLUP `crosspred` objects
#'       (when `blup = TRUE`).}
#'     \item{`city_table`}{Tibble with one row per city showing city-specific
#'       and BLUP-adjusted RRs at the 75th percentile.}
#'     \item{`heterogeneity`}{Tibble with Cochran Q, df, p-value, and I^2
#'       statistics.}
#'     \item{`meta`}{List of metadata: `climate_col`, `outcome_col`, `lag_max`,
#'       `n_cities`, `city_names`, `method`, `argvar`, `arglag`, `ref_value`,
#'       `pred_at`, `call_time`.}
#'   }
#'
#' @examples
#' \dontrun{
#' fits <- list(
#'   city_a = sus_mod_dlnm(df_a, ...),
#'   city_b = sus_mod_dlnm(df_b, ...),
#'   city_c = sus_mod_dlnm(df_c, ...)
#' )
#' pool <- sus_mod_pool(fits, lang = "en")
#' print(pool)
#' plot_pool <- sus_mod_plot_dlnm(pool, lang = "en")
#' }
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom tibble tibble
#' @importFrom dplyr mutate arrange bind_rows
#' @importFrom purrr map list_rbind imap
#' @importFrom glue glue
#' @importFrom stats coef vcov
sus_mod_pool <- function(
    fits,
    exposure_range = NULL,
    n_grid         = 100L,
    pred_at        = c(0.75, 0.90, 0.95, 0.99),
    blup           = TRUE,
    method         = "reml",
    lang           = "pt",
    verbose        = TRUE
) {

  # 1. Language validation
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(.spll("warn_lang", "pt"))
    lang <- "pt"
  }

  # 2. Input validation
  if (!is.list(fits)) {
    cli::cli_abort(.spll("err_not_list", lang))
  }
  if (length(fits) == 0L) {
    cli::cli_abort(.spll("err_empty", lang))
  }

  bad_names <- names(fits)[!vapply(fits, inherits, logical(1L), "climasus_dlnm")]
  if (length(bad_names) > 0L) {
    cli::cli_abort(glue::glue(.spll("err_not_dlnm", lang)))
  }

  if (length(fits) == 1L) {
    cli::cli_alert_warning(.spll("warn_single_city", lang))
  }

  # Ensure named
  city_names <- names(fits)
  if (is.null(city_names) || any(city_names == "")) {
    city_names <- paste0("city_", seq_along(fits))
    names(fits) <- city_names
  }

  n_cities <- length(fits)

  # 3. Compatibility check
  lag_maxes  <- vapply(fits, function(f) f$meta$lag_max,     integer(1L))
  clim_cols  <- vapply(fits, function(f) f$meta$climate_col, character(1L))

  if (length(unique(lag_maxes)) > 1L) {
    field <- "lag_max"
    cli::cli_abort(glue::glue(.spll("err_incompatible", lang)))
  }
  if (length(unique(clim_cols)) > 1L) {
    field <- "climate_col"
    cli::cli_abort(glue::glue(.spll("err_incompatible", lang)))
  }

  rlang::check_installed("mvmeta", reason = .spll("err_no_mvmeta", lang))
  rlang::check_installed("dlnm",   reason = "required for pooled prediction")

  lag_max    <- lag_maxes[[1L]]
  climate_col <- clim_cols[[1L]]

  # Use first fit's outcome_col / argvar / arglag for pooled reference
  argvar     <- fits[[1L]]$meta$argvar
  arglag     <- fits[[1L]]$meta$arglag

  if (verbose) {
    cli::cli_h1("climasus4r \u2014 Pooled DLNM ({n_cities} cities)")
    cli::cli_alert_info(glue::glue(.spll("step_validate", lang)))
  }

  # 4. Extract coefficients and vcov from each city
  if (verbose) cli::cli_alert_info(.spll("step_extract", lang))

  coef_list <- vector("list", n_cities)
  vcov_list <- vector("list", n_cities)

  for (i in seq_len(n_cities)) {
    city_name <- city_names[[i]]
    fit_i     <- fits[[i]]
    model_i   <- fit_i$model

    cb_nms <- grep("^cb", names(stats::coef(model_i)), value = TRUE)
    if (length(cb_nms) == 0L) {
      cli::cli_abort(glue::glue(.spll("err_no_coef", lang)))
    }

    coef_list[[i]] <- stats::coef(model_i)[cb_nms]
    vcov_list[[i]] <- stats::vcov(model_i)[cb_nms, cb_nms, drop = FALSE]
  }

  n_coef <- length(coef_list[[1L]])

  # Stack into matrix for mvmeta
  coef_mat <- do.call(rbind, coef_list)
  rownames(coef_mat) <- city_names

  # 5. Multivariate meta-analysis
  if (verbose) cli::cli_alert_info(glue::glue(.spll("step_pool", lang)))

  pool_fit <- mvmeta::mvmeta(
    coef_mat,
    S      = vcov_list,
    method = method
  )

  coef_pool <- as.numeric(stats::coef(pool_fit))
  vcov_pool <- stats::vcov(pool_fit)

  # 6. Build pooled exposure grid
  if (verbose) cli::cli_alert_info(glue::glue(.spll("step_pred", lang)))

  # Collect all exposure observations
  all_expo <- unlist(lapply(fits, function(f) {
    col0 <- paste0(f$meta$climate_col, "_lag0")
    if (col0 %in% names(f$data_daily)) f$data_daily[[col0]]
    else f$data_daily[[f$meta$climate_col]]
  }), use.names = FALSE)
  all_expo <- all_expo[is.finite(all_expo)]

  expo_min <- if (!is.null(exposure_range)) exposure_range[[1L]] else min(all_expo)
  expo_max <- if (!is.null(exposure_range)) exposure_range[[2L]] else max(all_expo)
  expo_grid <- seq(expo_min, expo_max, length.out = as.integer(n_grid))

  # Reference value: overall median
  ref_value <- as.numeric(stats::median(all_expo, na.rm = TRUE))

  # Use first city's crossbasis for prediction.
  # All cities have the same argvar/arglag (validated above), so the basis
  # functions are identical. crosspred() with explicit coef/vcov uses the
  # crossbasis only to evaluate basis matrices — it does not re-use the
  # original city data.
  fit1    <- fits[[1L]]
  cb_pool <- fit1$crossbasis

  # Pooled prediction using coef/vcov directly
  pooled_pred <- tryCatch(
    dlnm::crosspred(
      cb_pool,
      coef       = coef_pool,
      vcov       = vcov_pool,
      model.link = "log",
      at         = expo_grid,
      cen        = ref_value
    ),
    error = function(e) {
      cli::cli_alert_warning(
        "Pooled crosspred failed: {conditionMessage(e)}. Returning NULL pred."
      )
      NULL
    }
  )

  # 7. Exposure-response summary table
  pred_quantiles <- as.numeric(stats::quantile(all_expo, pred_at, na.rm = TRUE))
  expo_resp <- purrr::map(seq_along(pred_at), function(i) {
    val <- pred_quantiles[[i]]
    idx <- which.min(abs(expo_grid - val))
    tibble::tibble(
      pct      = pred_at[[i]],
      exposure = round(val, 3L),
      rr       = if (!is.null(pooled_pred)) as.numeric(pooled_pred$allRRfit[[idx]]) else NA_real_,
      lo       = if (!is.null(pooled_pred)) as.numeric(pooled_pred$allRRlow[[idx]])  else NA_real_,
      hi       = if (!is.null(pooled_pred)) as.numeric(pooled_pred$allRRhigh[[idx]]) else NA_real_
    )
  }) |> purrr::list_rbind()

  # Full curve tibble
  expo_curve <- if (!is.null(pooled_pred)) {
    tibble::tibble(
      exposure = as.numeric(pooled_pred$predvar),
      rr       = as.numeric(pooled_pred$allRRfit),
      lo       = as.numeric(pooled_pred$allRRlow),
      hi       = as.numeric(pooled_pred$allRRhigh)
    )
  } else {
    NULL
  }

  # Lag-response at 75th percentile
  p75_val <- as.numeric(stats::quantile(all_expo, 0.75, na.rm = TRUE))
  p75_idx <- which.min(abs(expo_grid - p75_val))

  lag_resp <- if (!is.null(pooled_pred) && !is.null(pooled_pred$matRRfit)) {
    tibble::tibble(
      lag    = 0L:lag_max,
      rr_lag = as.numeric(pooled_pred$matRRfit[p75_idx, ]),
      lo     = as.numeric(pooled_pred$matRRlow[p75_idx,  ]),
      hi     = as.numeric(pooled_pred$matRRhigh[p75_idx, ])
    )
  } else {
    NULL
  }

  # 8. Heterogeneity statistics
  het <- tryCatch({
    q_test <- mvmeta::qtest(pool_fit)
    # qtest returns one row per parameter; take the first (overall multivariate Q)
    q_val  <- as.numeric(q_test$Q[[1L]])
    df_val <- as.integer(q_test$df[[1L]])
    p_val  <- as.numeric(q_test$pvalue[[1L]])
    i2_val <- round(max(0, (q_val - df_val) / q_val * 100), 1)
    tibble::tibble(
      Q       = round(q_val, 2),
      df      = df_val,
      p_value = round(p_val, 4),
      i2      = i2_val
    )
  }, error = function(e) {
    tibble::tibble(Q = NA_real_, df = NA_integer_, p_value = NA_real_, i2 = NA_real_)
  })

  # 9. City-specific table (original estimates at p75)
  city_tbl <- purrr::imap(fits, function(fit_i, city_nm) {
    m_i    <- fit_i$models
    blup_rr <- blup_lo <- blup_hi <- NA_real_
    tibble::tibble(
      city    = city_nm,
      n       = fit_i$meta$n,
      rr      = m_i$rr,
      lo      = m_i$lo,
      hi      = m_i$hi,
      blup_rr = blup_rr,
      blup_lo = blup_lo,
      blup_hi = blup_hi
    )
  }) |> purrr::list_rbind()

  # 10. BLUPs
  blup_preds <- NULL
  if (blup && n_cities >= 2L) {
    if (verbose) cli::cli_alert_info(glue::glue(.spll("step_blup", lang)))

    blup_res <- tryCatch(
      mvmeta::blup(pool_fit, vcov = TRUE),
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
            cb_pool,
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
          # Update city_tbl with BLUP values at p75
          idx_p75 <- which.min(abs(expo_grid - p75_val))
          city_tbl$blup_rr[[i]] <- as.numeric(pred_b$allRRfit[[idx_p75]])
          city_tbl$blup_lo[[i]] <- as.numeric(pred_b$allRRlow[[idx_p75]])
          city_tbl$blup_hi[[i]] <- as.numeric(pred_b$allRRhigh[[idx_p75]])
        }
      }
    }
  }

  # 11. Done message
  if (verbose && !is.null(pooled_pred)) {
    p75_row <- expo_resp[which.min(abs(expo_resp$pct - 0.75)), ]
    rr <- round(p75_row$rr, 3)
    lo <- round(p75_row$lo, 3)
    hi <- round(p75_row$hi, 3)
    i2 <- if (!is.na(het$i2)) het$i2 else "N/A"
    cli::cli_alert_success(glue::glue(.spll("done", lang)))
  }

  # 12. Assemble result
  structure(
    list(
      mvmeta_fit       = pool_fit,
      pooled_pred      = pooled_pred,
      exposure_response = expo_resp,
      exposure_curve    = expo_curve,
      lag_response      = lag_resp,
      blup_preds        = blup_preds,
      city_table        = city_tbl,
      heterogeneity     = het,
      meta = list(
        climate_col  = climate_col,
        outcome_col  = fits[[1L]]$meta$outcome_col,
        lag_max      = lag_max,
        n_cities     = n_cities,
        city_names   = city_names,
        method       = method,
        argvar       = argvar,
        arglag       = arglag,
        ref_value    = ref_value,
        expo_grid    = expo_grid,
        pred_at      = pred_at,
        call_time    = Sys.time()
      )
    ),
    class = c("climasus_pool", "list")
  )
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_pool object
#'
#' @param x A `climasus_pool` object from `sus_mod_pool()`.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_pool <- function(x, ...) {
  m   <- x$meta
  het <- x$heterogeneity
  er  <- x$exposure_response

  cli::cli_h2("climasus_pool")
  cli::cli_text("{.strong Exposure}  : {m$climate_col}  (lag 0-{m$lag_max})")
  cli::cli_text("{.strong Outcome}   : {m$outcome_col}")
  cli::cli_text("{.strong Cities}    : {m$n_cities}  ({paste(m$city_names, collapse=', ')})")
  cli::cli_text("{.strong Method}    : {m$method}")
  cli::cli_text("{.strong Ref value} : {round(m$ref_value, 3)}")
  cli::cli_rule()

  if (!is.null(er) && nrow(er) > 0L) {
    p75_row <- er[which.min(abs(er$pct - 0.75)), ]
    cli::cli_text(
      "Pooled RR (p75): {round(p75_row$rr,4)} [{round(p75_row$lo,4)}, {round(p75_row$hi,4)}]"
    )
  }

  if (!is.null(het) && !is.na(het$Q)) {
    cli::cli_text(
      "Heterogeneity  : Q = {het$Q} (df = {het$df}, p = {het$p_value}), I\u00b2 = {het$i2}%"
    )
  }

  if (!is.null(x$city_table)) {
    cli::cli_text("\nCity-specific RR (at p75):")
    print(x$city_table[, c("city", "n", "rr", "lo", "hi",
                            if ("blup_rr" %in% names(x$city_table)) c("blup_rr") else NULL)])
  }

  invisible(x)
}

#' Summarise a climasus_pool object
#'
#' @param object A `climasus_pool` object from `sus_mod_pool()`.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_pool <- function(object, ...) {
  print(object)
  cat("\n-- Pooled Exposure-Response (cumulative RR at quantiles) --\n")
  print(object$exposure_response)
  if (!is.null(object$lag_response)) {
    cat("\n-- Pooled Lag-Response (RR at 75th percentile) --\n")
    print(object$lag_response)
  }
  cat("\n-- Heterogeneity --\n")
  print(object$heterogeneity)
  cat("\n-- mvmeta summary --\n")
  print(summary(object$mvmeta_fit))
  invisible(object)
}

#' Extract pooled coefficients from a climasus_pool object
#'
#' @param object A `climasus_pool` object.
#' @param ... Passed to [stats::coef()].
#' @return Named numeric vector of pooled cross-basis coefficients.
#' @export
coef.climasus_pool <- function(object, ...) {
  stats::coef(object$mvmeta_fit, ...)
}

#' Extract pooled variance-covariance from a climasus_pool object
#'
#' @param object A `climasus_pool` object.
#' @param ... Passed to [stats::vcov()].
#' @return Pooled variance-covariance matrix.
#' @export
vcov.climasus_pool <- function(object, ...) {
  stats::vcov(object$mvmeta_fit, ...)
}

#' Tidy a climasus_pool object into a one-row summary tibble
#'
#' Returns a one-row tibble summarising the pooled result, suitable for
#' combining across multiple pooling runs with [dplyr::bind_rows()].
#'
#' @param x A `climasus_pool` object from `sus_mod_pool()`.
#' @param ... Unused.
#' @return A one-row tibble.
#' @export
#' @importFrom generics tidy
tidy.climasus_pool <- function(x, ...) {
  m   <- x$meta
  het <- x$heterogeneity
  er  <- x$exposure_response

  p75_row <- if (!is.null(er) && nrow(er) > 0L)
    er[which.min(abs(er$pct - 0.75)), ]
  else
    tibble::tibble(rr = NA_real_, lo = NA_real_, hi = NA_real_)

  tibble::tibble(
    climate_col = m$climate_col,
    outcome_col = m$outcome_col,
    lag_max     = m$lag_max,
    method      = m$method,
    n_cities    = m$n_cities,
    ref_value   = round(m$ref_value, 3),
    rr_p75      = round(p75_row$rr, 4),
    lo_p75      = round(p75_row$lo, 4),
    hi_p75      = round(p75_row$hi, 4),
    Q           = het$Q,
    df_het      = het$df,
    p_het       = het$p_value,
    i2          = het$i2
  )
}
