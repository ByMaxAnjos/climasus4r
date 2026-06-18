# =============================================================================
# sus_mod_its.R
# Interrupted Time Series (ITS) Analysis for Climate-Health Interventions
#
# Theory:
#   Segmented Poisson regression: Bernal et al. (2017, BMJ);
#   Penfold & Zhang (2013, Gen Hosp Psychiatry);
#   Hutcheon et al. (2009, BMC Med Res Methodol)
# Input : climasus_df at stage "aggregate" or "climate", or plain data.frame
#         with date and outcome count columns.
# Output: climasus_its object (model, effects table, counterfactual, segments)
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "date", "outcome_val", "t_num",
  "segment", "start_date", "end_date",
  "mean_observed", "mean_predicted", "mean_counterfactual",
  "observed", "predicted", "counterfactual", "cf_lo", "cf_hi",
  "ratio_to_cf", "prevented",
  "term", "label", "estimate", "level_ratio", "level_ci_lo", "level_ci_hi",
  "level_p", "slope_daily_log", "slope_ratio_annual", "slope_p"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.its_labels <- list(
  step_validate = list(
    pt = "Validando entradas...",
    en = "Validating inputs...",
    es = "Validando entradas..."
  ),
  step_build = list(
    pt = "Construindo dataset ITS ({n_obs} dias, {n_int} intervencao(oes))...",
    en = "Building ITS dataset ({n_obs} days, {n_int} interruption(s))...",
    es = "Construyendo dataset ITS ({n_obs} dias, {n_int} interrupcion(es))..."
  ),
  step_fit = list(
    pt = "Ajustando modelo segmentado ({family})...",
    en = "Fitting segmented model ({family})...",
    es = "Ajustando modelo segmentado ({family})..."
  ),
  step_cf = list(
    pt = "Calculando contrafactual (tendencia pre-intervencao projetada)...",
    en = "Computing counterfactual (pre-interruption trend projected forward)...",
    es = "Calculando contrafactual (tendencia pre-intervencion proyectada)..."
  ),
  done = list(
    pt = "Concluido. Efeito imediato: RR = {rr} [{rr_lo}, {rr_hi}] na intervencao {idx}",
    en = "Done. Immediate effect: RR = {rr} [{rr_lo}, {rr_hi}] at interruption {idx}",
    es = "Listo. Efecto inmediato: RR = {rr} [{rr_lo}, {rr_hi}] en la interrupcion {idx}"
  ),
  err_no_date = list(
    pt = "Coluna de data {.val {date_col}} nao encontrada nos dados.",
    en = "Date column {.val {date_col}} not found in data.",
    es = "Columna de fecha {.val {date_col}} no encontrada en los datos."
  ),
  err_no_outcome = list(
    pt = "Coluna de desfecho {.val {outcome_col}} nao encontrada. Disponiveis: {.val {avail}}.",
    en = "Outcome column {.val {outcome_col}} not found. Available: {.val {avail}}.",
    es = "Columna de resultado {.val {outcome_col}} no encontrada. Disponibles: {.val {avail}}."
  ),
  err_no_interruptions = list(
    pt = "{.arg interruption_dates} deve conter pelo menos uma data.",
    en = "{.arg interruption_dates} must contain at least one date.",
    es = "{.arg interruption_dates} debe contener al menos una fecha."
  ),
  err_int_outside = list(
    pt = "Todas as datas de intervencao devem estar dentro do intervalo de dados ({d_min} a {d_max}).",
    en = "All interruption dates must be within the data range ({d_min} to {d_max}).",
    es = "Todas las fechas de interrupcion deben estar dentro del rango de datos ({d_min} a {d_max})."
  ),
  err_bad_family = list(
    pt = "{.arg family} deve ser 'quasipoisson' ou 'poisson'.",
    en = "{.arg family} must be 'quasipoisson' or 'poisson'.",
    es = "{.arg family} debe ser 'quasipoisson' o 'poisson'."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} nao suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Using {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  ),
  warn_short_pre = list(
    pt = "Periodo pre-intervencao curto ({n} dias). Recomenda-se >= 365 dias para tendencia estavel.",
    en = "Short pre-interruption period ({n} days). At least 365 days recommended for a stable trend.",
    es = "Periodo pre-intervencion corto ({n} dias). Se recomiendan >= 365 dias para una tendencia estable."
  )
)

#' @keywords internal
#' @noRd
.itsl <- function(key, lang, ...) {
  entry <- .its_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}


# ── Exported function ─────────────────────────────────────────────────────────

#' Interrupted Time Series Analysis for Health Outcome Counts
#'
#' Fits a segmented quasi-Poisson regression to estimate the immediate and
#' sustained effects of one or more interruptions (policies, extreme events,
#' pandemics) on a daily health outcome count series. The model decomposes
#' each interruption into a **level change** (immediate step) and a **slope
#' change** (sustained trend shift), controlling for the underlying time trend
#' and seasonal variation via harmonic terms. A counterfactual projection shows
#' what would have been expected had the interruption not occurred.
#'
#' @section Statistical framework:
#'
#' For \eqn{K} interruption dates \eqn{T_1 < T_2 < \ldots < T_K}, the
#' segmented Poisson GLM is:
#'
#' \deqn{\log(E[Y_t]) = \beta_0 + \beta_1 t
#'   + \sum_{j=1}^{K}\left[\beta_{2j}\, D_j(t)
#'                        + \beta_{2j+1}\, S_j(t)\right]
#'   + \sum_{k=1}^{H}\left[\gamma_k \sin\!\left(\tfrac{2\pi k\, t}{365.25}\right)
#'                        + \delta_k \cos\!\left(\tfrac{2\pi k\, t}{365.25}\right)\right]
#'   + \mathbf{z}_t^\top \boldsymbol{\eta}}
#'
#' where \eqn{D_j(t) = \mathbf{1}(t \ge T_j)} is the step indicator and
#' \eqn{S_j(t) = \max(0,\, t - T_j)} is the slope-change ("hockey stick")
#' basis. The immediate rate ratio at interruption \eqn{j} is
#' \eqn{RR_j = e^{\beta_{2j}}}; the sustained trend shift is
#' \eqn{e^{\beta_{2j+1}}} per day (or \eqn{e^{365.25\,\beta_{2j+1}}} per year).
#'
#' @section Counterfactual:
#'
#' The counterfactual is the model prediction with all \eqn{D_j} and \eqn{S_j}
#' set to zero — i.e., the pre-interruption baseline trend projected forward
#' through the study period. The difference `observed - counterfactual` is the
#' estimated total attributable change.
#'
#' @param data A `climasus_df` at stage `"aggregate"` or `"climate"`, or any
#'   `data.frame` with a date column and an integer count column.
#' @param outcome_col Character. Name of the daily health outcome count column.
#'   Default: `"n_obitos"`.
#' @param date_col Character. Name of the date column. Default: `"date"`.
#' @param interruption_dates Date or character vector of at least one
#'   interruption date (ISO `"YYYY-MM-DD"` format). Must lie strictly within
#'   the data range. For a single interruption pass a scalar;
#'   for multiple, pass a vector.
#' @param harmonics Integer. Number of sinusoidal harmonic pairs for seasonal
#'   control (\eqn{H} in the model above). `0` suppresses seasonal terms.
#'   Default: `2L`.
#' @param family Character. GLM family: `"quasipoisson"` (default, robust to
#'   overdispersion) or `"poisson"`.
#' @param covariates Character vector or `NULL`. Names of additional columns to
#'   include as linear confounders (e.g., `"rh_pct"`, `"holiday"`). Must be
#'   numeric or dummy-coded. Default: `NULL`.
#' @param alpha Numeric. Significance level for confidence intervals.
#'   Default: `0.05` (95% CI).
#' @param counterfactual Logical. Compute and return counterfactual predictions.
#'   Default: `TRUE`.
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_its` list with:
#'   \describe{
#'     \item{`$model`}{The fitted `glm` object.}
#'     \item{`$effects`}{Tibble with one row per interruption: `label`,
#'       `interruption_date`, `level_ratio`, `level_ci_lo`, `level_ci_hi`,
#'       `level_p`, `slope_daily_log`, `slope_ratio_annual`, `slope_p`.}
#'     \item{`$counterfactual`}{Tibble with daily `date`, `observed`,
#'       `predicted` (fitted values), `counterfactual`, `cf_lo`, `cf_hi`,
#'       `ratio_to_cf`, `prevented`. `NULL` when `counterfactual = FALSE`.}
#'     \item{`$segments`}{Tibble with one row per segment (pre-interruption and
#'       each post-interruption interval): `segment`, `start_date`, `end_date`,
#'       `n_days`, `mean_observed`, `mean_predicted`, `mean_counterfactual`.}
#'     \item{`$data`}{The analysis dataset with all model covariates.}
#'     \item{`$meta`}{Analysis parameters and diagnostics.}
#'   }
#'
#' @references
#' Bernal, J.L., Cummins, S., & Gasparrini, A. (2017). Interrupted time
#' series regression for the evaluation of public health interventions: a
#' tutorial. *International Journal of Epidemiology*, 46(1), 348-355.
#' \doi{10.1093/ije/dyw098}
#'
#' Penfold, R.B., & Zhang, F. (2013). Use of interrupted time series analysis
#' in evaluating health care quality improvements. *Academic Pediatrics*,
#' 13(6 Suppl), S38-44. \doi{10.1016/j.acap.2013.08.002}
#'
#' @examples
#' \dontrun{
#' # Single interruption (COVID-19 pandemic onset)
#' its <- sus_mod_its(
#'   df_daily,
#'   outcome_col       = "n_obitos",
#'   interruption_dates = "2020-03-17",
#'   harmonics         = 2L,
#'   lang              = "pt"
#' )
#' print(its)
#' tidy(its)
#'
#' # Multiple interruptions
#' its2 <- sus_mod_its(
#'   df_daily,
#'   outcome_col        = "n_obitos",
#'   interruption_dates = c("2020-03-17", "2021-01-18"),
#'   lang               = "en"
#' )
#' }
#'
#' @seealso [sus_mod_excess()], [sus_mod_casecrossover()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang .data
#' @importFrom dplyr mutate select arrange filter summarise all_of any_of
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @importFrom glue glue
sus_mod_its <- function(
    data,
    outcome_col        = "n_obitos",
    date_col           = "date",
    interruption_dates,
    harmonics          = 2L,
    family             = "quasipoisson",
    covariates         = NULL,
    alpha              = 0.05,
    counterfactual     = TRUE,
    lang               = "pt",
    verbose            = TRUE
) {
  # 1. Language
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(.itsl("warn_lang", "pt", lang = lang))
    lang <- "pt"
  }

  if (verbose) cli::cli_h1("climasus4r \u2014 ITS")

  # 2. Validate family
  if (!family %in% c("quasipoisson", "poisson"))
    cli::cli_abort(.itsl("err_bad_family", lang))

  # 3. Extract data frame
  df <- if (inherits(data, "climasus_df")) {
    as.data.frame(data)
  } else if (inherits(data, "data.frame")) {
    data
  } else {
    cli::cli_abort("'data' must be a {.cls climasus_df} or a {.cls data.frame}.")
  }

  if (verbose) cli::cli_alert_info(.itsl("step_validate", lang))

  # 4. Validate columns
  if (!date_col %in% names(df))
    cli::cli_abort(.itsl("err_no_date", lang, date_col = date_col))

  if (!outcome_col %in% names(df)) {
    avail <- paste(names(df), collapse = ", ")
    cli::cli_abort(.itsl("err_no_outcome", lang,
                         outcome_col = outcome_col, avail = avail))
  }

  if (!is.null(covariates)) {
    bad_cov <- setdiff(covariates, names(df))
    if (length(bad_cov) > 0L)
      cli::cli_abort("Covariates not found in data: {.val {bad_cov}}.")
  }

  # 5. Parse interruption dates
  if (missing(interruption_dates) || length(interruption_dates) == 0L)
    cli::cli_abort(.itsl("err_no_interruptions", lang))

  int_dates <- sort(as.Date(interruption_dates))
  n_int     <- length(int_dates)

  # 6. Prepare base data frame (date + outcome + covariates)
  extra <- covariates %||% character(0)
  df2 <- df |>
    dplyr::select(
      date        = dplyr::all_of(date_col),
      outcome_val = dplyr::all_of(outcome_col),
      dplyr::any_of(extra)
    ) |>
    dplyr::mutate(
      date        = as.Date(date),
      outcome_val = as.numeric(outcome_val)
    ) |>
    dplyr::arrange(date) |>
    tidyr::drop_na(date, outcome_val)

  d_min <- min(df2$date)
  d_max <- max(df2$date)

  if (any(int_dates <= d_min | int_dates >= d_max))
    cli::cli_abort(.itsl("err_int_outside", lang,
                         d_min = format(d_min, "%Y-%m-%d"),
                         d_max = format(d_max, "%Y-%m-%d")))

  n_pre <- sum(df2$date < int_dates[[1L]])
  if (n_pre < 90L)
    cli::cli_alert_warning(.itsl("warn_short_pre", lang, n = n_pre))

  n_obs <- nrow(df2)
  if (verbose)
    cli::cli_alert_info(.itsl("step_build", lang, n_obs = n_obs, n_int = n_int))

  # 7. Build model dataset (time var + step/slope terms + harmonics)
  df_model <- .its_build_dataset(df2, int_dates, harmonics, extra)

  # 8. Fit model
  if (verbose) cli::cli_alert_info(.itsl("step_fit", lang, family = family))

  glm_family <- switch(
    family,
    quasipoisson = stats::quasipoisson(),
    poisson      = stats::poisson()
  )
  model <- .its_fit(df_model, n_int, harmonics, extra, glm_family)

  # 9. Extract effects
  z_crit  <- stats::qnorm(1 - alpha / 2)
  effects <- .its_extract_effects(model, int_dates, n_int, z_crit, family)

  # 10. Counterfactual
  cf_tbl <- NULL
  if (counterfactual) {
    if (verbose) cli::cli_alert_info(.itsl("step_cf", lang))
    cf_tbl <- .its_predict_counterfactual(model, df_model, n_int, z_crit)
  }

  # 11. Segments
  seg_tbl <- .its_segment_summary(df_model, model, cf_tbl, int_dates)

  # 12. Diagnostics
  disp_ratio <- tryCatch({
    sm <- summary(model)
    if (!is.null(sm$dispersion)) round(sm$dispersion, 3) else NA_real_
  }, error = function(e) NA_real_)

  # 13. Report
  if (verbose && nrow(effects) > 0L) {
    r <- effects[1L, ]
    cli::cli_alert_success(
      .itsl("done", lang,
            rr    = round(r$level_ratio, 3),
            rr_lo = round(r$level_ci_lo, 3),
            rr_hi = round(r$level_ci_hi, 3),
            idx   = 1L)
    )
  }

  # 14. Return
  structure(
    list(
      model          = model,
      effects        = effects,
      counterfactual = cf_tbl,
      segments       = seg_tbl,
      data           = df_model,
      meta           = list(
        outcome_col        = outcome_col,
        date_col           = date_col,
        interruption_dates = int_dates,
        n_interruptions    = n_int,
        harmonics          = as.integer(harmonics),
        family             = family,
        covariates         = covariates,
        alpha              = alpha,
        n_obs              = n_obs,
        n_pre              = n_pre,
        disp_ratio         = disp_ratio,
        call_time          = Sys.time()
      )
    ),
    class = c("climasus_its", "list")
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' @keywords internal
#' @noRd
.its_build_dataset <- function(df2, int_dates, harmonics, covariate_cols) {
  t0    <- as.numeric(min(df2$date))
  t_vec <- as.numeric(df2$date) - t0
  n     <- nrow(df2)

  df2$t_num <- t_vec

  # Step (D_j) and slope (S_j) terms for each interruption
  for (j in seq_along(int_dates)) {
    t_j <- as.numeric(int_dates[[j]]) - t0
    df2[[paste0("step_", j)]]  <- as.numeric(df2$t_num >= t_j)
    df2[[paste0("slope_", j)]] <- pmax(0, df2$t_num - t_j)
  }

  # Harmonic terms
  if (harmonics > 0L) {
    doy <- as.numeric(format(df2$date, "%j"))
    for (k in seq_len(harmonics)) {
      df2[[paste0("sin", k)]] <- sin(2 * pi * k * doy / 365.25)
      df2[[paste0("cos", k)]] <- cos(2 * pi * k * doy / 365.25)
    }
  }

  df2
}

#' @keywords internal
#' @noRd
.its_fit <- function(df_model, n_int, harmonics, covariate_cols, glm_family) {
  int_terms <- character(0)
  for (j in seq_len(n_int))
    int_terms <- c(int_terms, paste0("step_", j), paste0("slope_", j))

  harm_terms <- character(0)
  if (harmonics > 0L) {
    for (k in seq_len(harmonics))
      harm_terms <- c(harm_terms, paste0("sin", k), paste0("cos", k))
  }

  rhs <- paste(
    c("t_num", int_terms, harm_terms,
      if (length(covariate_cols) > 0L) covariate_cols else character(0)),
    collapse = " + "
  )
  fml <- stats::as.formula(paste("outcome_val ~", rhs))

  tryCatch(
    stats::glm(fml, data = df_model, family = glm_family),
    error = function(e)
      cli::cli_abort("ITS GLM fitting failed: {conditionMessage(e)}")
  )
}

#' @keywords internal
#' @noRd
.its_extract_effects <- function(model, int_dates, n_int, z_crit, family) {
  cm  <- summary(model)$coefficients
  p_col <- if ("Pr(>|t|)" %in% colnames(cm)) "Pr(>|t|)" else "Pr(>|z|)"

  rows <- vector("list", n_int)
  for (j in seq_len(n_int)) {
    step_nm  <- paste0("step_",  j)
    slope_nm <- paste0("slope_", j)

    b_step  <- if (step_nm  %in% rownames(cm)) cm[step_nm,  "Estimate"]   else NA_real_
    se_step <- if (step_nm  %in% rownames(cm)) cm[step_nm,  "Std. Error"] else NA_real_
    p_step  <- if (step_nm  %in% rownames(cm)) cm[step_nm,  p_col]        else NA_real_

    b_slope  <- if (slope_nm %in% rownames(cm)) cm[slope_nm, "Estimate"]   else NA_real_
    se_slope <- if (slope_nm %in% rownames(cm)) cm[slope_nm, "Std. Error"] else NA_real_
    p_slope  <- if (slope_nm %in% rownames(cm)) cm[slope_nm, p_col]        else NA_real_

    rows[[j]] <- tibble::tibble(
      label              = paste0("Interruption ", j),
      interruption_date  = int_dates[[j]],
      level_ratio        = exp(b_step),
      level_ci_lo        = exp(b_step - z_crit * se_step),
      level_ci_hi        = exp(b_step + z_crit * se_step),
      level_p            = p_step,
      slope_daily_log    = b_slope,
      slope_ratio_annual = exp(365.25 * b_slope),
      slope_p            = p_slope
    )
  }

  do.call(rbind, rows)
}

#' @keywords internal
#' @noRd
.its_predict_counterfactual <- function(model, df_model, n_int, z_crit) {
  # Zero out all step and slope terms -> counterfactual prediction
  df_cf <- df_model
  for (j in seq_len(n_int)) {
    df_cf[[paste0("step_",  j)]] <- 0
    df_cf[[paste0("slope_", j)]] <- 0
  }

  # Fitted (with intervention)
  fitted_vals <- as.numeric(stats::fitted(model))

  # Counterfactual (without intervention) on link scale for CI
  pred_cf <- tryCatch(
    stats::predict(model, newdata = df_cf, type = "link", se.fit = TRUE),
    error = function(e) NULL
  )

  if (is.null(pred_cf)) {
    cf_vals <- as.numeric(exp(stats::predict(model, newdata = df_cf, type = "link")))
    cf_lo   <- rep(NA_real_, length(cf_vals))
    cf_hi   <- rep(NA_real_, length(cf_vals))
  } else {
    cf_vals <- exp(pred_cf$fit)
    cf_lo   <- exp(pred_cf$fit - z_crit * pred_cf$se.fit)
    cf_hi   <- exp(pred_cf$fit + z_crit * pred_cf$se.fit)
  }

  tibble::tibble(
    date          = df_model$date,
    observed      = df_model$outcome_val,
    predicted     = fitted_vals,
    counterfactual = cf_vals,
    cf_lo         = cf_lo,
    cf_hi         = cf_hi,
    ratio_to_cf   = dplyr::if_else(cf_vals > 0, df_model$outcome_val / cf_vals, NA_real_),
    prevented     = cf_vals - df_model$outcome_val
  )
}

#' @keywords internal
#' @noRd
.its_segment_summary <- function(df_model, model, cf_tbl, int_dates) {
  fitted_vals <- as.numeric(stats::fitted(model))
  cf_vals <- if (!is.null(cf_tbl)) cf_tbl$counterfactual else rep(NA_real_, nrow(df_model))

  # Build break date sequence: start, each interruption, end
  d_min <- min(df_model$date)
  d_max <- max(df_model$date)
  breaks <- c(d_min, int_dates, d_max + 1L)
  seg_labels <- c(
    "Pre-interruption",
    if (length(int_dates) == 1L) "Post-interruption"
    else paste0("Post-interruption ", seq_along(int_dates))
  )

  rows <- vector("list", length(seg_labels))
  for (s in seq_along(seg_labels)) {
    s_from <- breaks[[s]]
    s_to   <- breaks[[s + 1L]] - 1L
    mask   <- df_model$date >= s_from & df_model$date <= s_to
    rows[[s]] <- tibble::tibble(
      segment            = seg_labels[[s]],
      start_date         = s_from,
      end_date           = s_to,
      n_days             = sum(mask),
      mean_observed      = round(mean(df_model$outcome_val[mask], na.rm = TRUE), 2),
      mean_predicted     = round(mean(fitted_vals[mask],          na.rm = TRUE), 2),
      mean_counterfactual = round(mean(cf_vals[mask],             na.rm = TRUE), 2)
    )
  }
  do.call(rbind, rows)
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_its object
#'
#' @param x A `climasus_its` object from `sus_mod_its()`.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_its <- function(x, ...) {
  m  <- x$meta
  ef <- x$effects

  cli::cli_h2("climasus_its")
  cli::cli_text("{.strong Outcome}        : {m$outcome_col}")
  cli::cli_text("{.strong Family}         : {m$family}")
  cli::cli_text("{.strong Harmonics}      : {m$harmonics}")
  cli::cli_text("{.strong N obs}          : {m$n_obs} ({m$n_pre} pre-interruption)")
  cli::cli_text("{.strong Interruptions}  : {m$n_interruptions}")
  if (!is.na(m$disp_ratio))
    cli::cli_text("{.strong Dispersion}     : {m$disp_ratio}")
  cli::cli_rule()

  for (i in seq_len(nrow(ef))) {
    r <- ef[i, ]
    cli::cli_text(
      "{.strong {r$label}} ({format(r$interruption_date, '%Y-%m-%d')}):"
    )
    cli::cli_text(
      "  Level change : RR = {round(r$level_ratio, 4)} [{round(r$level_ci_lo, 4)}, {round(r$level_ci_hi, 4)}]  p = {signif(r$level_p, 3)}"
    )
    if (!is.na(r$slope_ratio_annual)) {
      cli::cli_text(
        "  Slope change : {round(r$slope_ratio_annual, 4)} per year  p = {signif(r$slope_p, 3)}"
      )
    }
  }
  invisible(x)
}

#' Summarise a climasus_its object
#'
#' @param object A `climasus_its` object from `sus_mod_its()`.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_its <- function(object, ...) {
  print(object)
  cat("\n-- Segment summary --\n")
  print(object$segments)
  cat("\n-- Effects table --\n")
  print(object$effects)
  cat("\n-- Model summary --\n")
  print(summary(object$model))
  invisible(object)
}

#' Tidy a climasus_its object
#'
#' Returns the effects table with analysis metadata columns prepended.
#' Suitable for combining results across series with [dplyr::bind_rows()].
#'
#' @param x A `climasus_its` object from `sus_mod_its()`.
#' @param ... Unused.
#' @return A tibble with one row per interruption.
#' @export
#' @importFrom generics tidy
tidy.climasus_its <- function(x, ...) {
  m <- x$meta
  dplyr::bind_cols(
    tibble::tibble(
      outcome_col     = m$outcome_col,
      family          = m$family,
      harmonics       = m$harmonics,
      n_obs           = m$n_obs,
      n_pre           = m$n_pre,
      n_interruptions = m$n_interruptions
    ),
    x$effects
  )
}
