# =============================================================================
# sus_mod_excess.R
# Excess Mortality / Morbidity Estimation for Climate-Health Time Series
#
# Theory:
#   Spline baseline : Serfling (1963); Viboud et al. (2004, PNAS);
#                     Gasparrini et al. (2017, Nat Commun)
#   Serfling model  : Serfling (1963, Public Health Reports)
#   DLNM counterfact: Gasparrini & Armstrong (2013, Epidemiology)
# Input : climasus_dlnm (from sus_mod_dlnm()) OR climasus_df / data.frame
#         with date and outcome count columns
# Output: climasus_excess object with daily excess series, CI, period summaries
#
# Methods:
#   "from_dlnm"  - Counterfactual baseline from a fitted DLNM: removes
#                   cumulative RR from fitted values to recover the expected
#                   counts at the reference exposure level.
#   "spline"     - Quasi-Poisson GLM with natural splines over calendar time
#                   fitted on the control period; predicts expected for study.
#   "serfling"   - Serfling periodic regression (time trend + harmonic terms)
#                   fit on reference season, predicts epidemic/heat-wave excess.
# =============================================================================

# -- NSE variable declarations ------------------------------------------------
utils::globalVariables(c(
  "date", "observed", "expected", "excess",
  "expected_lo", "expected_hi", "excess_lo", "excess_hi",
  "z_score", "is_excess", "cum_excess",
  "year", "month_num", "season",
  "t_num", "sin1", "cos1", "sin2", "cos2"
))

# -- Local i18n ---------------------------------------------------------------
.exc_labels <- list(
  step_validate = list(
    pt = "Validando entradas...",
    en = "Validating inputs...",
    es = "Validando entradas..."
  ),
  step_baseline = list(
    pt = "Estimando baseline ({method}) com {n_ctrl} dias de controle...",
    en = "Estimating baseline ({method}) using {n_ctrl} control days...",
    es = "Estimando linea base ({method}) con {n_ctrl} dias de control..."
  ),
  step_excess = list(
    pt = "Calculando excesso para {n_study} dias em estudo...",
    en = "Computing excess for {n_study} study days...",
    es = "Calculando exceso para {n_study} dias en estudio..."
  ),
  done = list(
    pt = "Concluido. Excesso total: {exc} [{exc_lo}, {exc_hi}] | Pico: {pk} em {pk_date}",
    en = "Done. Total excess: {exc} [{exc_lo}, {exc_hi}] | Peak: {pk} on {pk_date}",
    es = "Listo. Exceso total: {exc} [{exc_lo}, {exc_hi}] | Pico: {pk} en {pk_date}"
  ),
  err_not_dlnm = list(
    pt = "Para method='from_dlnm', {.arg data} deve ser um {.cls climasus_dlnm}.",
    en = "For method='from_dlnm', {.arg data} must be a {.cls climasus_dlnm}.",
    es = "Para method='from_dlnm', {.arg data} debe ser un {.cls climasus_dlnm}."
  ),
  err_no_date = list(
    pt = "Coluna de data {.val {date_col}} nao encontrada.",
    en = "Date column {.val {date_col}} not found.",
    es = "Columna de fecha {.val {date_col}} no encontrada."
  ),
  err_no_outcome = list(
    pt = "Coluna de desfecho {.val {outcome_col}} nao encontrada. Disponivel: {.val {avail}}.",
    en = "Outcome column {.val {outcome_col}} not found. Available: {.val {avail}}.",
    es = "Columna de resultado {.val {outcome_col}} no encontrada. Disponibles: {.val {avail}}."
  ),
  err_no_control = list(
    pt = "Nenhum dado no periodo de controle. Verifique {.arg control_period}.",
    en = "No data in control period. Check {.arg control_period}.",
    es = "Sin datos en el periodo de control. Verifique {.arg control_period}."
  ),
  err_bad_method = list(
    pt = "{.arg method} deve ser um de: 'from_dlnm', 'spline', 'serfling'.",
    en = "{.arg method} must be one of: 'from_dlnm', 'spline', 'serfling'.",
    es = "{.arg method} debe ser uno de: 'from_dlnm', 'spline', 'serfling'."
  ),
  err_short_series = list(
    pt = "Serie muito curta ({n} dias) para estimar baseline com {dof_per_year} gl/ano.",
    en = "Series too short ({n} days) to estimate baseline with {dof_per_year} df/year.",
    es = "Serie muy corta ({n} dias) para estimar la linea base con {dof_per_year} gl/ano."
  ),
  warn_no_dlnm = list(
    pt = "Pacote {.pkg dlnm} necessario para method='from_dlnm'. Usando 'spline' como alternativa.",
    en = "Package {.pkg dlnm} required for method='from_dlnm'. Falling back to 'spline'.",
    es = "Paquete {.pkg dlnm} necesario para method='from_dlnm'. Usando 'spline' como alternativa."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} nao suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Using {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  )
)

#' @keywords internal
#' @noRd
.excl <- function(key, lang, ...) {
  entry <- .exc_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}


# -- Exported function --------------------------------------------------------

#' Excess Mortality and Morbidity from Climate-Health Time Series
#'
#' Estimates excess counts (observed minus expected baseline) for daily health
#' outcome series in the context of climate-health research. Supports three
#' baseline methods: counterfactual extraction from a fitted DLNM
#' (`"from_dlnm"`), quasi-Poisson GLM with natural splines (`"spline"`), and
#' Serfling sinusoidal regression (`"serfling"`). Confidence intervals are
#' derived from the baseline model's prediction standard errors. A z-score
#' flag identifies statistically significant excess events.
#'
#' @section Statistical framework:
#'
#' **`"spline"` and `"serfling"` methods**: A quasi-Poisson GLM is fitted on
#' the `control_period`:
#'
#' \deqn{\log(\mu_t) = \alpha + f(t)}
#'
#' where \eqn{f(t)} is either a natural spline over calendar date (`"spline"`)
#' or a Serfling harmonic: \eqn{\beta_1 t + \beta_2 t^2 +
#' \sum_{k=1}^{H}[\gamma_k \sin(2\pi k t/365.25) + \delta_k \cos(2\pi k t/365.25)]}
#' (`"serfling"`). Expected counts for the study period are predicted from
#' this model.
#'
#' **`"from_dlnm"` method**: Given a `climasus_dlnm` object, the counterfactual
#' expected mortality without the climate-exposure effect is:
#'
#' \deqn{\hat\mu_t^{(0)} = \hat\mu_t / \widehat{RR}(x_t)}
#'
#' where \eqn{\hat\mu_t} are fitted values and \eqn{\widehat{RR}(x_t)} is the
#' cumulative relative risk at the observed exposure \eqn{x_t} from
#' `dlnm::crosspred()`. This yields the Gasparrini & Armstrong (2013)
#' temperature-attributable excess.
#'
#' @param data A `climasus_dlnm` object (for `method = "from_dlnm"`), a
#'   `climasus_df` at stage `"aggregate"` or later, or any `data.frame` with
#'   a date column and an integer count column.
#' @param outcome_col Character. Name of the outcome count column. Required
#'   when `data` is a data frame; inferred from `fit$meta$outcome_col` when
#'   `data` is a `climasus_dlnm`.
#' @param date_col Character. Name of the date column. Default `"date"`.
#' @param control_period Date vector of length 2 specifying the reference
#'   period used to fit the baseline model, e.g.
#'   `c(as.Date("2018-01-01"), as.Date("2019-12-31"))`. Default `NULL` uses
#'   all available data before `study_period`, or all data when both are `NULL`.
#' @param study_period Date vector of length 2 specifying the period for which
#'   excess is reported. Default `NULL` uses the full data range.
#' @param method Character. Baseline estimation method:
#'   - `"from_dlnm"` — counterfactual from DLNM (requires `climasus_dlnm`).
#'   - `"spline"` (default for data frames) — quasi-Poisson with ns().
#'   - `"serfling"` — Serfling harmonic regression.
#' @param dof_per_year Integer. Degrees of freedom per year for the spline
#'   baseline. Default `8L`.
#' @param harmonics Integer. Number of sinusoidal harmonics in the Serfling
#'   model. Default `2L`.
#' @param family Character. GLM family: `"quasipoisson"` (default) or
#'   `"poisson"`. Ignored for `method = "from_dlnm"`.
#' @param threshold_z Numeric. Z-score threshold for flagging excess events.
#'   Default `1.96` (95th percentile).
#' @param by Character or `NULL`. Temporal breakdown variable for the period
#'   summary: `"year"`, `"month"`, or `"season"`. Default `NULL`.
#' @param alpha Numeric. Significance level for confidence intervals.
#'   Default `0.05`.
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_excess` list with:
#'   \describe{
#'     \item{`$daily`}{Tibble with one row per day: `date`, `observed`,
#'       `expected`, `expected_lo`, `expected_hi`, `excess`, `excess_lo`,
#'       `excess_hi`, `z_score`, `is_excess`.}
#'     \item{`$total`}{One-row tibble: total observed, expected, excess,
#'       excess percentage, and their confidence bounds.}
#'     \item{`$by_period`}{Temporal breakdown tibble (when `by` is not `NULL`).}
#'     \item{`$model`}{The fitted baseline GLM object (`NULL` for
#'       `method = "from_dlnm"`).}
#'     \item{`$meta`}{Metadata list: `method`, `outcome_col`, `date_col`,
#'       `control_period`, `study_period`, `family`, `dof_per_year`,
#'       `harmonics`, `threshold_z`, `n_obs`, `call_time`.}
#'   }
#'
#' @examples
#' \dontrun{
#' # From a fitted DLNM
#' exc <- sus_mod_excess(fit_dlnm, method = "from_dlnm", lang = "en")
#'
#' # Standalone spline baseline for a heat wave period
#' exc <- sus_mod_excess(
#'   df_daily,
#'   outcome_col    = "n_obitos",
#'   control_period = c(as.Date("2018-01-01"), as.Date("2021-12-31")),
#'   study_period   = c(as.Date("2022-01-01"), as.Date("2022-12-31")),
#'   method         = "spline",
#'   lang           = "pt"
#' )
#' print(exc)
#' tidy(exc)
#' }
#'
#' @seealso [sus_mod_dlnm()], [sus_mod_af()], [sus_mod_plot_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom tibble tibble
#' @importFrom dplyr mutate arrange summarise all_of
#' @importFrom purrr map list_rbind
#' @importFrom glue glue
sus_mod_excess <- function(
    data,
    outcome_col    = NULL,
    date_col       = "date",
    control_period = NULL,
    study_period   = NULL,
    method         = NULL,
    dof_per_year   = 8L,
    harmonics      = 2L,
    family         = "quasipoisson",
    threshold_z    = 1.96,
    by             = NULL,
    alpha          = 0.05,
    lang           = "pt",
    verbose        = TRUE
) {
  # 1. Language
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(.excl("warn_lang", "pt", lang = lang))
    lang <- "pt"
  }

  if (verbose) cli::cli_h1("climasus4r \u2014 Excess Mortality / Morbidity")

  # 2. Detect input type and default method
  is_dlnm  <- inherits(data, "climasus_dlnm")
  is_df    <- inherits(data, "data.frame")

  if (!is_dlnm && !is_df)
    cli::cli_abort("'data' must be a {.cls climasus_dlnm} or a {.cls data.frame}.")

  if (is.null(method)) method <- if (is_dlnm) "from_dlnm" else "spline"

  if (!method %in% c("from_dlnm", "spline", "serfling"))
    cli::cli_abort(.excl("err_bad_method", lang))

  if (method == "from_dlnm" && !is_dlnm)
    cli::cli_abort(.excl("err_not_dlnm", lang))

  if (method == "from_dlnm" && !requireNamespace("dlnm", quietly = TRUE)) {
    cli::cli_alert_warning(.excl("warn_no_dlnm", lang))
    method <- "spline"
  }

  if (!is.null(by) && !by %in% c("year", "month", "season"))
    cli::cli_abort("`by` must be NULL, 'year', 'month', or 'season'.")

  if (verbose) cli::cli_alert_info(.excl("step_validate", lang))

  # 3. Extract time series -------------------------------------------------------
  if (is_dlnm) {
    df_ts       <- data$data_daily
    outcome_col <- outcome_col %||% data$meta$outcome_col
    date_col    <- "date"

    # outcome column in data_daily is "y"
    if ("y" %in% names(df_ts) && !outcome_col %in% names(df_ts))
      outcome_col_ts <- "y"
    else
      outcome_col_ts <- outcome_col
  } else {
    df_ts          <- data
    outcome_col_ts <- outcome_col
  }

  if (!date_col %in% names(df_ts))
    cli::cli_abort(.excl("err_no_date", lang, date_col = date_col))

  if (is.null(outcome_col_ts) || !outcome_col_ts %in% names(df_ts)) {
    avail <- paste(names(df_ts), collapse = ", ")
    cli::cli_abort(.excl("err_no_outcome", lang,
                         outcome_col = outcome_col_ts %||% "NULL",
                         avail = avail))
  }

  df_ts <- df_ts |>
    dplyr::rename(date = dplyr::all_of(date_col)) |>
    dplyr::mutate(
      date     = as.Date(date),
      observed = as.numeric(.data[[outcome_col_ts]])
    ) |>
    dplyr::arrange(date)

  # 4. Define control and study masks --------------------------------------------
  all_dates <- df_ts$date
  date_min  <- min(all_dates, na.rm = TRUE)
  date_max  <- max(all_dates, na.rm = TRUE)

  if (!is.null(control_period)) {
    ctrl_from <- as.Date(control_period[[1L]])
    ctrl_to   <- as.Date(control_period[[2L]])
  } else if (!is.null(study_period)) {
    ctrl_from <- date_min
    ctrl_to   <- as.Date(study_period[[1L]]) - 1L
  } else {
    ctrl_from <- date_min
    ctrl_to   <- date_max
  }

  if (!is.null(study_period)) {
    stdy_from <- as.Date(study_period[[1L]])
    stdy_to   <- as.Date(study_period[[2L]])
  } else {
    stdy_from <- date_min
    stdy_to   <- date_max
  }

  ctrl_mask <- df_ts$date >= ctrl_from & df_ts$date <= ctrl_to
  stdy_mask <- df_ts$date >= stdy_from & df_ts$date <= stdy_to

  n_ctrl  <- sum(ctrl_mask)
  n_study <- sum(stdy_mask)

  if (n_ctrl == 0L)
    cli::cli_abort(.excl("err_no_control", lang))

  # 5. Baseline estimation -------------------------------------------------------
  if (method == "from_dlnm") {
    baseline_out <- .sex_baseline_dlnm(data, df_ts, stdy_mask, alpha)
  } else {
    n_yrs   <- as.numeric(date_max - date_min) / 365.25
    ns_df   <- max(dof_per_year, as.integer(round(dof_per_year * n_yrs)))
    if (n_ctrl < ns_df * 2L)
      cli::cli_abort(.excl("err_short_series", lang,
                           n = n_ctrl, dof_per_year = dof_per_year))

    if (verbose)
      cli::cli_alert_info(.excl("step_baseline", lang,
                                method = method, n_ctrl = n_ctrl))

    if (method == "spline") {
      baseline_out <- .sex_baseline_spline(df_ts, ctrl_mask, stdy_mask,
                                           ns_df, family, alpha)
    } else {
      baseline_out <- .sex_baseline_serfling(df_ts, ctrl_mask, stdy_mask,
                                              harmonics, family, alpha)
    }
  }

  # 6. Compute excess series -----------------------------------------------------
  if (verbose)
    cli::cli_alert_info(.excl("step_excess", lang, n_study = n_study))

  z_thresh <- stats::qnorm(1 - alpha / 2)

  daily_tbl <- df_ts |>
    dplyr::filter(stdy_mask) |>
    dplyr::mutate(
      expected    = baseline_out$expected,
      expected_lo = baseline_out$expected_lo,
      expected_hi = baseline_out$expected_hi,
      excess      = observed - expected,
      excess_lo   = observed - expected_hi,
      excess_hi   = observed - expected_lo,
      z_score     = dplyr::if_else(
        expected > 0,
        (observed - expected) / sqrt(expected),
        NA_real_
      ),
      is_excess   = !is.na(z_score) & z_score > threshold_z
    ) |>
    dplyr::mutate(
      cum_excess = cumsum(excess)
    ) |>
    dplyr::select(
      date, observed, expected, expected_lo, expected_hi,
      excess, excess_lo, excess_hi,
      z_score, is_excess, cum_excess
    )

  # 7. Total summary -------------------------------------------------------------
  obs_tot    <- sum(daily_tbl$observed,    na.rm = TRUE)
  exp_tot    <- sum(daily_tbl$expected,    na.rm = TRUE)
  exp_lo_tot <- sum(daily_tbl$expected_lo, na.rm = TRUE)
  exp_hi_tot <- sum(daily_tbl$expected_hi, na.rm = TRUE)
  exc_tot    <- obs_tot - exp_tot
  exc_lo     <- obs_tot - exp_hi_tot
  exc_hi     <- obs_tot - exp_lo_tot
  exc_pct    <- if (exp_tot > 0) round(exc_tot / exp_tot * 100, 2) else NA_real_
  n_events   <- sum(daily_tbl$is_excess, na.rm = TRUE)
  peak_idx   <- which.max(daily_tbl$excess)
  peak_exc   <- round(daily_tbl$excess[[peak_idx]])
  peak_date  <- daily_tbl$date[[peak_idx]]

  total_tbl <- tibble::tibble(
    n_days      = n_study,
    observed    = obs_tot,
    expected    = round(exp_tot,    1),
    expected_lo = round(exp_lo_tot, 1),
    expected_hi = round(exp_hi_tot, 1),
    excess      = round(exc_tot,    1),
    excess_lo   = round(exc_lo,     1),
    excess_hi   = round(exc_hi,     1),
    excess_pct  = exc_pct,
    n_excess_days = n_events,
    peak_excess   = peak_exc,
    peak_date     = peak_date
  )

  # 8. Temporal breakdown --------------------------------------------------------
  by_period <- NULL
  if (!is.null(by)) {
    by_period <- .sex_by_period(daily_tbl, by)
  }

  # 9. Done message --------------------------------------------------------------
  if (verbose) {
    cli::cli_alert_success(
      .excl("done", lang,
            exc    = round(exc_tot),
            exc_lo = round(exc_lo),
            exc_hi = round(exc_hi),
            pk     = peak_exc,
            pk_date = format(peak_date, "%Y-%m-%d"))
    )
  }

  # 10. Return -------------------------------------------------------------------
  structure(
    list(
      daily     = daily_tbl,
      total     = total_tbl,
      by_period = by_period,
      model     = baseline_out$model,
      meta      = list(
        method         = method,
        outcome_col    = outcome_col %||% outcome_col_ts,
        date_col       = date_col,
        control_period = c(ctrl_from, ctrl_to),
        study_period   = c(stdy_from, stdy_to),
        family         = if (method == "from_dlnm") "from_dlnm" else family,
        dof_per_year   = dof_per_year,
        harmonics      = harmonics,
        threshold_z    = threshold_z,
        n_obs          = nrow(df_ts),
        n_study        = n_study,
        n_control      = n_ctrl,
        call_time      = Sys.time()
      )
    ),
    class = c("climasus_excess", "list")
  )
}


# =============================================================================
# INTERNAL HELPERS — baseline methods
# =============================================================================

#' @keywords internal
#' @noRd
.sex_baseline_spline <- function(df_ts, ctrl_mask, stdy_mask, ns_df,
                                  family, alpha) {
  df_ctrl <- df_ts[ctrl_mask, ]
  df_stdy <- df_ts[stdy_mask, ]

  t_ref  <- as.numeric(min(df_ctrl$date))
  t_num_ctrl  <- as.numeric(df_ctrl$date) - t_ref
  t_num_study <- as.numeric(df_stdy$date) - t_ref

  glm_family <- switch(
    family,
    quasipoisson = stats::quasipoisson(),
    poisson      = stats::poisson(),
    stats::quasipoisson()
  )

  df_fit <- data.frame(
    y     = df_ctrl$observed,
    t_num = t_num_ctrl
  )
  baseline_model <- tryCatch(
    stats::glm(y ~ splines::ns(t_num, df = ns_df), data = df_fit,
               family = glm_family),
    error = function(e) {
      cli::cli_abort("Spline baseline model failed: {conditionMessage(e)}")
    }
  )

  df_pred <- data.frame(t_num = t_num_study)
  pred_out <- stats::predict(baseline_model, newdata = df_pred,
                              type = "link", se.fit = TRUE)
  z_crit   <- stats::qnorm(1 - alpha / 2)
  expected    <- exp(pred_out$fit)
  expected_lo <- exp(pred_out$fit - z_crit * pred_out$se.fit)
  expected_hi <- exp(pred_out$fit + z_crit * pred_out$se.fit)

  list(expected    = expected,
       expected_lo = expected_lo,
       expected_hi = expected_hi,
       model       = baseline_model)
}

#' @keywords internal
#' @noRd
.sex_baseline_serfling <- function(df_ts, ctrl_mask, stdy_mask,
                                    harmonics, family, alpha) {
  df_ctrl <- df_ts[ctrl_mask, ]
  df_stdy <- df_ts[stdy_mask, ]

  t_ref        <- as.numeric(min(df_ctrl$date))
  t_num_ctrl   <- as.numeric(df_ctrl$date)  - t_ref
  t_num_study  <- as.numeric(df_stdy$date)  - t_ref

  # Build harmonic predictors
  make_harmonics <- function(t_vec) {
    out <- data.frame(t_num = t_vec, t_num2 = t_vec^2)
    for (k in seq_len(harmonics)) {
      out[[paste0("sin", k)]] <- sin(2 * pi * k * t_vec / 365.25)
      out[[paste0("cos", k)]] <- cos(2 * pi * k * t_vec / 365.25)
    }
    out
  }

  df_fit  <- cbind(y = df_ctrl$observed, make_harmonics(t_num_ctrl))
  df_pred <- make_harmonics(t_num_study)

  pred_names <- setdiff(names(df_fit), "y")
  fml <- stats::as.formula(
    paste("y ~", paste(pred_names, collapse = " + "))
  )

  glm_family <- switch(
    family,
    quasipoisson = stats::quasipoisson(),
    poisson      = stats::poisson(),
    stats::quasipoisson()
  )

  baseline_model <- tryCatch(
    stats::glm(fml, data = df_fit, family = glm_family),
    error = function(e) {
      cli::cli_abort("Serfling baseline model failed: {conditionMessage(e)}")
    }
  )

  pred_out <- stats::predict(baseline_model, newdata = df_pred,
                              type = "link", se.fit = TRUE)
  z_crit   <- stats::qnorm(1 - alpha / 2)
  expected    <- exp(pred_out$fit)
  expected_lo <- exp(pred_out$fit - z_crit * pred_out$se.fit)
  expected_hi <- exp(pred_out$fit + z_crit * pred_out$se.fit)

  list(expected    = expected,
       expected_lo = expected_lo,
       expected_hi = expected_hi,
       model       = baseline_model)
}

#' @keywords internal
#' @noRd
.sex_baseline_dlnm <- function(fit, df_ts, stdy_mask, alpha) {
  # Counterfactual expected = fitted(model) / RR_at_observed_exposure
  cb    <- fit$crossbasis
  model <- fit$model
  meta  <- fit$meta
  cen   <- meta$ref_value

  lag0_col <- paste0(meta$climate_col, "_lag0")
  df_agg   <- fit$data_daily
  x        <- df_agg[[lag0_col]]

  # Cumulative RR at each observed exposure
  pred_obs <- dlnm::crosspred(cb, model, at = x, cen = cen)
  rr_obs   <- as.numeric(pred_obs$allRRfit)
  rr_lo    <- as.numeric(pred_obs$allRRlow)
  rr_hi    <- as.numeric(pred_obs$allRRhigh)

  fitted_vals <- as.numeric(stats::fitted(model))

  # Counterfactual baseline (without temperature effect)
  expected    <- fitted_vals / rr_obs
  expected_lo <- fitted_vals / rr_hi   # larger RR -> smaller expected (reverse)
  expected_hi <- fitted_vals / rr_lo

  # Subset to study days — df_ts already aligned with fit$data_daily
  list(
    expected    = expected[stdy_mask],
    expected_lo = expected_lo[stdy_mask],
    expected_hi = expected_hi[stdy_mask],
    model       = NULL
  )
}

#' @keywords internal
#' @noRd
.sex_by_period <- function(daily_tbl, by) {
  dt <- daily_tbl |>
    dplyr::mutate(
      year      = as.integer(format(date, "%Y")),
      month_num = as.integer(format(date, "%m")),
      season    = dplyr::case_when(
        month_num %in% c(12L, 1L, 2L)  ~ "DJF",
        month_num %in% c(3L, 4L, 5L)   ~ "MAM",
        month_num %in% c(6L, 7L, 8L)   ~ "JJA",
        month_num %in% c(9L, 10L, 11L) ~ "SON",
        .default = NA_character_
      )
    )

  group_col <- switch(by,
    year   = "year",
    month  = c("year", "month_num"),
    season = c("year", "season")
  )

  dt |>
    dplyr::summarise(
      .by       = dplyr::all_of(group_col),
      n_days    = dplyr::n(),
      observed  = sum(observed,    na.rm = TRUE),
      expected  = round(sum(expected,    na.rm = TRUE), 1),
      excess    = round(sum(excess,      na.rm = TRUE), 1),
      excess_lo = round(sum(excess_lo,   na.rm = TRUE), 1),
      excess_hi = round(sum(excess_hi,   na.rm = TRUE), 1),
      excess_pct = dplyr::if_else(
        sum(expected, na.rm = TRUE) > 0,
        round(sum(excess, na.rm = TRUE) / sum(expected, na.rm = TRUE) * 100, 2),
        NA_real_
      ),
      n_excess_days = sum(is_excess, na.rm = TRUE)
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_col)))
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_excess object
#'
#' @param x A `climasus_excess` object from `sus_mod_excess()`.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_excess <- function(x, ...) {
  m  <- x$meta
  tt <- x$total

  cli::cli_h2("climasus_excess")
  cli::cli_text("{.strong Method}         : {m$method}")
  cli::cli_text("{.strong Outcome}        : {m$outcome_col}")
  cli::cli_text("{.strong Control period} : {format(m$control_period[[1]], '%Y-%m-%d')} to {format(m$control_period[[2]], '%Y-%m-%d')} ({m$n_control} days)")
  cli::cli_text("{.strong Study period}   : {format(m$study_period[[1]],  '%Y-%m-%d')} to {format(m$study_period[[2]],  '%Y-%m-%d')} ({m$n_study} days)")
  cli::cli_rule()

  cli::cli_text(
    "{.strong Observed}  : {tt$observed}"
  )
  cli::cli_text(
    "{.strong Expected}  : {round(tt$expected, 1)} [{round(tt$expected_lo, 1)}, {round(tt$expected_hi, 1)}]"
  )
  cli::cli_text(
    "{.strong Excess}    : {round(tt$excess, 1)} [{round(tt$excess_lo, 1)}, {round(tt$excess_hi, 1)}]"
  )
  cli::cli_text(
    "{.strong Excess %}  : {tt$excess_pct}%"
  )
  cli::cli_text(
    "{.strong Excess days} : {tt$n_excess_days} / {tt$n_days} (z > {m$threshold_z})"
  )
  cli::cli_text(
    "{.strong Peak excess} : {tt$peak_excess} on {format(tt$peak_date, '%Y-%m-%d')}"
  )

  invisible(x)
}

#' Summarise a climasus_excess object
#'
#' @param object A `climasus_excess` object from `sus_mod_excess()`.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_excess <- function(object, ...) {
  print(object)
  cat("\n-- Daily excess (first 10 rows) --\n")
  print(utils::head(object$daily, 10L))
  if (!is.null(object$by_period)) {
    cat("\n-- By-period breakdown --\n")
    print(object$by_period)
  }
  if (!is.null(object$model)) {
    cat("\n-- Baseline model --\n")
    print(summary(object$model))
  }
  invisible(object)
}

#' Tidy a climasus_excess object into a one-row summary tibble
#'
#' Returns a one-row tibble summarising the excess result, suitable for
#' combining across multiple analyses with [dplyr::bind_rows()].
#'
#' @param x A `climasus_excess` object from `sus_mod_excess()`.
#' @param ... Unused.
#' @return A one-row tibble.
#' @export
#' @importFrom generics tidy
tidy.climasus_excess <- function(x, ...) {
  m  <- x$meta
  tt <- x$total

  tibble::tibble(
    method          = m$method,
    outcome_col     = m$outcome_col,
    study_from      = m$study_period[[1L]],
    study_to        = m$study_period[[2L]],
    n_study_days    = tt$n_days,
    observed        = tt$observed,
    expected        = round(tt$expected,    1),
    expected_lo     = round(tt$expected_lo, 1),
    expected_hi     = round(tt$expected_hi, 1),
    excess          = round(tt$excess,    1),
    excess_lo       = round(tt$excess_lo, 1),
    excess_hi       = round(tt$excess_hi, 1),
    excess_pct      = tt$excess_pct,
    n_excess_days   = tt$n_excess_days,
    peak_excess     = tt$peak_excess,
    peak_date       = tt$peak_date
  )
}
