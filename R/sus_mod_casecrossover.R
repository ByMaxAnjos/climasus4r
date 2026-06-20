# =============================================================================
# sus_mod_casecrossover.R
# Time-Stratified Case-Crossover Analysis for Climate-Health Data
#
# Theory:
#   Conditional Poisson: Whitaker et al. (2006, Stat Med); Farrington (1995)
#   Case-crossover: Maclure (1991, Am J Epidemiol); Levy et al. (2001);
#                   Armstrong et al. (2014, Epidemiology)
# Input : climasus_df at stage "aggregate" or "climate", or plain data.frame
#         with a date column, an outcome count column, and an exposure column.
# Output: climasus_casecrossover object (list with model, OR table, diagnostics)
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "date", "outcome_val", "exposure_val", "exposure_raw", "stratum_id",
  "is_case", "year_val", "week_val", "month_val",
  "term", "lag_spec", "or", "or_lo", "or_hi", "p_value", "estimate"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.scc_labels <- list(
  step_validate = list(
    pt = "Validando entradas...",
    en = "Validating inputs...",
    es = "Validando entradas..."
  ),
  step_data = list(
    pt = "Preparando dataset ({n_obs} obs, lag(s): {lags_str}, {n_strata} estratos)...",
    en = "Preparing dataset ({n_obs} obs, lag(s): {lags_str}, {n_strata} strata)...",
    es = "Preparando dataset ({n_obs} obs, lag(s): {lags_str}, {n_strata} estratos)..."
  ),
  step_fit = list(
    pt = "Ajustando case-crossover ({method})...",
    en = "Fitting case-crossover ({method})...",
    es = "Ajustando case-crossover ({method})..."
  ),
  done = list(
    pt = "Concluido. OR = {or} [{or_lo}, {or_hi}] (p = {pval})",
    en = "Done. OR = {or} [{or_lo}, {or_hi}] (p = {pval})",
    es = "Listo. OR = {or} [{or_lo}, {or_hi}] (p = {pval})"
  ),
  err_no_date = list(
    pt = "Coluna 'date' nao encontrada nos dados.",
    en = "Column 'date' not found in data.",
    es = "Columna 'date' no encontrada en los datos."
  ),
  err_no_outcome = list(
    pt = "Coluna de desfecho {.val {outcome_col}} nao encontrada. Disponiveis: {.val {avail}}.",
    en = "Outcome column {.val {outcome_col}} not found. Available: {.val {avail}}.",
    es = "Columna de resultado {.val {outcome_col}} no encontrada. Disponibles: {.val {avail}}."
  ),
  err_no_exposure = list(
    pt = "Coluna de exposicao {.val {exposure_col}} nao encontrada. Disponiveis: {.val {avail}}.",
    en = "Exposure column {.val {exposure_col}} not found. Available: {.val {avail}}.",
    es = "Columna de exposicion {.val {exposure_col}} no encontrada. Disponibles: {.val {avail}}."
  ),
  err_bad_method = list(
    pt = "{.arg method} deve ser 'conditional_poisson' ou 'clogit'.",
    en = "{.arg method} must be 'conditional_poisson' or 'clogit'.",
    es = "{.arg method} debe ser 'conditional_poisson' o 'clogit'."
  ),
  err_bad_stratum = list(
    pt = "{.arg stratum} deve ser 'month', 'week', o nome de uma coluna existente, ou um vetor de nomes de colunas ({.val {stratum}}).",
    en = "{.arg stratum} must be 'month', 'week', an existing column name, or a vector of column names ({.val {stratum}}).",
    es = "{.arg stratum} debe ser 'month', 'week', el nombre de una columna existente, o un vector de nombres de columnas ({.val {stratum}})."
  ),
  err_no_cases = list(
    pt = "Nenhum caso encontrado ({outcome_col} > 0) apos remocao de missings.",
    en = "No cases found ({outcome_col} > 0) after removing missing values.",
    es = "Ningun caso encontrado ({outcome_col} > 0) tras eliminar valores faltantes."
  ),
  err_survival = list(
    pt = "Pacote {.pkg survival} necessario para {.arg method = 'clogit'}. Instale com install.packages('survival').",
    en = "Package {.pkg survival} required for {.arg method = 'clogit'}. Install with install.packages('survival').",
    es = "Paquete {.pkg survival} necesario para {.arg method = 'clogit'}. Instale con install.packages('survival')."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} nao suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Using {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  ),
  warn_few_strata = list(
    pt = "Apenas {n_strata} estrato(s). Recomenda-se >= 12 estratos para estimativas estaveis.",
    en = "Only {n_strata} strata. At least 12 recommended for stable estimates.",
    es = "Solo {n_strata} estratos. Se recomiendan >= 12 para estimaciones estables."
  ),
  warn_clogit_binary = list(
    pt = "method='clogit' trata o desfecho como binario (is_case = count > 0). Para dados de contagem, considere method='conditional_poisson'.",
    en = "method='clogit' treats outcome as binary (is_case = count > 0). For count data, consider method='conditional_poisson'.",
    es = "method='clogit' trata el resultado como binario (is_case = count > 0). Para datos de conteo, considere method='conditional_poisson'."
  )
)

#' @keywords internal
#' @noRd
.sccl <- function(key, lang, ...) {
  entry <- .scc_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}


# ── Exported function ─────────────────────────────────────────────────────────

#' Time-Stratified Case-Crossover Analysis for Climate-Health Data
#'
#' Fits a time-stratified case-crossover model to quantify the association
#' between a climate exposure and a daily health outcome. Each time stratum
#' (month or week) acts as its own control, removing long-term trends and
#' seasonal confounding by design. Two fitting methods are supported:
#' `"conditional_poisson"` (GLM with stratum fixed effects, default) for
#' aggregate count data, and `"clogit"` (conditional logistic regression) for
#' binary outcomes or rare events.
#'
#' @section Statistical framework:
#'
#' **`"conditional_poisson"` method** (Whitaker et al., 2006):
#'
#' \deqn{\log(E[Y_t]) = \alpha_s + \beta X_t + \mathbf{z}_t^\top \boldsymbol{\gamma}}
#'
#' where \eqn{\alpha_s} is a stratum-specific intercept (absorbed as
#' `factor(stratum_id)` in the GLM), \eqn{X_t} is the (possibly lagged or
#' averaged) exposure on day \eqn{t}, and \eqn{\mathbf{z}_t} are optional
#' time-varying covariates. The rate ratio is \eqn{RR = e^\beta}.
#'
#' **`"clogit"` method** (Maclure, 1991; Levy et al., 2001):
#'
#' Uses `survival::clogit()` with the binary indicator
#' \eqn{D_t = \mathbf{1}(Y_t > 0)} as the outcome. Each case day is matched
#' to all other days within the same stratum as controls. This method is most
#' appropriate for rare events (low daily counts) and binary outcomes.
#'
#' @section Lag specification:
#'
#' The `lag` argument controls how the daily exposure is lagged before the
#' association is estimated:
#' - **Single integer** (e.g., `lag = 0`): uses the exposure exactly `lag` days
#'   before the outcome day.
#' - **Integer vector** (e.g., `lag = 0:6`): uses the arithmetic mean of
#'   exposures at all specified lags — a moving average common for temperature
#'   (e.g., `lag = 0:6` is the "lag 0-6" or "mean temperature over 7 days").
#'
#' @param data A `climasus_df` at stage `"aggregate"` or `"climate"` (produced
#'   by `sus_climate_aggregate()`), or any `data.frame` containing at minimum a
#'   `date` column (Date), an outcome count column, and an exposure column.
#' @param outcome_col Character. Name of the daily health outcome count column
#'   (e.g., `"n_obitos"`, `"n_internacoes"`). Default: `"n_obitos"`.
#' @param exposure_col Character. Name of the exposure column. Required; no
#'   auto-detection is performed (unlike `sus_mod_dlnm()`). Examples:
#'   `"tair_dry_bulb_c"`, `"pm25_mean"`, `"precip_mm"`.
#' @param covariates Character vector or `NULL`. Names of additional columns to
#'   include as linear confounders. These should be time-varying within strata
#'   (e.g., day-of-week dummy, holiday indicator, relative humidity). Long-term
#'   trends are already controlled by the strata. Default: `NULL`.
#' @param stratum Character. Time stratum type or name of an existing column:
#'   - `"month"` (default): year-month strata (e.g., `"2021-07"`).
#'   - `"week"`: ISO year-week strata (e.g., `"2021-W27"`).
#'   - Any column name in `data`: uses that column's values as stratum IDs.
#' @param lag Integer or integer vector. Lag(s) to apply to the exposure before
#'   fitting. A single integer uses that specific lag; a vector uses the mean
#'   of exposures at all specified lags. Default: `0L` (same-day exposure).
#' @param method Character. Fitting method:
#'   - `"conditional_poisson"` (default): GLM with stratum fixed effects.
#'     Preferred for aggregate count data; uses `stats::glm()` with the
#'     specified `family`.
#'   - `"clogit"`: conditional logistic regression via `survival::clogit()`.
#'     Requires the `survival` package. Treats outcome as binary
#'     (`is_case = count > 0`); best for rare events.
#' @param family Character. GLM family for `method = "conditional_poisson"`:
#'   `"quasipoisson"` (default, robust to overdispersion) or `"poisson"`.
#'   Ignored for `method = "clogit"`.
#' @param alpha Numeric. Significance level for confidence intervals.
#'   Default: `0.05` (95% CI).
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_casecrossover` list with:
#'   \describe{
#'     \item{`$model`}{The fitted model object: a `glm` for
#'       `"conditional_poisson"` or a `clogit` object for `"clogit"`.}
#'     \item{`$or_table`}{Tibble with one row per exposure term:
#'       `term`, `lag_spec`, `estimate` (log-OR), `or`, `or_lo`, `or_hi`,
#'       `p_value`.}
#'     \item{`$data`}{The analysis dataset after lag creation and NA removal:
#'       columns `date`, `outcome_val`, `exposure_val`, `stratum_id`, and
#'       any covariates.}
#'     \item{`$diagnostics`}{List: `n_obs`, `n_cases`, `n_strata`,
#'       `disp_ratio` (for `"conditional_poisson"`), `method`, `family`.}
#'     \item{`$meta`}{List of analysis parameters: `outcome_col`,
#'       `exposure_col`, `covariates`, `stratum`, `lag`, `method`,
#'       `family`, `alpha`, `call_time`.}
#'   }
#'
#' @references
#' Maclure, M. (1991). The case-crossover design: a method for studying
#' transient effects on the risk of acute events. *American Journal of
#' Epidemiology*, 133(2), 144-153. \doi{10.1093/oxfordjournals.aje.a115853}
#'
#' Levy, D., Lumley, T., Sheppard, L., Kaufman, J., & Checkoway, H. (2001).
#' Referent selection in case-crossover analyses of acute health effects of
#' air pollution. *Epidemiology*, 12(2), 186-192.
#'
#' Whitaker, H.J., Farrington, C.P., Spiessens, B., & Musonda, P. (2006).
#' Tutorial in biostatistics: the self-controlled case series method.
#' *Statistics in Medicine*, 25(10), 1768-1797. \doi{10.1002/sim.2302}
#'
#' Armstrong, B.G., Gasparrini, A., & Tobias, A. (2014). Conditional
#' Poisson models: a flexible alternative to conditional logistic case
#' crossover analysis. *BMC Medical Research Methodology*, 14, 122.
#' \doi{10.1186/1471-2288-14-122}
#'
#' @examples
#' \dontrun{
#' # Conditional Poisson with lag-0 temperature
#' cc <- sus_mod_casecrossover(
#'   df_daily,
#'   outcome_col  = "n_obitos",
#'   exposure_col = "tair_dry_bulb_c",
#'   stratum      = "month",
#'   lag          = 0L,
#'   lang         = "pt"
#' )
#' print(cc)
#' tidy(cc)
#'
#' # Moving-average lag 0-6
#' cc_lag06 <- sus_mod_casecrossover(
#'   df_daily,
#'   outcome_col  = "n_obitos",
#'   exposure_col = "tair_dry_bulb_c",
#'   lag          = 0L:6L,
#'   lang         = "en"
#' )
#' }
#'
#' @seealso [sus_mod_dlnm()], [sus_mod_excess()], [sus_mod_af()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed .data
#' @importFrom dplyr mutate select arrange filter rename all_of any_of pick
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble
#' @importFrom glue glue
sus_mod_casecrossover <- function(
    data,
    outcome_col  = "n_obitos",
    exposure_col,
    covariates   = NULL,
    stratum      = "month",
    lag          = 0L,
    method       = "conditional_poisson",
    family       = "quasipoisson",
    alpha        = 0.05,
    lang         = "pt",
    verbose      = TRUE
) {
  # 1. Language
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(.sccl("warn_lang", "pt", lang = lang))
    lang <- "pt"
  }

  if (verbose) cli::cli_h1("climasus4r \u2014 Case-Crossover")

  # 2. Validate method
  if (!method %in% c("conditional_poisson", "clogit"))
    cli::cli_abort(.sccl("err_bad_method", lang))

  if (method == "clogit" && !requireNamespace("survival", quietly = TRUE))
    cli::cli_abort(.sccl("err_survival", lang))

  # 3. Extract data frame
  df <- if (inherits(data, "climasus_df")) {
    as.data.frame(data)
  } else if (inherits(data, "data.frame")) {
    data
  } else {
    cli::cli_abort("'data' must be a {.cls climasus_df} or a {.cls data.frame}.")
  }

  if (verbose) cli::cli_alert_info(.sccl("step_validate", lang))

  # 4. Check required columns
  if (!"date" %in% names(df))
    cli::cli_abort(.sccl("err_no_date", lang))

  if (!outcome_col %in% names(df)) {
    avail <- paste(names(df), collapse = ", ")
    cli::cli_abort(.sccl("err_no_outcome", lang,
                         outcome_col = outcome_col, avail = avail))
  }

  if (missing(exposure_col) || !exposure_col %in% names(df)) {
    avail <- paste(names(df), collapse = ", ")
    cli::cli_abort(.sccl("err_no_exposure", lang,
                         exposure_col = if (missing(exposure_col)) "(missing)" else exposure_col,
                         avail = avail))
  }

  if (length(stratum) == 1L) {
    if (!stratum %in% c("month", "week") && !stratum %in% names(df))
      cli::cli_abort(.sccl("err_bad_stratum", lang, stratum = stratum))
  } else {
    bad_strat <- setdiff(stratum, names(df))
    if (length(bad_strat) > 0L)
      cli::cli_abort(.sccl("err_bad_stratum", lang,
                           stratum = paste(bad_strat, collapse = ", ")))
  }

  if (!is.null(covariates)) {
    bad_cov <- setdiff(covariates, names(df))
    if (length(bad_cov) > 0L)
      cli::cli_abort("Covariates not found in data: {.val {bad_cov}}.")
  }

  lag <- as.integer(lag)

  if (method == "clogit")
    cli::cli_alert_warning(.sccl("warn_clogit_binary", lang))

  # 5. Build analysis dataset
  df_analysis <- .scc_build_dataset(
    df, outcome_col, exposure_col, covariates, stratum, lag
  )

  n_obs    <- nrow(df_analysis)
  n_strata <- length(unique(df_analysis$stratum_id))
  n_cases  <- sum(df_analysis$outcome_val > 0L, na.rm = TRUE)

  if (n_cases == 0L)
    cli::cli_abort(.sccl("err_no_cases", lang, outcome_col = outcome_col))

  if (n_strata < 12L)
    cli::cli_alert_warning(.sccl("warn_few_strata", lang, n_strata = n_strata))

  lags_str <- if (length(lag) == 1L) as.character(lag) else
    paste0(min(lag), "-", max(lag))

  if (verbose)
    cli::cli_alert_info(.sccl("step_data", lang,
                              n_obs = n_obs, lags_str = lags_str,
                              n_strata = n_strata))

  # 6. Fit model
  if (verbose) cli::cli_alert_info(.sccl("step_fit", lang, method = method))

  model <- .scc_fit(df_analysis, covariates, method, family)

  # 7. Extract OR table
  z_crit  <- stats::qnorm(1 - alpha / 2)
  or_tbl  <- .scc_extract_or(model, method, lag, z_crit)

  # 8. Diagnostics
  disp_ratio <- if (method == "conditional_poisson") {
    tryCatch({
      sm <- summary(model)
      if (!is.null(sm$dispersion)) round(sm$dispersion, 3) else NA_real_
    }, error = function(e) NA_real_)
  } else {
    NA_real_
  }

  diagnostics <- list(
    n_obs      = n_obs,
    n_cases    = n_cases,
    n_strata   = n_strata,
    disp_ratio = disp_ratio,
    method     = method,
    family     = if (method == "conditional_poisson") family else "binomial"
  )

  # 9. Report
  if (verbose && nrow(or_tbl) > 0L) {
    r <- or_tbl[1L, ]
    cli::cli_alert_success(
      .sccl("done", lang,
            or    = round(r$or, 3),
            or_lo = round(r$or_lo, 3),
            or_hi = round(r$or_hi, 3),
            pval  = signif(r$p_value, 3))
    )
  }

  # 10. Return
  structure(
    list(
      model       = model,
      or_table    = or_tbl,
      data        = df_analysis,
      diagnostics = diagnostics,
      meta        = list(
        outcome_col  = outcome_col,
        exposure_col = exposure_col,
        covariates   = covariates,
        stratum      = stratum,
        lag          = lag,
        method       = method,
        family       = family,
        alpha        = alpha,
        call_time    = Sys.time()
      )
    ),
    class = c("climasus_casecrossover", "list")
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' @keywords internal
#' @noRd
.scc_build_dataset <- function(df, outcome_col, exposure_col, covariates,
                                stratum, lag) {
  # Include the custom stratum column(s) when they are not keywords
  keyword_strata <- c("month", "week")
  custom_strat_cols <- if (length(stratum) > 1L) {
    stratum
  } else if (!stratum %in% keyword_strata) {
    stratum
  } else {
    character(0)
  }
  extra_cols <- c(covariates %||% character(0), custom_strat_cols)

  df <- df |>
    dplyr::select(
      date,
      outcome_val  = dplyr::all_of(outcome_col),
      exposure_raw = dplyr::all_of(exposure_col),
      dplyr::any_of(extra_cols)
    ) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      date         = as.Date(date),
      outcome_val  = as.numeric(outcome_val),
      exposure_raw = as.numeric(exposure_raw)
    )

  # Lag creation
  x <- df$exposure_raw
  make_lag_vec <- function(v, k) c(rep(NA_real_, k), head(v, length(v) - k))

  if (length(lag) == 1L) {
    df$exposure_val <- make_lag_vec(x, lag)
  } else {
    lag_mat <- matrix(NA_real_, nrow = nrow(df), ncol = length(lag))
    for (i in seq_along(lag))
      lag_mat[, i] <- make_lag_vec(x, lag[[i]])
    df$exposure_val <- rowMeans(lag_mat, na.rm = FALSE)
  }

  # Stratum definition
  d <- as.Date(df$date)
  df$stratum_id <- if (length(stratum) > 1L) {
    do.call(paste, c(lapply(stratum, function(s) as.character(df[[s]])), list(sep = "-")))
  } else if (stratum == "month") {
    paste(format(d, "%Y"), format(d, "%m"), sep = "-")
  } else if (stratum == "week") {
    paste(format(d, "%G"), sprintf("%02d", as.integer(format(d, "%V"))), sep = "-W")
  } else {
    as.character(df[[stratum]])
  }

  # Drop exposure_raw and NAs; keep only needed columns
  keep <- c("date", "outcome_val", "exposure_val", "stratum_id",
            covariates %||% character(0))
  df |>
    dplyr::select(dplyr::all_of(keep)) |>
    tidyr::drop_na()
}

#' @keywords internal
#' @noRd
.scc_fit <- function(df_analysis, covariates, method, family) {
  cov_terms <- if (!is.null(covariates) && length(covariates) > 0L)
    paste("+", paste(covariates, collapse = " + "))
  else
    ""

  if (method == "conditional_poisson") {
    glm_family <- switch(
      family,
      quasipoisson = stats::quasipoisson(),
      poisson      = stats::poisson(),
      stats::quasipoisson()
    )
    fml <- stats::as.formula(
      paste("outcome_val ~ exposure_val", cov_terms, "+ factor(stratum_id)")
    )
    tryCatch(
      stats::glm(fml, data = df_analysis, family = glm_family),
      error = function(e)
        cli::cli_abort("GLM fitting failed: {conditionMessage(e)}")
    )
  } else {
    # clogit -- binary outcome
    df_analysis$is_case <- as.integer(df_analysis$outcome_val > 0L)
    fml <- stats::as.formula(
      paste("is_case ~ exposure_val", cov_terms,
            "+ survival::strata(stratum_id)")
    )
    tryCatch(
      survival::clogit(fml, data = df_analysis, method = "efron"),
      error = function(e)
        cli::cli_abort("clogit fitting failed: {conditionMessage(e)}")
    )
  }
}

#' @keywords internal
#' @noRd
.scc_extract_or <- function(model, method, lag, z_crit) {
  lag_spec <- if (length(lag) == 1L) as.character(lag) else
    paste0(min(lag), "-", max(lag))

  if (method == "conditional_poisson") {
    sm      <- summary(model)$coefficients
    p_col   <- if ("Pr(>|t|)" %in% colnames(sm)) "Pr(>|t|)" else "Pr(>|z|)"
    if (!"exposure_val" %in% rownames(sm)) return(tibble::tibble())
    est     <- sm["exposure_val", "Estimate"]
    se      <- sm["exposure_val", "Std. Error"]
    p_val   <- sm["exposure_val", p_col]
  } else {
    sm      <- summary(model)$coefficients
    if (!"exposure_val" %in% rownames(sm)) return(tibble::tibble())
    est     <- sm["exposure_val", "coef"]
    se      <- sm["exposure_val", "se(coef)"]
    p_val   <- sm["exposure_val", "Pr(>|z|)"]
  }

  tibble::tibble(
    term     = "exposure_val",
    lag_spec = lag_spec,
    estimate = est,
    or       = exp(est),
    or_lo    = exp(est - z_crit * se),
    or_hi    = exp(est + z_crit * se),
    p_value  = p_val
  )
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_casecrossover object
#'
#' @param x A `climasus_casecrossover` object from `sus_mod_casecrossover()`.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_casecrossover <- function(x, ...) {
  m  <- x$meta
  dg <- x$diagnostics
  tt <- x$or_table

  cli::cli_h2("climasus_casecrossover")
  cli::cli_text("{.strong Method}    : {m$method} ({m$family})")
  cli::cli_text("{.strong Outcome}   : {m$outcome_col}")
  cli::cli_text("{.strong Exposure}  : {m$exposure_col}")
  cli::cli_text("{.strong Lag}       : {paste(m$lag, collapse = ', ')}")
  cli::cli_text("{.strong Stratum}   : {m$stratum}")
  cli::cli_text("{.strong N obs}     : {dg$n_obs} ({dg$n_cases} case days, {dg$n_strata} strata)")
  if (!is.na(dg$disp_ratio))
    cli::cli_text("{.strong Dispersion}: {dg$disp_ratio}")
  cli::cli_rule()

  if (nrow(tt) > 0L) {
    r <- tt[1L, ]
    cli::cli_text(
      "{.strong OR} = {round(r$or, 4)} [{round(r$or_lo, 4)}, {round(r$or_hi, 4)}]  p = {signif(r$p_value, 3)}"
    )
  } else {
    cli::cli_text("No exposure coefficient extracted.")
  }

  invisible(x)
}

#' Summarise a climasus_casecrossover object
#'
#' @param object A `climasus_casecrossover` object from `sus_mod_casecrossover()`.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_casecrossover <- function(object, ...) {
  print(object)
  cat("\n-- OR table --\n")
  print(object$or_table)
  cat("\n-- Model summary --\n")
  print(summary(object$model))
  invisible(object)
}

#' Tidy a climasus_casecrossover object
#'
#' Returns the OR table as a tibble, with analysis metadata columns prepended.
#' Suitable for row-binding across multiple analyses with [dplyr::bind_rows()].
#'
#' @param x A `climasus_casecrossover` object from `sus_mod_casecrossover()`.
#' @param ... Unused.
#' @return A tibble with one row per exposure term.
#' @export
#' @importFrom generics tidy
tidy.climasus_casecrossover <- function(x, ...) {
  m <- x$meta
  dg <- x$diagnostics
  dplyr::bind_cols(
    tibble::tibble(
      outcome_col  = m$outcome_col,
      exposure_col = m$exposure_col,
      method       = m$method,
      stratum      = m$stratum,
      n_obs        = dg$n_obs,
      n_cases      = dg$n_cases,
      n_strata     = dg$n_strata
    ),
    x$or_table
  )
}
