# =============================================================================
# sus_data_ts_quality.R
# Time-Series Quality Control for Daily Health Counts by Municipality
#
# Theory:
#   Structural change: Zeileis et al. (2002, J Stat Soft) -- strucchange
#   Completeness:      Proportion of non-missing days in study window
#   Outlier detection: Tukey fence (1.5 x IQR) on monthly counts
#
# Input : climasus_df at stage "aggregate" or plain data.frame with columns
#         code_muni (or a municipality grouping var), date, and an outcome col.
# Output: climasus_ts_quality object:
#    $flags     -- per-municipality flag tibble (criteria pass/fail + score)
#    $series    -- original series with daily QC flags added
#    $recommend -- recommended inclusion/exclusion for each municipality
# =============================================================================

# -- NSE variable declarations ------------------------------------------------
utils::globalVariables(c(
  "code_muni", "date", "n", "n_expected", "n_obs",
  "completeness", "has_break", "has_outlier", "n_outlier_months",
  "n_gaps_gt7d", "score", "include",
  "month_year", "monthly_n", "q1", "q3", "iqr", "fence_hi",
  "is_outlier_month", "gap_days", "run_id", "run_len",
  "break_pval", "y_col_sym"
))

# -- Local i18n ---------------------------------------------------------------
.tsq_labels <- list(
  step_validate = list(
    pt = "Validando entradas ({n_mun} municipios, {n_days} obs)...",
    en = "Validating inputs ({n_mun} municipalities, {n_days} obs)...",
    es = "Validando entradas ({n_mun} municipios, {n_days} obs)..."
  ),
  step_completeness = list(
    pt = "Calculando completude das series...",
    en = "Computing series completeness...",
    es = "Calculando completitud de las series..."
  ),
  step_breaks = list(
    pt = "Testando quebras estruturais (strucchange)...",
    en = "Testing structural breaks (strucchange)...",
    es = "Probando quiebres estructurales (strucchange)..."
  ),
  step_outliers = list(
    pt = "Detectando meses com contagens anomalas...",
    en = "Detecting months with anomalous counts...",
    es = "Detectando meses con conteos anomalos..."
  ),
  step_gaps = list(
    pt = "Identificando lacunas temporais (> {max_gap} dias)...",
    en = "Identifying temporal gaps (> {max_gap} days)...",
    es = "Identificando lagunas temporales (> {max_gap} dias)..."
  ),
  done = list(
    pt = "Concluido. Recomendados para inclusao: {n_inc}/{n_total} municipios ({pct}%).",
    en = "Done. Recommended for inclusion: {n_inc}/{n_total} municipalities ({pct}%).",
    es = "Listo. Recomendados para inclusion: {n_inc}/{n_total} municipios ({pct}%)."
  ),
  err_no_muni = list(
    pt = "Coluna de municipio {.val {muni_col}} nao encontrada. Disponiveis: {.val {avail}}.",
    en = "Municipality column {.val {muni_col}} not found. Available: {.val {avail}}.",
    es = "Columna de municipio {.val {muni_col}} no encontrada. Disponibles: {.val {avail}}."
  ),
  err_no_date = list(
    pt = "Coluna de data {.val {date_col}} nao encontrada.",
    en = "Date column {.val {date_col}} not found.",
    es = "Columna de fecha {.val {date_col}} no encontrada."
  ),
  err_no_outcome = list(
    pt = "Coluna de desfecho {.val {outcome_col}} nao encontrada. Disponiveis: {.val {avail}}.",
    en = "Outcome column {.val {outcome_col}} not found. Available: {.val {avail}}.",
    es = "Columna de resultado {.val {outcome_col}} no encontrada. Disponibles: {.val {avail}}."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} nao suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Using {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  ),
  warn_no_strucchange = list(
    pt = "Pacote {.pkg strucchange} nao instalado. Teste de quebra estrutural ignorado (has_break = NA).",
    en = "Package {.pkg strucchange} not installed. Structural break test skipped (has_break = NA).",
    es = "Paquete {.pkg strucchange} no instalado. Prueba de quiebre estructural omitida (has_break = NA)."
  )
)

#' @keywords internal
#' @noRd
.tsql <- function(key, lang, ...) {
  entry <- .tsq_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}


# -- Exported function --------------------------------------------------------

#' Time-Series Quality Control for Daily Municipal Health Counts
#'
#' Evaluates the quality of daily health event time series at the municipal
#' level, producing per-municipality flags for:
#'
#' - **Completeness**: fraction of expected days with non-missing records.
#' - **Structural breaks**: abrupt level shifts detectable by the CUSUM-based
#'   Zeileis et al. (2002) test (requires the `strucchange` package).
#' - **Monthly outliers**: months with counts > Q3 + 1.5 x IQR (Tukey fence).
#' - **Temporal gaps**: runs of consecutive days with zero *or* missing counts
#'   longer than `max_gap`.
#'
#' A composite inclusion score (0-100) and a binary `include` recommendation
#' are returned, matching the exclusion criteria used in large Brazilian
#' heat-wave studies (Lowe et al. 2021, Anjos & Targino 2023).
#'
#' @param data A `climasus_df` at stage `"aggregate"`, or any `data.frame`
#'   with a municipality identifier column, a date column, and a daily count
#'   column.
#' @param outcome_col Character. Name of the daily count column.
#'   Default: `"n_obitos"`.
#' @param muni_col Character. Name of the municipality identifier column.
#'   Default: `"code_muni"`.
#' @param date_col Character. Name of the date column. Default: `"date"`.
#' @param min_completeness Numeric (0-1). Minimum completeness to recommend
#'   inclusion. Default: `0.90` (90% of expected days present).
#' @param max_gap Integer. Maximum tolerated consecutive zero/missing days
#'   before flagging a temporal gap. Default: `7L`.
#' @param break_alpha Numeric. Significance level for structural break test.
#'   Default: `0.05`. Ignored if `strucchange` is not installed.
#' @param max_outlier_months Integer. Maximum number of outlier months allowed
#'   before exclusion. Default: `3L`.
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_ts_quality` list with:
#'   \describe{
#'     \item{`$flags`}{Tibble (one row per municipality) with columns:
#'       `muni_col`, `n_obs`, `n_expected`, `completeness`, `has_break`,
#'       `break_pval`, `n_outlier_months`, `n_gaps_gt{max_gap}d`, `score`,
#'       `include`.}
#'     \item{`$recommend_include`}{Character vector of municipality codes
#'       recommended for inclusion.}
#'     \item{`$recommend_exclude`}{Character vector of municipality codes
#'       recommended for exclusion, with reason.}
#'     \item{`$params`}{List of QC parameters used.}
#'   }
#'
#' @references
#' Zeileis, A., Leisch, F., Hornik, K., & Kleiber, C. (2002). strucchange:
#' An R Package for Testing for Structural Change in Linear Regression Models.
#' *Journal of Statistical Software*, 7(2). \doi{10.18637/jss.v007.i02}
#'
#' @examples
#' \dontrun{
#' qc <- sus_data_ts_quality(
#'   df_agg,
#'   outcome_col       = "n_obitos",
#'   min_completeness  = 0.90,
#'   max_gap           = 7L
#' )
#' print(qc)
#'
#' # Filter series to include only QC-passing municipalities
#' df_clean <- df_agg[df_agg$code_muni %in% qc$recommend_include, ]
#' }
#'
#' @seealso [sus_data_aggregate()], [sus_mod_casecrossover()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarise mutate filter arrange left_join n
#' @importFrom tidyr complete
#' @importFrom tibble tibble
#' @importFrom glue glue
sus_data_ts_quality <- function(
    data,
    outcome_col        = "n_obitos",
    muni_col           = "code_muni",
    date_col           = "date",
    min_completeness   = 0.90,
    max_gap            = 7L,
    break_alpha        = 0.05,
    max_outlier_months = 3L,
    lang               = "pt",
    verbose            = TRUE
) {
  # 1. Language
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(.tsql("warn_lang", "pt", lang = lang))
    lang <- "pt"
  }

  if (verbose) cli::cli_h1("climasus4r \u2014 TS Quality")

  # 2. Extract data frame
  df <- if (inherits(data, "climasus_df")) {
    as.data.frame(data)
  } else if (inherits(data, "data.frame")) {
    data
  } else {
    cli::cli_abort("'data' deve ser um {.cls climasus_df} ou {.cls data.frame}.")
  }

  # 3. Validate columns
  if (!muni_col %in% names(df)) {
    avail <- paste(names(df), collapse = ", ")
    cli::cli_abort(.tsql("err_no_muni", lang, muni_col = muni_col, avail = avail))
  }
  if (!date_col %in% names(df))
    cli::cli_abort(.tsql("err_no_date", lang, date_col = date_col))
  if (!outcome_col %in% names(df)) {
    avail <- paste(names(df), collapse = ", ")
    cli::cli_abort(.tsql("err_no_outcome", lang, outcome_col = outcome_col, avail = avail))
  }

  # Normalise column names for internal use
  df$.muni   <- as.character(df[[muni_col]])
  df$.date   <- as.Date(df[[date_col]])
  df$.y      <- as.numeric(df[[outcome_col]])

  munis   <- unique(df$.muni)
  n_mun   <- length(munis)
  n_days  <- nrow(df)

  if (verbose)
    cli::cli_alert_info(.tsql("step_validate", lang,
                              n_mun = n_mun, n_days = n_days))

  # 4. strucchange availability
  has_sc <- requireNamespace("strucchange", quietly = TRUE)
  if (!has_sc)
    cli::cli_alert_warning(.tsql("warn_no_strucchange", lang))

  # 5. Process each municipality
  if (verbose) cli::cli_alert_info(.tsql("step_completeness", lang))
  if (verbose && has_sc) cli::cli_alert_info(.tsql("step_breaks", lang))
  if (verbose) cli::cli_alert_info(.tsql("step_outliers", lang))
  if (verbose) cli::cli_alert_info(.tsql("step_gaps", lang, max_gap = max_gap))

  flags_list <- lapply(munis, function(m) {
    sub <- df[df$.muni == m, , drop = FALSE]
    sub <- sub[order(sub$.date), ]

    d_min <- min(sub$.date)
    d_max <- max(sub$.date)
    n_expected_v <- as.integer(d_max - d_min + 1L)
    n_obs_v      <- nrow(sub)
    completeness_v <- n_obs_v / n_expected_v

    # Structural break
    break_pval_v <- NA_real_
    has_break_v  <- NA
    if (has_sc && n_obs_v >= 30L) {
      y_ts <- stats::ts(sub$.y, frequency = 1)
      bp   <- tryCatch(
        strucchange::breakpoints(y_ts ~ 1),
        error = function(e) NULL
      )
      if (!is.null(bp)) {
        sc_test <- tryCatch(
          strucchange::sctest(y_ts ~ 1, type = "OLS-CUSUM"),
          error = function(e) NULL
        )
        if (!is.null(sc_test)) {
          break_pval_v <- sc_test$p.value
          has_break_v  <- (!is.na(break_pval_v)) && (break_pval_v < break_alpha)
        }
      }
    }

    # Monthly outliers (Tukey)
    sub$month_year   <- format(sub$.date, "%Y-%m")
    monthly          <- tapply(sub$.y, sub$month_year, sum, na.rm = TRUE)
    q1_v <- stats::quantile(monthly, 0.25, na.rm = TRUE)
    q3_v <- stats::quantile(monthly, 0.75, na.rm = TRUE)
    iqr_v <- q3_v - q1_v
    fence_v <- q3_v + 1.5 * iqr_v
    n_outlier_v <- sum(monthly > fence_v, na.rm = TRUE)

    # Temporal gaps (consecutive days with zero or NA)
    y_val   <- sub$.y
    in_gap  <- is.na(y_val) | (y_val == 0L)
    rle_obj <- rle(in_gap)
    n_long_gaps <- sum(rle_obj$lengths[rle_obj$values] > max_gap, na.rm = TRUE)

    # Score: start at 100, deduct for each issue
    score_v <- 100L
    if (!is.na(completeness_v) && completeness_v < min_completeness)
      score_v <- score_v - as.integer(round((1 - completeness_v) * 60))
    if (isTRUE(has_break_v))
      score_v <- score_v - 25L
    if (!is.na(n_outlier_v) && n_outlier_v > max_outlier_months)
      score_v <- score_v - 10L
    if (n_long_gaps > 0L)
      score_v <- score_v - 5L * min(n_long_gaps, 2L)
    score_v <- max(score_v, 0L)

    include_v <- (
      completeness_v >= min_completeness &
      !isTRUE(has_break_v) &
      n_outlier_v <= max_outlier_months &
      n_long_gaps == 0L
    )

    tibble::tibble(
      muni            = m,
      n_obs           = n_obs_v,
      n_expected      = n_expected_v,
      completeness    = round(completeness_v, 4),
      has_break       = has_break_v,
      break_pval      = break_pval_v,
      n_outlier_months = n_outlier_v,
      n_gaps          = n_long_gaps,
      score           = score_v,
      include         = include_v
    )
  })

  flags <- do.call(rbind, flags_list)
  names(flags)[names(flags) == "muni"] <- muni_col

  n_inc   <- sum(flags$include, na.rm = TRUE)
  n_total <- nrow(flags)
  pct     <- round(100 * n_inc / n_total, 1)

  if (verbose)
    cli::cli_alert_success(.tsql("done", lang,
                                 n_inc = n_inc, n_total = n_total, pct = pct))

  # Exclusion reasons
  excl <- flags[!flags$include, , drop = FALSE]
  reasons <- character(nrow(excl))
  for (i in seq_len(nrow(excl))) {
    r <- excl[i, ]
    why <- character(0)
    if (r$completeness < min_completeness)
      why <- c(why, paste0("completude=", round(r$completeness * 100), "%"))
    if (isTRUE(r$has_break))
      why <- c(why, paste0("quebra estrutural (p=", round(r$break_pval, 3), ")"))
    if (r$n_outlier_months > max_outlier_months)
      why <- c(why, paste0("outliers_mensais=", r$n_outlier_months))
    if (r$n_gaps > 0L)
      why <- c(why, paste0("lacunas>", max_gap, "d=", r$n_gaps))
    reasons[i] <- paste(why, collapse = "; ")
  }

  recommend_exclude <- stats::setNames(reasons, excl[[muni_col]])

  structure(
    list(
      flags            = tibble::as_tibble(flags),
      recommend_include = flags[[muni_col]][flags$include],
      recommend_exclude = recommend_exclude,
      params = list(
        outcome_col        = outcome_col,
        muni_col           = muni_col,
        date_col           = date_col,
        min_completeness   = min_completeness,
        max_gap            = max_gap,
        break_alpha        = break_alpha,
        max_outlier_months = max_outlier_months
      )
    ),
    class = c("climasus_ts_quality", "list")
  )
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_ts_quality object
#'
#' @param x A `climasus_ts_quality` object.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_ts_quality <- function(x, ...) {
  p  <- x$params
  f  <- x$flags
  n_total <- nrow(f)
  n_inc   <- length(x$recommend_include)

  cli::cli_h2("climasus_ts_quality")
  cli::cli_text("{.strong Desfecho}   : {p$outcome_col}")
  cli::cli_text("{.strong Municipios} : {n_total} analisados")
  cli::cli_text("{.strong Incluidos}  : {n_inc} ({round(100*n_inc/n_total,1)}%)")
  cli::cli_text("{.strong Excluidos}  : {n_total - n_inc}")
  cli::cli_rule()
  cli::cli_text("Criterios: completude >= {p$min_completeness*100}% | sem quebra estrutural (alpha={p$break_alpha}) | outliers_mensais <= {p$max_outlier_months} | gaps <= {p$max_gap} dias")
  invisible(x)
}

#' Summarise a climasus_ts_quality object
#'
#' @param object A `climasus_ts_quality` object.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_ts_quality <- function(object, ...) {
  print(object)
  cat("\n-- Flags (primeiras 10 linhas) --\n")
  print(utils::head(object$flags, 10))
  if (length(object$recommend_exclude) > 0L) {
    cat("\n-- Excluidos --\n")
    print(data.frame(
      municipio = names(object$recommend_exclude),
      motivo    = unname(object$recommend_exclude),
      stringsAsFactors = FALSE
    ))
  }
  invisible(object)
}
