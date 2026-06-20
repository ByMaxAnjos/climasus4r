# =============================================================================
# sus_data_ts_quality.R
# Time-series quality control for health count data
#
# Purpose: flag municipalities (or any grouping unit) whose daily count series
# are unsuitable for epidemiological modelling due to structural breaks,
# excessive gaps, or low completeness — mirroring the exclusion criteria used
# in large Brazilian multi-city studies (e.g., MCTI 2026 heat-wave study).
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "mun", "n_days", "n_nonmissing", "completeness",
  "has_break", "break_date", "max_gap_days", "recommend",
  "count_val", "date_val", "run_id", "run_len",
  "gap_flag", "sctest_pval"
))


# ── Local i18n ────────────────────────────────────────────────────────────────
.tsq_labels <- list(
  title = list(
    pt = "climasus4r — Qualidade de Séries Temporais",
    en = "climasus4r — Time-Series Quality Control",
    es = "climasus4r — Control de Calidad de Series Temporales"
  ),
  step_validate = list(
    pt = "Validando entradas...",
    en = "Validating inputs...",
    es = "Validando entradas..."
  ),
  step_compute = list(
    pt = "Avaliando {n_groups} série(s)...",
    en = "Evaluating {n_groups} series...",
    es = "Evaluando {n_groups} serie(s)..."
  ),
  done = list(
    pt = "Concluído: {n_inc} incluir, {n_rev} revisar, {n_exc} excluir.",
    en = "Done: {n_inc} include, {n_rev} review, {n_exc} exclude.",
    es = "Listo: {n_inc} incluir, {n_rev} revisar, {n_exc} excluir."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} não suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Falling back to {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  ),
  warn_no_strucchange = list(
    pt = "Pacote {.pkg strucchange} não encontrado. Detecção de quebras estruturais desativada.",
    en = "Package {.pkg strucchange} not found. Structural-break detection disabled.",
    es = "Paquete {.pkg strucchange} no encontrado. Detección de quiebras estructurales desactivada."
  ),
  err_no_date = list(
    pt = "Coluna de data {.val {date_col}} não encontrada. Disponíveis: {.val {avail}}.",
    en = "Date column {.val {date_col}} not found. Available: {.val {avail}}.",
    es = "Columna de fecha {.val {date_col}} no encontrada. Disponibles: {.val {avail}}."
  ),
  err_no_count = list(
    pt = "Coluna de contagem {.val {count_col}} não encontrada. Disponíveis: {.val {avail}}.",
    en = "Count column {.val {count_col}} not found. Available: {.val {avail}}.",
    es = "Columna de conteo {.val {count_col}} no encontrada. Disponibles: {.val {avail}}."
  )
)

.tsql <- function(key, lang, ...) {
  entry <- .tsq_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}


# ── Exported function ─────────────────────────────────────────────────────────

#' Time-Series Quality Control for Health Count Data
#'
#' Evaluates daily health count series by municipality (or any grouping unit)
#' and flags series with low completeness, structural breaks, or excessive
#' consecutive gaps. Returns a per-municipality recommendation (`"include"`,
#' `"review"`, or `"exclude"`) along with detailed diagnostics.
#'
#' This mirrors the exclusion criteria used in multi-city Brazilian health
#' studies (e.g., municipalities excluded due to structural breaks or
#' intermittent reporting).
#'
#' @section Criteria:
#' \describe{
#'   \item{Completeness}{Fraction of days (within the observed date range)
#'     with non-missing, non-zero counts. Series below `min_completeness`
#'     are flagged.}
#'   \item{Structural breaks}{The CUSUM-based empirical fluctuation process
#'     (`strucchange::efp()` + `strucchange::sctest()`) tests for parameter
#'     instability in the time trend. Requires the \pkg{strucchange} package
#'     (in `Suggests`); if unavailable, this check is skipped silently.}
#'   \item{Consecutive gaps}{Maximum run of consecutive days with `NA` or
#'     zero counts. Series with runs exceeding `max_gap_days` are flagged.}
#' }
#'
#' @section Recommendations:
#' \describe{
#'   \item{`"include"`}{Passes all three criteria.}
#'   \item{`"review"`}{Fails one criterion.}
#'   \item{`"exclude"`}{Fails two or more criteria.}
#' }
#'
#' @param df A data frame or `climasus_df` with at minimum a date column, a
#'   daily count column, and optionally a municipality column.
#' @param date_col Character. Name of the date column. Default: `"date"`.
#' @param count_col Character. Name of the daily count column (e.g.,
#'   `"n_obitos"`, `"n_internacoes"`). Default: `"n_obitos"`.
#' @param group_col Character or `NULL`. Column identifying the grouping unit
#'   (e.g., municipality code). When `NULL` (default), the entire dataset is
#'   treated as a single series.
#' @param min_completeness Numeric (0-1). Minimum acceptable fraction of
#'   non-missing, non-zero days. Default: `0.8` (80%).
#' @param max_gap_days Integer. Maximum acceptable consecutive gap (days with
#'   `NA` or zero). Default: `30L`.
#' @param break_alpha Numeric (0-1). Significance level for the structural-break
#'   test. Default: `0.05`.
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A tibble with one row per group (municipality). Columns:
#'   \describe{
#'     \item{`group`}{Group identifier (value from `group_col`, or `"all"`).}
#'     \item{`n_days`}{Integer. Total days in the observed date range.}
#'     \item{`n_nonmissing`}{Integer. Days with non-NA, positive counts.}
#'     \item{`completeness`}{Numeric. Fraction of non-missing days.}
#'     \item{`completeness_ok`}{Logical. `TRUE` if `>= min_completeness`.}
#'     \item{`has_break`}{Logical. `TRUE` if a structural break was detected.}
#'     \item{`break_pval`}{Numeric. P-value from the structural-break test
#'       (`NA` if \pkg{strucchange} unavailable).}
#'     \item{`max_gap_days`}{Integer. Longest consecutive gap.}
#'     \item{`gap_ok`}{Logical. `TRUE` if `max_gap_days <= max_gap_days` arg.}
#'     \item{`n_flags`}{Integer. Number of failed criteria (0-3).}
#'     \item{`recommend`}{Character. `"include"`, `"review"`, or `"exclude"`.}
#'   }
#'
#' @references
#' Zeileis, A., Leisch, F., Hornik, K., & Kleiber, C. (2002). strucchange:
#' an R package for testing for structural change in linear regression models.
#' *Journal of Statistical Software*, 7(2), 1-38. \doi{10.18637/jss.v007.i02}
#'
#' @examples
#' \dontrun{
#' # Run on a daily aggregated SIM dataset
#' qc <- sus_data_ts_quality(
#'   df        = sim_daily,
#'   date_col  = "date",
#'   count_col = "n_obitos",
#'   group_col = "municipio_ibge",
#'   lang      = "pt"
#' )
#'
#' # Keep only municipalities recommended for inclusion
#' good_muns <- qc$group[qc$recommend == "include"]
#' sim_filtered <- dplyr::filter(sim_daily, municipio_ibge %in% good_muns)
#' }
#'
#' @seealso [sus_data_quality_report()], [sus_mod_casecrossover()],
#'   [sus_mod_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'   cli_abort
#' @importFrom dplyr mutate filter arrange summarise group_by n rename
#'   bind_rows
#' @importFrom tibble tibble
#' @importFrom purrr map list_rbind
#' @importFrom glue glue
sus_data_ts_quality <- function(
    df,
    date_col          = "date",
    count_col         = "n_obitos",
    group_col         = NULL,
    min_completeness  = 0.8,
    max_gap_days      = 30L,
    break_alpha       = 0.05,
    lang              = "pt",
    verbose           = TRUE
) {
  # ── Language ────────────────────────────────────────────────────────────────
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(.tsql("warn_lang", "pt", lang = lang))
    lang <- "pt"
  }

  if (verbose) cli::cli_h1(.tsql("title", lang))
  if (verbose) cli::cli_alert_info(.tsql("step_validate", lang))

  # ── Materialise Arrow ───────────────────────────────────────────────────────
  if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table")))
    df <- dplyr::collect(df)

  # ── Validate columns ────────────────────────────────────────────────────────
  avail <- paste(names(df), collapse = ", ")

  if (!date_col %in% names(df))
    cli::cli_abort(.tsql("err_no_date", lang,
                         date_col = date_col, avail = avail))

  if (!count_col %in% names(df))
    cli::cli_abort(.tsql("err_no_count", lang,
                         count_col = count_col, avail = avail))

  # ── strucchange availability ─────────────────────────────────────────────
  use_strucchange <- requireNamespace("strucchange", quietly = TRUE)
  if (!use_strucchange)
    cli::cli_alert_warning(.tsql("warn_no_strucchange", lang))

  # ── Build groups ─────────────────────────────────────────────────────────
  if (is.null(group_col)) {
    groups  <- list(all = df)
    grp_ids <- "all"
  } else {
    group_vals <- unique(df[[group_col]])
    groups  <- lapply(group_vals, function(g) df[df[[group_col]] == g, ])
    grp_ids <- as.character(group_vals)
  }

  n_groups <- length(groups)
  if (verbose) cli::cli_alert_info(.tsql("step_compute", lang, n_groups = n_groups))

  # ── Evaluate each series ─────────────────────────────────────────────────
  results <- purrr::map(seq_along(groups), function(i) {
    .tsq_evaluate_series(
      df_sub       = groups[[i]],
      grp_id       = grp_ids[[i]],
      date_col     = date_col,
      count_col    = count_col,
      min_comp     = min_completeness,
      max_gap      = as.integer(max_gap_days),
      break_alpha  = break_alpha,
      use_strucc   = use_strucchange
    )
  }) |> purrr::list_rbind()

  n_inc <- sum(results$recommend == "include", na.rm = TRUE)
  n_rev <- sum(results$recommend == "review",  na.rm = TRUE)
  n_exc <- sum(results$recommend == "exclude", na.rm = TRUE)

  if (verbose)
    cli::cli_alert_success(.tsql("done", lang,
                                 n_inc = n_inc, n_rev = n_rev, n_exc = n_exc))
  results
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' @keywords internal
#' @noRd
.tsq_evaluate_series <- function(df_sub, grp_id, date_col, count_col,
                                   min_comp, max_gap, break_alpha, use_strucc) {
  dates  <- as.Date(df_sub[[date_col]])
  counts <- as.numeric(df_sub[[count_col]])

  # Completeness
  date_range <- range(dates, na.rm = TRUE)
  n_days_tot <- as.integer(diff(date_range)) + 1L
  n_nonmiss  <- sum(!is.na(counts) & counts > 0L, na.rm = TRUE)
  comp       <- n_nonmiss / n_days_tot
  comp_ok    <- comp >= min_comp

  # Maximum consecutive gap (NA or zero)
  gap_series <- as.integer(is.na(counts) | counts == 0L)
  max_run    <- .tsq_max_run(gap_series)
  gap_ok     <- max_run <= max_gap

  # Structural break (CUSUM-based)
  has_break  <- FALSE
  break_pval <- NA_real_

  if (use_strucc && n_nonmiss >= 20L) {
    x <- seq_along(counts)
    y <- ifelse(is.na(counts), 0L, counts)
    efp_obj <- tryCatch(
      strucchange::efp(y ~ x, type = "OLS-CUSUM"),
      error   = function(e) NULL
    )
    if (!is.null(efp_obj)) {
      sc <- tryCatch(
        strucchange::sctest(efp_obj),
        error = function(e) NULL
      )
      if (!is.null(sc)) {
        break_pval <- sc$p.value
        has_break  <- !is.na(break_pval) && break_pval < break_alpha
      }
    }
  }

  n_flags  <- sum(!comp_ok, has_break, !gap_ok, na.rm = TRUE)
  rec <- if (n_flags == 0L) "include" else if (n_flags == 1L) "review" else "exclude"

  tibble::tibble(
    group          = grp_id,
    n_days         = n_days_tot,
    n_nonmissing   = n_nonmiss,
    completeness   = round(comp, 4L),
    completeness_ok= comp_ok,
    has_break      = has_break,
    break_pval     = break_pval,
    max_gap_days   = max_run,
    gap_ok         = gap_ok,
    n_flags        = n_flags,
    recommend      = rec
  )
}

#' @keywords internal
#' @noRd
.tsq_max_run <- function(x) {
  if (length(x) == 0L || all(x == 0L)) return(0L)
  rle_obj <- rle(x)
  max(rle_obj$lengths[rle_obj$values == 1L], 0L, na.rm = TRUE)
}
