# =============================================================================
# sus_mod_sensitivity.R
# Stratified Climate-Health Sensitivity Analysis from DLNM Fits
#
# Theory:
#   Baccini et al. (2011, Epidemiology); Benmarhnia et al. (2015, Environ Health)
#   Armstrong et al. (2017, Environ Epidemiol); Honda et al. (2014)
# Input : named list of climasus_dlnm objects (one per stratum/subgroup)
# Output: climasus_sensitivity object with per-stratum RRs, ranking, full ERCs
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "stratum", "component", "quantile_prob", "exposure", "rr", "rr_lo", "rr_hi",
  "ref_exposure", "hot_rr", "hot_rr_lo", "hot_rr_hi",
  "cold_rr", "cold_rr_lo", "cold_rr_hi",
  "hot_log_rr", "cold_log_rr", "sensitivity_index", "hot_rank", "cold_rank"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.sns_labels <- list(
  step_validate = list(
    pt = "Validando {n_strata} estrato(s)...",
    en = "Validating {n_strata} stratum/strata...",
    es = "Validando {n_strata} estrato(s)..."
  ),
  step_extract = list(
    pt = "Extraindo curvas de exposicao-resposta por estrato...",
    en = "Extracting exposure-response curves per stratum...",
    es = "Extrayendo curvas de exposicion-respuesta por estrato..."
  ),
  step_compare = list(
    pt = "Calculando metricas de sensibilidade (p{hot_pct} quente, p{cold_pct} frio)...",
    en = "Computing sensitivity metrics (p{hot_pct} hot, p{cold_pct} cold)...",
    es = "Calculando metricas de sensibilidad (p{hot_pct} calor, p{cold_pct} frio)..."
  ),
  done = list(
    pt = "Concluido. Estrato mais sensivel ao calor: {top_hot} (RR = {rr_hot}); frio: {top_cold} (RR = {rr_cold})",
    en = "Done. Most heat-sensitive stratum: {top_hot} (RR = {rr_hot}); cold: {top_cold} (RR = {rr_cold})",
    es = "Listo. Estrato mas sensible al calor: {top_hot} (RR = {rr_hot}); frio: {top_cold} (RR = {rr_cold})"
  ),
  err_empty = list(
    pt = "{.arg fits} deve conter pelo menos 2 objetos {.cls climasus_dlnm}.",
    en = "{.arg fits} must contain at least 2 {.cls climasus_dlnm} objects.",
    es = "{.arg fits} debe contener al menos 2 objetos {.cls climasus_dlnm}."
  ),
  err_not_dlnm = list(
    pt = "{.arg fits} deve ser uma lista de objetos {.cls climasus_dlnm}. Elemento(s) invalido(s): {.val {bad}}.",
    en = "{.arg fits} must be a list of {.cls climasus_dlnm} objects. Invalid element(s): {.val {bad}}.",
    es = "{.arg fits} debe ser una lista de objetos {.cls climasus_dlnm}. Elemento(s) invalido(s): {.val {bad}}."
  ),
  err_diff_clim = list(
    pt = "Todos os ajustes devem usar a mesma variavel climatica. Encontrado: {.val {vars}}.",
    en = "All fits must use the same climate variable. Found: {.val {vars}}.",
    es = "Todos los ajustes deben usar la misma variable climatica. Encontradas: {.val {vars}}."
  ),
  err_bad_quantile = list(
    pt = "{.arg hot_percentile} e {.arg cold_percentile} devem estar em (0, 1) com hot > cold.",
    en = "{.arg hot_percentile} and {.arg cold_percentile} must be in (0, 1) with hot > cold.",
    es = "{.arg hot_percentile} y {.arg cold_percentile} deben estar en (0, 1) con hot > cold."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} nao suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Using {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  ),
  warn_extrapolate = list(
    pt = "Estrato {.val {stratum}}: percentil {q} ({exp_val}) fora da grade crosspred [{grid_min}, {grid_max}]. RR interpolado com extrapolacao.",
    en = "Stratum {.val {stratum}}: percentile {q} ({exp_val}) outside crosspred grid [{grid_min}, {grid_max}]. RR extrapolated.",
    es = "Estrato {.val {stratum}}: percentil {q} ({exp_val}) fuera de la grilla crosspred [{grid_min}, {grid_max}]. RR extrapolado."
  )
)

#' @keywords internal
#' @noRd
.snsl <- function(key, lang, ...) {
  entry <- .sns_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}


# ── Exported function ─────────────────────────────────────────────────────────

#' Stratified Climate-Health Sensitivity Analysis from DLNM Fits
#'
#' Compares exposure-response curves and cumulative relative risks across
#' population strata (e.g., age groups, sexes, income quintiles, municipalities)
#' to identify which subgroups are most sensitive to a climate exposure.
#' Takes a named list of `climasus_dlnm` fits — one per stratum — and extracts
#' the cumulative RR at user-specified hot and cold percentiles for each,
#' producing a ranked comparison table and full stratum-specific ERCs for
#' plotting.
#'
#' @section Statistical framework:
#'
#' For each stratum \eqn{s} with fitted DLNM \eqn{\hat{f}_s}, the cumulative
#' RR at exposure quantile \eqn{q} (relative to the stratum's reference
#' \eqn{x_{0,s}}) is:
#'
#' \deqn{RR_s(q) = \exp\!\left\{\sum_{l=0}^{L}
#'   \left[\hat{f}_s\!\left(F_s^{-1}(q),\, l\right)
#'         - \hat{f}_s\!\left(x_{0,s},\, l\right)\right]\right\}}
#'
#' where \eqn{F_s^{-1}(q)} is the \eqn{q}-th quantile of the observed
#' exposure distribution in stratum \eqn{s}. RRs are extracted by linear
#' interpolation from the pre-computed `dlnm::crosspred` grid stored in
#' each `climasus_dlnm` object — no re-fitting or additional `dlnm` calls
#' are needed.
#'
#' The **sensitivity index** for stratum \eqn{s} is defined as:
#'
#' \deqn{SI_s = \log RR_s(\texttt{hot\_percentile}) + \log RR_s(\texttt{cold\_percentile})}
#'
#' a combined measure of total climate-attributable vulnerability (heat + cold).
#'
#' @param fits A named list of `climasus_dlnm` objects, one per stratum.
#'   All fits must use the same `climate_col` and were produced by
#'   `sus_mod_dlnm()`. If the list is unnamed, strata are labelled
#'   `"Stratum 1"`, `"Stratum 2"`, etc.
#' @param hot_percentile Numeric in (0, 1). Quantile of the observed exposure
#'   distribution used as the "hot" comparison point. Default: `0.99` (99th
#'   percentile — extreme heat). Must be greater than `cold_percentile`.
#' @param cold_percentile Numeric in (0, 1). Quantile of the observed exposure
#'   distribution used as the "cold" comparison point. Default: `0.01` (1st
#'   percentile — extreme cold). Must be less than `hot_percentile`.
#' @param stratum_labels Named character vector or `NULL`. Custom display labels
#'   for the strata, with names matching `names(fits)`. If `NULL`, the list
#'   names are used as-is. Useful for plotting (e.g.,
#'   `c(age_65plus = "65+ years", age_1864 = "18-64 years")`).
#' @param alpha Numeric. Significance level for confidence intervals.
#'   Default: `0.05`.
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_sensitivity` list with:
#'   \describe{
#'     \item{`$rr_table`}{Tibble with one row per (stratum, component):
#'       `stratum`, `label`, `component` (`"hot"` or `"cold"`),
#'       `quantile_prob`, `exposure`, `rr`, `rr_lo`, `rr_hi`, `ref_exposure`.}
#'     \item{`$comparison`}{Tibble ranked by `sensitivity_index` (descending):
#'       one row per stratum with `hot_rr`, `hot_rr_lo`, `hot_rr_hi`,
#'       `cold_rr`, `cold_rr_lo`, `cold_rr_hi`, `sensitivity_index`,
#'       `hot_rank`, `cold_rank`.}
#'     \item{`$stratum_curves`}{Tibble with the full exposure-response curve
#'       for each stratum (100-point grid from each `$pred`): columns
#'       `stratum`, `label`, `exposure`, `rr`, `rr_lo`, `rr_hi`. Suitable
#'       for overlaid ERC plots.}
#'     \item{`$meta`}{List: `climate_col`, `n_strata`, `stratum_names`,
#'       `hot_percentile`, `cold_percentile`, `alpha`, `call_time`.}
#'   }
#'
#' @references
#' Baccini, M., et al. (2011). Heat effects on mortality in 15 European cities.
#' *Epidemiology*, 22(1), 68-77. \doi{10.1097/EDE.0b013e3181fdcd5e}
#'
#' Benmarhnia, T., et al. (2015). A difference-in-differences approach to
#' assess the effect of a heat action plan on heat-related mortality, and
#' differences in effectiveness according to sex, age, and socioeconomic status.
#' *Environmental Health Perspectives*, 124(11), 1694-1699.
#' \doi{10.1289/ehp.1510173}
#'
#' Armstrong, B.G., et al. (2017). The role of humidity in associations of
#' high temperature with mortality. *Epidemiology*, 28(6), 781-789.
#'
#' @examples
#' \dontrun{
#' # Fit separate DLNMs per age group
#' fits <- list(
#'   elderly = sus_mod_dlnm(df_65plus, ...),
#'   adults  = sus_mod_dlnm(df_1864,   ...),
#'   youth   = sus_mod_dlnm(df_under18, ...)
#' )
#'
#' sens <- sus_mod_sensitivity(fits, lang = "pt")
#' print(sens)          # ranked comparison table
#' tidy(sens)           # one-row-per-stratum tibble
#' }
#'
#' @seealso [sus_mod_dlnm()], [sus_mod_af()], [sus_mod_pool()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang .data
#' @importFrom dplyr mutate select arrange filter bind_rows rename all_of
#' @importFrom tibble tibble
#' @importFrom purrr map list_rbind imap
#' @importFrom glue glue
sus_mod_sensitivity <- function(
    fits,
    hot_percentile  = 0.99,
    cold_percentile = 0.01,
    stratum_labels  = NULL,
    alpha           = 0.05,
    lang            = "pt",
    verbose         = TRUE
) {
  # 1. Language
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(
      "Unsupported language {.val {lang}}. Using {.val pt}."
    )
    lang <- "pt"
  }

  if (verbose) cli::cli_h1("climasus4r \u2014 Sensitivity Analysis")

  # 2. Validate fits
  if (!is.list(fits) || length(fits) < 2L)
    cli::cli_abort(.snsl("err_empty", lang))

  bad <- names(fits)[!vapply(fits, inherits, logical(1L), "climasus_dlnm")]
  if (length(bad) > 0L) {
    if (is.null(names(fits))) bad <- paste0("[[", which(!vapply(fits, inherits, logical(1L), "climasus_dlnm")), "]]")
    cli::cli_abort(.snsl("err_not_dlnm", lang, bad = paste(bad, collapse = ", ")))
  }

  # Auto-name if unnamed
  if (is.null(names(fits))) names(fits) <- paste0("Stratum ", seq_along(fits))
  stratum_names <- names(fits)
  n_strata      <- length(fits)

  # 3. All fits must use the same climate variable
  clim_cols <- vapply(fits, function(f) f$meta$climate_col, character(1L))
  if (length(unique(clim_cols)) > 1L) {
    cli::cli_abort(.snsl("err_diff_clim", lang, vars = paste(unique(clim_cols), collapse = ", ")))
  }
  climate_col <- clim_cols[[1L]]

  # 4. Validate percentiles
  if (!is.numeric(hot_percentile)  || hot_percentile  <= 0 || hot_percentile  >= 1 ||
      !is.numeric(cold_percentile) || cold_percentile <= 0 || cold_percentile >= 1 ||
      hot_percentile <= cold_percentile) {
    cli::cli_abort(.snsl("err_bad_quantile", lang))
  }

  # 5. Resolve display labels
  labels <- .sns_resolve_labels(stratum_names, stratum_labels)

  if (verbose)
    cli::cli_alert_info(.snsl("step_validate", lang, n_strata = n_strata))

  # 6. Extract full ERCs (100-point grid)
  if (verbose) cli::cli_alert_info(.snsl("step_extract", lang))

  stratum_curves <- purrr::imap(fits, function(fit, nm) {
    pred <- fit$pred
    tibble::tibble(
      stratum  = nm,
      label    = labels[[nm]],
      exposure = as.numeric(pred$predvar),
      rr       = as.numeric(pred$allRRfit),
      rr_lo    = as.numeric(pred$allRRlow),
      rr_hi    = as.numeric(pred$allRRhigh)
    )
  }) |> purrr::list_rbind()

  # 7. Extract RR at hot and cold percentiles per stratum
  hot_pct_label  <- as.integer(round(hot_percentile  * 100))
  cold_pct_label <- as.integer(round(cold_percentile * 100))

  if (verbose)
    cli::cli_alert_info(.snsl("step_compare", lang,
                              hot_pct  = hot_pct_label,
                              cold_pct = cold_pct_label))

  z_crit <- stats::qnorm(1 - alpha / 2)

  rr_rows <- purrr::imap(fits, function(fit, nm) {
    lag0_col  <- paste0(climate_col, "_lag0")
    expo_vals <- fit$data_daily[[lag0_col]]

    hot_x  <- as.numeric(stats::quantile(expo_vals, probs = hot_percentile,  na.rm = TRUE))
    cold_x <- as.numeric(stats::quantile(expo_vals, probs = cold_percentile, na.rm = TRUE))
    ref_x  <- as.numeric(fit$meta$ref_value)

    pred      <- fit$pred
    grid_x    <- as.numeric(pred$predvar)
    grid_rr   <- as.numeric(pred$allRRfit)

    # CI from allRRlow/allRRhigh (already exp-scale)
    grid_lo   <- as.numeric(pred$allRRlow)
    grid_hi   <- as.numeric(pred$allRRhigh)

    .interp <- function(x_at, q_label, comp) {
      # Warn on extrapolation
      if (x_at < min(grid_x) || x_at > max(grid_x)) {
        cli::cli_alert_warning(.snsl("warn_extrapolate", lang,
          stratum  = nm,
          q        = q_label,
          exp_val  = round(x_at, 2),
          grid_min = round(min(grid_x), 2),
          grid_max = round(max(grid_x), 2)
        ))
      }
      rr_val <- stats::approx(grid_x, grid_rr, xout = x_at, rule = 2)$y
      lo_val <- stats::approx(grid_x, grid_lo, xout = x_at, rule = 2)$y
      hi_val <- stats::approx(grid_x, grid_hi, xout = x_at, rule = 2)$y

      tibble::tibble(
        stratum       = nm,
        label         = labels[[nm]],
        component     = comp,
        quantile_prob = if (comp == "hot") hot_percentile else cold_percentile,
        exposure      = x_at,
        rr            = rr_val,
        rr_lo         = lo_val,
        rr_hi         = hi_val,
        ref_exposure  = ref_x
      )
    }

    dplyr::bind_rows(
      .interp(hot_x,  hot_pct_label,  "hot"),
      .interp(cold_x, cold_pct_label, "cold")
    )
  }) |> purrr::list_rbind()

  # 8. Build comparison table: one row per stratum with hot + cold side by side
  comparison <- .sns_build_comparison(rr_rows, stratum_names, labels)

  # 9. Report
  if (verbose && nrow(comparison) > 0L) {
    top_h <- comparison$label[[1L]]
    rr_h  <- round(comparison$hot_rr[[1L]], 3)
    cold_ord <- comparison |> dplyr::arrange(dplyr::desc(cold_rr))
    top_c  <- cold_ord$label[[1L]]
    rr_c   <- round(cold_ord$cold_rr[[1L]], 3)
    cli::cli_alert_success(
      .snsl("done", lang,
            top_hot  = top_h, rr_hot  = rr_h,
            top_cold = top_c, rr_cold = rr_c)
    )
  }

  # 10. Return
  structure(
    list(
      rr_table       = rr_rows,
      comparison     = comparison,
      stratum_curves = stratum_curves,
      meta           = list(
        climate_col     = climate_col,
        n_strata        = n_strata,
        stratum_names   = stratum_names,
        stratum_labels  = labels,
        hot_percentile  = hot_percentile,
        cold_percentile = cold_percentile,
        alpha           = alpha,
        call_time       = Sys.time()
      )
    ),
    class = c("climasus_sensitivity", "list")
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' @keywords internal
#' @noRd
.sns_resolve_labels <- function(stratum_names, stratum_labels) {
  labels <- stats::setNames(stratum_names, stratum_names)
  if (!is.null(stratum_labels)) {
    matched <- intersect(names(stratum_labels), stratum_names)
    labels[matched] <- stratum_labels[matched]
  }
  labels
}

#' @keywords internal
#' @noRd
.sns_build_comparison <- function(rr_rows, stratum_names, labels) {
  hot_rows  <- rr_rows[rr_rows$component == "hot",  ]
  cold_rows <- rr_rows[rr_rows$component == "cold", ]

  rows <- lapply(stratum_names, function(nm) {
    h <- hot_rows[hot_rows$stratum  == nm, ]
    c <- cold_rows[cold_rows$stratum == nm, ]

    h_rr  <- if (nrow(h) > 0L) h$rr[[1L]]    else NA_real_
    h_lo  <- if (nrow(h) > 0L) h$rr_lo[[1L]] else NA_real_
    h_hi  <- if (nrow(h) > 0L) h$rr_hi[[1L]] else NA_real_
    c_rr  <- if (nrow(c) > 0L) c$rr[[1L]]    else NA_real_
    c_lo  <- if (nrow(c) > 0L) c$rr_lo[[1L]] else NA_real_
    c_hi  <- if (nrow(c) > 0L) c$rr_hi[[1L]] else NA_real_

    hot_log  <- if (!is.na(h_rr) && h_rr > 0) log(h_rr) else NA_real_
    cold_log <- if (!is.na(c_rr) && c_rr > 0) log(c_rr) else NA_real_
    si       <- if (!is.na(hot_log) && !is.na(cold_log)) hot_log + cold_log else NA_real_

    tibble::tibble(
      stratum            = nm,
      label              = labels[[nm]],
      hot_rr             = h_rr,
      hot_rr_lo          = h_lo,
      hot_rr_hi          = h_hi,
      cold_rr            = c_rr,
      cold_rr_lo         = c_lo,
      cold_rr_hi         = c_hi,
      sensitivity_index  = si
    )
  })

  tbl <- do.call(rbind, rows)

  # Rank strata
  tbl$hot_rank  <- rank(-tbl$hot_rr,  na.last = "keep", ties.method = "min")
  tbl$cold_rank <- rank(-tbl$cold_rr, na.last = "keep", ties.method = "min")

  tbl |> dplyr::arrange(dplyr::desc(sensitivity_index))
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_sensitivity object
#'
#' @param x A `climasus_sensitivity` object from `sus_mod_sensitivity()`.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_sensitivity <- function(x, ...) {
  m  <- x$meta
  cm <- x$comparison

  cli::cli_h2("climasus_sensitivity")
  cli::cli_text("{.strong Climate variable}: {m$climate_col}")
  cli::cli_text("{.strong Strata}          : {m$n_strata}")
  cli::cli_text("{.strong Hot percentile}  : p{as.integer(m$hot_percentile * 100)}")
  cli::cli_text("{.strong Cold percentile} : p{as.integer(m$cold_percentile * 100)}")
  cli::cli_rule()

  cli::cli_text("{.strong Sensitivity ranking (heat + cold combined):}")
  for (i in seq_len(nrow(cm))) {
    r <- cm[i, ]
    cli::cli_text(
      "  #{i}  {r$label}  |  Hot RR: {round(r$hot_rr, 3)} [{round(r$hot_rr_lo, 3)}, {round(r$hot_rr_hi, 3)}]  |  Cold RR: {round(r$cold_rr, 3)} [{round(r$cold_rr_lo, 3)}, {round(r$cold_rr_hi, 3)}]  |  SI: {round(r$sensitivity_index, 4)}"
    )
  }
  invisible(x)
}

#' Summarise a climasus_sensitivity object
#'
#' @param object A `climasus_sensitivity` object from `sus_mod_sensitivity()`.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_sensitivity <- function(object, ...) {
  print(object)
  cat("\n-- Full RR table --\n")
  print(object$rr_table)
  invisible(object)
}

#' Tidy a climasus_sensitivity object
#'
#' Returns the comparison table with metadata columns prepended, suitable for
#' combining results across analyses with [dplyr::bind_rows()].
#'
#' @param x A `climasus_sensitivity` object from `sus_mod_sensitivity()`.
#' @param ... Unused.
#' @return A tibble with one row per stratum.
#' @export
#' @importFrom generics tidy
tidy.climasus_sensitivity <- function(x, ...) {
  m <- x$meta
  dplyr::bind_cols(
    tibble::tibble(
      climate_col     = m$climate_col,
      hot_percentile  = m$hot_percentile,
      cold_percentile = m$cold_percentile,
      n_strata        = m$n_strata
    ),
    x$comparison
  )
}
