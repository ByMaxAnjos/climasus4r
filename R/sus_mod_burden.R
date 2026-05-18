# =============================================================================
# sus_mod_burden.R
# Ranked Attributable-Fraction / Excess-Count Table Across Cities or Strata
#
# Theory:
#   Gasparrini et al. (2017, Lancet Planet Health) \u2014 multi-city burden approach
#   Gasparrini & Armstrong (2013, Epidemiology) \u2014 attributable fraction
# Input : Named list of climasus_af, climasus_excess, or climasus_dlnm objects
# Output: climasus_burden with ranked burden table, concentration curve,
#         and aggregate burden statistics
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "city", "component",
  "rank", "pct_of_total", "cumulative_pct"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.burd_labels <- list(
  step_validate = list(
    pt = "Validando {n_cities} ajuste(s) ({input_type})...",
    en = "Validating {n_cities} fit(s) ({input_type})...",
    es = "Validando {n_cities} ajuste(s) ({input_type})..."
  ),
  step_auto_af = list(
    pt = "Calculando FA automaticamente para {n_cities} ajustes DLNM...",
    en = "Auto-computing AF for {n_cities} DLNM fits...",
    es = "Calculando FA automaticamente para {n_cities} ajustes DLNM..."
  ),
  step_rank = list(
    pt = "Classificando por '{rank_by}'...",
    en = "Ranking by '{rank_by}'...",
    es = "Clasificando por '{rank_by}'..."
  ),
  done_af = list(
    pt = "Concluido. AN total = {an_total} | FA media = {af_avg}% | Cidade lider: {top_city}",
    en = "Done. Total AN = {an_total} | Mean AF = {af_avg}% | Top city: {top_city}",
    es = "Listo. AN total = {an_total} | FA media = {af_avg}% | Ciudad principal: {top_city}"
  ),
  done_excess = list(
    pt = "Concluido. Excesso total = {exc_total} | Media = {exc_avg}% | Cidade lider: {top_city}",
    en = "Done. Total excess = {exc_total} | Mean = {exc_avg}% | Top city: {top_city}",
    es = "Listo. Exceso total = {exc_total} | Media = {exc_avg}% | Ciudad principal: {top_city}"
  )
)

#' @keywords internal
#' @noRd
.sbrl <- function(key, lang) {
  entry <- .burd_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Ranked Disease Burden Table Across Cities or Strata
#'
#' Aggregates city-level attributable fraction or excess mortality/morbidity
#' results into a ranked burden table showing each city's contribution to the
#' total population burden. Accepts `climasus_af`, `climasus_excess`, or
#' `climasus_dlnm` objects (DLNM fits are automatically converted to AF via
#' [sus_mod_af()]). Produces a concentration curve suitable for Lorenz-style
#' inequality analysis: how unevenly burden is distributed across cities.
#'
#' @param fits Named list of `climasus_af`, `climasus_excess`, or
#'   `climasus_dlnm` objects. Names become city/stratum labels in the output.
#'   All elements must be the same class; mixing types is not allowed.
#' @param component Character. For `climasus_af` inputs: which heat/cold
#'   component to display and rank. One of `"total"` (default), `"heat"`,
#'   `"cold"`, or `"all"` (all three components; ranking is always derived
#'   from the `"total"` component). Ignored for `climasus_excess` inputs.
#' @param rank_by Character or `NULL`. Metric for ranking cities.
#'   For AF inputs: `"an"` (attributable number, default) or `"af_pct"`
#'   (attributable fraction percent). For excess inputs: `"excess"` (default)
#'   or `"excess_pct"`. `NULL` selects the default for the detected input type.
#' @param top_n Integer or `NULL`. Keep only the top-N cities after ranking.
#'   `NULL` (default) retains all cities.
#' @param nsim Integer. Monte Carlo simulations used when auto-computing AF
#'   from `climasus_dlnm` inputs. `0L` (default) uses the faster delta-method CI.
#' @param alpha Numeric. Significance level for confidence intervals.
#'   Default `0.05`.
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_burden` object (named list) with:
#'   \describe{
#'     \item{`$burden_table`}{Tibble with one row per city (three rows per city
#'       when `component = "all"`). For AF inputs: `city`, `component`,
#'       `n_cases`, `an`, `an_lo`, `an_hi`, `af_pct`, `af_pct_lo`,
#'       `af_pct_hi`, `rank`, `pct_of_total`. For excess inputs: `city`,
#'       `n_days`, `observed`, `expected`, `excess`, `excess_lo`,
#'       `excess_hi`, `excess_pct`, `rank`, `pct_of_total`.}
#'     \item{`$concentration`}{City-level tibble with `city`, `rank`,
#'       `pct_of_total`, and `cumulative_pct` for Lorenz-style concentration
#'       analysis. Always one row per city, based on the total component.}
#'     \item{`$total_burden`}{Named list with aggregate statistics: `an_total`
#'       and `af_pct_avg` for AF inputs; `excess_total` and `excess_pct_avg`
#'       for excess inputs. Also contains `top_city` and the top city's metric.}
#'     \item{`$meta`}{Named list of all parameters used in this call.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Rank cities by attributable number (AN)
#' af_list <- list(
#'   fortaleza = sus_mod_af(fit_fortaleza, verbose = FALSE),
#'   recife    = sus_mod_af(fit_recife,    verbose = FALSE),
#'   salvador  = sus_mod_af(fit_salvador,  verbose = FALSE)
#' )
#' burden <- sus_mod_burden(af_list, lang = "pt")
#' print(burden)
#' tidy(burden)
#'
#' # Show heat/cold split, keep top 5 cities
#' sus_mod_burden(af_list, component = "all", top_n = 5L)
#'
#' # From DLNM fits directly (AF auto-computed via delta method)
#' burden2 <- sus_mod_burden(dlnm_list, nsim = 0L, lang = "en")
#'
#' # Excess-based ranking
#' exc_list <- list(
#'   sp  = sus_mod_excess(fit_sp,  method = "from_dlnm", verbose = FALSE),
#'   rj  = sus_mod_excess(fit_rj,  method = "from_dlnm", verbose = FALSE)
#' )
#' sus_mod_burden(exc_list, rank_by = "excess_pct")
#' }
#'
#' @seealso [sus_mod_af()], [sus_mod_excess()], [sus_mod_pool()],
#'   [sus_mod_metaregression()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success
#' @importFrom cli cli_alert_warning cli_abort cli_text cli_rule
#' @importFrom tibble tibble
#' @importFrom dplyr arrange desc mutate left_join select n
#' @importFrom purrr imap list_rbind
#' @importFrom glue glue
#' @importFrom utils head
sus_mod_burden <- function(
    fits,
    component  = "total",
    rank_by    = NULL,
    top_n      = NULL,
    nsim       = 0L,
    alpha      = 0.05,
    lang       = "pt",
    verbose    = TRUE
) {
  # 1. Language
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(
      "Unsupported language {.val {lang}}. Using {.val pt}."
    )
    lang <- "pt"
  }

  # 2. Validate fits list
  if (!is.list(fits))
    cli::cli_abort("{.arg fits} must be a named list of climasus objects.")
  if (length(fits) == 0L)
    cli::cli_abort("{.arg fits} cannot be empty.")

  city_names <- names(fits)
  if (is.null(city_names) || any(city_names == "")) {
    city_names <- paste0("city_", seq_along(fits))
    names(fits) <- city_names
  }
  n_cities <- length(fits)

  if (verbose)
    cli::cli_h1("climasus4r \u2014 Disease Burden Ranking ({n_cities} cities)")

  # 3. Detect and validate input class
  allowed <- c("climasus_af", "climasus_excess", "climasus_dlnm")

  obj_type <- vapply(fits, function(f) {
    cls <- intersect(class(f), allowed)
    if (length(cls) == 0L) NA_character_ else cls[[1L]]
  }, character(1L))

  if (any(is.na(obj_type))) {
    bad <- names(obj_type)[is.na(obj_type)]
    cli::cli_abort(
      "Elements of {.arg fits} must be {.cls climasus_af}, {.cls climasus_excess}, or {.cls climasus_dlnm}. Invalid: {.val {bad}}."
    )
  }

  unique_types <- unique(obj_type)
  if (length(unique_types) > 1L)
    cli::cli_abort(
      "{.arg fits} contains mixed types ({.val {unique_types}}). All elements must be the same class."
    )

  input_type <- unique_types[[1L]]

  # 4. Auto-compute AF from DLNM inputs
  if (input_type == "climasus_dlnm") {
    if (verbose)
      cli::cli_alert_info(glue::glue(.sbrl("step_auto_af", lang)))
    fits <- lapply(fits, function(f) {
      sus_mod_af(f, nsim = as.integer(nsim), alpha = alpha,
                 lang = lang, verbose = FALSE)
    })
    names(fits) <- city_names
    input_type  <- "climasus_af"
  }

  # 5. Validate component
  if (!component %in% c("total", "heat", "cold", "all"))
    cli::cli_abort(
      "{.arg component} must be {.val total}, {.val heat}, {.val cold}, or {.val all}."
    )

  # 6. Validate and resolve rank_by
  rank_af_ok  <- c("an", "af_pct")
  rank_exc_ok <- c("excess", "excess_pct")

  if (is.null(rank_by)) {
    rank_by <- if (input_type == "climasus_af") "an" else "excess"
  } else if (!rank_by %in% c(rank_af_ok, rank_exc_ok)) {
    cli::cli_abort(
      "{.arg rank_by} must be {.val an}, {.val af_pct}, {.val excess}, or {.val excess_pct}."
    )
  } else if (input_type == "climasus_af" && rank_by %in% rank_exc_ok) {
    cli::cli_alert_warning(
      "{.arg rank_by} = {.val {rank_by}} is not applicable for AF inputs. Using {.val an}."
    )
    rank_by <- "an"
  } else if (input_type == "climasus_excess" && rank_by %in% rank_af_ok) {
    cli::cli_alert_warning(
      "{.arg rank_by} = {.val {rank_by}} is not applicable for excess inputs. Using {.val excess}."
    )
    rank_by <- "excess"
  }

  if (verbose)
    cli::cli_alert_info(glue::glue(.sbrl("step_validate", lang)))

  # 7. Build raw burden table
  if (input_type == "climasus_af") {
    burden_tbl <- .sbrd_from_af(fits, component)
  } else {
    burden_tbl <- .sbrd_from_excess(fits)
  }

  # 8. Rank cities and compute concentration curve
  if (verbose)
    cli::cli_alert_info(glue::glue(.sbrl("step_rank", lang)))

  ranked     <- .sbrd_rank(burden_tbl, rank_by, component, input_type)
  burden_tbl <- ranked$burden
  conc_tbl   <- ranked$concentration

  # 9. Apply top_n filter
  if (!is.null(top_n) && is.numeric(top_n) && top_n > 0L) {
    top_n_int   <- as.integer(top_n)
    keep_cities <- conc_tbl$city[seq_len(min(top_n_int, nrow(conc_tbl)))]
    burden_tbl  <- burden_tbl[burden_tbl$city %in% keep_cities, ]
    conc_tbl    <- conc_tbl[seq_len(min(top_n_int, nrow(conc_tbl))), ]
  }

  # 10. Compute aggregate totals and top-city summary
  if (input_type == "climasus_af") {
    total_rows <- if ("component" %in% names(burden_tbl)) {
      burden_tbl[burden_tbl$component == "total", ]
    } else {
      burden_tbl
    }
    an_total <- round(sum(total_rows$an,      na.rm = TRUE))
    af_avg   <- round(mean(total_rows$af_pct, na.rm = TRUE), 2L)
    top_city <- conc_tbl$city[[1L]]
    top_an   <- total_rows$an[match(top_city, total_rows$city)][[1L]]

    total_burden <- list(
      an_total    = an_total,
      af_pct_avg  = af_avg,
      top_city    = top_city,
      top_city_an = round(top_an)
    )

    if (verbose)
      cli::cli_alert_success(glue::glue(.sbrl("done_af", lang)))

  } else {
    exc_total <- round(sum(burden_tbl$excess,     na.rm = TRUE))
    exc_avg   <- round(mean(burden_tbl$excess_pct, na.rm = TRUE), 2L)
    top_city  <- conc_tbl$city[[1L]]
    top_exc   <- burden_tbl$excess[match(top_city, burden_tbl$city)][[1L]]

    total_burden <- list(
      excess_total    = exc_total,
      excess_pct_avg  = exc_avg,
      top_city        = top_city,
      top_city_excess = round(top_exc)
    )

    if (verbose)
      cli::cli_alert_success(glue::glue(.sbrl("done_excess", lang)))
  }

  # 11. Return
  structure(
    list(
      burden_table  = burden_tbl,
      concentration = conc_tbl,
      total_burden  = total_burden,
      meta          = list(
        input_type = input_type,
        component  = component,
        rank_by    = rank_by,
        n_cities   = n_cities,
        top_n      = top_n,
        nsim       = nsim,
        alpha      = alpha,
        call_time  = Sys.time()
      )
    ),
    class = c("climasus_burden", "list")
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Extract burden table from a list of climasus_af objects
#' @keywords internal
#' @noRd
.sbrd_from_af <- function(fits, component) {
  purrr::imap(fits, function(af, nm) {
    t <- af$total
    rows <- if (component == "all") t else t[t$component == component, ]
    tibble::tibble(
      city      = nm,
      component = rows$component,
      n_cases   = rows$n_cases,
      an        = rows$an,
      an_lo     = rows$an_lo,
      an_hi     = rows$an_hi,
      af_pct    = rows$af_pct,
      af_pct_lo = if ("af_pct_lo" %in% names(rows)) rows$af_pct_lo else NA_real_,
      af_pct_hi = if ("af_pct_hi" %in% names(rows)) rows$af_pct_hi else NA_real_
    )
  }) |> purrr::list_rbind()
}

#' Extract burden table from a list of climasus_excess objects
#' @keywords internal
#' @noRd
.sbrd_from_excess <- function(fits) {
  purrr::imap(fits, function(ex, nm) {
    tt <- ex$total
    tibble::tibble(
      city       = nm,
      n_days     = tt$n_days,
      observed   = tt$observed,
      expected   = round(tt$expected,    1L),
      excess     = round(tt$excess,      1L),
      excess_lo  = round(tt$excess_lo,   1L),
      excess_hi  = round(tt$excess_hi,   1L),
      excess_pct = tt$excess_pct
    )
  }) |> purrr::list_rbind()
}

#' Rank burden table and compute concentration curve
#'
#' For component = "all", ranking is derived from the "total" component and
#' spread to all three component rows of the same city. The concentration curve
#' is always city-level (one row per city) based on the total/primary metric.
#' pct_of_total = val / sum(val) * 100 — always sums to 100 regardless of sign.
#' @keywords internal
#' @noRd
.sbrd_rank <- function(burden_tbl, rank_by, component, input_type) {
  eps <- .Machine$double.eps

  .pct <- function(vals) {
    s <- sum(vals, na.rm = TRUE)
    if (abs(s) < eps) rep(NA_real_, length(vals)) else vals / s * 100
  }

  if (input_type == "climasus_af" && component == "all") {
    total_rows <- burden_tbl[burden_tbl$component == "total", ] |>
      dplyr::arrange(dplyr::desc(.data[[rank_by]])) |>
      dplyr::mutate(rank = seq_len(dplyr::n()))
    total_rows$pct_of_total <- .pct(total_rows[[rank_by]])

    burden_out <- dplyr::left_join(
      burden_tbl,
      total_rows[, c("city", "rank", "pct_of_total")],
      by = "city"
    ) |>
      dplyr::arrange(rank, component)

    conc <- total_rows[, c("city", "rank", "pct_of_total")] |>
      dplyr::mutate(cumulative_pct = cumsum(pct_of_total))

  } else {
    burden_out <- burden_tbl |>
      dplyr::arrange(dplyr::desc(.data[[rank_by]])) |>
      dplyr::mutate(rank = seq_len(dplyr::n()))
    burden_out$pct_of_total <- .pct(burden_out[[rank_by]])

    conc <- burden_out[, c("city", "rank", "pct_of_total")] |>
      dplyr::mutate(cumulative_pct = cumsum(pct_of_total))
  }

  list(burden = burden_out, concentration = conc)
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_burden object
#'
#' @param x A `climasus_burden` object from [sus_mod_burden()].
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_burden <- function(x, ...) {
  m  <- x$meta
  tb <- x$total_burden

  cli::cli_h2("climasus_burden")
  cli::cli_text("{.strong Input type} : {m$input_type}")
  cli::cli_text("{.strong Cities}     : {m$n_cities}")
  cli::cli_text("{.strong Component}  : {m$component}")
  cli::cli_text("{.strong Ranked by}  : {m$rank_by}")
  if (!is.null(m$top_n))
    cli::cli_text("{.strong Top N}      : {m$top_n}")
  cli::cli_rule()

  if (m$input_type == "climasus_af") {
    cli::cli_text("{.strong Total AN}    : {tb$an_total}")
    cli::cli_text("{.strong Mean AF}     : {tb$af_pct_avg}%")
    cli::cli_text("{.strong Top city}    : {tb$top_city} (AN = {tb$top_city_an})")
  } else {
    cli::cli_text("{.strong Total excess}: {tb$excess_total}")
    cli::cli_text("{.strong Mean excess%}: {tb$excess_pct_avg}%")
    cli::cli_text("{.strong Top city}    : {tb$top_city} (excess = {tb$top_city_excess})")
  }

  cli::cli_rule()
  n_show <- min(10L, nrow(x$burden_table))
  cli::cli_text("Burden table (top {n_show} of {nrow(x$burden_table)} rows):")
  print(utils::head(x$burden_table, n_show))
  invisible(x)
}

#' Summarise a climasus_burden object
#'
#' @param object A `climasus_burden` object from [sus_mod_burden()].
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_burden <- function(object, ...) {
  print(object)
  cat("\n-- Concentration curve (cumulative burden by city rank) --\n")
  print(object$concentration)
  invisible(object)
}

#' Tidy a climasus_burden object into a flat tibble
#'
#' Returns the full ranked burden table with the cumulative concentration
#' percentage joined. Each row is one city (one row per component when
#' `component = "all"` was used). Suitable for `dplyr::bind_rows()` across
#' multiple analyses or for plotting.
#'
#' @param x A `climasus_burden` object from [sus_mod_burden()].
#' @param ... Unused.
#' @return A tibble with all burden-table columns plus `cumulative_pct`.
#' @export
#' @importFrom generics tidy
tidy.climasus_burden <- function(x, ...) {
  cn <- x$concentration[, c("city", "cumulative_pct")]
  dplyr::left_join(x$burden_table, cn, by = "city")
}
