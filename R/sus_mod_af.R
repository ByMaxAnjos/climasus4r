# =============================================================================
# sus_mod_af.R
# Attributable Fraction and Attributable Number from a DLNM fit
#
# Theory: Gasparrini & Armstrong (2013, Epidemiology);
#         Gasparrini et al. (2017, Lancet Planet Health)
# Input : climasus_dlnm object from sus_mod_dlnm()
# Output: climasus_af object with AF/AN + Monte Carlo CI
#
# Methodology:
#   Point estimates: dlnm::crosspred() at observed exposure values.
#   CI: Monte Carlo simulation drawing nsim coefficient vectors from
#       N(coef_cb, vcov_cb) via MASS::mvrnorm, re-computing crosspred
#       for each draw. Falls back to crosspred delta-method bounds when
#       MASS is unavailable.
# =============================================================================

# -- NSE variable declarations ------------------------------------------------
utils::globalVariables(c(
  "period", "an", "cases", "af",
  "month_num", "season",
  "component", "quantile_label", "threshold_val",
  "quantile_prob", "af_pct", "an_lo", "an_hi"
))

# -- Local i18n ---------------------------------------------------------------
.af_labels <- list(
  step_check = list(
    pt = "Verificando entradas...",
    en = "Checking inputs...",
    es = "Verificando entradas..."
  ),
  step_total = list(
    pt = "Calculando FA total ({nsim} simulacoes MC)...",
    en = "Computing total AF ({nsim} MC simulations)...",
    es = "Calculando FA total ({nsim} simulaciones MC)..."
  ),
  step_components = list(
    pt = "Calculando componentes calor/frio (limiar = {thr})...",
    en = "Computing heat/cold components (threshold = {thr})...",
    es = "Calculando componentes calor/frio (umbral = {thr})..."
  ),
  step_quantiles = list(
    pt = "Calculando FA por faixa de percentil...",
    en = "Computing AF by percentile range...",
    es = "Calculando FA por rango de percentil..."
  ),
  step_period = list(
    pt = "Calculando FA por {by}...",
    en = "Computing AF by {by}...",
    es = "Calculando FA por {by}..."
  ),
  done = list(
    pt = "FA total: {af_pct}% [{lo_pct}%, {hi_pct}%] | NA: {an_val} [{an_lo}, {an_hi}]",
    en = "Total AF: {af_pct}% [{lo_pct}%, {hi_pct}%] | AN: {an_val} [{an_lo}, {an_hi}]",
    es = "FA total: {af_pct}% [{lo_pct}%, {hi_pct}%] | NA: {an_val} [{an_lo}, {an_hi}]"
  ),
  warn_mc_fallback = list(
    pt = "Pacote MASS nao encontrado. CI calculado pelo metodo delta (limites do crosspred).",
    en = "MASS not found. CI computed via delta method (crosspred bounds).",
    es = "Paquete MASS no encontrado. IC calculado por metodo delta (limites del crosspred)."
  ),
  err_not_dlnm = list(
    pt = "{.arg fit} deve ser um {.cls climasus_dlnm} de {.fn sus_mod_dlnm}.",
    en = "{.arg fit} must be a {.cls climasus_dlnm} from {.fn sus_mod_dlnm}.",
    es = "{.arg fit} debe ser un {.cls climasus_dlnm} de {.fn sus_mod_dlnm}."
  ),
  err_no_dlnm_pkg = list(
    pt = "Pacote {.pkg dlnm} necessario. Instale com install.packages('dlnm').",
    en = "Package {.pkg dlnm} required. Install with install.packages('dlnm').",
    es = "Paquete {.pkg dlnm} requerido. Instale con install.packages('dlnm')."
  ),
  err_bad_by = list(
    pt = "{.arg by} deve ser NULL, {.val month}, {.val year} ou {.val season}.",
    en = "{.arg by} must be NULL, {.val month}, {.val year}, or {.val season}.",
    es = "{.arg by} debe ser NULL, {.val month}, {.val year} o {.val season}."
  )
)

.mdafl <- function(key, lang, ...) {
  entry <- .af_labels[[key]]
  if (is.null(entry)) return(key)
  txt <- entry[[lang]] %||% entry[["pt"]]
  if (length(list(...)) > 0L) glue::glue(txt, .envir = list2env(list(...))) else txt
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Attributable Fraction and Number from a DLNM Fit
#'
#' Computes population attributable fractions (AF) and attributable numbers (AN)
#' from a `climasus_dlnm` object returned by [sus_mod_dlnm()]. Decomposes the
#' total burden into heat and cold components, reports AF at user-defined
#' exposure percentile ranges, and optionally breaks the burden down by month,
#' year, or season.
#'
#' @section Methodology:
#' Point estimates are obtained by calling `dlnm::crosspred()` at the observed
#' exposure values and computing \eqn{AN_t = n_t \times (1 - 1/RR_t)} for each
#' day. Confidence intervals use Monte Carlo simulation: `nsim` coefficient
#' vectors are drawn from \eqn{N(\hat{\beta}_{cb}, \widehat{V}_{cb})} via
#' `MASS::mvrnorm`, crosspred is re-evaluated for each draw, and AF quantiles
#' yield the CI. When \pkg{MASS} is unavailable the function falls back to
#' delta-method bounds from the original crosspred object.
#'
#' @param fit A `climasus_dlnm` object produced by [sus_mod_dlnm()].
#' @param threshold Numeric or `NULL`. Exposure value splitting heat (above) and
#'   cold (below) components. `NULL` (default) uses `fit$meta$ref_value`.
#' @param range Numeric vector `c(low, high)` or `NULL`. When provided, AF is
#'   also computed for this custom exposure range.
#' @param pred_at Numeric vector (0-1). Quantiles defining the percentile-band
#'   table. For each `q`, the AF above the `q`-th quantile (hot) and below the
#'   `(1-q)`-th quantile (cold) is reported. Default `c(0.75, 0.90, 0.95, 0.99)`.
#' @param by Character or `NULL`. Temporal grouping for the period table:
#'   `"month"`, `"year"`, or `"season"`. `NULL` (default) skips the breakdown.
#' @param nsim Integer. Number of Monte Carlo simulations for CI. Default
#'   `1000L`. Reduce to `200L` for quick exploratory runs; increase to `5000L`
#'   for publication-quality CI.
#' @param alpha Numeric. Significance level for CI. Default `0.05` (95%).
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_af` object (named list) with:
#'   \describe{
#'     \item{`$total`}{One-row-per-component tibble (total/heat/cold) with AF,
#'       AN, and 95% CI.}
#'     \item{`$by_quantile`}{AF/AN at each percentile band in `pred_at`.}
#'     \item{`$by_period`}{Monthly/yearly/seasonal point estimates; `NULL` when
#'       `by = NULL`.}
#'     \item{`$daily`}{Per-day date + exposure + cases + AN + AF for mapping.}
#'     \item{`$custom`}{AF/AN for the custom `range`; `NULL` when not provided.}
#'     \item{`$meta`}{Named list of all parameters used.}
#'   }
#'
#' @references
#' Gasparrini, A., & Armstrong, B. (2013). The impact of heat waves on mortality.
#' *Epidemiology*, 24(1), 64-71.
#' \doi{10.1097/EDE.0b013e3182770cb4}
#'
#' Gasparrini, A., *et al.* (2017). Attributable causes of heat-related mortality
#' across 739 locations. *The Lancet Planetary Health*, 1(9), e360-e367.
#' \doi{10.1016/S2542-5196(17)30156-0}
#'
#' @examples
#' \dontrun{
#' fit <- sus_mod_dlnm(df_dl, outcome_col = "n_obitos", lang = "pt")
#'
#' # Total burden with heat/cold split
#' af <- sus_mod_af(fit, lang = "pt")
#' print(af)
#' tidy(af)
#'
#' # Monthly temporal breakdown
#' af_m <- sus_mod_af(fit, by = "month", nsim = 200L, lang = "en")
#' af_m$by_period
#'
#' # Pool across multiple cities
#' dplyr::bind_rows(tidy(af_city1), tidy(af_city2))
#' }
#'
#' @seealso [sus_mod_dlnm()], [sus_mod_plot_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom tibble tibble
#' @importFrom dplyr mutate summarise arrange across all_of any_of if_else
#' @importFrom purrr map list_rbind
#' @importFrom glue glue
sus_mod_af <- function(
    fit,
    threshold = NULL,
    range     = NULL,
    pred_at   = c(0.75, 0.90, 0.95, 0.99),
    by        = NULL,
    nsim      = 1000L,
    alpha     = 0.05,
    lang      = c("pt", "en", "es"),
    verbose   = TRUE
) {
  lang <- match.arg(lang)

  # -- Package checks ----------------------------------------------------------
  if (!requireNamespace("dlnm", quietly = TRUE))
    cli::cli_abort(.mdafl("err_no_dlnm_pkg", lang))

  if (!is.null(by) && !by %in% c("month", "year", "season"))
    cli::cli_abort(.mdafl("err_bad_by", lang))

  # -- Validate input ----------------------------------------------------------
  if (verbose) cli::cli_h1("climasus4r \u2014 Attributable Fraction (DLNM)")
  if (verbose) cli::cli_alert_info(.mdafl("step_check", lang))

  if (!inherits(fit, "climasus_dlnm"))
    cli::cli_abort(.mdafl("err_not_dlnm", lang))

  # -- Extract model components ------------------------------------------------
  cb      <- fit$crossbasis
  model   <- fit$model
  meta    <- fit$meta
  cen     <- threshold %||% meta$ref_value
  lag0    <- paste0(meta$climate_col, "_lag0")
  df_agg  <- fit$data_daily
  x       <- df_agg[[lag0]]
  cases   <- df_agg$y
  n_cases <- sum(cases, na.rm = TRUE)

  # Pre-compute crosspred at observed exposures (point estimates)
  pred_obs  <- dlnm::crosspred(cb, model, at = x, cen = cen)
  rr_obs    <- as.numeric(pred_obs$allRRfit)
  rr_obs_lo <- as.numeric(pred_obs$allRRlow)
  rr_obs_hi <- as.numeric(pred_obs$allRRhigh)

  # Pre-compute MC coefficient draws if MASS is available
  mc_draws <- NULL
  if (nsim > 0L && requireNamespace("MASS", quietly = TRUE)) {
    cb_nms  <- grep("^cb", names(stats::coef(model)), value = TRUE)
    coef_cb <- stats::coef(model)[cb_nms]
    vcov_cb <- stats::vcov(model)[cb_nms, cb_nms]
    mc_draws <- MASS::mvrnorm(nsim, coef_cb, vcov_cb)
    colnames(mc_draws) <- cb_nms
  } else if (nsim > 0L) {
    cli::cli_alert_warning(.mdafl("warn_mc_fallback", lang))
  }

  # -- 1. Total AF/AN ----------------------------------------------------------
  if (verbose)
    cli::cli_alert_info(.mdafl("step_total", lang, nsim = nsim))

  tot  <- .saf_component(x, cases, cen, n_cases, cb, model,
                         range = NULL, rr_obs, rr_obs_lo, rr_obs_hi,
                         mc_draws, alpha)

  # -- 2. Heat / cold split ----------------------------------------------------
  if (verbose)
    cli::cli_alert_info(
      .mdafl("step_components", lang, thr = round(cen, 2L))
    )

  heat <- .saf_component(x, cases, cen, n_cases, cb, model,
                         range = c(cen, Inf), rr_obs, rr_obs_lo, rr_obs_hi,
                         mc_draws, alpha)
  cold <- .saf_component(x, cases, cen, n_cases, cb, model,
                         range = c(-Inf, cen), rr_obs, rr_obs_lo, rr_obs_hi,
                         mc_draws, alpha)

  total_tbl <- tibble::tibble(
    component = c("total", "heat", "cold"),
    threshold = c(NA_real_, cen, cen),
    af        = c(tot$af,  heat$af,  cold$af),
    af_lo     = c(tot$lo,  heat$lo,  cold$lo),
    af_hi     = c(tot$hi,  heat$hi,  cold$hi),
    af_pct    = round(c(tot$af,  heat$af,  cold$af)  * 100, 2L),
    af_pct_lo = round(c(tot$lo,  heat$lo,  cold$lo)  * 100, 2L),
    af_pct_hi = round(c(tot$hi,  heat$hi,  cold$hi)  * 100, 2L),
    an        = c(tot$an,  heat$an,  cold$an),
    an_lo     = c(tot$an_lo,  heat$an_lo,  cold$an_lo),
    an_hi     = c(tot$an_hi,  heat$an_hi,  cold$an_hi),
    n_cases   = n_cases
  )

  # -- 3. Percentile-band table ------------------------------------------------
  if (verbose) cli::cli_alert_info(.mdafl("step_quantiles", lang))

  qtl_hot  <- stats::quantile(x, pred_at,     na.rm = TRUE)
  qtl_cold <- stats::quantile(x, 1 - pred_at, na.rm = TRUE)

  by_qtl <- purrr::map(seq_along(pred_at), function(i) {
    q     <- pred_at[[i]]
    qh    <- as.numeric(qtl_hot[[i]])
    qc    <- as.numeric(qtl_cold[[i]])
    hot_c <- .saf_component(x, cases, cen, n_cases, cb, model,
                             c(qh, Inf),  rr_obs, rr_obs_lo, rr_obs_hi,
                             mc_draws, alpha)
    cld_c <- .saf_component(x, cases, cen, n_cases, cb, model,
                             c(-Inf, qc), rr_obs, rr_obs_lo, rr_obs_hi,
                             mc_draws, alpha)
    tibble::tibble(
      component      = c("hot",  "cold"),
      quantile_prob  = c(q,      1 - q),
      quantile_label = c(
        paste0("Above P", round(q * 100)),
        paste0("Below P", round((1 - q) * 100))
      ),
      threshold_val  = c(round(qh, 3L), round(qc, 3L)),
      af             = c(hot_c$af,    cld_c$af),
      af_lo          = c(hot_c$lo,    cld_c$lo),
      af_hi          = c(hot_c$hi,    cld_c$hi),
      af_pct         = round(c(hot_c$af, cld_c$af) * 100, 2L),
      an             = c(hot_c$an,    cld_c$an),
      an_lo          = c(hot_c$an_lo, cld_c$an_lo),
      an_hi          = c(hot_c$an_hi, cld_c$an_hi)
    )
  }) |> purrr::list_rbind()

  # -- 4. Custom range ---------------------------------------------------------
  custom_tbl <- NULL
  if (!is.null(range)) {
    cust <- .saf_component(x, cases, cen, n_cases, cb, model,
                           range, rr_obs, rr_obs_lo, rr_obs_hi,
                           mc_draws, alpha)
    custom_tbl <- tibble::tibble(
      range_lo = range[[1L]], range_hi = range[[2L]],
      af    = cust$af, af_lo = cust$lo,    af_hi = cust$hi,
      af_pct = round(cust$af * 100, 2L),
      an    = cust$an, an_lo = cust$an_lo, an_hi = cust$an_hi
    )
  }

  # -- 5. Daily obs-level AN (for mapping / temporal breakdown) ----------------
  an_daily <- cases * (1 - 1 / rr_obs)
  af_daily <- an_daily / cases
  af_daily[cases == 0L] <- NA_real_

  daily_tbl <- tibble::tibble(
    date     = df_agg$date,
    exposure = x,
    cases    = cases,
    an       = an_daily,
    af       = af_daily
  )

  # -- 6. Temporal period breakdown (point estimates only) ----------------------
  period_tbl <- NULL
  if (!is.null(by)) {
    if (verbose)
      cli::cli_alert_info(.mdafl("step_period", lang, by = by))
    period_tbl <- .saf_by_period(daily_tbl, by)
  }

  # -- Verbose summary ---------------------------------------------------------
  if (verbose) {
    cli::cli_alert_success(
      .mdafl("done", lang,
             af_pct = round(tot$af * 100, 2L),
             lo_pct = round(tot$lo * 100, 2L),
             hi_pct = round(tot$hi * 100, 2L),
             an_val = round(tot$an),
             an_lo  = round(tot$an_lo),
             an_hi  = round(tot$an_hi))
    )
  }

  # -- Build result ------------------------------------------------------------
  structure(
    list(
      total       = total_tbl,
      by_quantile = by_qtl,
      by_period   = period_tbl,
      daily       = daily_tbl,
      custom      = custom_tbl,
      meta        = list(
        climate_col = meta$climate_col,
        outcome_col = meta$outcome_col,
        family      = meta$family,
        lag_max     = meta$lag_max,
        ref_value   = meta$ref_value,
        threshold   = cen,
        n_cases     = n_cases,
        n_obs       = nrow(df_agg),
        pred_at     = pred_at,
        nsim        = nsim,
        alpha       = alpha,
        by          = by,
        ci_method   = if (!is.null(mc_draws)) "monte_carlo" else "delta",
        call_time   = Sys.time()
      )
    ),
    class = c("climasus_af", "list")
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Compute AF, AN and CI for one exposure range
#'
#' When mc_draws is non-NULL (MASS available), uses MC CI. Otherwise uses
#' delta-method bounds from the pre-computed crosspred.
#'
#' @keywords internal
#' @noRd
.saf_component <- function(x, cases, cen, n_cases, cb, model,
                            range, rr_obs, rr_obs_lo, rr_obs_hi,
                            mc_draws, alpha) {

  in_range <- if (is.null(range)) {
    rep(TRUE, length(x))
  } else {
    x >= range[[1L]] & x <= range[[2L]]
  }

  # Point estimate
  an_i   <- cases * (1 - 1 / rr_obs)
  an_i[!in_range] <- 0
  an_pt  <- sum(an_i, na.rm = TRUE)
  af_pt  <- an_pt / n_cases

  # CI: Monte Carlo or delta method
  if (!is.null(mc_draws)) {
    cb_nms <- colnames(mc_draws)
    af_sim <- vapply(seq_len(nrow(mc_draws)), function(s) {
      m_s <- model
      m_s$coefficients[cb_nms] <- mc_draws[s, ]
      p_s   <- dlnm::crosspred(cb, m_s, at = x, cen = cen)
      rr_s  <- as.numeric(p_s$allRRfit)
      an_s  <- cases * (1 - 1 / rr_s)
      an_s[!in_range] <- 0
      sum(an_s, na.rm = TRUE) / n_cases
    }, numeric(1L))

    af_lo <- as.numeric(stats::quantile(af_sim, alpha / 2,     na.rm = TRUE))
    af_hi <- as.numeric(stats::quantile(af_sim, 1 - alpha / 2, na.rm = TRUE))
    an_lo <- af_lo * n_cases
    an_hi <- af_hi * n_cases

  } else {
    # Delta-method fallback: use crosspred pointwise bounds
    an_lo_i <- cases * (1 - 1 / rr_obs_hi)
    an_hi_i <- cases * (1 - 1 / rr_obs_lo)
    an_lo_i[!in_range] <- 0
    an_hi_i[!in_range] <- 0
    an_lo_raw <- sum(an_lo_i, na.rm = TRUE)
    an_hi_raw <- sum(an_hi_i, na.rm = TRUE)
    # Ensure ordering (RR < 1 inverts the bounds)
    an_lo <- min(an_lo_raw, an_hi_raw)
    an_hi <- max(an_lo_raw, an_hi_raw)
    af_lo <- an_lo / n_cases
    af_hi <- an_hi / n_cases
  }

  list(af = af_pt, lo = af_lo, hi = af_hi, an = an_pt, an_lo = an_lo, an_hi = an_hi)
}


#' Temporal period breakdown of AN/AF (point estimates)
#' @keywords internal
#' @noRd
.saf_by_period <- function(daily_tbl, by) {

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
      .by    = dplyr::all_of(group_col),
      cases  = sum(cases, na.rm = TRUE),
      an     = round(sum(an, na.rm = TRUE), 1L),
      af     = dplyr::if_else(
        sum(cases, na.rm = TRUE) > 0L,
        sum(an, na.rm = TRUE) / sum(cases, na.rm = TRUE),
        NA_real_
      ),
      af_pct = round(af * 100, 2L)
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_col)))
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_af object
#'
#' @param x A `climasus_af` object.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_af <- function(x, ...) {
  m <- x$meta
  cat("\n-- climasus_af: Attributable Fraction (DLNM) --\n")
  cat(sprintf("  Climate var  : %s\n", m$climate_col))
  cat(sprintf("  Outcome var  : %s\n", m$outcome_col))
  cat(sprintf("  Family       : %s  |  lag max: %d\n", m$family, m$lag_max))
  cat(sprintf("  N obs        : %d  |  N cases: %d\n", m$n_obs, m$n_cases))
  cat(sprintf("  Threshold    : %.3f (heat/cold split)\n", m$threshold))
  cat(sprintf("  CI method    : %s  |  nsim: %d  |  alpha: %.3f\n\n",
              m$ci_method, m$nsim, m$alpha))

  t <- x$total
  cat(sprintf("  %-9s  %s\n", "Component",
              "AF (%)             AN (cases)"))
  cat(sprintf("  %s\n", strrep("-", 56)))
  for (i in seq_len(nrow(t))) {
    cat(sprintf("  %-9s  %6.2f [%6.2f, %6.2f]  %8.0f [%8.0f, %8.0f]\n",
                t$component[i],
                t$af_pct[i], t$af_pct_lo[i], t$af_pct_hi[i],
                t$an[i],     t$an_lo[i],     t$an_hi[i]))
  }
  cat("\n")
  invisible(x)
}

#' Summarise a climasus_af object
#'
#' @param object A `climasus_af` object.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_af <- function(object, ...) {
  print(object)
  cat("-- AF by Percentile Band --\n")
  print(object$by_quantile[, c("quantile_label", "threshold_val",
                                "af_pct", "an")])
  if (!is.null(object$by_period)) {
    cat("\n-- AN/AF by Period --\n")
    print(object$by_period)
  }
  invisible(object)
}

#' Reduce climasus_af to a tidy one-row tibble
#'
#' Returns a single-row tibble suitable for `dplyr::bind_rows()` pooling
#' across multiple cities or time periods.
#'
#' @param x A `climasus_af` object.
#' @param ... Unused.
#' @return A one-row tibble with key attributable fraction statistics.
#' @export
#' @importFrom generics tidy
tidy.climasus_af <- function(x, ...) {
  t <- x$total
  m <- x$meta
  r <- function(comp) t[t$component == comp, ]
  tibble::tibble(
    climate_col  = m$climate_col,
    outcome_col  = m$outcome_col,
    family       = m$family,
    lag_max      = m$lag_max,
    threshold    = m$threshold,
    n_cases      = m$n_cases,
    ci_method    = m$ci_method,
    nsim         = m$nsim,
    af_total     = r("total")$af,
    af_total_lo  = r("total")$af_lo,
    af_total_hi  = r("total")$af_hi,
    af_pct       = r("total")$af_pct,
    af_pct_lo    = r("total")$af_pct_lo,
    af_pct_hi    = r("total")$af_pct_hi,
    an_total     = r("total")$an,
    an_total_lo  = r("total")$an_lo,
    an_total_hi  = r("total")$an_hi,
    af_heat      = r("heat")$af,
    af_cold      = r("cold")$af,
    an_heat      = r("heat")$an,
    an_cold      = r("cold")$an
  )
}
