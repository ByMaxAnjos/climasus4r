# =============================================================================
# sus_mod_spacetime_exceedance.R
# Posterior Exceedance Probabilities from Spatiotemporal Bayesian Models
#
# Theory:
#   Richardson, S., Thomson, A., Best, N., & Elliott, P. (2004). Interpreting
#     posterior relative risk estimates in disease-mapping studies.
#     Environmental Health Perspectives, 112(9), 1016-1025.
#   Lawson, A. B. (2018). Bayesian Disease Mapping: Hierarchical Modeling in
#     Spatial Epidemiology (3rd ed.). CRC Press.
#   Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., &
#     Rubin, D. B. (2013). Bayesian Data Analysis (3rd ed.). CRC Press.
# Input : climasus_spacetime_bayes object from sus_mod_spacetime_bayes()
# Output: climasus_spacetime_exceedance (list) with per-cell exceedance
#         probabilities P(RR > threshold) for each municipality x time cell
# =============================================================================

# -- NSE variable declarations -------------------------------------------------
utils::globalVariables(c(
  "code_muni", "time_idx", "rr_mean", "rr_lower95", "rr_upper95",
  "sigma_log", "log_rr_mean", "n_exceed", "time_period"
))

# -- Local i18n ----------------------------------------------------------------
.exc_msgs <- list(

  step_validate = list(
    pt = "Validando objeto de entrada ({.cls climasus_spacetime_bayes})...",
    en = "Validating input object ({.cls climasus_spacetime_bayes})...",
    es = "Validando objeto de entrada ({.cls climasus_spacetime_bayes})..."
  ),

  step_compute = list(
    pt = "Calculando P(RR > limiar) para {n_thresh} limiar(es) em {n_cells} c\u00e9lulas...",
    en = "Computing P(RR > threshold) for {n_thresh} threshold(s) across {n_cells} cells...",
    es = "Calculando P(RR > umbral) para {n_thresh} umbral(es) en {n_cells} celdas..."
  ),

  step_aggregate = list(
    pt = "Agregando dimens\u00e3o temporal por {period}...",
    en = "Aggregating temporal dimension by {period}...",
    es = "Agregando dimensi\u00f3n temporal por {period}..."
  ),

  step_spatial_join = list(
    pt = "Adicionando geometria espacial ao resultado...",
    en = "Adding spatial geometry to the result...",
    es = "Agregando geometr\u00eda espacial al resultado..."
  ),

  step_summarise = list(
    pt = "Sumarizando contagens de exceed\u00eancia por limiar...",
    en = "Summarising exceedance counts per threshold...",
    es = "Resumiendo conteos de excedencia por umbral..."
  ),

  done = list(
    pt = "Conclu\u00eddo. {n_cells} c\u00e9lulas avaliadas | {n_thresh} limiares | {pct_exceed}% das c\u00e9lulas excedem RR > 1 (P > 0.80)",
    en = "Done. {n_cells} cells evaluated | {n_thresh} thresholds | {pct_exceed}% of cells exceed RR > 1 (P > 0.80)",
    es = "Listo. {n_cells} celdas evaluadas | {n_thresh} umbrales | {pct_exceed}% de celdas superan RR > 1 (P > 0.80)"
  ),

  err_not_spacetime = list(
    pt = "{.arg fit} deve ser um objeto {.cls climasus_spacetime_bayes} produzido por {.fn sus_mod_spacetime_bayes}.",
    en = "{.arg fit} must be a {.cls climasus_spacetime_bayes} object from {.fn sus_mod_spacetime_bayes}.",
    es = "{.arg fit} debe ser un objeto {.cls climasus_spacetime_bayes} de {.fn sus_mod_spacetime_bayes}."
  ),

  err_no_rr = list(
    pt = "O objeto {.arg fit} n\u00e3o cont\u00e9m {.field $rr}. Verifique se o modelo foi ajustado corretamente.",
    en = "The {.arg fit} object does not contain {.field $rr}. Check that the model was fitted correctly.",
    es = "El objeto {.arg fit} no contiene {.field $rr}. Verifique que el modelo fue ajustado correctamente."
  ),

  err_rr_cols = list(
    pt = "{.field $rr} deve conter as colunas: {.val {required}}. Colunas encontradas: {.val {found}}.",
    en = "{.field $rr} must contain columns: {.val {required}}. Columns found: {.val {found}}.",
    es = "{.field $rr} debe contener las columnas: {.val {required}}. Columnas encontradas: {.val {found}}."
  ),

  err_bad_thresholds = list(
    pt = "{.arg thresholds} deve ser um vetor num\u00e9rico positivo. Valores inv\u00e1lidos detectados.",
    en = "{.arg thresholds} must be a positive numeric vector. Invalid values detected.",
    es = "{.arg thresholds} debe ser un vector num\u00e9rico positivo. Se detectaron valores inv\u00e1lidos."
  ),

  err_bad_aggregate = list(
    pt = "{.arg aggregate_time} deve ser {.val year} ou {.val month}, ou {.code NULL} para sem agrega\u00e7\u00e3o.",
    en = "{.arg aggregate_time} must be {.val year} or {.val month}, or {.code NULL} for no aggregation.",
    es = "{.arg aggregate_time} debe ser {.val year} o {.val month}, o {.code NULL} para sin agregaci\u00f3n."
  ),

  err_no_sf = list(
    pt = "{.arg municipalities} deve ser um objeto {.cls sf}. Use {.fn geobr::read_municipality} para obter as geometrias.",
    en = "{.arg municipalities} must be an {.cls sf} object. Use {.fn geobr::read_municipality} to obtain geometries.",
    es = "{.arg municipalities} debe ser un objeto {.cls sf}. Use {.fn geobr::read_municipality} para obtener las geometr\u00edas."
  ),

  err_no_time_col = list(
    pt = "{.field $rr} cont\u00e9m {.field time_idx} mas agrega\u00e7\u00e3o por {.val {period}} requer um formato de data v\u00e1lido (YYYY-MM-DD ou YYYY-MM).",
    en = "{.field $rr} contains {.field time_idx} but aggregation by {.val {period}} requires a valid date format (YYYY-MM-DD or YYYY-MM).",
    es = "{.field $rr} contiene {.field time_idx} pero la agregaci\u00f3n por {.val {period}} requiere un formato de fecha v\u00e1lido (YYYY-MM-DD o YYYY-MM)."
  ),

  warn_na_rr = list(
    pt = "{n_na} c\u00e9lulas com RR NA ou zero removidas antes do c\u00e1lculo de exceed\u00eancia.",
    en = "{n_na} cells with NA or zero RR removed before exceedance computation.",
    es = "{n_na} celdas con RR NA o cero eliminadas antes del c\u00e1lculo de excedencia."
  ),

  warn_sigma_zero = list(
    pt = "{n_zero} c\u00e9lulas com intervalo de credibilidade zero (rr_lower95 == rr_upper95). P(RR > t) ser\u00e1 0 ou 1 para essas c\u00e9lulas.",
    en = "{n_zero} cells with zero credible interval (rr_lower95 == rr_upper95). P(RR > t) will be 0 or 1 for those cells.",
    es = "{n_zero} celdas con intervalo de credibilidad cero (rr_lower95 == rr_upper95). P(RR > t) ser\u00e1 0 o 1 para esas celdas."
  )
)

# -- Internal message helper ---------------------------------------------------
#' @keywords internal
#' @noRd
.excl <- function(key, lang, ...) {
  entry <- .exc_msgs[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) {
    vars <- list(...)
    for (nm in names(vars)) msg <- gsub(sprintf("\\{%s\\}", nm), as.character(vars[[nm]]), msg)
  }
  msg
}

# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Posterior Exceedance Probabilities from a Spatiotemporal Bayesian Model
#'
#' @description
#' Given a fitted `climasus_spacetime_bayes` object, computes for each
#' municipality \eqn{\times} time cell the posterior probability that the
#' relative risk (RR) exceeds one or more critical thresholds:
#' \eqn{P(RR > t \mid \text{data})}.
#'
#' Exceedance probabilities are a cornerstone of Bayesian disease surveillance
#' because they translate uncertain posterior estimates into decision-relevant
#' quantities. For example, \eqn{P(RR > 1.5) > 0.80} can trigger an alert for
#' a municipality in a given week, whereas the raw posterior mean alone may
#' understate uncertainty.
#'
#' @section Approximation method:
#'
#' Exact computation requires access to the full MCMC sample or INLA posterior
#' marginals. When only the posterior mean and 95\% credible interval are
#' stored (as in the `$rr` slot), exceedance probabilities are approximated
#' via a **log-normal approximation**:
#'
#' \deqn{
#'   \sigma_{\log} = \frac{\log(\text{RR}_{\text{upper95}}) -
#'                         \log(\text{RR}_{\text{lower95}})}{2 \times 1.96}
#' }
#'
#' \deqn{
#'   P(RR > t) \approx \Phi\!\left(
#'     \frac{\log(\text{RR}_{\text{mean}}) - \log(t)}{\sigma_{\log}}
#'   \right)
#' }
#'
#' where \eqn{\Phi} is the standard normal CDF. This is equivalent to assuming
#' that \eqn{\log(RR)} is approximately normally distributed in the posterior,
#' which is a standard approximation for count-data hierarchical models
#' (Richardson et al., 2004).
#'
#' For exact computation when the full posterior is available, use
#' `INLA::inla.pmarginal()` on the fitted INLA marginals directly.
#'
#' @section Temporal aggregation:
#'
#' When `aggregate_time` is `"year"` or `"month"`, the function averages
#' \eqn{\log(RR_{\text{mean}})} and \eqn{\sigma_{\log}} across cells within
#' each municipality \eqn{\times} time period, then recomputes exceedance
#' probabilities for the aggregated values. This is appropriate for
#' surveillance summaries but not for causal inference.
#'
#' @param fit A `climasus_spacetime_bayes` object produced by
#'   [sus_mod_spacetime_bayes()]. Must contain a `$rr` data frame with
#'   columns `code_muni`, `time_idx`, `rr_mean`, `rr_lower95`, `rr_upper95`.
#' @param thresholds Numeric vector of positive RR thresholds for which
#'   \eqn{P(RR > t)} is computed. Default `c(1.0, 1.5, 2.0)`.
#' @param aggregate_time Character or `NULL`. If `"year"` or `"month"`,
#'   aggregate the temporal dimension before reporting exceedances. Requires
#'   that `time_idx` be coercible to `Date` (formats `"YYYY-MM-DD"` or
#'   `"YYYY-MM"`). Default `NULL` (no aggregation).
#' @param municipalities An `sf` object with at least a `code_muni` column
#'   and geometry. If provided, geometry is joined to the exceedance table
#'   so the result can be mapped directly. Requires the **sf** package.
#'   Default `NULL`.
#' @param lang Character. Language for CLI messages: `"pt"` (default),
#'   `"en"`, or `"es"`.
#' @param verbose Logical. If `TRUE` (default), print progress messages via
#'   **cli**.
#'
#' @return A named list of class `c("climasus_spacetime_exceedance", "list")`
#'   with the following slots:
#'
#'   \describe{
#'     \item{`$exceedance`}{`data.frame` with columns `code_muni`,
#'       `time_idx` (or `time_period` if aggregated), `rr_mean`, and one
#'       `p_gt_X` column per threshold (e.g. `p_gt_1_0`, `p_gt_1_5`,
#'       `p_gt_2_0`).}
#'     \item{`$thresholds`}{Numeric vector of thresholds used.}
#'     \item{`$n_exceed`}{`data.frame` with columns `threshold` and
#'       `n_cells_exceed` counting how many cells have
#'       \eqn{P(RR > t) > 0.80}.}
#'     \item{`$call`}{The matched call.}
#'   }
#'
#' @references
#' Richardson, S., Thomson, A., Best, N., & Elliott, P. (2004). Interpreting
#' posterior relative risk estimates in disease-mapping studies.
#' \emph{Environmental Health Perspectives}, 112(9), 1016-1025.
#'
#' Lawson, A. B. (2018). \emph{Bayesian Disease Mapping: Hierarchical Modeling
#' in Spatial Epidemiology} (3rd ed.). CRC Press.
#'
#' Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., &
#' Rubin, D. B. (2013). \emph{Bayesian Data Analysis} (3rd ed.). CRC Press.
#'
#' @seealso
#' [sus_mod_spacetime_bayes()] to fit the spatiotemporal Bayesian model,
#' [sus_mod_spatial_bayes()] for the purely spatial (cross-sectional) version,
#' [sus_mod_spatial_moran()] for exploratory spatial autocorrelation analysis.
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Fit a spatiotemporal Bayesian model (requires climasus_spacetime_bayes)
#' # fit <- sus_mod_spacetime_bayes(df = agg, outcome = "deaths", ...)
#'
#' # Compute exceedance probabilities for RR thresholds 1.0, 1.5, and 2.0
#' exc <- sus_mod_spacetime_exceedance(
#'   fit        = fit,
#'   thresholds = c(1.0, 1.5, 2.0),
#'   lang       = "pt"
#' )
#'
#' print(exc)
#' head(exc$exceedance)
#' exc$n_exceed
#'
#' # Aggregate to annual exceedances and join municipality geometry
#' library(geobr)
#' shp <- geobr::read_municipality(code_muni = "all", year = 2022)
#' exc_annual <- sus_mod_spacetime_exceedance(
#'   fit            = fit,
#'   thresholds     = c(1.0, 1.5, 2.0),
#'   aggregate_time = "year",
#'   municipalities = shp,
#'   lang           = "pt"
#' )
#' }
#'
#' @export
sus_mod_spacetime_exceedance <- function(
    fit,
    thresholds     = c(1.0, 1.5, 2.0),
    aggregate_time = NULL,
    municipalities = NULL,
    lang           = "pt",
    verbose        = TRUE
) {
  .call <- match.call()
  lang  <- match.arg(lang, c("pt", "en", "es"))

  # ── 1. validate inputs ───────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.excl("step_validate", lang))

  if (!inherits(fit, "climasus_spacetime_bayes")) {
    cli::cli_abort(.excl("err_not_spacetime", lang))
  }

  if (is.null(fit$rr) || !is.data.frame(fit$rr)) {
    cli::cli_abort(.excl("err_no_rr", lang))
  }

  required_cols <- c("code_muni", "time_idx", "rr_mean", "rr_lower95", "rr_upper95")
  found_cols    <- names(fit$rr)
  missing_req   <- setdiff(required_cols, found_cols)
  if (length(missing_req) > 0L) {
    cli::cli_abort(
      .excl("err_rr_cols", lang,
            required = paste(required_cols, collapse = ", "),
            found    = paste(found_cols,    collapse = ", "))
    )
  }

  # validate thresholds
  if (!is.numeric(thresholds) || any(is.na(thresholds)) || any(thresholds <= 0)) {
    cli::cli_abort(.excl("err_bad_thresholds", lang))
  }
  thresholds <- sort(unique(thresholds))

  # validate aggregate_time
  if (!is.null(aggregate_time)) {
    aggregate_time <- match.arg(aggregate_time, c("year", "month"))
  }

  # validate municipalities (sf)
  if (!is.null(municipalities)) {
    if (!inherits(municipalities, "sf")) {
      cli::cli_abort(.excl("err_no_sf", lang))
    }
    rlang::check_installed("sf", reason = "for spatial join of municipality geometries")
  }

  # ── 2. prepare working data.frame ────────────────────────────────────────────
  rr_df <- fit$rr

  # remove cells with NA or non-positive RR (cannot compute log)
  bad_rows <- is.na(rr_df$rr_mean) | rr_df$rr_mean <= 0 |
              is.na(rr_df$rr_lower95) | is.na(rr_df$rr_upper95)
  n_na <- sum(bad_rows)
  if (n_na > 0L && verbose) {
    cli::cli_alert_warning(.excl("warn_na_rr", lang, n_na = n_na))
  }
  rr_df <- rr_df[!bad_rows, , drop = FALSE]

  # ── 3. optional temporal aggregation ─────────────────────────────────────────
  if (!is.null(aggregate_time)) {
    if (verbose) {
      cli::cli_progress_step(.excl("step_aggregate", lang, period = aggregate_time))
    }

    # attempt to parse time_idx as Date
    time_vec <- tryCatch(
      {
        tv <- rr_df$time_idx
        if (is.numeric(tv)) {
          # assume integer year
          as.Date(paste0(as.integer(tv), "-01-01"))
        } else {
          tv_chr <- as.character(tv)
          # try YYYY-MM-DD first, then YYYY-MM
          parsed <- suppressWarnings(as.Date(tv_chr, format = "%Y-%m-%d"))
          na_idx <- is.na(parsed)
          if (any(na_idx)) {
            parsed[na_idx] <- suppressWarnings(
              as.Date(paste0(tv_chr[na_idx], "-01"), format = "%Y-%m-%d")
            )
          }
          parsed
        }
      },
      error = function(e) NULL
    )

    if (is.null(time_vec) || all(is.na(time_vec))) {
      cli::cli_abort(.excl("err_no_time_col", lang, period = aggregate_time))
    }

    rr_df$time_vec_parsed <- time_vec

    if (aggregate_time == "year") {
      rr_df$time_period <- format(rr_df$time_vec_parsed, "%Y")
    } else {
      rr_df$time_period <- format(rr_df$time_vec_parsed, "%Y-%m")
    }

    # aggregate: average log-scale RR mean and sigma per (code_muni, time_period)
    rr_df$log_rr_mean <- log(rr_df$rr_mean)
    rr_df$sigma_log   <- (log(rr_df$rr_upper95) - log(rr_df$rr_lower95)) /
                          (2 * 1.96)
    # clamp sigma to non-negative (handles floating point rr_lower95 > rr_upper95)
    rr_df$sigma_log[rr_df$sigma_log < 0] <- 0

    agg_list <- split(rr_df, list(rr_df$code_muni, rr_df$time_period),
                      drop = TRUE)

    agg_rows <- lapply(agg_list, function(chunk) {
      data.frame(
        code_muni   = chunk$code_muni[[1L]],
        time_period = chunk$time_period[[1L]],
        rr_mean     = exp(mean(chunk$log_rr_mean, na.rm = TRUE)),
        sigma_log   = mean(chunk$sigma_log,       na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })

    work_df <- do.call(rbind, agg_rows)
    row.names(work_df) <- NULL
    time_col <- "time_period"

  } else {
    # no temporal aggregation — compute sigma_log per row
    work_df <- rr_df[, c("code_muni", "time_idx", "rr_mean",
                          "rr_lower95", "rr_upper95"), drop = FALSE]
    work_df$sigma_log <- (log(work_df$rr_upper95) - log(work_df$rr_lower95)) /
                          (2 * 1.96)
    work_df$sigma_log[work_df$sigma_log < 0] <- 0
    time_col <- "time_idx"
  }

  # warn about zero-width credible intervals
  n_zero_sigma <- sum(work_df$sigma_log == 0, na.rm = TRUE)
  if (n_zero_sigma > 0L && verbose) {
    cli::cli_alert_warning(.excl("warn_sigma_zero", lang, n_zero = n_zero_sigma))
  }

  # ── 4. compute P(RR > t) for each threshold ──────────────────────────────────
  n_cells  <- nrow(work_df)
  n_thresh <- length(thresholds)

  if (verbose) {
    cli::cli_progress_step(
      .excl("step_compute", lang, n_thresh = n_thresh, n_cells = n_cells)
    )
  }

  # column names: p_gt_1_0, p_gt_1_5, p_gt_2_0, etc.
  thresh_col_names <- paste0("p_gt_", gsub("\\.", "_", as.character(thresholds)))

  log_rr_mean_vec <- log(work_df$rr_mean)
  sigma_vec       <- work_df$sigma_log

  p_matrix <- matrix(NA_real_, nrow = n_cells, ncol = n_thresh)

  for (j in seq_len(n_thresh)) {
    log_t <- log(thresholds[j])
    # P(RR > t) = P(log RR > log t) = 1 - Phi((log_t - log_rr_mean) / sigma)
    #           = Phi((log_rr_mean - log_t) / sigma)   [upper tail]
    # When sigma == 0: deterministic — P = 1 if rr_mean > t, else 0
    p_j <- ifelse(
      sigma_vec == 0,
      as.numeric(work_df$rr_mean > thresholds[j]),
      stats::pnorm(log_t,
                   mean       = log_rr_mean_vec,
                   sd         = sigma_vec,
                   lower.tail = FALSE)
    )
    p_matrix[, j] <- p_j
  }

  colnames(p_matrix) <- thresh_col_names

  # ── 5. assemble exceedance data.frame ────────────────────────────────────────
  exc_df <- data.frame(
    code_muni = work_df$code_muni,
    stringsAsFactors = FALSE
  )
  exc_df[[time_col]] <- work_df[[time_col]]
  exc_df$rr_mean     <- work_df$rr_mean

  for (j in seq_len(n_thresh)) {
    exc_df[[thresh_col_names[j]]] <- p_matrix[, j]
  }

  row.names(exc_df) <- NULL

  # ── 6. n_exceed summary ──────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.excl("step_summarise", lang))

  # cells where P(RR > t) > 0.80 — standard epidemiological exceedance criterion
  n_exceed_vec <- vapply(thresh_col_names, function(col) {
    sum(exc_df[[col]] > 0.80, na.rm = TRUE)
  }, integer(1L))

  n_exceed_df <- data.frame(
    threshold      = thresholds,
    n_cells_exceed = n_exceed_vec,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # ── 7. optional spatial join ─────────────────────────────────────────────────
  if (!is.null(municipalities)) {
    if (verbose) cli::cli_progress_step(.excl("step_spatial_join", lang))

    # keep only code_muni + geometry from sf object
    muni_sf <- municipalities[, c("code_muni", attr(municipalities, "sf_column")),
                               drop = FALSE]
    exc_df <- sf::st_as_sf(
      merge(exc_df, muni_sf, by = "code_muni", all.x = TRUE)
    )
  }

  # ── 8. done message ──────────────────────────────────────────────────────────
  if (verbose) {
    first_thresh_col <- thresh_col_names[which(thresholds == min(thresholds))[1L]]
    n_above_80 <- if (first_thresh_col %in% names(exc_df)) {
      sum(exc_df[[first_thresh_col]] > 0.80, na.rm = TRUE)
    } else {
      0L
    }
    pct_exceed <- formatC(
      100 * n_above_80 / max(n_cells, 1L),
      digits = 1, format = "f"
    )
    cli::cli_alert_success(
      .excl("done", lang,
            n_cells    = n_cells,
            n_thresh   = n_thresh,
            pct_exceed = pct_exceed)
    )
  }

  # ── 9. assemble output ───────────────────────────────────────────────────────
  structure(
    list(
      exceedance = exc_df,
      thresholds = thresholds,
      n_exceed   = n_exceed_df,
      call       = .call
    ),
    class = c("climasus_spacetime_exceedance", "list")
  )
}

# =============================================================================
# S3 METHODS
# =============================================================================

#' Print method for climasus_spacetime_exceedance objects
#'
#' Displays a formatted summary of exceedance probabilities including the
#' number of municipality-time cells that exceed each RR threshold with
#' posterior probability greater than 0.80.
#'
#' @param x A `climasus_spacetime_exceedance` object from
#'   [sus_mod_spacetime_exceedance()].
#' @param n_rows Integer. Maximum number of exceedance table rows to display.
#'   Default `10L`.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.climasus_spacetime_exceedance <- function(x, n_rows = 10L, ...) {
  cli::cli_h1(
    "climasus4r -- Spatiotemporal Exceedance Probabilities ({.cls climasus_spacetime_exceedance})"
  )

  n_cells <- nrow(x$exceedance)
  n_thresh <- length(x$thresholds)

  thresh_col_names <- paste0("p_gt_", gsub("\\.", "_", as.character(x$thresholds)))

  cli::cli_dl(c(
    "Cells (muni \u00d7 time)" = as.character(n_cells),
    "Thresholds"             = paste(x$thresholds, collapse = ", "),
    "Approximation"          = "Log-normal (log-scale normal posterior)"
  ))

  cli::cli_rule(left = "Exceedance counts [P(RR > t) > 0.80]")

  for (j in seq_len(n_thresh)) {
    t_val  <- x$thresholds[j]
    col_j  <- thresh_col_names[j]
    n_exc  <- x$n_exceed$n_cells_exceed[j]
    pct    <- formatC(100 * n_exc / max(n_cells, 1L), digits = 1, format = "f")
    cli::cli_text(
      "  RR > {t_val}: {n_exc} cells ({pct}%)"
    )
  }

  cli::cli_rule(left = "Top exceedance cells (by RR mean)")

  exc_show <- x$exceedance
  # strip sf geometry for display if present
  if (inherits(exc_show, "sf")) {
    exc_show <- as.data.frame(exc_show)[, setdiff(names(exc_show),
                                                    attr(exc_show, "sf_column")),
                                         drop = FALSE]
  }
  exc_show <- exc_show[order(-exc_show$rr_mean), , drop = FALSE]
  n_show   <- min(nrow(exc_show), as.integer(n_rows))

  # determine time column name
  time_col <- if ("time_period" %in% names(exc_show)) "time_period" else "time_idx"

  for (i in seq_len(n_show)) {
    row_i    <- exc_show[i, , drop = FALSE]
    muni_str <- row_i$code_muni
    time_str <- as.character(row_i[[time_col]])
    rr_str   <- formatC(row_i$rr_mean, digits = 3, format = "f")

    p_parts <- vapply(seq_len(n_thresh), function(j) {
      col_j <- thresh_col_names[j]
      p_val <- if (col_j %in% names(row_i)) row_i[[col_j]] else NA_real_
      paste0("P>", x$thresholds[j], "=", formatC(p_val, digits = 3, format = "f"))
    }, character(1L))

    cli::cli_text(
      "  {muni_str} [{time_str}] RR={rr_str}  {paste(p_parts, collapse = '  ')}"
    )
  }

  if (nrow(exc_show) > n_show) {
    cli::cli_text(
      "  ... {nrow(exc_show) - n_show} more cells not shown."
    )
  }

  cli::cli_rule()
  invisible(x)
}

#' Summary method for climasus_spacetime_exceedance objects
#'
#' Alias for [print.climasus_spacetime_exceedance()].
#'
#' @param object A `climasus_spacetime_exceedance` object.
#' @param ... Additional arguments passed to
#'   [print.climasus_spacetime_exceedance()].
#'
#' @return Invisibly returns `object`.
#'
#' @export
summary.climasus_spacetime_exceedance <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
