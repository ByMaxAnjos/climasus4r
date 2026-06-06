# =============================================================================
# sus_mod_spatial_scan.R
# Kulldorff Circular Scan Statistic for Spatial Cluster Detection
#
# Theory:
#   Kulldorff, M. & Nagarwalla, N. (1995). Spatial disease clusters: detection
#     and inference. Statistics in Medicine, 14(8), 799-810.
#   Kulldorff, M. (1997). A spatial scan statistic. Communications in
#     Statistics - Theory and Methods, 26(6), 1481-1496.
# Input : data.frame with cases + population per municipality + sf geometry
# Output: climasus_spatial_scan (list) with most-likely cluster,
#         secondary clusters, and diagnostics
# =============================================================================

# -- NSE variable declarations ------------------------------------------------
utils::globalVariables(c(
  "code_muni", ".row_idx"
))

# -- Local i18n ---------------------------------------------------------------
.scan_msgs <- list(

  title = list(
    pt = "climasus4r \u2014 Varredura Espacial de Kulldorff",
    en = "climasus4r \u2014 Kulldorff Spatial Scan",
    es = "climasus4r \u2014 Exploraci\u00f3n Espacial de Kulldorff"
  ),

  step_check_pkgs = list(
    pt = "Verificando pacotes necess\u00e1rios...",
    en = "Checking required packages...",
    es = "Verificando paquetes necesarios..."
  ),

  step_join = list(
    pt = "Unindo dados tabulares \u00e0 geometria ({n} munic\u00edpios)...",
    en = "Joining tabular data to geometry ({n} municipalities)...",
    es = "Uniendo datos tabulares a la geometr\u00eda ({n} municipios)..."
  ),

  step_centroids = list(
    pt = "Calculando centr\u00f3ides dos munic\u00edpios...",
    en = "Computing municipality centroids...",
    es = "Calculando centroides de los municipios..."
  ),

  step_scan = list(
    pt = "Executando varredura circular ({n_sim} simula\u00e7\u00f5es, frac. m\u00e1x. pop. = {pop_frac})...",
    en = "Running circular scan ({n_sim} simulations, max pop. frac. = {pop_frac})...",
    es = "Ejecutando exploraci\u00f3n circular ({n_sim} simulaciones, frac. m\u00e1x. pob. = {pop_frac})..."
  ),

  step_parse = list(
    pt = "Analisando resultados e mapeando c\u00f3digos de munic\u00edpio...",
    en = "Parsing results and mapping municipality codes...",
    es = "Analizando resultados y mapeando c\u00f3digos de municipio..."
  ),

  done = list(
    pt = "Conclu\u00eddo. {n_sig} aglomerado(s) significativo(s) detectado(s) (alpha = {alpha}).",
    en = "Done. {n_sig} significant cluster(s) detected (alpha = {alpha}).",
    es = "Listo. {n_sig} aglomerado(s) significativo(s) detectado(s) (alpha = {alpha})."
  ),

  warn_no_secondary = list(
    pt = "Nenhum aglomerado secund\u00e1rio significativo encontrado.",
    en = "No significant secondary clusters found.",
    es = "No se encontraron aglomerados secundarios significativos."
  ),

  warn_expected_null = list(
    pt = "Coluna {.arg expected} n\u00e3o encontrada; usando NULL (Poisson simples).",
    en = "Column {.arg expected} not found; using NULL (simple Poisson).",
    es = "Columna {.arg expected} no encontrada; usando NULL (Poisson simple)."
  ),

  warn_missing_muni = list(
    pt = "{n_miss} munic\u00edpio(s) em {.arg df} sem correspond\u00eancia em {.arg municipalities}; removido(s).",
    en = "{n_miss} municipality(ies) in {.arg df} without match in {.arg municipalities}; removed.",
    es = "{n_miss} municipio(s) en {.arg df} sin correspondencia en {.arg municipalities}; eliminado(s)."
  ),

  err_col_missing = list(
    pt = "Coluna {.val {col}} n\u00e3o encontrada em {.arg df}. Colunas dispon\u00edveis: {.val {cols}}.",
    en = "Column {.val {col}} not found in {.arg df}. Available columns: {.val {cols}}.",
    es = "Columna {.val {col}} no encontrada en {.arg df}. Columnas disponibles: {.val {cols}}."
  ),

  err_code_muni_missing = list(
    pt = "Coluna {.val code_muni} n\u00e3o encontrada em {.arg df} nem em {.arg municipalities}.",
    en = "Column {.val code_muni} not found in {.arg df} or {.arg municipalities}.",
    es = "Columna {.val code_muni} no encontrada en {.arg df} ni en {.arg municipalities}."
  ),

  err_not_sf = list(
    pt = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    en = "{.arg municipalities} must be an {.cls sf} object.",
    es = "{.arg municipalities} debe ser un objeto {.cls sf}."
  ),

  err_no_rows = list(
    pt = "Nenhuma linha restante ap\u00f3s o join. Verifique se os c\u00f3digos de munic\u00edpio correspondem.",
    en = "No rows remaining after join. Check that municipality codes match.",
    es = "No quedan filas despu\u00e9s del join. Verifique que los c\u00f3digos municipales coincidan."
  )
)

#' @keywords internal
#' @noRd
.sscan_msg <- function(key, lang, ...) {
  entry <- .scan_msgs[[key]]
  if (is.null(entry)) return(key)
  txt <- entry[[lang]] %||% entry[["pt"]]
  dots <- list(...)
  if (length(dots) > 0L) glue::glue(txt, .envir = list2env(dots)) else txt
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Kulldorff Circular Scan Statistic for Spatial Cluster Detection
#'
#' Applies the Kulldorff circular spatial scan statistic to detect geographic
#' clusters of disease excess. The most-likely cluster and any significant
#' secondary clusters are identified by Monte Carlo hypothesis testing. The
#' analysis requires a tabular dataset with case counts and population
#' denominators at the municipality level, plus an `sf` polygon layer
#' providing municipality boundaries.
#'
#' @section Methodology:
#' The scan statistic imposes circular windows of variable radius over the
#' map. For each window the likelihood ratio (LR) under a Poisson model
#' (observed vs. expected cases inside vs. outside the window) is computed.
#' The window with the maximum LR is the most-likely cluster. Its p-value is
#' obtained by comparing to the empirical distribution of max-LR under
#' `n_simulations` Monte Carlo permutations. Secondary clusters are non-
#' overlapping windows with LR > the `alpha`-level critical value from the
#' same Monte Carlo distribution.
#'
#' Computations are delegated to `SpatialEpi::kulldorff()`. Municipality
#' centroids are derived from the polygon geometry using
#' [sf::st_centroid()] after reprojecting to WGS 84 (EPSG 4326) so that
#' longitude/latitude coordinates are passed to `kulldorff()`.
#'
#' @param df A `data.frame` (or `climasus_df`) containing at minimum:
#'   a `code_muni` column (character or integer) and the columns named by
#'   `cases`, `population`, and optionally `expected`.
#' @param cases Character. Name of the column in `df` with case counts
#'   (non-negative integer or numeric).
#' @param population Character. Name of the column in `df` with the
#'   at-risk population denominator.
#' @param municipalities An `sf` object with POLYGON or MULTIPOLYGON
#'   geometry and a `code_muni` column used to join to `df`.
#' @param expected Character or `NULL`. Name of the column in `df` with
#'   the expected number of cases under the null model. When `NULL`
#'   (default), `SpatialEpi::kulldorff()` computes expected counts
#'   internally from the overall rate.
#' @param max_pop_frac Numeric in (0, 1]. Maximum fraction of the total
#'   population that a single cluster window may contain. Default `0.5`.
#' @param n_simulations Positive integer. Number of Monte Carlo simulations
#'   for hypothesis testing. Default `999`. Increase to `9999` for
#'   publication-quality p-values.
#' @param alpha Numeric in (0, 1). Significance level for secondary cluster
#'   filtering. Default `0.05`.
#' @param lang Character. Output language: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_spatial_scan` object (named list) with:
#' \describe{
#'   \item{`$most_likely_cluster`}{Named list:
#'     `location_ids` (character vector of `code_muni` in the cluster),
#'     `observed` (numeric), `expected` (numeric), `RR` (relative risk),
#'     `log_lik` (log-likelihood ratio), `p_value` (Monte Carlo p-value).}
#'   \item{`$secondary_clusters`}{List of lists with the same structure as
#'     `most_likely_cluster`, one per significant secondary cluster. Empty
#'     list when no secondary cluster is significant.}
#'   \item{`$n_clusters`}{Integer. Total number of significant clusters
#'     (1 + length of `secondary_clusters`), or 0 if the most-likely cluster
#'     itself is not significant.}
#'   \item{`$data`}{Tibble with one row per municipality: `code_muni`,
#'     `cases`, `population`, `expected`, and a logical column `in_mlc`
#'     indicating membership in the most-likely cluster.}
#'   \item{`$meta`}{Named list of analysis parameters.}
#'   \item{`$call`}{The matched call.}
#' }
#'
#' @references
#' Kulldorff, M. & Nagarwalla, N. (1995). Spatial disease clusters:
#' detection and inference. *Statistics in Medicine*, 14(8), 799--810.
#' \doi{10.1002/sim.4780140809}
#'
#' Kulldorff, M. (1997). A spatial scan statistic. *Communications in
#' Statistics - Theory and Methods*, 26(6), 1481--1496.
#' \doi{10.1080/03610929708831995}
#'
#' Kim, A. Y. & Wakefield, J. (2010). R data and methods for spatial
#' epidemiology: the SpatialEpi package. University of Washington.
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' library(dplyr)
#'
#' # Download municipality polygons for Minas Gerais (state code 31)
#' muni_sf <- geobr::read_municipality(code_muni = 31, year = 2020)
#' muni_sf <- dplyr::rename(muni_sf, code_muni = code_muni)
#'
#' # Build a synthetic data.frame
#' set.seed(42)
#' df_cases <- tibble::tibble(
#'   code_muni  = as.character(muni_sf$code_muni),
#'   cases      = rpois(nrow(muni_sf), lambda = 5),
#'   population = sample(5000:200000, nrow(muni_sf), replace = TRUE)
#' )
#'
#' result <- sus_mod_spatial_scan(
#'   df            = df_cases,
#'   cases         = "cases",
#'   population    = "population",
#'   municipalities = muni_sf,
#'   n_simulations = 199,
#'   lang          = "pt"
#' )
#'
#' print(result)
#' result$most_likely_cluster
#' result$secondary_clusters
#' }
#'
#' @seealso [sus_spatial_join()], [sus_mod_dlnm()], [sus_mod_af()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom dplyr left_join select mutate arrange all_of
#' @importFrom tibble tibble as_tibble
#' @importFrom glue glue
sus_mod_spatial_scan <- function(
    df,
    cases,
    population,
    municipalities,
    expected       = NULL,
    max_pop_frac   = 0.5,
    n_simulations  = 999L,
    alpha          = 0.05,
    lang           = c("pt", "en", "es"),
    verbose        = TRUE
) {
  lang    <- match.arg(lang)
  the_call <- match.call()

  # -- Package checks ----------------------------------------------------------
  if (verbose) cli::cli_h1(.sscan_msg("title", lang))
  if (verbose) cli::cli_alert_info(.sscan_msg("step_check_pkgs", lang))

  rlang::check_installed("SpatialEpi",
    reason = "for Kulldorff scan statistic")
  rlang::check_installed("sf",
    reason = "to compute municipality centroids")

  # -- Validate municipalities -------------------------------------------------
  if (!inherits(municipalities, "sf"))
    cli::cli_abort(.sscan_msg("err_not_sf", lang))

  if (!"code_muni" %in% names(df))
    cli::cli_abort(.sscan_msg("err_code_muni_missing", lang))

  if (!"code_muni" %in% names(municipalities))
    cli::cli_abort(.sscan_msg("err_code_muni_missing", lang))

  # -- Validate column names in df --------------------------------------------
  .check_col <- function(col_name, arg_name) {
    if (!col_name %in% names(df)) {
      cli::cli_abort(
        .sscan_msg("err_col_missing", lang,
                   col  = col_name,
                   cols = paste(names(df), collapse = ", "))
      )
    }
  }
  .check_col(cases,      "cases")
  .check_col(population, "population")

  expected_col <- NULL
  if (!is.null(expected)) {
    if (!expected %in% names(df)) {
      cli::cli_alert_warning(.sscan_msg("warn_expected_null", lang))
      expected_col <- NULL
    } else {
      expected_col <- expected
    }
  }

  # -- Coerce code_muni to character in both objects --------------------------
  df             <- tibble::as_tibble(df)
  df$code_muni   <- as.character(df$code_muni)
  municipalities$code_muni <- as.character(municipalities$code_muni)

  # -- Join df to municipalities -----------------------------------------------
  n_df <- nrow(df)
  if (verbose)
    cli::cli_alert_info(.sscan_msg("step_join", lang, n = n_df))

  muni_joined <- dplyr::left_join(
    municipalities,
    df,
    by = "code_muni"
  )

  n_miss <- sum(is.na(muni_joined[[cases]]))
  if (n_miss > 0L)
    cli::cli_alert_warning(
      .sscan_msg("warn_missing_muni", lang, n_miss = n_miss)
    )

  # Drop rows where cases is NA (municipalities in geometry not in df)
  muni_joined <- muni_joined[!is.na(muni_joined[[cases]]), ]

  if (nrow(muni_joined) == 0L)
    cli::cli_abort(.sscan_msg("err_no_rows", lang))

  # Sort consistently by code_muni so indices align
  muni_joined <- muni_joined[order(muni_joined$code_muni), ]

  # -- Compute centroids -------------------------------------------------------
  if (verbose) cli::cli_alert_info(.sscan_msg("step_centroids", lang))

  centroids <- sf::st_centroid(sf::st_transform(muni_joined, 4326L))
  coords    <- sf::st_coordinates(centroids)   # matrix [lon, lat]

  # -- Prepare vectors ---------------------------------------------------------
  cases_vec      <- as.numeric(muni_joined[[cases]])
  pop_vec        <- as.numeric(muni_joined[[population]])
  expected_vec   <- if (!is.null(expected_col)) {
    as.numeric(muni_joined[[expected_col]])
  } else {
    NULL
  }
  code_muni_vec  <- as.character(muni_joined$code_muni)

  # -- Run Kulldorff scan ------------------------------------------------------
  if (verbose)
    cli::cli_alert_info(
      .sscan_msg("step_scan", lang,
                 n_sim    = n_simulations,
                 pop_frac = max_pop_frac)
    )

  scan_result <- SpatialEpi::kulldorff(
    geo             = coords,
    cases           = cases_vec,
    population      = pop_vec,
    expected         = expected_vec,
    pop.upper.bound = max_pop_frac,
    n.simulations   = as.integer(n_simulations),
    alpha.level     = alpha,
    plot            = FALSE
  )

  # -- Parse results -----------------------------------------------------------
  if (verbose) cli::cli_alert_info(.sscan_msg("step_parse", lang))

  mlc_raw <- scan_result$most.likely.cluster

  .parse_cluster <- function(cl) {
    idx      <- cl$location.IDs.included
    obs      <- cl$number.of.cases
    exp      <- cl$expected.cases
    rr       <- if (!is.null(cl$SMR)) cl$SMR else if (exp > 0) obs / exp else NA_real_
    log_lik  <- cl$log.likelihood.ratio
    p_val    <- cl$p.value

    list(
      location_ids = code_muni_vec[idx],
      observed     = as.numeric(obs),
      expected     = as.numeric(exp),
      RR           = as.numeric(rr),
      log_lik      = as.numeric(log_lik),
      p_value      = as.numeric(p_val)
    )
  }

  mlc <- .parse_cluster(mlc_raw)

  # Secondary clusters: filter to those with p < alpha
  sec_raw  <- scan_result$secondary.clusters
  sec_list <- list()
  if (length(sec_raw) > 0L) {
    for (i in seq_along(sec_raw)) {
      parsed <- .parse_cluster(sec_raw[[i]])
      if (!is.na(parsed$p_value) && parsed$p_value < alpha) {
        sec_list <- c(sec_list, list(parsed))
      }
    }
  }

  # Determine total significant cluster count
  mlc_sig   <- !is.na(mlc$p_value) && mlc$p_value < alpha
  n_sig     <- as.integer(mlc_sig) + length(sec_list)

  # -- Build data tibble -------------------------------------------------------
  in_mlc_flag <- code_muni_vec %in% mlc$location_ids

  exp_values <- if (!is.null(expected_col)) {
    as.numeric(muni_joined[[expected_col]])
  } else {
    # Replicate what kulldorff uses internally: rate * pop
    total_rate <- sum(cases_vec, na.rm = TRUE) / sum(pop_vec, na.rm = TRUE)
    total_rate * pop_vec
  }

  data_tbl <- tibble::tibble(
    code_muni  = code_muni_vec,
    cases      = cases_vec,
    population = pop_vec,
    expected   = exp_values,
    in_mlc     = in_mlc_flag
  )

  # -- Verbose summary ---------------------------------------------------------
  if (verbose) {
    if (length(sec_list) == 0L)
      cli::cli_alert_warning(.sscan_msg("warn_no_secondary", lang))
    cli::cli_alert_success(
      .sscan_msg("done", lang, n_sig = n_sig, alpha = alpha)
    )
  }

  # -- Return ------------------------------------------------------------------
  structure(
    list(
      most_likely_cluster = mlc,
      secondary_clusters  = sec_list,
      n_clusters          = n_sig,
      data                = data_tbl,
      meta                = list(
        cases_col      = cases,
        population_col = population,
        expected_col   = expected_col,
        max_pop_frac   = max_pop_frac,
        n_simulations  = as.integer(n_simulations),
        alpha          = alpha,
        n_municipalities = nrow(muni_joined),
        call_time      = Sys.time()
      ),
      call = the_call
    ),
    class = c("climasus_spatial_scan", "list")
  )
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_spatial_scan object
#'
#' @param x A `climasus_spatial_scan` object returned by [sus_mod_spatial_scan()].
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_spatial_scan <- function(x, ...) {
  m   <- x$meta
  mlc <- x$most_likely_cluster

  cat("\n-- climasus_spatial_scan: Kulldorff Circular Scan --\n")
  cat(sprintf("  Municipalities   : %d\n",   m$n_municipalities))
  cat(sprintf("  Cases column     : %s\n",   m$cases_col))
  cat(sprintf("  Population column: %s\n",   m$population_col))
  cat(sprintf("  Max pop. fraction: %.2f\n", m$max_pop_frac))
  cat(sprintf("  Simulations      : %d\n",   m$n_simulations))
  cat(sprintf("  Alpha            : %.3f\n", m$alpha))
  cat(sprintf("  Significant clusters: %d\n\n", x$n_clusters))

  cat("  Most-likely cluster:\n")
  cat(sprintf("    Municipalities : %d (%s%s)\n",
              length(mlc$location_ids),
              paste(utils::head(mlc$location_ids, 3L), collapse = ", "),
              if (length(mlc$location_ids) > 3L) ", ..." else ""))
  cat(sprintf("    Observed / Expected: %.1f / %.1f\n",
              mlc$observed, mlc$expected))
  cat(sprintf("    RR      : %.3f\n", mlc$RR))
  cat(sprintf("    Log-LR  : %.4f\n", mlc$log_lik))
  cat(sprintf("    p-value : %.4f\n", mlc$p_value))

  if (length(x$secondary_clusters) > 0L) {
    cat(sprintf("\n  Secondary significant clusters: %d\n",
                length(x$secondary_clusters)))
    for (i in seq_along(x$secondary_clusters)) {
      sc <- x$secondary_clusters[[i]]
      cat(sprintf("    [%d] n=%d  RR=%.3f  p=%.4f\n",
                  i, length(sc$location_ids), sc$RR, sc$p_value))
    }
  }

  cat("\n")
  invisible(x)
}


#' Summarise a climasus_spatial_scan object
#'
#' @param object A `climasus_spatial_scan` object.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_spatial_scan <- function(object, ...) {
  print(object)
  cat("-- Municipality data --\n")
  print(object$data)
  invisible(object)
}
