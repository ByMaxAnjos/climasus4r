# =============================================================================
# sus_mod_spatial_weights.R
# Spatial Weights Construction from Municipality Polygons (spdep)
#
# Theory:
#   Cliff & Ord (1981) - Spatial Processes: Models and Applications
#   Anselin (1988) - Spatial Econometrics: Methods and Models
#   Bivand & Wong (2018) - Comparing implementations of global indicators
#     of spatial association, TEST 27(3):716-748
# Input : sf object with municipality polygons (e.g. from geobr::read_municipality())
# Output: climasus_weights object with spdep listw, nb, island diagnostics
#
# All other sus_mod_spatial_* functions consume this object.
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.weights_msgs <- list(
  step_validate = list(
    pt = "Validando geometria de {n_muni} munic\u00edpios...",
    en = "Validating geometry of {n_muni} municipalities...",
    es = "Validando geometr\u00eda de {n_muni} municipios..."
  ),
  step_makevalid = list(
    pt = "Reparando geometrias inv\u00e1lidas com sf::st_make_valid()...",
    en = "Repairing invalid geometries with sf::st_make_valid()...",
    es = "Reparando geometr\u00edas inv\u00e1lidas con sf::st_make_valid()..."
  ),
  step_nb = list(
    pt = "Construindo matriz de vizinhan\u00e7a ({type_nb}, snap = {snap_val})...",
    en = "Building neighbourhood matrix ({type_nb}, snap = {snap_val})...",
    es = "Construyendo matriz de vecindad ({type_nb}, snap = {snap_val})..."
  ),
  step_listw = list(
    pt = "Convertendo para lista de pesos espaciais (estilo {style})...",
    en = "Converting to spatial weights list (style {style})...",
    es = "Convirtiendo a lista de pesos espaciales (estilo {style})..."
  ),
  step_matrix = list(
    pt = "Construindo matriz densa W ({n} x {n})...",
    en = "Building dense W matrix ({n} x {n})...",
    es = "Construyendo matriz densa W ({n} x {n})..."
  ),
  warn_islands = list(
    pt = "{n_islands} munic\u00edpio(s) sem vizinho (ilhas espaciais): {ids}. Use zero_policy = TRUE ou verifique a geometria.",
    en = "{n_islands} municipality/municipalities with no neighbour (spatial islands): {ids}. Use zero_policy = TRUE or check geometry.",
    es = "{n_islands} municipio(s) sin vecino (islas espaciales): {ids}. Use zero_policy = TRUE o verifique la geometr\u00eda."
  ),
  done = list(
    pt = "Pesos espaciais prontos. Regi\u00f5es: {n}  |  Ilhas: {n_isl}  |  Vizinhos m\u00e9d/m\u00edn/m\u00e1x: {mn}/{mi}/{mx}",
    en = "Spatial weights ready. Regions: {n}  |  Islands: {n_isl}  |  Neighbours avg/min/max: {mn}/{mi}/{mx}",
    es = "Pesos espaciales listos. Regiones: {n}  |  Islas: {n_isl}  |  Vecinos prom/m\u00edn/m\u00e1x: {mn}/{mi}/{mx}"
  ),
  err_not_sf = list(
    pt = "{.arg municipalities} deve ser um objeto {.cls sf}. Obtenha pol\u00edgonos com {.fn geobr::read_municipality}.",
    en = "{.arg municipalities} must be an {.cls sf} object. Obtain polygons with {.fn geobr::read_municipality}.",
    es = "{.arg municipalities} debe ser un objeto {.cls sf}. Obtenga pol\u00edgonos con {.fn geobr::read_municipality}."
  ),
  err_no_rows = list(
    pt = "{.arg municipalities} est\u00e1 vazio (0 linhas).",
    en = "{.arg municipalities} is empty (0 rows).",
    es = "{.arg municipalities} est\u00e1 vac\u00edo (0 filas)."
  ),
  err_style = list(
    pt = "Estilo de normaliza\u00e7\u00e3o {.val {style}} inv\u00e1lido. Use um de: {.val {valid}}.",
    en = "Normalisation style {.val {style}} is invalid. Use one of: {.val {valid}}.",
    es = "Estilo de normalizaci\u00f3n {.val {style}} inv\u00e1lido. Use uno de: {.val {valid}}."
  ),
  err_islands_strict = list(
    pt = "{n_islands} ilha(s) encontrada(s) e {.arg zero_policy} = FALSE. Defina zero_policy = TRUE ou remova ilhas.",
    en = "{n_islands} island(s) found and {.arg zero_policy} = FALSE. Set zero_policy = TRUE or remove islands.",
    es = "{n_islands} isla(s) encontrada(s) y {.arg zero_policy} = FALSE. Defina zero_policy = TRUE o elimine las islas."
  )
)

# ── Internal message helper ───────────────────────────────────────────────────
#' @keywords internal
#' @noRd
.wrl <- function(key, lang, ...) {
  entry <- .weights_msgs[[key]]
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

#' Build Spatial Weights from Municipality Polygons
#'
#' Constructs a `spdep` spatial weights object from an `sf` data frame of
#' municipality (or any polygon) boundaries. The result — a `climasus_weights`
#' object — is the required input for all other `sus_mod_spatial_*` functions
#' in the climasus4r pipeline.
#'
#' Contiguity neighbours are detected with [spdep::poly2nb()], which supports
#' both Queen (default, shared edge **or** vertex) and Rook (shared edge only)
#' contiguity. Municipalities that share no boundary ("spatial islands") are
#' automatically detected and reported. The resulting neighbour list is
#' normalised to a `listw` object via [spdep::nb2listw()].
#'
#' @section Snap distance:
#' Municipal boundaries digitised at different scales often have small gaps or
#' overlaps at shared borders. The `snap` argument passes a distance tolerance
#' (in the units of the CRS) to [spdep::poly2nb()]: any two polygon boundaries
#' within `snap` distance are treated as touching. The default (`NULL`) uses
#' `1e-3` (appropriate for degree-based CRS such as EPSG:4674 / SIRGAS2000).
#' For projected CRS (metres) consider values around `1` to `100`.
#'
#' @section Weight styles:
#' * `"W"` (default) — row-standardised: each neighbour weight = 1/n_neighbours
#' * `"B"` — binary: 1 if neighbour, 0 otherwise
#' * `"C"` — globally standardised
#' * `"U"` — equal to `"C"` divided by number of neighbours
#' * `"S"` — variance-stabilising (Tiefelsdorf et al. 1999)
#' * `"minmax"` — min/max normalisation
#'
#' @param municipalities An `sf` object with **polygon** or **multipolygon**
#'   geometry. Typically the result of [geobr::read_municipality()] or any
#'   shape file read with [sf::st_read()]. A column named `code_muni` is used
#'   as row labels when present; otherwise integer indices are used.
#' @param style Character. Weight normalisation style passed to
#'   [spdep::nb2listw()]. One of `"W"` (default), `"B"`, `"C"`, `"U"`,
#'   `"S"`, `"minmax"`. See **Weight styles** section.
#' @param queen Logical. If `TRUE` (default) Queen contiguity is used (shared
#'   edge or vertex). If `FALSE`, Rook contiguity (shared edge only).
#' @param snap Numeric or `NULL`. Snap distance tolerance passed to
#'   [spdep::poly2nb()]. `NULL` (default) uses `1e-3`. See **Snap distance**
#'   section.
#' @param zero_policy Logical. If `TRUE` (default) municipalities with no
#'   neighbours ("islands") are allowed — their spatial lag is set to `NA`.
#'   If `FALSE` an error is raised when islands are found.
#' @param return_matrix Logical. If `TRUE`, a dense \eqn{n \times n} spatial
#'   weights matrix `W` is included in the output (slot `$W`). This can be
#'   memory-intensive for large datasets. Default `FALSE`.
#' @param lang Character. Language for CLI messages: `"pt"` (default),
#'   `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_weights` object (named list with class
#'   `c("climasus_weights", "list")`) containing:
#'   \describe{
#'     \item{`$listw`}{`spdep` `listw` object ready for spatial modelling.}
#'     \item{`$nb`}{`spdep` `nb` neighbour list (before weight normalisation).}
#'     \item{`$n_regions`}{Integer. Total number of regions / municipalities.}
#'     \item{`$n_islands`}{Integer. Number of regions with zero neighbours.}
#'     \item{`$island_ids`}{Character or integer vector of island identifiers
#'       (empty if none).}
#'     \item{`$W`}{Dense numeric \eqn{n \times n} weight matrix, or `NULL` when
#'       `return_matrix = FALSE`.}
#'     \item{`$style`}{Character. Weight normalisation style used.}
#'     \item{`$call`}{The matched [call()].}
#'   }
#'
#' @references
#' Bivand, R. S., Pebesma, E., & Gomez-Rubio, V. (2013).
#' *Applied Spatial Data Analysis with R* (2nd ed.). Springer.
#'
#' Cliff, A. D., & Ord, J. K. (1981).
#' *Spatial Processes: Models and Applications*. Pion.
#'
#' Anselin, L. (1988).
#' *Spatial Econometrics: Methods and Models*. Kluwer Academic.
#'
#' @seealso
#' [sus_mod_spatial_reg()] for spatial lag model (SAR/SLM),
#' [sus_mod_spatial_reg()] for spatial error model (SEM),
#' [sus_mod_spatial_bayes()] for Bayesian hierarchical spatial models,
#' [sus_mod_spatial_scan()] for spatial scan statistics.
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' library(climasus4r)
#'
#' # Download Nordeste municipalities (SIRGAS2000 / EPSG:4674)
#' muni <- geobr::read_municipality(code_muni = "all", year = 2022)
#' muni_ne <- muni[muni$abbrev_state %in% c("CE","RN","PB","PE","AL",
#'                                          "SE","BA","PI","MA"), ]
#'
#' # Row-standardised Queen contiguity (default)
#' w <- sus_mod_spatial_weights(muni_ne)
#' print(w)
#'
#' # Binary Rook weights, returning the dense W matrix
#' w_rook <- sus_mod_spatial_weights(
#'   municipalities = muni_ne,
#'   style          = "B",
#'   queen          = FALSE,
#'   return_matrix  = TRUE
#' )
#' dim(w_rook$W)
#' }
#'
#' @export
sus_mod_spatial_weights <- function(
    municipalities,
    style        = "W",
    queen        = TRUE,
    snap         = NULL,
    zero_policy  = TRUE,
    return_matrix = FALSE,
    lang         = "pt",
    verbose      = TRUE
) {
  # ── 0. dependency check ────────────────────────────────────────────────────
  rlang::check_installed("spdep", reason = "to build spatial weights")
  rlang::check_installed("sf",    reason = "to validate polygon geometry")

  lang <- match.arg(lang, c("pt", "en", "es"))
  mc   <- match.call()

  valid_styles <- c("W", "B", "C", "U", "S", "minmax")

  # ── 1. validate inputs ─────────────────────────────────────────────────────
  if (!inherits(municipalities, "sf")) {
    cli::cli_abort(.wrl("err_not_sf", lang))
  }
  if (nrow(municipalities) == 0L) {
    cli::cli_abort(.wrl("err_no_rows", lang))
  }
  if (!style %in% valid_styles) {
    cli::cli_abort(.wrl("err_style", lang, style = style,
                        valid = paste(valid_styles, collapse = ", ")))
  }

  n_muni   <- nrow(municipalities)
  snap_val <- if (!is.null(snap)) snap else 1e-3

  if (verbose) {
    cli::cli_progress_step(.wrl("step_validate", lang, n_muni = n_muni))
  }

  # ── 2. repair geometries ───────────────────────────────────────────────────
  if (verbose) {
    cli::cli_progress_step(.wrl("step_makevalid", lang))
  }
  municipalities <- sf::st_make_valid(municipalities)

  # ── 3. row names for nb object ─────────────────────────────────────────────
  row_names <- if ("code_muni" %in% names(municipalities)) {
    as.character(municipalities[["code_muni"]])
  } else {
    as.character(seq_len(n_muni))
  }

  # ── 4. build neighbour list ────────────────────────────────────────────────
  type_nb <- if (queen) "Queen" else "Rook"
  if (verbose) {
    cli::cli_progress_step(
      .wrl("step_nb", lang, type_nb = type_nb, snap_val = snap_val)
    )
  }

  nb <- spdep::poly2nb(
    municipalities,
    queen     = queen,
    snap      = snap_val,
    row.names = row_names
  )

  # ── 5. detect islands ──────────────────────────────────────────────────────
  card_nb    <- spdep::card(nb)
  island_idx <- which(card_nb == 0L)
  n_islands  <- length(island_idx)
  island_ids <- if (n_islands > 0L) row_names[island_idx] else character(0L)

  if (n_islands > 0L && !zero_policy) {
    cli::cli_abort(.wrl("err_islands_strict", lang, n_islands = n_islands))
  }

  if (n_islands > 0L && verbose) {
    ids_str <- paste(island_ids, collapse = ", ")
    cli::cli_alert_warning(
      .wrl("warn_islands", lang,
           n_islands = n_islands,
           ids       = ids_str)
    )
  }

  # ── 6. convert to listw ────────────────────────────────────────────────────
  if (verbose) {
    cli::cli_progress_step(.wrl("step_listw", lang, style = style))
  }

  listw <- spdep::nb2listw(nb, style = style, zero.policy = zero_policy)

  # ── 7. dense matrix (optional) ────────────────────────────────────────────
  W <- NULL
  if (return_matrix) {
    if (verbose) {
      cli::cli_progress_step(
        .wrl("step_matrix", lang, n = n_muni)
      )
    }
    W <- spdep::listw2mat(listw)
    rownames(W) <- row_names
    colnames(W) <- row_names
  }

  # ── 8. summary stats ──────────────────────────────────────────────────────
  nonzero_card <- card_nb[card_nb > 0L]
  mn <- if (length(nonzero_card) > 0L) round(mean(nonzero_card), 2) else 0
  mi <- if (length(nonzero_card) > 0L) min(nonzero_card) else 0L
  mx <- if (length(nonzero_card) > 0L) max(nonzero_card) else 0L

  if (verbose) {
    cli::cli_alert_success(
      .wrl("done", lang,
           n     = n_muni,
           n_isl = n_islands,
           mn    = mn,
           mi    = mi,
           mx    = mx)
    )
  }

  # ── 9. assemble output ────────────────────────────────────────────────────
  out <- structure(
    list(
      listw     = listw,
      nb        = nb,
      n_regions = n_muni,
      n_islands = n_islands,
      island_ids = island_ids,
      W         = W,
      style     = style,
      call      = mc
    ),
    class = c("climasus_weights", "list")
  )

  out
}

# =============================================================================
# S3 METHODS
# =============================================================================

#' Print method for climasus_weights
#'
#' Displays a compact summary of the spatial weights object including the
#' number of regions, islands, weight style, and neighbour statistics.
#'
#' @param x A `climasus_weights` object.
#' @param ... Ignored.
#'
#' @return `x` invisibly.
#'
#' @export
print.climasus_weights <- function(x, ...) {
  cli::cli_rule(
    left = "climasus4r \u2014 Spatial Weights ({.cls climasus_weights})"
  )

  card_vec   <- spdep::card(x$nb)
  nonzero    <- card_vec[card_vec > 0L]
  mn_nb      <- if (length(nonzero) > 0L) round(mean(nonzero), 2) else 0
  min_nb     <- if (length(nonzero) > 0L) min(nonzero) else 0L
  max_nb     <- if (length(nonzero) > 0L) max(nonzero) else 0L

  cli::cli_dl(c(
    "Regions"           = as.character(x$n_regions),
    "Islands (no nb)"   = as.character(x$n_islands),
    "Style"             = x$style,
    "Neighbours (mean)" = as.character(mn_nb),
    "Neighbours (min)"  = as.character(min_nb),
    "Neighbours (max)"  = as.character(max_nb),
    "Dense W matrix"    = if (!is.null(x$W)) "yes" else "no"
  ))

  if (x$n_islands > 0L) {
    cli::cli_alert_warning(
      "Island IDs: {.val {x$island_ids}}"
    )
  }

  cli::cli_rule()
  invisible(x)
}

#' Summary method for climasus_weights
#'
#' Alias for [print.climasus_weights()].
#'
#' @param object A `climasus_weights` object.
#' @param ... Ignored.
#'
#' @return `object` invisibly.
#'
#' @export
summary.climasus_weights <- function(object, ...) {
  print(object, ...)
}
