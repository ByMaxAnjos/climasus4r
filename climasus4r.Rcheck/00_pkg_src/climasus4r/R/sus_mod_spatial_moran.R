# =============================================================================
# sus_mod_spatial_moran.R
# Global Moran's I + LISA Local Autocorrelation for Health Outcomes
#
# Theory:
#   Moran (1950, Biometrika) - original spatial autocorrelation statistic
#   Anselin (1995, Geogr Anal 27:93-115) - Local Indicators of Spatial
#     Association (LISA)
#   Bivand et al. (2013) - Applied Spatial Data Analysis with R (spdep)
# Input : data.frame or climasus_df with code_muni + numeric outcome column
#         W: climasus_weights object from sus_mod_spatial_weights()
# Output: climasus_spatial_moran with global I, LISA clusters, and quadrant
#         classification (HH/LL/HL/LH/NS)
# =============================================================================

# -- NSE variable declarations ------------------------------------------------
utils::globalVariables(c(
  "code_muni", "Ii", "Z.Ii", "p_raw", "p_adj", "quadrant"
))

# -- Local i18n ---------------------------------------------------------------
.moran_msgs <- list(

  title = list(
    pt = "climasus4r \u2014 Autocorrela\u00e7\u00e3o Espacial de Moran",
    en = "climasus4r \u2014 Moran Spatial Autocorrelation",
    es = "climasus4r \u2014 Autocorrelaci\u00f3n Espacial de Moran"
  ),

  step_check = list(
    pt = "Verificando entradas...",
    en = "Checking inputs...",
    es = "Verificando entradas..."
  ),

  step_global = list(
    pt = "Calculando I de Moran global ({permutations} permuta\u00e7\u00f5es)...",
    en = "Computing global Moran's I ({permutations} permutations)...",
    es = "Calculando I de Moran global ({permutations} permutaciones)..."
  ),

  step_local = list(
    pt = "Calculando LISA local ({permutations} permuta\u00e7\u00f5es)...",
    en = "Computing local LISA ({permutations} permutations)...",
    es = "Calculando LISA local ({permutations} permutaciones)..."
  ),

  step_quadrant = list(
    pt = "Classificando quadrantes LISA (alfa = {alpha}, ajuste p = {adjust_p})...",
    en = "Classifying LISA quadrants (alpha = {alpha}, p-adjust = {adjust_p})...",
    es = "Clasificando cuadrantes LISA (alfa = {alpha}, ajuste p = {adjust_p})..."
  ),

  done = list(
    pt = "Conclu\u00eddo. I = {I_val} (p-sim = {p_val}) | HH = {nHH}, LL = {nLL}, HL = {nHL}, LH = {nLH}, NS = {nNS}",
    en = "Done. I = {I_val} (p-sim = {p_val}) | HH = {nHH}, LL = {nLL}, HL = {nHL}, LH = {nLH}, NS = {nNS}",
    es = "Listo. I = {I_val} (p-sim = {p_val}) | HH = {nHH}, LL = {nLL}, HL = {nHL}, LH = {nLH}, NS = {nNS}"
  ),

  err_no_outcome = list(
    pt = "Coluna de desfecho {.val {outcome}} nao encontrada em {.arg df}. Colunas dispon\u00edveis: {.val {avail}}.",
    en = "Outcome column {.val {outcome}} not found in {.arg df}. Available columns: {.val {avail}}.",
    es = "Columna de resultado {.val {outcome}} no encontrada en {.arg df}. Columnas disponibles: {.val {avail}}."
  ),

  err_no_muni = list(
    pt = "Coluna {.val code_muni} n\u00e3o encontrada em {.arg df}.",
    en = "Column {.val code_muni} not found in {.arg df}.",
    es = "Columna {.val code_muni} no encontrada en {.arg df}."
  ),

  err_not_weights = list(
    pt = "{.arg W} deve ser um objeto {.cls climasus_weights} produzido por {.fn sus_mod_spatial_weights}.",
    en = "{.arg W} must be a {.cls climasus_weights} object from {.fn sus_mod_spatial_weights}.",
    es = "{.arg W} debe ser un objeto {.cls climasus_weights} de {.fn sus_mod_spatial_weights}."
  ),

  err_no_listw = list(
    pt = "{.arg W} n\u00e3o cont\u00e9m o elemento {.val listw}. Reconstrua com {.fn sus_mod_spatial_weights}.",
    en = "{.arg W} does not contain element {.val listw}. Rebuild with {.fn sus_mod_spatial_weights}.",
    es = "{.arg W} no contiene el elemento {.val listw}. Reconstruya con {.fn sus_mod_spatial_weights}."
  ),

  err_length_mismatch = list(
    pt = "Comprimento do vetor de desfecho ({n_x}) difere do numero de regi\u00f5es na matriz de pesos ({n_w}). Filtre {.arg df} para corresponder ao objeto {.arg W}.",
    en = "Outcome vector length ({n_x}) differs from number of regions in weight matrix ({n_w}). Filter {.arg df} to match {.arg W}.",
    es = "La longitud del vector de resultado ({n_x}) difiere del numero de regiones en la matriz de pesos ({n_w}). Filtre {.arg df} para coincidir con {.arg W}."
  ),

  err_all_na = list(
    pt = "Coluna de desfecho {.val {outcome}} cont\u00e9m apenas NA. N\u00e3o \u00e9 poss\u00edvel calcular Moran.",
    en = "Outcome column {.val {outcome}} contains only NA. Cannot compute Moran.",
    es = "Columna de resultado {.val {outcome}} contiene solo NA. No es posible calcular Moran."
  ),

  warn_na_values = list(
    pt = "{n_na} valor(es) NA em {.val {outcome}} substituidos pela m\u00e9dia antes do c\u00e1lculo.",
    en = "{n_na} NA value(s) in {.val {outcome}} replaced by mean before computation.",
    es = "{n_na} valor(es) NA en {.val {outcome}} reemplazados por la media antes del c\u00e1lculo."
  ),

  warn_municipalities = list(
    pt = "Filtrando {.arg df} para {n_keep} munic\u00edpios fornecidos em {.arg municipalities}.",
    en = "Filtering {.arg df} to {n_keep} municipalities provided in {.arg municipalities}.",
    es = "Filtrando {.arg df} a {n_keep} municipios provistos en {.arg municipalities}."
  )
)

# -- Internal helper: message dispatcher --------------------------------------
#' @keywords internal
#' @noRd
.mrl <- function(key, lang, ...) {
  entry <- .moran_msgs[[key]]
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

#' Global Moran's I and LISA Local Autocorrelation for Health Outcomes
#'
#' Computes Moran's I global spatial autocorrelation statistic and Local
#' Indicators of Spatial Association (LISA) for a numeric health outcome
#' measured at municipality level. The global statistic tests whether the
#' outcome is spatially clustered across the study region; the local statistics
#' identify significant **High-High** (hotspots), **Low-Low** (coldspots),
#' **High-Low**, and **Low-High** spatial outlier clusters.
#'
#' @section Global statistic:
#'
#' Moran's I is computed via permutation inference using
#' [spdep::moran.mc()]. The observed statistic is compared against the
#' empirical null distribution obtained by randomly permuting the outcome
#' values across spatial units. The reported `p_simulated` is the proportion
#' of permuted values as extreme as or more extreme than the observed I.
#'
#' @section Local statistic (LISA):
#'
#' Local Moran statistics are computed via [spdep::localmoran_perm()], which
#' uses conditional permutation to generate unit-specific null distributions.
#' Each observation is assigned a LISA quadrant based on its standardised
#' value and the standardised spatial lag:
#'
#' \describe{
#'   \item{`HH`}{High value surrounded by high neighbours (hotspot).}
#'   \item{`LL`}{Low value surrounded by low neighbours (coldspot).}
#'   \item{`HL`}{High value surrounded by low neighbours (spatial outlier).}
#'   \item{`LH`}{Low value surrounded by high neighbours (spatial outlier).}
#'   \item{`NS`}{Not significant at the specified `alpha` level.}
#' }
#'
#' @param df A `data.frame` or `climasus_df` containing at least the columns
#'   `code_muni` (7-digit IBGE municipality code) and the outcome column
#'   named by `outcome`. The row order must correspond to the spatial units
#'   encoded in `W` unless `municipalities` is supplied to subset and reorder.
#' @param outcome Character. Name of the numeric outcome column in `df`
#'   (e.g. `"deaths"`, `"rate_100k"`, `"hospitalization_rate"`).
#' @param W A `climasus_weights` object produced by
#'   `sus_mod_spatial_weights()`. Must contain a `listw` element (a
#'   `listw`-class object from **spdep**) whose length matches `nrow(df)` after
#'   any filtering by `municipalities`.
#' @param municipalities Optional character or integer vector of `code_muni`
#'   values to include. When supplied the function subsets and reorders `df`
#'   to match this vector before computation. Default `NULL` (use all rows).
#' @param permutations Positive integer. Number of Monte Carlo permutations
#'   for both global and local inference. Default `999`.
#' @param alpha Numeric in (0, 1). Significance threshold applied to
#'   p-adjusted local Moran p-values when assigning quadrant labels.
#'   Default `0.05`.
#' @param adjust_p Character. Method passed to [stats::p.adjust()] for
#'   multiple-testing correction of local Moran p-values. One of `"fdr"`
#'   (default), `"bonferroni"`, or `"none"`.
#' @param lang Character. Output language for messages: `"pt"` (default),
#'   `"en"`, or `"es"`.
#' @param verbose Logical. If `TRUE` (default), print progress messages via
#'   **cli**.
#'
#' @return An object of class `c("climasus_spatial_moran", "list")` with the
#'   following named elements:
#'   \describe{
#'     \item{`$global`}{One-row `data.frame` with columns `I`, `E.I`,
#'       `Var.I`, `Z.I`, `p_value` (analytical), and `p_simulated`
#'       (permutation-based).}
#'     \item{`$local`}{`data.frame` with one row per spatial unit and columns
#'       `code_muni`, `Ii` (local Moran statistic), `Z.Ii` (z-score),
#'       `p_raw` (unit-level permutation p), `p_adj` (adjusted p), and
#'       `quadrant` (factor: `"HH"`, `"LL"`, `"HL"`, `"LH"`, `"NS"`).}
#'     \item{`$n_HH`}{Integer. Number of significant HH (hotspot) units.}
#'     \item{`$n_LL`}{Integer. Number of significant LL (coldspot) units.}
#'     \item{`$n_HL`}{Integer. Number of significant HL spatial outlier units.}
#'     \item{`$n_LH`}{Integer. Number of significant LH spatial outlier units.}
#'     \item{`$outcome_name`}{Character. The `outcome` argument value.}
#'     \item{`$call`}{The matched function call.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Requires spdep and a climasus_weights object W
#' library(climasus4r)
#'
#' # W <- sus_mod_spatial_weights(shp, style = "W")
#' result <- sus_mod_spatial_moran(
#'   df           = my_df,
#'   outcome      = "deaths",
#'   W            = W,
#'   permutations = 999,
#'   alpha        = 0.05,
#'   adjust_p     = "fdr",
#'   lang         = "pt"
#' )
#' print(result)
#'
#' # Access local clusters
#' result$local[result$local$quadrant == "HH", ]
#' }
#'
#' @references
#' Moran, P. A. P. (1950). Notes on continuous stochastic phenomena.
#'   \emph{Biometrika}, 37(1-2), 17-23.
#'
#' Anselin, L. (1995). Local indicators of spatial association---LISA.
#'   \emph{Geographical Analysis}, 27(2), 93-115.
#'
#' Bivand, R. S., Pebesma, E., & Gomez-Rubio, V. (2013).
#'   \emph{Applied Spatial Data Analysis with R} (2nd ed.). Springer.
#'
#' @seealso [sus_mod_spatial_weights()], [sus_spatial_join()]
#'
#' @export
sus_mod_spatial_moran <- function(
    df,
    outcome,
    W,
    municipalities = NULL,
    permutations   = 999L,
    alpha          = 0.05,
    adjust_p       = c("fdr", "bonferroni", "none"),
    lang           = "pt",
    verbose        = TRUE
) {
  .call <- match.call()
  adjust_p <- match.arg(adjust_p)
  lang <- match.arg(lang, c("pt", "en", "es"))

  # -- 1. Check spdep availability -------------------------------------------
  rlang::check_installed(
    "spdep",
    reason = "to compute Moran's I and LISA statistics"
  )

  # -- 2. Validate inputs -------------------------------------------------------
  if (verbose) cli::cli_progress_step(.mrl("step_check", lang))

  if (!inherits(W, "climasus_weights")) {
    cli::cli_abort(.mrl("err_not_weights", lang))
  }
  if (!("listw" %in% names(W))) {
    cli::cli_abort(.mrl("err_no_listw", lang))
  }

  if (!("code_muni" %in% names(df))) {
    cli::cli_abort(.mrl("err_no_muni", lang))
  }

  avail <- paste(names(df), collapse = ", ")
  if (!(outcome %in% names(df))) {
    cli::cli_abort(.mrl("err_no_outcome", lang, outcome = outcome, avail = avail))
  }

  # -- 3. Optional municipality filter -----------------------------------------
  if (!is.null(municipalities)) {
    n_keep <- length(municipalities)
    if (verbose) {
      cli::cli_alert_info(.mrl("warn_municipalities", lang, n_keep = n_keep))
    }
    df <- df[df[["code_muni"]] %in% municipalities, , drop = FALSE]
    # Reorder to match provided order
    row_idx <- match(municipalities, df[["code_muni"]])
    row_idx <- row_idx[!is.na(row_idx)]
    df <- df[row_idx, , drop = FALSE]
  }

  # -- 4. Extract and validate outcome vector ----------------------------------
  x <- as.numeric(df[[outcome]])

  n_w <- length(W$listw$neighbours)
  n_x <- length(x)
  if (n_x != n_w) {
    cli::cli_abort(.mrl("err_length_mismatch", lang, n_x = n_x, n_w = n_w))
  }

  if (all(is.na(x))) {
    cli::cli_abort(.mrl("err_all_na", lang, outcome = outcome))
  }

  # Impute NA with mean (with warning)
  n_na <- sum(is.na(x))
  if (n_na > 0L) {
    if (verbose) {
      cli::cli_alert_warning(
        .mrl("warn_na_values", lang, n_na = n_na, outcome = outcome)
      )
    }
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }

  # Standardise
  z <- as.vector(scale(x))

  # -- 5. Global Moran's I (Monte Carlo) ----------------------------------------
  if (verbose) {
    cli::cli_progress_step(
      .mrl("step_global", lang, permutations = permutations)
    )
  }

  mc_res <- spdep::moran.mc(
    z,
    W$listw,
    nsim        = as.integer(permutations),
    zero.policy = TRUE
  )

  # Analytical moments via moran.test for E.I, Var.I, Z.I, p_value
  mt_res <- spdep::moran.test(
    z,
    W$listw,
    randomisation = TRUE,
    zero.policy   = TRUE
  )

  global_df <- data.frame(
    I           = as.numeric(mc_res$statistic),
    E.I         = as.numeric(mt_res$estimate["Expectation"]),
    Var.I       = as.numeric(mt_res$estimate["Variance"]),
    Z.I         = as.numeric(mt_res$statistic),
    p_value     = as.numeric(mt_res$p.value),
    p_simulated = as.numeric(mc_res$p.value),
    stringsAsFactors = FALSE
  )

  # -- 6. Local Moran's I (LISA, conditional permutation) ----------------------
  if (verbose) {
    cli::cli_progress_step(
      .mrl("step_local", lang, permutations = permutations)
    )
  }

  lm_res <- spdep::localmoran_perm(
    z,
    W$listw,
    nsim        = as.integer(permutations),
    zero.policy = TRUE,
    iseed       = 1L
  )

  # Spatial lag of standardised values
  lag_z <- spdep::lag.listw(W$listw, z, zero.policy = TRUE)

  # p-values: column "Pr(z != E(Ii))" from localmoran_perm
  p_col <- grep("^Pr\\(z", colnames(lm_res), value = TRUE)[1L]
  p_raw_vec <- as.numeric(lm_res[, p_col])
  p_adj_vec <- stats::p.adjust(p_raw_vec, method = adjust_p)

  # -- 7. Quadrant classification ----------------------------------------------
  if (verbose) {
    cli::cli_progress_step(
      .mrl("step_quadrant", lang, alpha = alpha, adjust_p = adjust_p)
    )
  }

  sig <- p_adj_vec < alpha
  quad <- rep("NS", n_x)
  quad[sig & z >  0 & lag_z >  0] <- "HH"
  quad[sig & z <  0 & lag_z <  0] <- "LL"
  quad[sig & z >  0 & lag_z <= 0] <- "HL"
  quad[sig & z <= 0 & lag_z >  0] <- "LH"

  quad_factor <- factor(quad, levels = c("HH", "LL", "HL", "LH", "NS"))

  local_df <- data.frame(
    code_muni = df[["code_muni"]],
    Ii        = as.numeric(lm_res[, "Ii"]),
    Z.Ii      = as.numeric(lm_res[, "Z.Ii"]),
    p_raw     = p_raw_vec,
    p_adj     = p_adj_vec,
    quadrant  = quad_factor,
    stringsAsFactors = FALSE
  )

  # -- 8. Cluster counts -------------------------------------------------------
  n_HH <- sum(quad_factor == "HH")
  n_LL <- sum(quad_factor == "LL")
  n_HL <- sum(quad_factor == "HL")
  n_LH <- sum(quad_factor == "LH")
  n_NS <- sum(quad_factor == "NS")

  # -- 9. Done message ---------------------------------------------------------
  if (verbose) {
    I_val <- formatC(global_df$I,           digits = 4, format = "f")
    p_val <- formatC(global_df$p_simulated, digits = 4, format = "f")
    cli::cli_alert_success(
      .mrl(
        "done", lang,
        I_val = I_val, p_val = p_val,
        nHH = n_HH, nLL = n_LL,
        nHL = n_HL, nLH = n_LH,
        nNS = n_NS
      )
    )
  }

  # -- 10. Assemble output -----------------------------------------------------
  structure(
    list(
      global       = global_df,
      local        = local_df,
      n_HH         = n_HH,
      n_LL         = n_LL,
      n_HL         = n_HL,
      n_LH         = n_LH,
      outcome_name = outcome,
      call         = .call
    ),
    class = c("climasus_spatial_moran", "list")
  )
}

# =============================================================================
# S3 METHODS
# =============================================================================

#' Print method for climasus_spatial_moran objects
#'
#' Displays a formatted summary of the global Moran's I test and the LISA
#' cluster counts.
#'
#' @param x A `climasus_spatial_moran` object from [sus_mod_spatial_moran()].
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.climasus_spatial_moran <- function(x, ...) {
  g <- x$global

  cli::cli_h1("climasus4r \u2014 Moran's I Spatial Autocorrelation")
  cli::cli_text("{.strong Outcome:} {.val {x$outcome_name}}")
  cli::cli_text("{.strong N units:} {nrow(x$local)}")
  cli::cli_rule(left = "Global Moran's I")
  cli::cli_dl(c(
    "I"           = formatC(g$I,           digits = 6, format = "f"),
    "E[I]"        = formatC(g$E.I,         digits = 6, format = "f"),
    "Var[I]"      = formatC(g$Var.I,       digits = 6, format = "f"),
    "Z"           = formatC(g$Z.I,         digits = 4, format = "f"),
    "p (theory)"  = formatC(g$p_value,     digits = 4, format = "f"),
    "p (MC sim)"  = formatC(g$p_simulated, digits = 4, format = "f")
  ))
  cli::cli_rule(left = "LISA Clusters")
  cli::cli_dl(c(
    "HH (hotspot)"  = as.character(x$n_HH),
    "LL (coldspot)" = as.character(x$n_LL),
    "HL (outlier)"  = as.character(x$n_HL),
    "LH (outlier)"  = as.character(x$n_LH),
    "NS"            = as.character(sum(x$local$quadrant == "NS"))
  ))
  cli::cli_rule()

  invisible(x)
}

#' Summary method for climasus_spatial_moran objects
#'
#' Returns the global statistics data frame and the local LISA data frame.
#'
#' @param object A `climasus_spatial_moran` object.
#' @param ... Additional arguments (ignored).
#'
#' @return A list with elements `global` and `local`, invisibly.
#'
#' @export
summary.climasus_spatial_moran <- function(object, ...) {
  print(object, ...)
  invisible(list(global = object$global, local = object$local))
}
