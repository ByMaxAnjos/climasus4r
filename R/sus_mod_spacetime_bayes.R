# =============================================================================
# sus_mod_spacetime_bayes.R
# Bayesian Spatiotemporal Hierarchical Model with INLA
#
# Theory:
#   Knorr-Held, L. (2000). Bayesian modelling of inseparable space-time
#     variation in disease risk. Statistics in Medicine, 19(17-18), 2555-2567.
#   Riebler, A. et al. (2016). An intuitive Bayesian spatial model for disease
#     mapping that accounts for scaling. Statistical Methods in Medical Research,
#     25(4), 1145-1165. (BYM2)
#   Rue, H., Martino, S., & Chopin, N. (2009). Approximate Bayesian inference
#     for latent Gaussian models using integrated nested Laplace approximations.
#     Journal of the Royal Statistical Society B, 71(2), 319-392. (INLA)
#   Blangiardo, M. & Cameletti, M. (2015). Spatial and Spatio-temporal Bayesian
#     Models with R-INLA. Wiley.
# Input : climasus_df (or data.frame) at aggregate/spatial/climate stage
#         + climasus_weights object from sus_mod_spatial_weights()
# Output: climasus_spacetime_bayes (list) with fixed effects, RR grid,
#         spatial RE, temporal RE, space-time interaction RE, WAIC, DIC
# =============================================================================

# -- NSE variable declarations -------------------------------------------------
utils::globalVariables(c(
  "code_muni", "term", "mean", "sd", "lower95", "upper95", "mode",
  "time_idx", "area_idx", "phi_mean", "phi_sd",
  "psi_mean", "psi_sd", "gamma_mean", "gamma_sd",
  "rr_mean", "rr_lower95", "rr_upper95", "p_exceed",
  "area.time", ".area_idx_st", ".time_idx_st"
))

# -- Local i18n ----------------------------------------------------------------
.st_msgs <- list(

  step_check_pkgs = list(
    pt = "Verificando pacotes necess\u00e1rios (INLA)...",
    en = "Checking required packages (INLA)...",
    es = "Verificando paquetes necesarios (INLA)..."
  ),

  step_validate = list(
    pt = "Validando entradas e verificando estrutura dos dados...",
    en = "Validating inputs and checking data structure...",
    es = "Validando entradas y verificando estructura de los datos..."
  ),

  step_sort = list(
    pt = "Ordenando dados por code_muni e \u00edndice temporal ({n_areas} \u00e1reas x {n_times} per\u00edodos)...",
    en = "Sorting data by code_muni and time index ({n_areas} areas x {n_times} periods)...",
    es = "Ordenando datos por code_muni e \u00edndice temporal ({n_areas} \u00e1reas x {n_times} per\u00edodos)..."
  ),

  step_formula = list(
    pt = "Montando f\u00f3rmula INLA: {fml}",
    en = "Building INLA formula: {fml}",
    es = "Construyendo f\u00f3rmula INLA: {fml}"
  ),

  step_fit = list(
    pt = "Ajustando modelo INLA ({family} | espacial={spatial_model} | temporal={temporal_model} | intera\u00e7\u00e3o={interaction_type})...",
    en = "Fitting INLA model ({family} | spatial={spatial_model} | temporal={temporal_model} | interaction={interaction_type})...",
    es = "Ajustando modelo INLA ({family} | espacial={spatial_model} | temporal={temporal_model} | interacci\u00f3n={interaction_type})..."
  ),

  step_extract = list(
    pt = "Extraindo efeitos fixos, aleat\u00f3rios e riscos relativos...",
    en = "Extracting fixed effects, random effects, and relative risks...",
    es = "Extrayendo efectos fijos, aleatorios y riesgos relativos..."
  ),

  step_exceed = list(
    pt = "Calculando P(RR > {threshold}) por c\u00e9lula espacio-temporal...",
    en = "Computing P(RR > {threshold}) per space-time cell...",
    es = "Calculando P(RR > {threshold}) por celda espacio-temporal..."
  ),

  done = list(
    pt = "Conclu\u00eddo. WAIC = {waic_val} | DIC = {dic_val} | \u00c1reas = {n_areas} | Per\u00edodos = {n_times}",
    en = "Done. WAIC = {waic_val} | DIC = {dic_val} | Areas = {n_areas} | Periods = {n_times}",
    es = "Listo. WAIC = {waic_val} | DIC = {dic_val} | \u00c1reas = {n_areas} | Per\u00edodos = {n_times}"
  ),

  err_no_inla = list(
    pt = "O pacote INLA \u00e9 necess\u00e1rio para modelos Bayesianos espa\u00e7o-temporais.",
    en = "The INLA package is required for spatiotemporal Bayesian modeling.",
    es = "El paquete INLA es necesario para modelos Bayesianos espacio-temporales."
  ),

  err_inla_install = list(
    pt = "Instale com: install.packages('INLA', repos=c(INLA='https://inla.r-inla-download.org/R/stable', getOption('repos')))",
    en = "Install with: install.packages('INLA', repos=c(INLA='https://inla.r-inla-download.org/R/stable', getOption('repos')))",
    es = "Instale con: install.packages('INLA', repos=c(INLA='https://inla.r-inla-download.org/R/stable', getOption('repos')))"
  ),

  err_inla_alt = list(
    pt = "Ou execute: climasus4r::sus_install_deps('models')",
    en = "Or run: climasus4r::sus_install_deps('models')",
    es = "O ejecute: climasus4r::sus_install_deps('models')"
  ),

  err_no_muni = list(
    pt = "Coluna {.val code_muni} n\u00e3o encontrada em {.arg df}.",
    en = "Column {.val code_muni} not found in {.arg df}.",
    es = "Columna {.val code_muni} no encontrada en {.arg df}."
  ),

  err_no_outcome = list(
    pt = "Coluna de desfecho {.val {outcome}} n\u00e3o encontrada em {.arg df}. Colunas dispon\u00edveis: {.val {avail}}.",
    en = "Outcome column {.val {outcome}} not found in {.arg df}. Available columns: {.val {avail}}.",
    es = "Columna de resultado {.val {outcome}} no encontrada en {.arg df}. Columnas disponibles: {.val {avail}}."
  ),

  err_no_time = list(
    pt = "Coluna de tempo {.val {time_col}} n\u00e3o encontrada em {.arg df}.",
    en = "Time column {.val {time_col}} not found in {.arg df}.",
    es = "Columna de tiempo {.val {time_col}} no encontrada en {.arg df}."
  ),

  err_not_weights = list(
    pt = "{.arg W} deve ser um objeto {.cls climasus_weights} produzido por {.fn sus_mod_spatial_weights}.",
    en = "{.arg W} must be a {.cls climasus_weights} object from {.fn sus_mod_spatial_weights}.",
    es = "{.arg W} debe ser un objeto {.cls climasus_weights} de {.fn sus_mod_spatial_weights}."
  ),

  err_bad_covariate = list(
    pt = "Covari\u00e1vel(is) ausente(s) em {.arg df}: {.val {missing}}.",
    en = "Covariate(s) missing from {.arg df}: {.val {missing}}.",
    es = "Covariable(s) ausente(s) en {.arg df}: {.val {missing}}."
  ),

  err_bad_offset = list(
    pt = "Coluna de offset {.val {offset}} n\u00e3o encontrada em {.arg df}.",
    en = "Offset column {.val {offset}} not found in {.arg df}.",
    es = "Columna de offset {.val {offset}} no encontrada en {.arg df}."
  ),

  err_n_mismatch = list(
    pt = "N\u00famero de \u00e1reas \u00fanicas ({n_areas}) difere do n\u00famero de regi\u00f5es em {.arg W} ({n_w}). Verifique se {.arg df} e {.arg W} cobrem os mesmos munic\u00edpios.",
    en = "Number of unique areas ({n_areas}) differs from number of regions in {.arg W} ({n_w}). Ensure {.arg df} and {.arg W} cover the same municipalities.",
    es = "El n\u00famero de \u00e1reas \u00fanicas ({n_areas}) difiere del n\u00famero de regiones en {.arg W} ({n_w}). Aseg\u00farese de que {.arg df} y {.arg W} cubren los mismos municipios."
  ),

  err_unbalanced = list(
    pt = "Painel desbalanceado: {n_rows} linhas para {n_areas} \u00e1reas x {n_times} per\u00edodos ({expected} esperado). Certifique-se de que todos os munic\u00edpios possuem todas as observa\u00e7\u00f5es temporais.",
    en = "Unbalanced panel: {n_rows} rows for {n_areas} areas x {n_times} periods ({expected} expected). Ensure all municipalities have all time observations.",
    es = "Panel desbalanceado: {n_rows} filas para {n_areas} \u00e1reas x {n_times} per\u00edodos ({expected} esperado). Aseg\u00farese de que todos los municipios tengan todas las observaciones temporales."
  ),

  warn_unbalanced = list(
    pt = "Painel levemente desbalanceado detectado. Ajuste prosseguir\u00e1 com os dados dispon\u00edveis.",
    en = "Slightly unbalanced panel detected. Fitting will proceed with available data.",
    es = "Panel levemente desbalanceado detectado. El ajuste proceder\u00e1 con los datos disponibles."
  ),

  warn_na_outcome = list(
    pt = "{n_na} valor(es) NA em {.val {outcome}}. Linhas com NA ser\u00e3o mantidas (INLA lida com NAs como observa\u00e7\u00f5es ausentes).",
    en = "{n_na} NA value(s) in {.val {outcome}}. Rows with NA are kept (INLA handles NAs as missing observations).",
    es = "{n_na} valor(es) NA en {.val {outcome}}. Las filas con NA se conservan (INLA trata NAs como observaciones faltantes)."
  ),

  warn_no_waic = list(
    pt = "WAIC n\u00e3o dispon\u00edvel no objeto INLA. Defina {.code compute_waic = TRUE} e verifique a vers\u00e3o do INLA.",
    en = "WAIC not available in INLA object. Set {.code compute_waic = TRUE} and check INLA version.",
    es = "WAIC no disponible en el objeto INLA. Configure {.code compute_waic = TRUE} y verifique la versi\u00f3n de INLA."
  ),

  warn_no_cpo = list(
    pt = "CPO/PIT n\u00e3o dispon\u00edvel. Verifique a vers\u00e3o do INLA.",
    en = "CPO/PIT not available. Check INLA version.",
    es = "CPO/PIT no disponible. Verifique la versi\u00f3n de INLA."
  ),

  warn_interaction_graph = list(
    pt = "Intera\u00e7\u00e3o tipo {type} requer grafo espacial. Usando {.val W$W} ou {.val W$nb} para construir a estrutura.",
    en = "Interaction type {type} requires a spatial graph. Using {.val W$W} or {.val W$nb} to build structure.",
    es = "La interacci\u00f3n tipo {type} requiere un grafo espacial. Usando {.val W$W} o {.val W$nb} para construir la estructura."
  )
)

# -- Internal message helper ---------------------------------------------------
#' @keywords internal
#' @noRd
.stl <- function(key, lang, ...) {
  entry <- .st_msgs[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) {
    vars <- list(...)
    for (nm in names(vars)) msg <- gsub(sprintf("\\{%s\\}", nm), as.character(vars[[nm]]), msg)
  }
  msg
}

# -- Internal: build time index from a time column ----------------------------
#' @keywords internal
#' @noRd
.build_time_index <- function(x, time_unit) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    x <- as.Date(x)
    if (time_unit == "year") {
      keys <- as.integer(format(x, "%Y"))
    } else if (time_unit == "month") {
      keys <- as.integer(format(x, "%Y")) * 100L +
              as.integer(format(x, "%m"))
    } else if (time_unit == "week") {
      keys <- as.integer(format(x, "%Y")) * 100L +
              as.integer(format(x, "%W"))
    } else {
      # auto: choose unit based on date range
      rng <- as.numeric(diff(range(x, na.rm = TRUE)))
      if (rng > 365 * 2) {
        keys <- as.integer(format(x, "%Y"))
      } else if (rng > 60) {
        keys <- as.integer(format(x, "%Y")) * 100L +
                as.integer(format(x, "%m"))
      } else {
        keys <- as.integer(format(x, "%Y")) * 100L +
                as.integer(format(x, "%W"))
      }
    }
  } else if (is.integer(x) || is.numeric(x)) {
    keys <- as.integer(x)
  } else {
    keys <- as.integer(factor(x))
  }
  # map unique sorted keys to 1:T
  ux   <- sort(unique(keys))
  idx  <- match(keys, ux)
  list(index = idx, keys = ux, n = length(ux))
}

# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Bayesian Spatiotemporal Hierarchical Model with INLA
#'
#' @description
#' Fits a full Bayesian spatiotemporal hierarchical model using INLA
#' (Integrated Nested Laplace Approximation). The model decomposes disease
#' risk into structured spatial effects (BYM2, BYM, Besag, or IID), structured
#' temporal effects (RW1, RW2, AR1, or IID), and optionally a space-time
#' interaction following the four Knorr-Held (2000) interaction types.
#' Climate and environmental covariates from a `climasus_df` can be included
#' as fixed effects.
#'
#' @section Model structure:
#'
#' The linear predictor for area \eqn{i} at time \eqn{t} is:
#'
#' \deqn{\eta_{it} = \alpha + \mathbf{x}_{it}^{\top}\boldsymbol{\beta}
#'   + \phi_i + \psi_t + \delta_{it} + \log(E_{it})}
#'
#' where \eqn{\phi_i} is the spatial random effect, \eqn{\psi_t} the temporal
#' random effect, \eqn{\delta_{it}} the optional space-time interaction, and
#' \eqn{E_{it}} the expected counts offset.
#'
#' @section Spatial model options:
#'
#' \describe{
#'   \item{`"bym2"`}{BYM2 model (Riebler et al. 2016): scaled ICAR + IID with
#'     a single precision and a mixing parameter. Recommended for disease
#'     mapping. PC priors applied.}
#'   \item{`"bym"`}{Original BYM model (Besag, York & Mollie 1991): ICAR +
#'     independent Gaussian components with separate precision parameters.}
#'   \item{`"besag"`}{Intrinsic CAR (Besag 1974): purely structured spatial
#'     random effect.}
#'   \item{`"iid"`}{Independent normal random effects (no spatial structure).}
#' }
#'
#' @section Knorr-Held interaction types:
#'
#' \describe{
#'   \item{`"none"`}{No space-time interaction.}
#'   \item{`"I"`}{IID interaction: \eqn{\delta_{it} \sim N(0, \sigma^2_\delta)}.}
#'   \item{`"II"`}{Structured in time, unstructured in space:
#'     separate RW1 per area.}
#'   \item{`"III"`}{Structured in space, unstructured in time:
#'     separate ICAR per time period.}
#'   \item{`"IV"`}{Structured in both space and time (inseparable interaction).}
#' }
#'
#' @section PC priors:
#'
#' For the BYM2 spatial model, penalised-complexity (PC) priors are used
#' for the precision hyperparameter: \eqn{P(\sigma > u) = \alpha} where
#' `pc_prior_u` and `pc_prior_alpha` control the tail probability. Default
#' values `u = 0.5`, `alpha = 0.01` are weakly informative and suitable for
#' standardised disease mapping.
#'
#' @param df A `data.frame` or `climasus_df` at the `aggregate`, `spatial`, or
#'   `climate` pipeline stage. Must contain `code_muni` (7-digit IBGE code),
#'   the `outcome` column, the `time_col` column, all columns named in
#'   `covariates`, and optionally an `offset` column. The data must form a
#'   panel (one row per area-time combination).
#' @param outcome Character. Name of the response (count) column in `df`.
#' @param W A `climasus_weights` object produced by
#'   [sus_mod_spatial_weights()]. Must contain either a `$W` dense matrix or
#'   a `$nb` neighbour list used to build the INLA graph.
#' @param time_col Character. Name of the column containing time information.
#'   Can be a `Date`, `integer`, or `character` column. Default `"date"`.
#' @param time_unit Character. Temporal aggregation unit used to build the
#'   integer time index. One of `"year"`, `"month"`, `"week"`, or `"auto"`
#'   (automatic detection based on date range). Default `"auto"`.
#' @param covariates Character vector of covariate column names in `df` to
#'   include as fixed effects. Default `NULL` (intercept-only model).
#' @param offset Character. Name of a **population or expected counts** column
#'   in `df` (natural scale). Entered as `log(offset)` in the linear
#'   predictor. Default `NULL`.
#' @param family Character. Likelihood family. One of `"poisson"` (default),
#'   `"nbinomial"` (negative binomial), `"binomial"`, or `"gaussian"`.
#' @param spatial_model Character. Spatial random effect structure. One of
#'   `"bym2"` (default), `"bym"`, `"besag"`, or `"iid"`. See **Spatial model
#'   options**.
#' @param temporal_model Character. Temporal random effect structure. One of
#'   `"rw1"` (default), `"rw2"`, `"ar1"`, or `"iid_time"`.
#' @param interaction_type Character. Space-time interaction type following
#'   Knorr-Held (2000). One of `"none"` (default), `"I"`, `"II"`, `"III"`,
#'   or `"IV"`. See **Knorr-Held interaction types**.
#' @param pc_prior_u Positive numeric. PC prior parameter \eqn{u} for the
#'   BYM2 spatial precision: \eqn{P(\sigma > u) = \alpha}. Default `0.5`.
#' @param pc_prior_alpha Numeric in (0, 1). PC prior parameter \eqn{\alpha}
#'   (tail probability). Default `0.01`.
#' @param compute_waic Logical. If `TRUE` (default), compute WAIC via
#'   `control.compute = list(waic = TRUE)`.
#' @param compute_cpo Logical. If `TRUE`, compute CPO/PIT values via
#'   `control.compute = list(cpo = TRUE)`. Default `FALSE` (slower).
#' @param exceedance_threshold Positive numeric. Compute \eqn{P(RR > threshold)}
#'   for each space-time cell using the posterior marginals. Default `1.0`.
#' @param n_samples Positive integer. Number of posterior samples drawn via
#'   `INLA::inla.posterior.sample()` to compute RR credible intervals and
#'   exceedance probabilities. Default `1000L`.
#' @param seed Integer. Random seed for reproducibility. Default `42L`.
#' @param lang Character. Language for CLI messages: `"pt"` (default),
#'   `"en"`, `"es"`.
#' @param verbose Logical. If `TRUE` (default), emit progress messages via
#'   **cli**.
#'
#' @return A named list of class `c("climasus_spacetime_bayes", "list")` with
#'   the following slots:
#'   \describe{
#'     \item{`$fixed`}{`data.frame` with columns `term`, `mean`, `sd`,
#'       `lower95`, `upper95`, `mode` (posterior fixed-effect summaries).}
#'     \item{`$rr`}{`data.frame` with columns `code_muni`, `time_idx`,
#'       `rr_mean`, `rr_lower95`, `rr_upper95`, `p_exceed` (relative risk
#'       per space-time cell with exceedance probability).}
#'     \item{`$spatial_re`}{`data.frame` with columns `code_muni`, `phi_mean`,
#'       `phi_sd` (posterior summaries of structured spatial random effect).}
#'     \item{`$temporal_re`}{`data.frame` with columns `time_idx`, `psi_mean`,
#'       `psi_sd` (posterior summaries of structured temporal random effect).}
#'     \item{`$interaction_re`}{`data.frame` with columns `code_muni`,
#'       `time_idx`, `gamma_mean`, `gamma_sd`, or `NULL` if
#'       `interaction_type = "none"`.}
#'     \item{`$fitted`}{Numeric vector of posterior mean fitted values
#'       (response scale).}
#'     \item{`$waic`}{Named list with `$waic` (scalar) and `$p_eff` (effective
#'       number of parameters), or `NULL` if `compute_waic = FALSE`.}
#'     \item{`$dic`}{Named list with `$dic` (scalar) and `$p_d` (effective
#'       parameters), or `NULL`.}
#'     \item{`$model_spec`}{Named list with `$spatial_model`,
#'       `$temporal_model`, `$interaction_type`, `$family`.}
#'     \item{`$call`}{The matched call.}
#'     \item{`$n_areas`}{Integer. Number of unique municipalities.}
#'     \item{`$n_times`}{Integer. Number of unique time periods.}
#'   }
#'
#' @references
#' Knorr-Held, L. (2000). Bayesian modelling of inseparable space-time
#' variation in disease risk. \emph{Statistics in Medicine}, 19(17-18),
#' 2555-2567.
#'
#' Riebler, A., Sorbye, S. H., Simpson, D., & Rue, H. (2016). An intuitive
#' Bayesian spatial model for disease mapping that accounts for scaling.
#' \emph{Statistical Methods in Medical Research}, 25(4), 1145-1165.
#'
#' Rue, H., Martino, S., & Chopin, N. (2009). Approximate Bayesian inference
#' for latent Gaussian models using integrated nested Laplace approximations.
#' \emph{Journal of the Royal Statistical Society B}, 71(2), 319-392.
#'
#' Blangiardo, M., & Cameletti, M. (2015). \emph{Spatial and Spatio-temporal
#' Bayesian Models with R-INLA}. Wiley.
#'
#' Besag, J., York, J., & Mollie, A. (1991). Bayesian image restoration, with
#' two applications in spatial statistics. \emph{Annals of the Institute of
#' Statistical Mathematics}, 43(1), 1-20.
#'
#' @seealso
#' [sus_mod_spatial_weights()] to build the required adjacency object,
#' [sus_mod_spatial_bayes()] for purely cross-sectional spatial Bayesian
#' models (CARBayes), [sus_mod_dlnm()] for time-series DLNM models.
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#' library(geobr)
#'
#' # Build spatial weights for Rio Grande do Norte
#' shp <- geobr::read_municipality(code_muni = "RN", year = 2022)
#' W   <- sus_mod_spatial_weights(shp, return_matrix = TRUE)
#'
#' # Prepare aggregated health-climate panel data (areas x time periods)
#' # agg <- sus_data_aggregate(cleaned_df) |>
#' #          sus_climate_inmet() |>
#' #          sus_climate_aggregate(temporal_strategy = "exact")
#'
#' # BYM2 spatiotemporal model with Poisson family
#' result <- sus_mod_spacetime_bayes(
#'   df               = agg,
#'   outcome          = "deaths",
#'   W                = W,
#'   time_col         = "date",
#'   time_unit        = "month",
#'   covariates       = c("temp_mean", "prec_total"),
#'   offset           = "population",
#'   family           = "poisson",
#'   spatial_model    = "bym2",
#'   temporal_model   = "rw1",
#'   interaction_type = "I",
#'   pc_prior_u       = 0.5,
#'   pc_prior_alpha   = 0.01,
#'   compute_waic     = TRUE,
#'   n_samples        = 1000L,
#'   seed             = 42L,
#'   lang             = "pt"
#' )
#'
#' print(result)
#' result$fixed
#' result$rr
#' result$spatial_re
#' result$temporal_re
#' result$waic
#' }
#'
#' @export
#' @importFrom stats as.formula quantile sd
#' @importFrom cli cli_h1 cli_dl cli_rule cli_text cli_alert_success
#'   cli_alert_warning cli_progress_step cli_abort
#' @importFrom glue glue
#' @importFrom rlang check_installed env
sus_mod_spacetime_bayes <- function(
    df,
    outcome,
    W,
    time_col          = "date",
    time_unit         = c("year", "month", "week", "auto"),
    covariates        = NULL,
    offset            = NULL,
    family            = c("poisson", "nbinomial", "binomial", "gaussian"),
    spatial_model     = c("bym2", "bym", "besag", "iid"),
    temporal_model    = c("rw1", "rw2", "ar1", "iid_time"),
    interaction_type  = c("none", "I", "II", "III", "IV"),
    pc_prior_u        = 0.5,
    pc_prior_alpha    = 0.01,
    compute_waic      = TRUE,
    compute_cpo       = FALSE,
    exceedance_threshold = 1.0,
    n_samples         = 1000L,
    seed              = 42L,
    lang              = "pt",
    verbose           = TRUE
) {
  .call          <- match.call()
  time_unit      <- match.arg(time_unit)
  family         <- match.arg(family)
  spatial_model  <- match.arg(spatial_model)
  temporal_model <- match.arg(temporal_model)
  interaction_type <- match.arg(interaction_type)
  lang           <- match.arg(lang, c("pt", "en", "es"))

  # ── 0. INLA availability check ──────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.stl("step_check_pkgs", lang))

  if (!requireNamespace("INLA", quietly = TRUE)) {
    cli::cli_abort(c(
      .stl("err_no_inla", lang),
      "i" = .stl("err_inla_install", lang),
      "i" = .stl("err_inla_alt", lang)
    ))
  }

  # ── 1. input validation ──────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.stl("step_validate", lang))

  if (!("code_muni" %in% names(df))) {
    cli::cli_abort(.stl("err_no_muni", lang))
  }

  avail <- paste(names(df), collapse = ", ")
  if (!(outcome %in% names(df))) {
    cli::cli_abort(.stl("err_no_outcome", lang,
                        outcome = outcome, avail = avail))
  }

  if (!(time_col %in% names(df))) {
    cli::cli_abort(.stl("err_no_time", lang, time_col = time_col))
  }

  if (!inherits(W, "climasus_weights")) {
    cli::cli_abort(.stl("err_not_weights", lang))
  }

  if (!is.null(covariates) && length(covariates) > 0L) {
    missing_cov <- setdiff(covariates, names(df))
    if (length(missing_cov) > 0L) {
      cli::cli_abort(
        .stl("err_bad_covariate", lang,
             missing = paste(missing_cov, collapse = ", "))
      )
    }
  }

  if (!is.null(offset) && !(offset %in% names(df))) {
    cli::cli_abort(.stl("err_bad_offset", lang, offset = offset))
  }

  # ── 2. build integer area and time indices ────────────────────────────────
  # sort by code_muni then time_col to get a consistent panel ordering
  df <- as.data.frame(df)

  time_info   <- .build_time_index(df[[time_col]], time_unit)
  time_idx_v  <- time_info$index      # integer 1..T per row
  n_times     <- time_info$n

  area_keys   <- sort(unique(df[["code_muni"]]))
  area_idx_v  <- match(df[["code_muni"]], area_keys)
  n_areas     <- length(area_keys)

  n_w <- W$n_regions
  if (n_areas != n_w) {
    cli::cli_abort(
      .stl("err_n_mismatch", lang, n_areas = n_areas, n_w = n_w)
    )
  }

  # sort rows: area first, then time
  ord    <- order(area_idx_v, time_idx_v)
  df     <- df[ord, , drop = FALSE]
  area_idx_v <- area_idx_v[ord]
  time_idx_v <- time_idx_v[ord]

  n_rows   <- nrow(df)
  expected <- n_areas * n_times

  if (n_rows != expected) {
    if (abs(n_rows - expected) / expected < 0.05) {
      if (verbose) cli::cli_alert_warning(.stl("warn_unbalanced", lang))
    } else {
      cli::cli_abort(
        .stl("err_unbalanced", lang,
             n_rows   = n_rows,
             n_areas  = n_areas,
             n_times  = n_times,
             expected = expected)
      )
    }
  }

  if (verbose) {
    cli::cli_progress_step(
      .stl("step_sort", lang, n_areas = n_areas, n_times = n_times)
    )
  }

  # NA check on outcome (INLA treats NAs as missing - just warn)
  n_na <- sum(is.na(df[[outcome]]))
  if (n_na > 0L && verbose) {
    cli::cli_alert_warning(
      .stl("warn_na_outcome", lang, n_na = n_na, outcome = outcome)
    )
  }

  # ── 3. build adjacency graph for INLA ────────────────────────────────────
  # Write adjacency graph to a temp file so INLA can read it
  if (!is.null(W$W)) {
    W_mat <- W$W
  } else {
    W_mat <- spdep::nb2mat(W$nb, style = "B", zero.policy = TRUE)
  }
  W_mat <- (W_mat != 0) * 1L
  storage.mode(W_mat) <- "numeric"

  # Write graph in INLA format (nb2INLA-style)
  graph_file <- tempfile(fileext = ".graph")
  INLA::inla.write.graph(
    INLA::inla.matrix2graph(W_mat),
    filename = graph_file
  )

  # ── 4. build INLA data frame ──────────────────────────────────────────────
  # Attach computed indices as columns for the formula
  df[[".area_idx_st"]] <- area_idx_v
  df[[".time_idx_st"]] <- time_idx_v
  # Space-time interaction index: Kronecker row index
  df[["area.time"]]    <- (area_idx_v - 1L) * n_times + time_idx_v

  # ── 5. define PC priors and hyperparameter specs ──────────────────────────
  pc_prec_hyper <- list(
    prec = list(
      prior = "pc.prec",
      param = c(pc_prior_u, pc_prior_alpha)
    )
  )

  # Temporal RW priors (PC priors for smoothness)
  pc_time_hyper <- list(
    prec = list(
      prior = "pc.prec",
      param = c(0.5, 0.01)
    )
  )

  # ── 6. build INLA formula components ─────────────────────────────────────
  if (verbose) {
    fml_preview <- paste0(
      outcome, " ~ 1 [spatial=", spatial_model,
      " temporal=", temporal_model,
      " interaction=", interaction_type, "]"
    )
    cli::cli_progress_step(
      .stl("step_formula", lang, fml = fml_preview)
    )
  }

  # -- 6a. fixed effects --
  cov_rhs <- if (!is.null(covariates) && length(covariates) > 0L) {
    paste(covariates, collapse = " + ")
  } else {
    character(0L)
  }

  # -- 6b. spatial random effect --
  spatial_term <- switch(
    spatial_model,
    bym2 = paste0(
      "f(.area_idx_st, model='bym2', graph='", graph_file, "',",
      " scale.model=TRUE,",
      " hyper=list(",
      "   phi=list(prior='pc', param=c(0.5, 0.5), initial=-3),",
      "   prec=list(prior='pc.prec', param=c(", pc_prior_u, ",",
      pc_prior_alpha, "), initial=5)",
      " ))"
    ),
    bym = paste0(
      "f(.area_idx_st, model='bym', graph='", graph_file, "',",
      " scale.model=TRUE,",
      " hyper=list(",
      "   prec.unstruct=list(prior='loggamma', param=c(1,0.01)),",
      "   prec.spatial=list(prior='loggamma', param=c(1,0.01))",
      " ))"
    ),
    besag = paste0(
      "f(.area_idx_st, model='besag', graph='", graph_file, "',",
      " scale.model=TRUE,",
      " hyper=list(prec=list(prior='pc.prec', param=c(",
      pc_prior_u, ",", pc_prior_alpha, "))))"
    ),
    iid = paste0(
      "f(.area_idx_st, model='iid',",
      " hyper=list(prec=list(prior='pc.prec', param=c(",
      pc_prior_u, ",", pc_prior_alpha, "))))"
    )
  )

  # -- 6c. temporal random effect --
  temporal_inla_model <- switch(
    temporal_model,
    rw1      = "rw1",
    rw2      = "rw2",
    ar1      = "ar1",
    iid_time = "iid"
  )
  temporal_term <- paste0(
    "f(.time_idx_st, model='", temporal_inla_model, "',",
    " scale.model=TRUE,",
    " hyper=list(prec=list(prior='pc.prec', param=c(0.5, 0.01))))"
  )

  # -- 6d. space-time interaction --
  interaction_term <- NULL
  if (interaction_type != "none") {
    if (interaction_type %in% c("II", "III", "IV")) {
      if (verbose) {
        cli::cli_alert_warning(
          .stl("warn_interaction_graph", lang, type = interaction_type)
        )
      }
    }
    interaction_term <- switch(
      interaction_type,
      "I" = paste0(
        "f(area.time, model='iid',",
        " hyper=list(prec=list(prior='pc.prec', param=c(0.5,0.01))))"
      ),
      "II" = paste0(
        "f(area.time, model='iid',",
        " group=.time_idx_st,",
        " control.group=list(model='rw1'),",
        " hyper=list(prec=list(prior='pc.prec', param=c(0.5,0.01))))"
      ),
      "III" = paste0(
        "f(area.time, model='besag',",
        " graph='", graph_file, "',",
        " group=.time_idx_st,",
        " control.group=list(model='iid'),",
        " hyper=list(prec=list(prior='pc.prec', param=c(0.5,0.01))))"
      ),
      "IV" = paste0(
        "f(area.time, model='bym2',",
        " graph='", graph_file, "',",
        " group=.time_idx_st,",
        " control.group=list(model='rw1'),",
        " scale.model=TRUE,",
        " hyper=list(",
        "   phi=list(prior='pc', param=c(0.5,0.5), initial=-3),",
        "   prec=list(prior='pc.prec', param=c(0.5,0.01), initial=5)",
        " ))"
      )
    )
  }

  # -- 6e. assemble formula --
  rhs_parts <- c(
    "1",
    cov_rhs,
    spatial_term,
    temporal_term,
    interaction_term
  )
  rhs_parts <- rhs_parts[nzchar(rhs_parts)]
  fml_str   <- paste0(outcome, " ~ ", paste(rhs_parts, collapse = " + "))
  fml       <- stats::as.formula(fml_str, env = parent.frame())

  # ── 7. offset vector ─────────────────────────────────────────────────────
  E_vec <- if (!is.null(offset)) {
    as.numeric(df[[offset]])
  } else {
    rep(1.0, n_rows)
  }

  # ── 8. control objects ────────────────────────────────────────────────────
  control_compute <- list(
    dic         = TRUE,
    waic        = compute_waic,
    cpo         = compute_cpo,
    config      = TRUE,
    return.marginals.predictor = TRUE
  )
  control_predictor <- list(compute = TRUE, link = 1)

  # ── 9. fit INLA model ─────────────────────────────────────────────────────
  if (verbose) {
    cli::cli_progress_step(
      .stl("step_fit", lang,
           family           = family,
           spatial_model    = spatial_model,
           temporal_model   = temporal_model,
           interaction_type = interaction_type)
    )
  }

  set.seed(seed)

  # Map family name: INLA uses "nbinomial" internally
  inla_family <- switch(
    family,
    poisson   = "poisson",
    nbinomial = "nbinomial",
    binomial  = "binomial",
    gaussian  = "gaussian"
  )

  fit <- INLA::inla(
    formula           = fml,
    family            = inla_family,
    data              = df,
    E                 = E_vec,
    control.compute   = control_compute,
    control.predictor = control_predictor,
    verbose           = FALSE
  )

  # ── 10. extract results ───────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.stl("step_extract", lang))

  # -- 10a. fixed effects ---------------------------------------------------
  fe_sum  <- fit$summary.fixed
  fe_names <- rownames(fe_sum)

  fixed_df <- data.frame(
    term    = fe_names,
    mean    = as.numeric(fe_sum[, "mean"]),
    sd      = as.numeric(fe_sum[, "sd"]),
    lower95 = as.numeric(fe_sum[, "0.025quant"]),
    upper95 = as.numeric(fe_sum[, "0.975quant"]),
    mode    = as.numeric(fe_sum[, "mode"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # -- 10b. spatial random effect (area-level) ------------------------------
  # For bym2/bym: INLA returns summary for the full effect under the first
  # component name; for besag/iid it is direct.
  re_names <- names(fit$summary.random)

  spatial_re_name <- ".area_idx_st"
  spatial_sum <- if (spatial_re_name %in% re_names) {
    fit$summary.random[[spatial_re_name]]
  } else {
    NULL
  }

  if (!is.null(spatial_sum)) {
    # For BYM2/BYM INLA returns 2*n_areas rows (structured + unstructured);
    # use only first n_areas rows (total effect or spatial component)
    n_sp <- min(nrow(spatial_sum), n_areas)
    spatial_re_df <- data.frame(
      code_muni = area_keys[seq_len(n_sp)],
      phi_mean  = as.numeric(spatial_sum[seq_len(n_sp), "mean"]),
      phi_sd    = as.numeric(spatial_sum[seq_len(n_sp), "sd"]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    spatial_re_df <- data.frame(
      code_muni = area_keys,
      phi_mean  = rep(NA_real_, n_areas),
      phi_sd    = rep(NA_real_, n_areas),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  # -- 10c. temporal random effect ------------------------------------------
  temporal_re_name <- ".time_idx_st"
  temporal_sum <- if (temporal_re_name %in% re_names) {
    fit$summary.random[[temporal_re_name]]
  } else {
    NULL
  }

  if (!is.null(temporal_sum)) {
    n_tp <- min(nrow(temporal_sum), n_times)
    temporal_re_df <- data.frame(
      time_idx = seq_len(n_tp),
      psi_mean = as.numeric(temporal_sum[seq_len(n_tp), "mean"]),
      psi_sd   = as.numeric(temporal_sum[seq_len(n_tp), "sd"]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    temporal_re_df <- data.frame(
      time_idx = seq_len(n_times),
      psi_mean = rep(NA_real_, n_times),
      psi_sd   = rep(NA_real_, n_times),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  # -- 10d. interaction random effect ---------------------------------------
  interaction_re_df <- NULL
  if (interaction_type != "none") {
    inter_sum <- if ("area.time" %in% re_names) {
      fit$summary.random[["area.time"]]
    } else {
      NULL
    }
    if (!is.null(inter_sum)) {
      n_inter <- nrow(inter_sum)
      # reconstruct area and time from sequential index
      ai_seq <- ((seq_len(n_inter) - 1L) %/% n_times) + 1L
      ti_seq <- ((seq_len(n_inter) - 1L) %%  n_times) + 1L
      interaction_re_df <- data.frame(
        code_muni  = area_keys[pmin(ai_seq, n_areas)],
        time_idx   = ti_seq,
        gamma_mean = as.numeric(inter_sum[, "mean"]),
        gamma_sd   = as.numeric(inter_sum[, "sd"]),
        stringsAsFactors = FALSE,
        row.names  = NULL
      )
    }
  }

  # -- 10e. fitted values (posterior mean, response scale) ------------------
  fitted_vals <- fit$summary.fitted.values[, "mean"]
  if (family == "poisson" || family == "nbinomial") {
    fitted_vals <- fitted_vals * E_vec
  }

  # -- 10f. relative risks and exceedance probabilities --------------------
  if (verbose) {
    cli::cli_progress_step(
      .stl("step_exceed", lang, threshold = exceedance_threshold)
    )
  }

  # Linear predictor posterior mean and sd (log-RR for Poisson)
  lp_mean <- fit$summary.linear.predictor[, "mean"]
  lp_sd   <- fit$summary.linear.predictor[, "sd"]

  # Posterior mean RR = exp(linear predictor mean) for Poisson/NB
  rr_mean_v  <- exp(lp_mean)
  rr_lower_v <- exp(fit$summary.linear.predictor[, "0.025quant"])
  rr_upper_v <- exp(fit$summary.linear.predictor[, "0.975quant"])

  # P(RR > threshold) = P(lp > log(threshold)) using normal approximation
  # on the linear predictor marginals
  log_thresh <- log(exceedance_threshold)
  p_exceed_v <- stats::pnorm(
    log_thresh,
    mean  = lp_mean,
    sd    = lp_sd,
    lower.tail = FALSE
  )

  rr_df <- data.frame(
    code_muni  = df[["code_muni"]],
    time_idx   = time_idx_v,
    rr_mean    = as.numeric(rr_mean_v),
    rr_lower95 = as.numeric(rr_lower_v),
    rr_upper95 = as.numeric(rr_upper_v),
    p_exceed   = as.numeric(p_exceed_v),
    stringsAsFactors = FALSE,
    row.names  = NULL
  )

  # -- 10g. model fit criteria: WAIC and DIC --------------------------------
  waic_out <- NULL
  if (compute_waic) {
    tryCatch({
      waic_val <- fit$waic$waic
      p_eff    <- fit$waic$p.eff
      if (!is.null(waic_val)) {
        waic_out <- list(waic = as.numeric(waic_val),
                         p_eff = as.numeric(p_eff))
      } else {
        if (verbose) cli::cli_alert_warning(.stl("warn_no_waic", lang))
      }
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning(.stl("warn_no_waic", lang))
    })
  }

  dic_out <- tryCatch({
    dic_val <- fit$dic$dic
    p_d     <- fit$dic$p.eff
    if (!is.null(dic_val)) {
      list(dic = as.numeric(dic_val), p_d = as.numeric(p_d))
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # ── 11. done message ──────────────────────────────────────────────────────
  if (verbose) {
    waic_str <- if (!is.null(waic_out)) {
      formatC(waic_out$waic, digits = 2, format = "f")
    } else "NA"
    dic_str <- if (!is.null(dic_out)) {
      formatC(dic_out$dic, digits = 2, format = "f")
    } else "NA"
    cli::cli_alert_success(
      .stl("done", lang,
           waic_val = waic_str,
           dic_val  = dic_str,
           n_areas  = n_areas,
           n_times  = n_times)
    )
  }

  # ── 12. assemble output ────────────────────────────────────────────────────
  structure(
    list(
      fixed          = fixed_df,
      rr             = rr_df,
      spatial_re     = spatial_re_df,
      temporal_re    = temporal_re_df,
      interaction_re = interaction_re_df,
      fitted         = as.numeric(fitted_vals),
      waic           = waic_out,
      dic            = dic_out,
      model_spec     = list(
        spatial_model    = spatial_model,
        temporal_model   = temporal_model,
        interaction_type = interaction_type,
        family           = family
      ),
      call     = .call,
      n_areas  = n_areas,
      n_times  = n_times
    ),
    class = c("climasus_spacetime_bayes", "list")
  )
}

# =============================================================================
# S3 METHODS
# =============================================================================

#' Print method for climasus_spacetime_bayes objects
#'
#' Displays a formatted summary of the spatiotemporal Bayesian model fit,
#' including model specification, goodness-of-fit statistics, posterior
#' fixed-effect estimates with 95% credible intervals, and a summary of
#' relative risks across the space-time grid.
#'
#' @param x A `climasus_spacetime_bayes` object from
#'   [sus_mod_spacetime_bayes()].
#' @param n_fixed Integer. Maximum number of fixed-effect rows to display.
#'   Default `10L`.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.climasus_spacetime_bayes <- function(x, n_fixed = 10L, ...) {
  cli::cli_h1(
    paste0(
      "climasus4r -- Bayesian Spatiotemporal Model",
      " ({.cls climasus_spacetime_bayes})"
    )
  )

  # -- model specification ---------------------------------------------------
  ms <- x$model_spec
  cli::cli_rule(left = "Model Specification")
  cli::cli_dl(c(
    "Spatial effect"  = ms$spatial_model,
    "Temporal effect" = ms$temporal_model,
    "Interaction"     = ms$interaction_type,
    "Family"          = ms$family
  ))

  # -- fit statistics --------------------------------------------------------
  cli::cli_rule(left = "Fit Statistics")
  waic_str <- if (!is.null(x$waic)) {
    paste0(formatC(x$waic$waic, digits = 2, format = "f"),
           "  (p_eff = ",
           formatC(x$waic$p_eff, digits = 1, format = "f"), ")")
  } else {
    "not computed"
  }
  dic_str <- if (!is.null(x$dic)) {
    paste0(formatC(x$dic$dic, digits = 2, format = "f"),
           "  (p_d = ",
           formatC(x$dic$p_d, digits = 1, format = "f"), ")")
  } else {
    "not computed"
  }
  cli::cli_dl(c(
    "WAIC"       = waic_str,
    "DIC"        = dic_str,
    "Areas"      = as.character(x$n_areas),
    "Time steps" = as.character(x$n_times),
    "Obs. total" = as.character(nrow(x$rr))
  ))

  # -- fixed effects ---------------------------------------------------------
  cli::cli_rule(left = "Fixed Effects  (mean  [95% CrI]   mode)")
  fx     <- x$fixed
  n_show <- min(nrow(fx), as.integer(n_fixed))
  for (i in seq_len(n_show)) {
    term_str <- fx$term[i]
    mn_str   <- formatC(fx$mean[i],    digits = 4, format = "f")
    lo_str   <- formatC(fx$lower95[i], digits = 4, format = "f")
    hi_str   <- formatC(fx$upper95[i], digits = 4, format = "f")
    mo_str   <- formatC(fx$mode[i],    digits = 4, format = "f")
    cli::cli_text(
      "{.field {term_str}}: {mn_str}  [{lo_str}, {hi_str}]  mode={mo_str}"
    )
  }
  if (nrow(fx) > n_show) {
    cli::cli_text(
      "... {nrow(fx) - n_show} more fixed effect(s) not shown."
    )
  }

  # -- relative risks --------------------------------------------------------
  cli::cli_rule(left = "Relative Risk across space-time grid")
  rr_mn  <- mean(x$rr$rr_mean,    na.rm = TRUE)
  rr_min <- min(x$rr$rr_mean,     na.rm = TRUE)
  rr_max <- max(x$rr$rr_mean,     na.rm = TRUE)
  p_ex_mn <- mean(x$rr$p_exceed,  na.rm = TRUE)
  cli::cli_dl(c(
    "RR mean (all cells)"      = formatC(rr_mn,   digits = 4, format = "f"),
    "RR range"                 = paste0(
      formatC(rr_min, digits = 4, format = "f"),
      " - ",
      formatC(rr_max, digits = 4, format = "f")
    ),
    "Mean P(RR > 1.0)"          = formatC(p_ex_mn, digits = 4, format = "f")
  ))

  cli::cli_rule()
  invisible(x)
}

#' Summary method for climasus_spacetime_bayes objects
#'
#' Alias for [print.climasus_spacetime_bayes()].
#'
#' @param object A `climasus_spacetime_bayes` object.
#' @param ... Additional arguments passed to [print.climasus_spacetime_bayes()].
#'
#' @return Invisibly returns `object`.
#'
#' @export
summary.climasus_spacetime_bayes <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
