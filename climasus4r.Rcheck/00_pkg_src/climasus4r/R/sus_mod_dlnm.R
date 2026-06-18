# =============================================================================
# sus_mod_dlnm.R
# Distributed Lag Non-linear Model (DLNM) for Climate-Health Analyses
#
# Theory: Gasparrini et al. (2010, 2011, 2014); Armstrong (2006)
# Input : climasus_df at stage "climate", type "distributed_lag"
#         produced by sus_climate_aggregate(temporal_strategy = "distributed_lag")
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "y", "date", "pct", "exposure", "rr", "lo", "hi", "rr_cum"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.mdl_labels <- list(
  step_validate  = list(
    pt = "Validando entradas...",
    en = "Validating inputs...",
    es = "Validando entradas..."
  ),
  step_aggregate = list(
    pt = "Agregando dados por data ({n} obs)...",
    en = "Aggregating data by date ({n} obs)...",
    es = "Agregando datos por fecha ({n} obs)..."
  ),
  step_crossbasis = list(
    pt = "Construindo crossbasis (lag 0-{lag_max})...",
    en = "Building crossbasis (lag 0-{lag_max})...",
    es = "Construyendo crossbasis (lag 0-{lag_max})..."
  ),
  step_fit = list(
    pt = "Ajustando GLM {.val {fam}}...",
    en = "Fitting {.val {fam}} GLM...",
    es = "Ajustando GLM {.val {fam}}..."
  ),
  step_crosspred = list(
    pt = "Calculando crosspred (grade de {n_grid} pontos)...",
    en = "Computing crosspred ({n_grid}-point grid)...",
    es = "Calculando crosspred (grilla de {n_grid} puntos)..."
  ),
  step_diagnostics = list(
    pt = "Executando diagnosticos do modelo...",
    en = "Running model diagnostics...",
    es = "Ejecutando diagnosticos del modelo..."
  ),
  done = list(
    pt = "DLNM ajustado. RR cumulativo (p75 vs mediana): {rr} [{lo}, {hi}] -- lag pico: {lag_pk} dias",
    en = "DLNM fitted. Cumulative RR (p75 vs median): {rr} [{lo}, {hi}] -- peak lag: {lag_pk} days",
    es = "DLNM ajustado. RR acumulado (p75 vs mediana): {rr} [{lo}, {hi}] -- lag pico: {lag_pk} dias"
  ),
  err_not_climasus = list(
    pt = "{.arg df} deve ser um {.cls climasus_df}. Use {.fn sus_climate_aggregate} para crialo.",
    en = "{.arg df} must be a {.cls climasus_df}. Use {.fn sus_climate_aggregate} to create it.",
    es = "{.arg df} debe ser un {.cls climasus_df}. Use {.fn sus_climate_aggregate} para crearlo."
  ),
  err_stage = list(
    pt = "Dados devem estar no estagio {.val climate}. Estagio atual: {.val {current_stage}}. Execute {.fn sus_climate_aggregate} primeiro.",
    en = "Data must be at stage {.val climate}. Current stage: {.val {current_stage}}. Run {.fn sus_climate_aggregate} first.",
    es = "Los datos deben estar en etapa {.val climate}. Etapa actual: {.val {current_stage}}. Ejecute {.fn sus_climate_aggregate} primero."
  ),
  err_outcome = list(
    pt = "Coluna de desfecho {.val {outcome_col}} nao encontrada. Colunas disponiveis: {.val {avail_cols}}.",
    en = "Outcome column {.val {outcome_col}} not found. Available columns: {.val {avail_cols}}.",
    es = "Columna de resultado {.val {outcome_col}} no encontrada. Columnas disponibles: {.val {avail_cols}}."
  ),
  err_no_lag_cols = list(
    pt = "Nenhuma coluna {.val {climate_col}_lagN} encontrada. Execute {.fn sus_climate_aggregate} com {.code temporal_strategy = 'distributed_lag'}.",
    en = "No {.val {climate_col}_lagN} column found. Run {.fn sus_climate_aggregate} with {.code temporal_strategy = 'distributed_lag'}.",
    es = "No se encontro la columna {.val {climate_col}_lagN}. Ejecute {.fn sus_climate_aggregate} con {.code temporal_strategy = 'distributed_lag'}."
  ),
  err_missing_lags = list(
    pt = "Colunas de lag ausentes: {.val {missing_str}}. Verifique {.arg lag_max}.",
    en = "Missing lag columns: {.val {missing_str}}. Check {.arg lag_max}.",
    es = "Columnas de lag faltantes: {.val {missing_str}}. Verifique {.arg lag_max}."
  ),
  err_bad_cov = list(
    pt = "Covariavel(is) nao encontrada(s): {.val {bad_cov_str}}.",
    en = "Covariate(s) not found: {.val {bad_cov_str}}.",
    es = "Covariable(s) no encontrada(s): {.val {bad_cov_str}}."
  ),
  err_insufficient_obs = list(
    pt = "Observacoes insuficientes apos drop_na: {n_obs} (minimo necessario: {min_need}). Verifique os dados de entrada.",
    en = "Insufficient observations after drop_na: {n_obs} (minimum needed: {min_need}). Check input data.",
    es = "Observaciones insuficientes tras drop_na: {n_obs} (minimo necesario: {min_need}). Verifique los datos de entrada."
  ),
  warn_type_not_dl = list(
    pt = "Tipo de dados {.val {current_type}} nao e {.val distributed_lag}. Verifique se as colunas {.val _lagN} estao presentes.",
    en = "Data type {.val {current_type}} is not {.val distributed_lag}. Check that {.val _lagN} columns are present.",
    es = "Tipo de datos {.val {current_type}} no es {.val distributed_lag}. Verifique que las columnas {.val _lagN} esten presentes."
  ),
  warn_negbin = list(
    pt = "Binomial negativa nao e suportada com {.fn dlnm::crossbasis}. Usando {.val quasipoisson} (alternativa robusta recomendada por Gasparrini, 2014).",
    en = "Negative binomial is not supported with {.fn dlnm::crossbasis}. Using {.val quasipoisson} (robust alternative recommended by Gasparrini, 2014).",
    es = "Binomial negativa no es soportada con {.fn dlnm::crossbasis}. Usando {.val quasipoisson} (alternativa robusta recomendada por Gasparrini, 2014)."
  ),
  warn_autocorr = list(
    pt = "Autocorrelacao residual detectada (Ljung-Box p = {p_lb}). Aumente {.arg ns_df} ou {.arg dof_per_year} para melhor controle temporal (Bhaskaran et al., 2013).",
    en = "Residual autocorrelation detected (Ljung-Box p = {p_lb}). Increase {.arg ns_df} or {.arg dof_per_year} for better temporal control (Bhaskaran et al., 2013).",
    es = "Autocorrelacion residual detectada (Ljung-Box p = {p_lb}). Aumente {.arg ns_df} o {.arg dof_per_year} para mejor control temporal (Bhaskaran et al., 2013)."
  ),
  warn_overdispersion = list(
    pt = "Razao de dispersao = {phi}. Superdispersao detectada; {.val quasipoisson} e adequado (Armstrong, 2006).",
    en = "Dispersion ratio = {phi}. Overdispersion detected; {.val quasipoisson} is appropriate (Armstrong, 2006).",
    es = "Razon de dispersion = {phi}. Superdispersion detectada; {.val quasipoisson} es adecuado (Armstrong, 2006)."
  ),
  warn_short_series = list(
    pt = "Serie curta ({n_days} dias / {n_yrs} ano(s)). Resultados podem ser pouco robustos com bases de alta complexidade.",
    en = "Short series ({n_days} days / {n_yrs} year(s)). Results may not be robust with high-complexity bases.",
    es = "Serie corta ({n_days} dias / {n_yrs} ano(s)). Los resultados pueden no ser robustos con bases de alta complejidad."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} nao suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Falling back to {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  ),
  info_var_detected = list(
    pt = "Variavel climatica detectada: {.val {climate_col}}",
    en = "Climate variable detected: {.val {climate_col}}",
    es = "Variable climatica detectada: {.val {climate_col}}"
  ),
  info_lagmax_detected = list(
    pt = "lag_max detectado automaticamente: {lag_max}",
    en = "lag_max auto-detected: {lag_max}",
    es = "lag_max detectado automaticamente: {lag_max}"
  ),
  info_ns_df_auto = list(
    pt = "ns_df calculado automaticamente: {ns_df} ({dof_per_year} gl/ano x {n_yrs} anos)",
    en = "ns_df auto-computed: {ns_df} ({dof_per_year} df/yr x {n_yrs} years)",
    es = "ns_df calculado automaticamente: {ns_df} ({dof_per_year} gl/ano x {n_yrs} anos)"
  ),
  info_ref_value = list(
    pt = "Valor de referencia (RR = 1): mediana = {ref_val}",
    en = "Reference value (RR = 1): median = {ref_val}",
    es = "Valor de referencia (RR = 1): mediana = {ref_val}"
  )
)

#' @keywords internal
#' @noRd
.mdll <- function(key, lang) {
  entry <- .mdl_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# ── Exported wrapper ──────────────────────────────────────────────────────────

#' Distributed Lag Non-linear Model (DLNM) for Climate-Health Analyses
#'
#' Fits a Distributed Lag Non-linear Model (DLNM) to quantify the association
#' between a climate exposure and a daily health outcome count. The function
#' accepts a `climasus_df` at stage `"climate"` / type `"distributed_lag"`
#' produced by `sus_climate_aggregate(temporal_strategy = "distributed_lag")`,
#' constructs the bidimensional `crossbasis`, fits a GLM, and returns a
#' `climasus_dlnm` object with the fitted model, pre-computed `crosspred`,
#' exposure-response and lag-response tables, and diagnostic statistics.
#'
#' @section Statistical framework:
#'
#' The DLNM models the log-linear association between an exposure history
#' \eqn{X_{t-0}, X_{t-1}, \ldots, X_{t-L}} and the daily count outcome
#' \eqn{Y_t \sim \text{Quasi-Poisson}(\mu_t)}:
#'
#' \deqn{\log(\mu_t) = \alpha + \mathbf{w}_t^\top \boldsymbol{\theta}
#'   + \sum_{k=1}^{K} \gamma_k s_k(t)}
#'
#' where \eqn{\mathbf{w}_t} is the cross-basis vector (outer product of the
#' exposure and lag basis matrices), and \eqn{s_k(t)} are natural splines
#' controlling seasonal confounding. The **cumulative overall effect** across
#' all lags at exposure level \eqn{x} is:
#'
#' \deqn{RR_{\text{cum}}(x) = \exp\!\left\{\sum_{l=0}^{L}
#'   [f(x,l) - f(x_0, l)]\right\}}
#'
#' where \eqn{x_0} is the reference value (default: sample median).
#'
#' @section Family selection:
#'
#' - **`"quasipoisson"`** (default, recommended): Robust to overdispersion
#'   (\eqn{\hat\phi > 1}). SE inflated by \eqn{\sqrt{\hat\phi}}.
#'   Recommended when `disp_ratio > 1.5` (Armstrong, 2006).
#' - **`"poisson"`**: Valid only when \eqn{\hat\phi \approx 1}. Provides
#'   AIC/BIC but underestimates SE under overdispersion.
#' - **`"negbin"`**: Redirects to `"quasipoisson"` with a warning —
#'   negative binomial is incompatible with `dlnm::crossbasis()`.
#'
#' @section Basis selection (`argvar`, `arglag`):
#'
#' | `argvar` | When to use |
#' |-----------|-------------|
#' | `list(fun = "ns", df = 3)` | Mild non-linearity; conservative |
#' | `list(fun = "ns", df = 4)` | U/J shape (heat+cold, default) |
#' | `list(fun = "ns", df = 5)` | Complex dose-response; large N |
#' | `list(fun = "lin")` | Linear hypothesis test |
#'
#' | `arglag` | When to use |
#' |----------|-------------|
#' | `list(fun = "ns", df = 3)` | Smooth decay; temperature mortality |
#' | `list(fun = "ns", df = 4)` | Bimodal lag; cold effect |
#' | `list(fun = "poly", df = 2)` | Conservative; small samples |
#'
#' @section Temporal confounding (`ns_df`, `dof_per_year`):
#'
#' A natural spline \eqn{\text{ns}(\text{date}, df)} controls long-term
#' trends and seasonality that confound the climate-health association
#' (Bhaskaran et al., 2013). When `ns_df = NULL` the function auto-computes
#' \eqn{df = \texttt{dof\_per\_year} \times n_{\text{years}}}. Recommended:
#' - `dof_per_year = 4`: 1-2 year series; captures annual seasonality.
#' - `dof_per_year = 6`: Subtropical Brazil; stronger seasonality.
#' - `dof_per_year = 8`: Arboviral diseases; sub-seasonal variation.
#' - `ns_df = NULL` with `dof_per_year = 0`: suppress time control
#'   (use only when exposure is already an anomaly from climatology).
#'
#' @param df A `climasus_df` at stage `"climate"` and type `"distributed_lag"`,
#'   produced by `sus_climate_aggregate(temporal_strategy = "distributed_lag")`.
#'   Must contain a `date` column (Date), the outcome column, and lag columns
#'   `{climate_col}_lag0`, ..., `{climate_col}_lag{L}`. Geometry columns
#'   (sf) are automatically dropped before modelling.
#' @param outcome_col Character. Name of the daily health count column
#'   (e.g., `"n_obitos"`, `"n_internacoes"`, `"n_dengue"`). Default:
#'   `"n_obitos"`.
#' @param climate_col Character or `NULL`. Base name of the climate exposure
#'   (without `_lagN` suffix, e.g., `"tair_dry_bulb_c"`). `NULL` auto-detects
#'   by priority: air temperature > rainfall > humidity > other known variables.
#' @param lag_max Integer or `NULL`. Maximum lag to include in the crossbasis.
#'   `NULL` auto-detects from the highest `N` present in `{climate_col}_lagN`
#'   columns.
#' @param covariates Character vector or `NULL`. Names of additional columns in
#'   `df` to include as linear confounders in the GLM formula (e.g.,
#'   `c("rainfall_mm", "rh_mean_porc")`). These are averaged across
#'   municipalities before fitting. Default: `NULL`.
#' @param argvar Named list. Arguments for the exposure dimension of the
#'   `crossbasis` (passed to `dlnm::crossbasis(argvar = ...)`). Default:
#'   `list(fun = "ns", df = 4)`. See **Basis selection** section.
#' @param arglag Named list. Arguments for the lag dimension of the
#'   `crossbasis` (passed to `dlnm::crossbasis(arglag = ...)`). Default:
#'   `list(fun = "ns", df = 3)`. See **Basis selection** section.
#' @param family Character. GLM family: `"quasipoisson"` (default, recommended),
#'   `"poisson"`, or `"negbin"` (redirected to `"quasipoisson"` with warning).
#' @param ns_df Integer or `NULL`. Degrees of freedom for the natural spline
#'   time trend (`splines::ns(date, df = ns_df)`). `NULL` (default) triggers
#'   automatic computation based on `dof_per_year`. Set to `0` to suppress
#'   time control entirely.
#' @param dof_per_year Integer. Degrees of freedom per year of data used for
#'   automatic `ns_df` computation. Default: `4L`. Ignored when `ns_df` is
#'   explicitly set.
#' @param ref_value Numeric or `NULL`. Exposure value where RR = 1 (the
#'   centring value for `dlnm::crosspred()`). `NULL` (default) uses the
#'   sample median of lag-0 exposure values.
#' @param pred_at Numeric vector of quantile probabilities (0-1) at which to
#'   report the cumulative exposure-response curve in `$exposure_response`.
#'   Default: `c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99)`.
#' @param alpha Numeric (0-1). Confidence level for intervals:
#'   CI = \eqn{(1 - \alpha)}. Default: `0.05` (95% CI).
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress steps and warnings. Default `TRUE`.
#'
#' @return A `climasus_dlnm` list with the following components:
#'   \describe{
#'     \item{`$model`}{The fitted `glm` object. Includes the `crossbasis`
#'       in its model frame — compatible with `dlnm::crosspred()`.}
#'     \item{`$crossbasis`}{The `dlnm::crossbasis` object encoding both
#'       the exposure and lag dimensions.}
#'     \item{`$pred`}{A `dlnm::crosspred` object computed on a 100-point
#'       grid from the 1st to 99th percentile of exposure. Contains
#'       `allRRfit`, `allRRlow`, `allRRhigh` (cumulative overall effect)
#'       and `matRRfit`, `matRRlow`, `matRRhigh` (lag-specific effects).}
#'     \item{`$exposure_response`}{Tibble with cumulative RR at the
#'       percentiles specified in `pred_at`: columns `pct`, `exposure`,
#'       `rr`, `lo`, `hi`.}
#'     \item{`$lag_response`}{Tibble with lag-specific RR at the 75th
#'       percentile of exposure (for `dlnm_lag` plots): columns `lag`,
#'       `rr`, `lo`, `hi`, `rr_cum`.}
#'     \item{`$models`}{One-row summary tibble for integration with
#'       `plot_climate_health(fit = ...)`: `variable`, `n`, `family`,
#'       `lag_max`, `ref_value`, `exposure_p75`, `rr`, `lo`, `hi`,
#'       `lag_peak`, `disp_ratio`, `aic_poisson`.}
#'     \item{`$data_daily`}{Aggregated daily tibble used for model
#'       fitting (outcome summed; climate averaged across municipalities).}
#'     \item{`$diagnostics`}{List: `disp_ratio`, `disp_category`,
#'       `autocorr_pval`, `has_autocorr`, `aic_poisson`, `deviance`,
#'       `null_deviance`, `df_residual`.}
#'     \item{`$meta`}{List of modelling parameters plus the input
#'       `sus_meta` history for pipeline tracing.}
#'   }
#'   The object also carries `attr(result, "sus_meta")` with an updated
#'   history entry.
#'
#' @references
#' Gasparrini, A., Armstrong, B., & Kenward, M.G. (2010). Distributed lag
#' non-linear models. *Statistics in Medicine*, 29(21), 2224-2234.
#' \doi{10.1002/sim.3940}
#'
#' Gasparrini, A. (2011). Distributed lag linear and non-linear models in R:
#' the package dlnm. *Journal of Statistical Software*, 43(8), 1-20.
#' \doi{10.18637/jss.v043.i08}
#'
#' Gasparrini, A., Armstrong, B., & Kenward, M.G. (2014). Reducing and
#' meta-analysing estimates from distributed lag non-linear models.
#' *BMC Medical Research Methodology*, 14, 70. \doi{10.1186/1471-2288-14-70}
#'
#' Armstrong, B. (2006). Models for the relationship between ambient
#' temperature and daily mortality. *Epidemiology*, 17(6), 624-631.
#'
#' Bhaskaran, K., Gasparrini, A., Hajat, S., Smeeth, L., & Armstrong, B.
#' (2013). Time series regression studies in environmental epidemiology.
#' *International Journal of Epidemiology*, 42(4), 1187-1195.
#' \doi{10.1093/ije/dyt092}
#'
#' Perkins, S.E. (2015). A review on the scientific understanding of
#' heatwaves. *Atmospheric Research*, 164-165, 242-267.
#'
#' @examples
#' \dontrun{
#' # Step 1: produce distributed_lag exposure data
#' df_dl <- sus_climate_aggregate(
#'   health_data       = sf_sim_spatial,
#'   climate_data      = df_inmet_filled,
#'   climate_var       = "tair_dry_bulb_c",
#'   temporal_strategy = "distributed_lag",
#'   lag_days          = 21,
#'   lang              = "pt"
#' )
#'
#' # Step 2: fit DLNM
#' fit <- sus_mod_dlnm(
#'   df          = df_dl,
#'   outcome_col = "n_obitos",
#'   lag_max     = 21,
#'   argvar      = list(fun = "ns", df = 4),
#'   arglag      = list(fun = "ns", df = 3),
#'   family      = "quasipoisson",
#'   dof_per_year = 4L,
#'   lang        = "pt"
#' )
#'
#' print(fit)
#' fit$exposure_response   # RR cumulative at percentile grid
#' fit$lag_response        # RR by lag at p75
#' fit$diagnostics$disp_ratio
#'
#' # Step 3: visualise (requires plot_climate_health)
#' # plots <- plot_climate_health(
#' #   data = df_dl, fit = fit,
#' #   plot_type = c("dlnm_surface", "dlnm_overall", "dlnm_lag")
#' # )
#' }
#'
#' @seealso [sus_climate_aggregate()], [sus_climate_compute_heatwaves()]
#'
#' @export
#' @importFrom dplyr mutate select filter arrange summarise across any_of
#'   all_of rename n_distinct
#' @importFrom tidyr drop_na
#' @importFrom purrr map list_rbind
#' @importFrom lubridate year
#' @importFrom cli cli_h1 cli_h2 cli_rule cli_text cli_alert_info
#'   cli_alert_success cli_alert_warning cli_abort cli_progress_step
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom stats glm poisson quasipoisson formula median quantile AIC
#'   residuals Box.test as.formula
sus_mod_dlnm <- function(
    df,
    outcome_col  = "n_obitos",
    climate_col  = NULL,
    lag_max      = NULL,
    covariates   = NULL,
    argvar       = list(fun = "ns", df = 4),
    arglag       = list(fun = "ns", df = 3),
    family       = "quasipoisson",
    ns_df        = NULL,
    dof_per_year = 4L,
    ref_value    = NULL,
    pred_at      = c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
    alpha        = 0.05,
    lang         = "pt",
    verbose      = TRUE
) {
  # ── Package checks ─────────────────────────────────────────────────────────
  rlang::check_installed(
    c("dlnm", "splines", "glue"),
    reason = "to run sus_mod_dlnm()"
  )

  # ── Language validation ────────────────────────────────────────────────────
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(.mdll("warn_lang", "pt"))
    lang <- "pt"
  }

  # ── Family validation ──────────────────────────────────────────────────────
  family <- match.arg(family, c("quasipoisson", "poisson", "negbin"))
  if (identical(family, "negbin")) {
    cli::cli_alert_warning(.mdll("warn_negbin", lang))
    family <- "quasipoisson"
  }

  # ── Arrow materialisation ──────────────────────────────────────────────────
  if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
    meta_backup <- tryCatch(sus_meta(df), error = function(e) list())
    df <- dplyr::collect(df)
    if (length(meta_backup) > 0L) df <- new_climasus_df(df, meta_backup)
  }

  # ── climasus_df validation ─────────────────────────────────────────────────
  if (!inherits(df, "climasus_df")) {
    cli::cli_abort(.mdll("err_not_climasus", lang))
  }

  current_stage <- sus_meta(df, "stage")
  if (!identical(current_stage, "climate")) {
    cli::cli_abort(.mdll("err_stage", lang))
  }

  current_type <- sus_meta(df, "type")
  if (!identical(current_type, "distributed_lag")) {
    cli::cli_warn(.mdll("warn_type_not_dl", lang))
  }

  # ── Step 1: Validate inputs ────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.mdll("step_validate", lang))

  if (!outcome_col %in% names(df)) {
    avail_cols <- paste(head(names(df), 10L), collapse = ", ")
    cli::cli_abort(.mdll("err_outcome", lang))
  }

  # Drop sf geometry
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)

  # Detect climate_col
  if (is.null(climate_col)) {
    climate_col <- .sdl_detect_base_var(df)
    if (verbose) cli::cli_alert_info(.mdll("info_var_detected", lang))
  }

  # Discover and validate lag columns
  lag_cols <- grep(paste0("^", climate_col, "_lag\\d+$"), names(df), value = TRUE)
  if (length(lag_cols) == 0L) {
    cli::cli_abort(.mdll("err_no_lag_cols", lang))
  }

  if (is.null(lag_max)) {
    lag_nums <- as.integer(gsub(paste0("^", climate_col, "_lag"), "", lag_cols))
    lag_max  <- max(lag_nums, na.rm = TRUE)
    if (verbose) cli::cli_alert_info(.mdll("info_lagmax_detected", lang))
  }

  lag_col_names <- paste0(climate_col, "_lag", seq(0L, lag_max))
  missing_lags  <- setdiff(lag_col_names, names(df))
  if (length(missing_lags) > 0L) {
    missing_str <- paste(missing_lags, collapse = ", ")
    cli::cli_abort(.mdll("err_missing_lags", lang))
  }

  # Validate covariates
  if (!is.null(covariates)) {
    bad_cov <- setdiff(covariates, names(df))
    if (length(bad_cov) > 0L) {
      bad_cov_str <- paste(bad_cov, collapse = ", ")
      cli::cli_abort(.mdll("err_bad_cov", lang))
    }
  }

  # ── Step 2: Aggregate by date ──────────────────────────────────────────────
  n <- nrow(df)
  if (verbose) cli::cli_progress_step(.mdll("step_aggregate", lang))

  df_agg <- df |>
    dplyr::rename(y = dplyr::all_of(outcome_col)) |>
    dplyr::select(
      date, y,
      dplyr::all_of(lag_col_names),
      dplyr::any_of(covariates)
    ) |>
    dplyr::summarise(
      y = sum(y, na.rm = TRUE),
      dplyr::across(dplyr::all_of(lag_col_names), ~ mean(.x, na.rm = TRUE)),
      dplyr::across(dplyr::any_of(covariates),   ~ mean(.x, na.rm = TRUE)),
      .by = date
    ) |>
    dplyr::arrange(date) |>
    tidyr::drop_na(y, dplyr::all_of(lag_col_names))

  n_obs     <- nrow(df_agg)
  min_need  <- lag_max + 10L
  if (n_obs < min_need) {
    cli::cli_abort(.mdll("err_insufficient_obs", lang))
  }

  # Warn if series is short
  n_days <- as.numeric(diff(range(df_agg$date, na.rm = TRUE)))
  n_yrs  <- round(n_days / 365.25, 1)
  if (n_days < 365L) {
    cli::cli_alert_warning(.mdll("warn_short_series", lang))
  }

  # ── Step 3: Exposure matrix ────────────────────────────────────────────────
  expo_matrix  <- as.matrix(dplyr::select(df_agg, dplyr::all_of(lag_col_names)))
  colnames(expo_matrix) <- as.character(seq(0L, lag_max))

  # ── Step 4: Crossbasis ────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.mdll("step_crossbasis", lang))

  cb <- dlnm::crossbasis(
    expo_matrix,
    lag    = lag_max,
    argvar = argvar,
    arglag = arglag
  )

  # ── Step 5: Auto ns_df from dof_per_year ─────────────────────────────────
  if (is.null(ns_df) && !is.null(dof_per_year) && dof_per_year > 0L) {
    ns_df <- .sdl_compute_ns_df(df_agg, dof_per_year)
    if (verbose) cli::cli_alert_info(.mdll("info_ns_df_auto", lang))
  }

  # ── Step 6: Reference value ───────────────────────────────────────────────
  if (is.null(ref_value)) {
    ref_value <- stats::median(expo_matrix[, 1L], na.rm = TRUE)
    ref_val   <- round(ref_value, 2)
    if (verbose) cli::cli_alert_info(.mdll("info_ref_value", lang))
  }

  # ── Step 7: GLM formula ───────────────────────────────────────────────────
  rhs <- "cb"
  if (!is.null(ns_df) && ns_df > 0L) {
    rhs <- paste0(rhs, " + splines::ns(as.integer(date), df = ", ns_df, ")")
  }
  if (!is.null(covariates) && length(covariates) > 0L) {
    rhs <- paste0(rhs, " + ", paste(covariates, collapse = " + "))
  }
  formula_obj <- stats::as.formula(paste("y ~", rhs))

  # ── Step 8: Fit GLM ───────────────────────────────────────────────────────
  fam_obj <- if (identical(family, "poisson")) stats::poisson() else stats::quasipoisson()
  fam     <- toupper(family)
  if (verbose) cli::cli_progress_step(.mdll("step_fit", lang))

  model <- stats::glm(formula_obj, data = df_agg, family = fam_obj)

  # ── Step 9: Crosspred on a 100-point exposure grid ────────────────────────
  expo_range <- stats::quantile(expo_matrix[, 1L], c(0.01, 0.99), na.rm = TRUE)
  expo_grid  <- seq(expo_range[[1L]], expo_range[[2L]], length.out = 100L)
  n_grid     <- length(expo_grid)

  if (verbose) cli::cli_progress_step(.mdll("step_crosspred", lang))

  pred <- dlnm::crosspred(
    cb,
    model,
    at       = expo_grid,
    bylag    = 1L,
    cumul    = TRUE,
    cen      = ref_value,
    ci.level = 1 - alpha
  )

  # ── Step 10: Model diagnostics ────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.mdll("step_diagnostics", lang))

  diagnostics <- .sdl_diagnostics(model, df_agg, alpha)

  p_lb <- round(diagnostics$autocorr_pval, 4L)
  phi  <- round(diagnostics$disp_ratio, 2L)

  if (verbose) {
    if (isTRUE(diagnostics$has_autocorr)) {
      cli::cli_alert_warning(.mdll("warn_autocorr", lang))
    }
    if (diagnostics$disp_ratio > 1.5) {
      cli::cli_alert_info(.mdll("warn_overdispersion", lang))
    }
  }

  # ── Step 11: Output tables ────────────────────────────────────────────────
  expo_pcts    <- stats::quantile(expo_matrix[, 1L], pred_at, na.rm = TRUE)
  expo_resp    <- .sdl_exposure_response_tbl(pred, expo_pcts, pred_at)

  p75_val <- stats::quantile(expo_matrix[, 1L], 0.75, na.rm = TRUE)
  p75_idx <- which.min(abs(as.numeric(pred$predvar) - p75_val))
  lag_resp <- .sdl_lag_response_tbl(pred, p75_idx)

  models_tbl <- .sdl_models_tbl(
    pred        = pred,
    df_agg      = df_agg,
    climate_col = climate_col,
    lag_max     = lag_max,
    family      = family,
    disp_ratio  = diagnostics$disp_ratio,
    aic_poisson = diagnostics$aic_poisson,
    p75_idx     = p75_idx,
    expo_matrix = expo_matrix
  )

  # ── Finalise result with sus_meta history ─────────────────────────────────
  rr    <- round(models_tbl$rr, 4L)
  lo    <- round(models_tbl$lo, 4L)
  hi    <- round(models_tbl$hi, 4L)
  lag_pk <- models_tbl$lag_peak

  if (verbose) {
    cli::cli_alert_success(.mdll("done", lang))
  }

  ts         <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  input_meta <- sus_meta(df)
  input_meta[["history"]] <- c(
    input_meta[["history"]],
    sprintf(
      "[%s] sus_mod_dlnm(): var=%s; lag_max=%d; family=%s; ns_df=%s; n=%d; RR_p75=%.4f [%.4f,%.4f]; lag_peak=%d",
      ts, climate_col, lag_max, family,
      if (is.null(ns_df)) "NULL" else as.character(ns_df),
      n_obs,
      models_tbl$rr, models_tbl$lo, models_tbl$hi,
      models_tbl$lag_peak
    )
  )

  result <- structure(
    list(
      model             = model,
      crossbasis        = cb,
      pred              = pred,
      exposure_response = expo_resp,
      lag_response      = lag_resp,
      models            = models_tbl,
      data_daily        = df_agg,
      diagnostics       = diagnostics,
      meta              = list(
        climate_col  = climate_col,
        outcome_col  = outcome_col,
        lag_max      = lag_max,
        ref_value    = ref_value,
        argvar       = argvar,
        arglag       = arglag,
        family       = family,
        ns_df        = ns_df,
        dof_per_year = dof_per_year,
        n            = n_obs,
        n_days       = n_days,
        alpha        = alpha,
        pred_at      = pred_at,
        call_time    = Sys.time(),
        input_meta   = input_meta
      )
    ),
    class = c("climasus_dlnm", "list")
  )
  attr(result, "sus_meta") <- input_meta
  result
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_dlnm model
#'
#' @param x A `climasus_dlnm` object from `sus_mod_dlnm()`.
#' @param ... Unused.
#' @return `x` invisibly.
#' @export
print.climasus_dlnm <- function(x, ...) {
  cli::cli_h2("climasus_dlnm")
  cli::cli_text("{.strong Exposicao} : {x$meta$climate_col}  (lag 0-{x$meta$lag_max})")
  cli::cli_text("{.strong Desfecho}  : {x$meta$outcome_col}")
  cli::cli_text("{.strong Familia}   : {x$meta$family}")
  cli::cli_text("{.strong N obs}     : {x$meta$n}")
  cli::cli_text("{.strong Ref value} : {round(x$meta$ref_value, 3)}")
  cli::cli_text("{.strong ns_df}     : {x$meta$ns_df %||% 'NULL (sem controle temporal)'}")
  cli::cli_rule()

  m <- x$models
  cli::cli_text(
    "RR cumulativo (p75 vs ref): {round(m$rr,4)} [{round(m$lo,4)}, {round(m$hi,4)}]"
  )
  cli::cli_text("Lag de efeito pico : {m$lag_peak} dias")
  cli::cli_text(
    "Dispersao          : {round(x$diagnostics$disp_ratio, 2)} ({x$diagnostics$disp_category})"
  )
  if (!is.na(x$diagnostics$aic_poisson))
    cli::cli_text("AIC (Poisson)      : {round(x$diagnostics$aic_poisson, 1)}")
  if (isTRUE(x$diagnostics$has_autocorr))
    cli::cli_alert_warning(
      "Autocorrelacao residual detectada (Ljung-Box p = {round(x$diagnostics$autocorr_pval, 4)})."
    )
  invisible(x)
}

#' Summarise a climasus_dlnm model
#'
#' @param object A `climasus_dlnm` object from `sus_mod_dlnm()`.
#' @param ... Unused.
#' @return `object` invisibly.
#' @export
summary.climasus_dlnm <- function(object, ...) {
  print(object)
  cat("\n-- Exposure-Response (cumulative RR at percentile grid) --\n")
  print(object$exposure_response)
  cat("\n-- Lag-Response (RR at 75th percentile of exposure) --\n")
  print(object$lag_response)
  cat("\n-- crossbasis --\n")
  print(summary(object$crossbasis))
  cat("\n-- GLM --\n")
  print(summary(object$model))
  invisible(object)
}

#' Extract coefficients from a climasus_dlnm model
#'
#' @param object A `climasus_dlnm` object.
#' @param ... Passed to [stats::coef()].
#' @return Named numeric vector of GLM coefficients.
#' @export
coef.climasus_dlnm <- function(object, ...) {
  stats::coef(object$model, ...)
}

#' Reduce climasus_dlnm to a tidy one-row tibble
#'
#' Convenience extractor compatible with `dplyr::bind_rows()` for pooling
#' results across multiple model runs.
#'
#' @param x A `climasus_dlnm` object.
#' @param ... Unused.
#' @return A one-row tibble with key model statistics.
#' @export
#' @importFrom generics tidy
tidy.climasus_dlnm <- function(x, ...) {
  m <- x$models
  tibble::tibble(
    climate_col  = x$meta$climate_col,
    outcome_col  = x$meta$outcome_col,
    lag_max      = x$meta$lag_max,
    family       = x$meta$family,
    ns_df        = x$meta$ns_df,
    n            = x$meta$n,
    ref_value    = round(x$meta$ref_value, 3),
    rr           = m$rr,
    lo           = m$lo,
    hi           = m$hi,
    lag_peak     = m$lag_peak,
    disp_ratio   = m$disp_ratio,
    aic_poisson  = x$diagnostics$aic_poisson,
    has_autocorr = x$diagnostics$has_autocorr
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' @keywords internal
#' @noRd
.sdl_detect_base_var <- function(df) {
  lag_cols  <- grep("_lag\\d+$", names(df), value = TRUE)
  if (length(lag_cols) == 0L)
    cli::cli_abort("No _lagN columns found. Run sus_climate_aggregate() with temporal_strategy = 'distributed_lag'.")
  base_vars  <- unique(sub("_lag\\d+$", "", lag_cols))
  known_vars <- c(
    "tair_dry_bulb_c", "tair_max_c", "tair_min_c",
    "rainfall_mm", "rh_mean_porc", "patm_mb",
    "sr_kj_m2", "wd_degrees", "ws_2_m_s",
    "utci_c", "wbgt_c", "hi_c", "pet_c",
    "diurnal_range_c", "vapor_pressure_kpa"
  )
  preferred <- intersect(known_vars, base_vars)
  if (length(preferred) > 0L) return(preferred[1L])
  base_vars[1L]
}

#' @keywords internal
#' @noRd
.sdl_compute_ns_df <- function(df_agg, dof_per_year) {
  n_yrs <- as.numeric(diff(range(df_agg$date, na.rm = TRUE))) / 365.25
  max(dof_per_year, as.integer(round(dof_per_year * n_yrs)))
}

#' @keywords internal
#' @noRd
.sdl_diagnostics <- function(model, df_agg, alpha) {
  resid_dev  <- stats::residuals(model, type = "deviance")
  disp_ratio <- model$deviance / model$df.residual

  disp_cat <- if (disp_ratio < 1.5)       "adequate"
              else if (disp_ratio < 3.0)  "moderate"
              else                         "high"

  bt <- tryCatch(
    stats::Box.test(resid_dev, lag = 10L, type = "Ljung-Box"),
    error = function(e) NULL
  )

  # AIC via Poisson re-fit (quasi has no log-likelihood; Winkelmann 2008)
  aic_pois <- tryCatch({
    m_pois <- stats::glm(
      stats::formula(model),
      data   = df_agg,
      family = stats::poisson()
    )
    stats::AIC(m_pois)
  }, error = function(e) NA_real_)

  list(
    disp_ratio    = disp_ratio,
    disp_category = disp_cat,
    autocorr_pval = if (!is.null(bt)) bt$p.value else NA_real_,
    autocorr_test = bt,
    has_autocorr  = if (!is.null(bt)) bt$p.value < alpha else NA,
    aic_poisson   = aic_pois,
    deviance      = model$deviance,
    null_deviance = model$null.deviance,
    df_residual   = model$df.residual
  )
}

#' @keywords internal
#' @noRd
.sdl_exposure_response_tbl <- function(pred, expo_pcts, pred_at) {
  purrr::map(seq_along(expo_pcts), function(i) {
    val <- expo_pcts[[i]]
    idx <- which.min(abs(as.numeric(pred$predvar) - val))
    tibble::tibble(
      pct      = pred_at[[i]],
      exposure = round(val, 3L),
      rr       = pred$allRRfit[[idx]],
      lo       = pred$allRRlow[[idx]],
      hi       = pred$allRRhigh[[idx]]
    )
  }) |> list_rbind()
}

#' @keywords internal
#' @noRd
.sdl_lag_response_tbl <- function(pred, p75_idx) {
  rr_lag <- as.numeric(pred$matRRfit[p75_idx, ])
  lo_lag <- as.numeric(pred$matRRlow[p75_idx,  ])
  hi_lag <- as.numeric(pred$matRRhigh[p75_idx, ])

  # pred$lag is a 2-element range c(lag_min, lag_max), not the full sequence
  lag_seq <- as.integer(seq(pred$lag[1L], pred$lag[2L]))

  tibble::tibble(
    lag    = lag_seq,
    rr     = rr_lag,
    lo     = lo_lag,
    hi     = hi_lag,
    rr_cum = cumprod(rr_lag)
  )
}

#' @keywords internal
#' @noRd
.sdl_models_tbl <- function(pred, df_agg, climate_col, lag_max,
                              family, disp_ratio, aic_poisson,
                              p75_idx, expo_matrix) {
  p75_val  <- stats::quantile(expo_matrix[, 1L], 0.75, na.rm = TRUE)
  lag_rr   <- abs(log(as.numeric(pred$matRRfit[p75_idx, ])))
  # pred$lag is c(lag_min, lag_max); reconstruct the full sequence to index into
  lag_seq  <- as.integer(seq(pred$lag[1L], pred$lag[2L]))
  lag_peak <- lag_seq[which.max(lag_rr)]

  tibble::tibble(
    variable     = climate_col,
    n            = nrow(df_agg),
    family       = family,
    lag_max      = lag_max,
    ref_value    = round(as.numeric(pred$cen), 3L),
    exposure_p75 = round(p75_val, 3L),
    rr           = pred$allRRfit[[p75_idx]],
    lo           = pred$allRRlow[[p75_idx]],
    hi           = pred$allRRhigh[[p75_idx]],
    lag_peak     = lag_peak,
    disp_ratio   = round(disp_ratio, 3L),
    aic_poisson  = round(aic_poisson, 1L)
  )
}
